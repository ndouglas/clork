(ns clork.planner.actions
  "Action schema and extraction for GOAP planning.

   This module defines a unified action schema for all game actions and
   provides functions to extract actions from game state automatically.

   Action Schema:
   {:id          :action-id
    :type        :movement | :take | :puzzle | :combat | :deposit
    :preconditions
      {:here      :room-id        ; must be in this room (optional)
       :inventory #{:items}       ; must have these items
       :flags     #{:flags}}      ; these flags must be set
    :effects
      {:flags-set     #{:flags}   ; flags to set
       :flags-clear   #{:flags}   ; flags to clear
       :inventory-add #{:items}   ; items added to inventory
       :inventory-remove #{:items}} ; items removed from inventory
    :cost         1               ; estimated moves
    :reversible?  true            ; can action be undone?
    :commands     [\"cmd\" ...]}" ; actual game commands
  (:require [clojure.set :as set]
            [clojure.string :as str]))

;; =============================================================================
;; Combat Markov Chain Model
;; =============================================================================
;; Models combat as a Markov chain where states are villain health levels.
;; This gives exact expected attack counts based on the ZIL combat tables.
;;
;; State space for villain with max health H:
;;   H, H-1, H-2, ..., 1, 0 (dead), :unconscious
;;   States 0 and :unconscious are "absorbing" - combat ends
;;   From :unconscious, next attack is guaranteed kill
;;
;; Player fight-strength = 2 + (score / 70), ranging 2-7
;; Villain strengths: Troll = 2, Thief = 5

(def combat-outcome-tables
  "Probability distributions for combat outcomes by table.
   Values are [p-miss p-stagger p-light-wound p-serious-wound p-unconscious p-killed]
   Based on actual DEF table entries in combat.clj"
  {:def2a [0.50 0.20 0.20 0.00 0.10 0.00]   ; Troll at low scores (strength <= 2 vs 2)
   :def2b [0.25 0.17 0.25 0.00 0.08 0.25]   ; Troll at higher scores (strength > 3 vs 2)
   :def3a [0.45 0.18 0.18 0.18 0.00 0.00]   ; Thief, big disadvantage (diff <= -2)
   :def3b [0.27 0.18 0.27 0.27 0.00 0.00]   ; Thief, slight disadvantage (diff -1 to 0)
   :def3c [0.10 0.20 0.40 0.30 0.00 0.00]}) ; Thief, advantage (diff >= 1)

(defn fight-strength
  "Calculate player's fighting strength from score.
   Formula: 2 + (score / 70), clamped to 2-7"
  [score]
  (min 7 (max 2 (+ 2 (int (/ score 70))))))

(defn select-combat-table
  "Select which DEF table to use based on defender and attacker strengths."
  [defender-strength attacker-strength]
  (cond
    ;; Troll (strength 2) uses DEF2
    (<= defender-strength 2)
    (if (> attacker-strength 3) :def2b :def2a)

    ;; Thief (strength 5) uses DEF3
    :else
    (let [diff (- attacker-strength defender-strength)]
      (cond
        (<= diff -2) :def3a
        (<= diff 0) :def3b
        :else :def3c))))

;; =============================================================================
;; Markov Chain State Transitions
;; =============================================================================

(defn combat-transitions
  "Get transition probabilities from a health state.
   Returns map of {new-state probability}.

   Special states:
   - 0 = dead (absorbing)
   - :unconscious = knocked out, next hit kills (absorbing for this model)

   From health h > 0:
   - Stay at h: miss or stagger (no damage)
   - Go to h-1: light wound
   - Go to h-2: serious wound (or 0 if h < 2)
   - Go to :unconscious: unconscious result
   - Go to 0: killed result"
  [health table-key]
  (if (<= health 0)
    ;; Absorbing state - stay here
    {health 1.0}
    (let [[p-miss p-stagger p-light p-serious p-uncon p-killed]
          (get combat-outcome-tables table-key)
          p-no-damage (+ p-miss p-stagger)]
      (cond-> {health p-no-damage}  ; Stay at current health

        ;; Light wound: h -> h-1 (or 0 if h=1)
        (pos? p-light)
        (assoc (max 0 (- health 1)) p-light)

        ;; Serious wound: h -> h-2 (or 0 if h<=2)
        (pos? p-serious)
        (assoc (max 0 (- health 2)) p-serious)

        ;; Unconscious: h -> :unconscious
        (pos? p-uncon)
        (assoc :unconscious p-uncon)

        ;; Killed: h -> 0
        (pos? p-killed)
        (update 0 (fnil + 0) p-killed)))))

(defn merge-transitions
  "Merge transition probabilities, summing duplicates."
  [trans]
  (reduce (fn [acc [state prob]]
            (update acc state (fnil + 0) prob))
          {}
          trans))

;; =============================================================================
;; Expected Attacks Calculation (Markov Chain Analysis)
;; =============================================================================

(defn expected-attacks-markov
  "Calculate exact expected attacks to kill villain using Markov chain analysis.

   Uses iterative value computation with self-loop correction:
   E[h] = (1 + sum over OTHER states of P(h->h') * E[h']) / (1 - P(stay at h))

   This accounts for the fact that misses/staggers cause you to stay at the
   same health state, creating a self-loop in the Markov chain.

   For absorbing states (0, :unconscious): E = 0
   For :unconscious, we add 1 for the finishing blow.

   Parameters:
   - max-health: Villain's starting health
   - table-key: Which DEF table to use

   Returns: Expected number of attacks from full health to dead"
  [max-health table-key]
  (let [;; Initialize expected values: 0 for absorbing states
        ;; For :unconscious, it takes 1 more attack to finish
        init-expected {0 0.0, :unconscious 1.0}

        ;; Iteratively compute expected values from low health to high
        ;; E[h] = (1 + sum over other states of P(h->h') * E[h']) / (1 - P(stay))
        expected
        (reduce
         (fn [exp health]
           (let [trans (combat-transitions health table-key)
                 ;; Separate self-loop probability from transitions to other states
                 p-stay (get trans health 0.0)
                 p-leave (- 1.0 p-stay)
                 ;; Sum expected values for transitions to OTHER states only
                 other-sum (reduce-kv
                            (fn [sum next-state prob]
                              (if (= next-state health)
                                sum  ; Skip self-loop
                                (+ sum (* prob (get exp next-state 0.0)))))
                            0.0
                            trans)
                 ;; E[h] = (1 + other-sum) / p-leave
                 ;; If p-leave is 0, combat would never end (shouldn't happen)
                 e-value (if (pos? p-leave)
                           (/ (+ 1.0 other-sum) p-leave)
                           Double/POSITIVE_INFINITY)]
             (assoc exp health e-value)))
         init-expected
         (range 1 (inc max-health)))]

    (get expected max-health)))

(defn combat-variance-markov
  "Calculate variance in attacks needed using second moment.

   Var[h] = E[attacks^2 from h] - E[attacks from h]^2

   Both E[X] and E[X^2] must account for self-loops:
   E[X|h] = (1 + sum over others of P(h->h') * E[X|h']) / (1 - P(stay))
   E[X^2|h] = (1 + 2*other-e1-sum + other-e2-sum) / (1 - P(stay))

   Returns: {:expected n :variance v :std-dev s}"
  [max-health table-key]
  (let [init-e2 {0 0.0, :unconscious 1.0}  ; E[X^2] for absorbing states
        init-e1 {0 0.0, :unconscious 1.0}  ; E[X] for absorbing states

        [e1-map e2-map]
        (reduce
         (fn [[e1 e2] health]
           (let [trans (combat-transitions health table-key)
                 p-stay (get trans health 0.0)
                 p-leave (- 1.0 p-stay)

                 ;; E[X] from this state (excluding self-loop)
                 other-e1-sum (reduce-kv
                               (fn [sum ns prob]
                                 (if (= ns health)
                                   sum
                                   (+ sum (* prob (get e1 ns 0.0)))))
                               0.0
                               trans)
                 e1-val (if (pos? p-leave)
                          (/ (+ 1.0 other-e1-sum) p-leave)
                          Double/POSITIVE_INFINITY)

                 ;; E[X^2] = (1 + 2*other-e1-sum + other-e2-sum) / p-leave
                 other-e2-sum (reduce-kv
                               (fn [sum ns prob]
                                 (if (= ns health)
                                   sum
                                   (let [e1-next (get e1 ns 0.0)
                                         e2-next (get e2 ns 0.0)]
                                     (+ sum (* prob (+ (* 2.0 e1-next) e2-next))))))
                               0.0
                               trans)
                 e2-val (if (pos? p-leave)
                          (/ (+ 1.0 other-e2-sum) p-leave)
                          Double/POSITIVE_INFINITY)]
             [(assoc e1 health e1-val)
              (assoc e2 health e2-val)]))
         [init-e1 init-e2]
         (range 1 (inc max-health)))

        e1 (get e1-map max-health)
        e2 (get e2-map max-health)
        variance (- e2 (* e1 e1))]

    {:expected e1
     :variance variance
     :std-dev (Math/sqrt (max 0 variance))}))

;; =============================================================================
;; Combat Cost Functions (using Markov model)
;; =============================================================================

(defn estimate-combat-moves
  "Estimate total moves for combat including retries from player death.

   Uses Markov chain for exact expected attacks, then adds retry overhead.

   Parameters:
   - enemy-strength: Villain's strength (troll=2, thief=5)
   - player-score: Player's current score (affects fight-strength)
   - base-retry-cost: Moves to return to combat location after death
   - p-player-death-per-round: Probability player dies each combat round

   Returns: {:expected-attacks n :std-dev s :retry-overhead m :total-moves t}"
  [enemy-strength player-score base-retry-cost p-player-death-per-round]
  (let [player-str (fight-strength player-score)
        table-key (select-combat-table enemy-strength player-str)

        ;; Use Markov chain for exact expected attacks
        {:keys [expected std-dev]} (combat-variance-markov enemy-strength table-key)

        ;; Expected player deaths during combat
        ;; Geometric: E[deaths] ≈ p-death * expected-attacks / (1 - cumulative-kill-prob)
        ;; Simplified: E[deaths] = p-death * expected-attacks (assuming we eventually win)
        expected-deaths (* p-player-death-per-round expected)

        ;; Each death costs: return trip + partial re-fight (average half the combat)
        retry-overhead (* expected-deaths (+ base-retry-cost (* 0.5 expected)))]

    {:expected-attacks (Math/ceil expected)
     :std-dev std-dev
     :retry-overhead (Math/ceil retry-overhead)
     :total-moves (Math/ceil (+ expected retry-overhead))}))

(defn troll-combat-cost
  "Estimate moves to kill the troll using Markov chain model.
   Troll has strength 2. At score 0, player has strength 2 (even match)."
  [player-score]
  (let [result (estimate-combat-moves
                2                    ; troll strength
                player-score
                15                   ; moves to return after death
                0.10)]               ; ~10% chance player dies per round vs troll
    (:total-moves result)))

(defn thief-combat-cost
  "Estimate moves to kill the thief using Markov chain model.
   Thief has strength 5, much harder than troll.
   At score 0 (strength 2 vs 5), combat is extremely difficult.
   At score 210 (strength 5 vs 5), even match.
   At score 280+ (strength 6+ vs 5), player has advantage.

   Note: Thief fights to death in treasure room (no flee)."
  [player-score]
  (let [result (estimate-combat-moves
                5                    ; thief strength
                player-score
                20                   ; moves to return after death
                (cond               ; death probability varies by score
                  (< player-score 70) 0.25    ; Very risky at low scores
                  (< player-score 210) 0.15   ; Moderate risk
                  :else 0.08))]               ; Low risk at high scores
    (:total-moves result)))

;; =============================================================================
;; Combat Analysis Utilities
;; =============================================================================

(defn analyze-combat
  "Analyze combat statistics for a given matchup.
   Useful for debugging and understanding combat dynamics.

   Returns detailed breakdown including:
   - Expected attacks and standard deviation
   - Health state probabilities after N attacks
   - Recommended score for reliable combat"
  [villain-strength player-score]
  (let [player-str (fight-strength player-score)
        table-key (select-combat-table villain-strength player-str)
        {:keys [expected variance std-dev]} (combat-variance-markov villain-strength table-key)

        ;; 95% confidence interval (assuming roughly normal for large N)
        ci-low (max 1 (- expected (* 1.96 std-dev)))
        ci-high (+ expected (* 1.96 std-dev))]

    {:villain-strength villain-strength
     :player-strength player-str
     :table table-key
     :expected-attacks expected
     :std-dev std-dev
     :variance variance
     :ci-95 [ci-low ci-high]
     :pessimistic-estimate (Math/ceil (+ expected std-dev))}))

;; =============================================================================
;; Object Name Generation
;; =============================================================================
;; The parser expects natural language object names, not object IDs.
;; Object IDs use hyphens (e.g., :brass-lantern) but the parser expects
;; words separated by spaces or just the noun (e.g., "brass lantern" or "lantern").

(defn object-id->parser-name
  "Convert an object ID to a parser-friendly name.

   Uses the object's :synonym (nouns) and :adjective fields to generate
   a name the parser will understand. Prefers the first synonym.

   Examples:
   :brass-lantern -> \"lantern\" (uses first synonym)
   :jeweled-egg -> \"egg\" (uses first synonym)
   :elvish-sword -> \"sword\" (uses first synonym)"
  [game-state obj-id]
  (let [obj (get-in game-state [:objects obj-id])
        synonyms (:synonym obj)
        ;; Use first synonym if available, otherwise derive from ID
        base-name (if (seq synonyms)
                    (first synonyms)
                    ;; Fallback: convert :some-thing to "thing" (last word)
                    (last (str/split (name obj-id) #"-")))]
    base-name))

(defn object-id->full-parser-name
  "Convert an object ID to a full parser-friendly name with adjective.

   Uses adjective + noun format when the object has an adjective defined.
   This can help disambiguate when multiple objects share a noun.

   Examples:
   :brass-lantern -> \"brass lantern\"
   :elvish-sword -> \"elvish sword\"
   :jeweled-egg -> \"jeweled egg\""
  [game-state obj-id]
  (let [obj (get-in game-state [:objects obj-id])
        synonyms (:synonym obj)
        adjectives (:adjective obj)
        ;; Get base noun
        base-name (if (seq synonyms)
                    (first synonyms)
                    (last (str/split (name obj-id) #"-")))
        ;; Get adjective (first if multiple)
        adj (cond
              (string? adjectives) adjectives
              (seq adjectives) (first adjectives)
              :else nil)]
    (if adj
      (str adj " " base-name)
      base-name)))

;; =============================================================================
;; Exit Parsing
;; =============================================================================

(defn parse-exit
  "Parse an exit definition into a standardized format.

   Exit formats in Clork:
   - keyword: :other-room (direct bidirectional)
   - string: \"blocked message\" (permanently blocked)
   - {:to :room :if :flag} (conditional on flag)
   - {:to :room :door :door-id} (requires door open)
   - {:per :function} (computed exit, often one-way)

   Returns:
   {:to       :destination-room   ; or nil if blocked
    :requires {:flags #{...} :door :door-id :computed :fn-name}
    :one-way? boolean
    :blocked? boolean}"
  [exit-def direction]
  (cond
    ;; Simple keyword - direct connection
    (keyword? exit-def)
    {:to exit-def
     :requires nil
     :one-way? false
     :blocked? false}

    ;; String - blocked exit
    (string? exit-def)
    {:to nil
     :requires nil
     :one-way? false
     :blocked? true
     :message exit-def}

    ;; Map - conditional exit
    (map? exit-def)
    (cond
      ;; Computed exit via :per function
      (:per exit-def)
      (let [fn-name (:per exit-def)
            ;; Known one-way paths
            one-way-fns #{:maze-diodes}]
        {:to nil ; Computed at runtime
         :requires {:computed fn-name}
         :one-way? (contains? one-way-fns fn-name)
         :blocked? false
         :computed fn-name})

      ;; Flag-conditional exit
      (:if exit-def)
      {:to (:to exit-def)
       :requires {:flags #{(:if exit-def)}}
       :one-way? false
       :blocked? false}

      ;; Door-gated exit
      (:door exit-def)
      {:to (:to exit-def)
       :requires {:door (:door exit-def)}
       :one-way? false
       :blocked? false}

      ;; Unknown map format
      :else
      {:to (:to exit-def)
       :requires nil
       :one-way? false
       :blocked? false})

    :else
    {:to nil :requires nil :one-way? false :blocked? true}))

;; =============================================================================
;; Computed Exit Specifications
;; =============================================================================
;; These define :per function exits with their destinations and preconditions.
;; For speedrun planning, we "cheat" by knowing where computed exits go.

(def per-exit-specs
  "Specifications for :per function exits.
   Format: {[room per-fn] {:to dest :preconditions {...} :one-way? bool}}

   Preconditions can include:
   - :inventory - items required in inventory
   - :flags - flags that must be set
   - :max-inventory - maximum number of items you can carry"
  {;; Chimney: studio -> kitchen
   ;; Requires brass-lantern and only 1-2 items total
   [:studio :up-chimney-function]
   {:to :kitchen
    :preconditions {:inventory #{:brass-lantern}
                    :max-inventory 2}
    :one-way? true
    :notes "Can only climb with lantern and 1-2 items"}

   ;; Grating: clearing -> grating-room (going DOWN)
   ;; Requires grate to be revealed and open
   [:grating-clearing :grating-exit]
   {:to :grating-room
    :preconditions {:flags #{:grate-revealed :grate-open}}
    :one-way? false
    :notes "Grate must be revealed (move leaves) and opened"}

   ;; Maze diodes - one-way teleporters, no preconditions
   [:maze-2 :maze-diodes]
   {:to :maze-4 :preconditions {} :one-way? true}
   [:maze-7 :maze-diodes]
   {:to :dead-end-1 :preconditions {} :one-way? true}
   [:maze-9 :maze-diodes]
   {:to :maze-11 :preconditions {} :one-way? true}
   [:maze-12 :maze-diodes]
   {:to :maze-5 :preconditions {} :one-way? true}})

(def door-gated-exits
  "Map of [from-room to-room] -> {:flags #{...}} for regular door-gated exits.

   These are exits that require specific game-state flags to use.
   The navigation graph builder checks this map for regular exits
   (not computed :per exits, which are handled by per-exit-specs)."
  {;; Grating: grating-room -> grating-clearing (going UP)
   ;; Requires grate to be revealed and open
   [:grating-room :grating-clearing]
   {:flags #{:grate-revealed :grate-open}
    :notes "Grate must be revealed (move leaves) and opened"}

   ;; Trap door: living-room -> cellar (going DOWN)
   ;; Rug must be moved and trap door opened
   [:living-room :cellar]
   {:flags #{:rug-moved :trap-door-open}
    :notes "Rug must be moved and trap door opened"}

   ;; Note: trap door return (cellar -> living-room) is handled separately
   ;; via blocked-edges in plan-to-command-sequence because it depends on
   ;; whether we're underground and have magic-flag/grating-unlocked
   })

(def teleport-edges
  "Map of special teleport actions that aren't normal room exits.

   These are one-way shortcuts triggered by verbs rather than movement.
   Format: {[from-room to-room] {:command \"verb\" :flags #{...}}}"
  {;; Praying at the south temple altar teleports you to the forest
   ;; This is a significant shortcut from underground back to the surface
   [:south-temple :forest-1]
   {:command "pray"
    :flags #{}
    :notes "Praying at altar teleports to forest (one-way)"}

   ;; Mirror teleportation between the two mirror rooms
   ;; Rubbing the mirror with hands teleports player and swaps room contents
   ;; ZIL: MIRROR-MIRROR routine (1actions.zil lines 984-1011)
   [:mirror-room-1 :mirror-room-2]
   {:command "rub mirror"
    :flags #{}
    :notes "Mirror teleport (bidirectional)"}

   [:mirror-room-2 :mirror-room-1]
   {:command "rub mirror"
    :flags #{}
    :notes "Mirror teleport (bidirectional)"}})

(defn get-computed-exit
  "Get the destination and preconditions for a computed exit.
   Returns nil if the exit is not known."
  [from-room per-fn]
  (get per-exit-specs [from-room per-fn]))

;; Convenience accessor for just the destination
(def computed-exit-destinations
  "Map of [room :per-function] -> destination room.
   Derived from per-exit-specs for backwards compatibility."
  (into {}
        (for [[[room per-fn] spec] per-exit-specs]
          [[room per-fn] (:to spec)])))

;; =============================================================================
;; Flag-Gated Regions (Location Dependency Detection)
;; =============================================================================
;; Some rooms can only be reached after certain flags are achieved.
;; This is used by both navigation and action generation.

(def flag-gated-regions
  "Map of flags to the rooms they unlock access to.
   Format: {flag #{rooms...}}

   When a room is in a flag-gated region, reaching that room requires
   the corresponding flag to be achieved first."
  {:troll-flag
   ;; Troll blocks west exit from troll-room to maze-1
   ;; All maze rooms require troll-flag
   #{:maze-1 :maze-2 :maze-3 :maze-4 :maze-5 :maze-6 :maze-7 :maze-8
     :maze-9 :maze-10 :maze-11 :maze-12 :maze-13 :maze-14 :maze-15
     :dead-end :grating-room
     ;; Also deep canyon direction from troll-room
     :deep-canyon :loud-room :round-room
     ;; And chasm area (needs :troll-flag for safe passage)
     :east-of-chasm :west-of-chasm}

   :cyclops-flag
   ;; Cyclops blocks up from cyclops-room to treasure-room
   #{:treasure-room :strange-passage}

   :dome-flag
   ;; Rope must be tied at dome-room to access torch-room
   ;; Egypt-room and temple rooms are only accessible through torch-room
   #{:torch-room :north-temple :south-temple :egypt-room}

   :lld-flag
   ;; Exorcism required to enter land of living dead
   #{:land-of-living-dead :entrance-to-hades}

   ;; Trap door access - these rooms are reached via trap door from living room
   ;; Requires moving rug and opening trap door first
   :rug-moved
   #{:cellar :troll-room}

   :trap-door-open
   #{:cellar :troll-room}

   ;; Coffin passage: south-temple -> tiny-cave requires not carrying gold-coffin
   ;; This flag is effectively always set when not carrying the coffin.
   ;; Rooms accessible only via this passage require it.
   :coffin-cure
   #{:tiny-cave}

   ;; River navigation: requires being in the boat
   ;; Boat must be inflated at dam-base, then entered
   :in-boat
   #{:river-1 :river-2 :river-3 :river-4 :river-5}

   ;; Low tide: dam must be drained to cross reservoir
   ;; Required to reach atlantis-room where the trident is
   :low-tide
   #{:atlantis-room :reservoir :reservoir-north}})

(def flag-gated-rooms
  "Inverse map: room -> set of flags required to reach it.
   Built from flag-gated-regions."
  (reduce-kv
   (fn [m flag rooms]
     (reduce (fn [m room]
               (update m room (fnil conj #{}) flag))
             m
             rooms))
   {}
   flag-gated-regions))

(defn flags-required-for-room
  "Get the set of flags required to reach a specific room.
   Returns empty set if room is always accessible."
  [room-id]
  (get flag-gated-rooms room-id #{}))

;; =============================================================================
;; Movement Action Extraction
;; =============================================================================

(defn extract-room-exits
  "Extract all exits from a room definition.
   Returns sequence of parsed exits with direction."
  [room-def]
  (for [[direction exit-def] (:exits room-def {})]
    (assoc (parse-exit exit-def direction) :direction direction)))

(defn generate-movement-action
  "Generate a movement action for a single exit."
  [from-room direction parsed-exit]
  (let [to-room (:to parsed-exit)
        requires (:requires parsed-exit)]
    (when to-room
      {:id (keyword (str (name from-room) "->" (name to-room)))
       :type :movement
       :preconditions
       {:here from-room
        :inventory #{}
        :flags (or (get requires :flags) #{})}
       :effects
       {:flags-set #{}
        :flags-clear #{}
        :inventory-add #{}
        :inventory-remove #{}
        :new-location to-room}
       :cost 1
       :reversible? (not (:one-way? parsed-exit))
       :commands [(name direction)]
       :door (get requires :door)
       :computed (get requires :computed)})))

(defn generate-computed-exit-action
  "Generate a movement action for a known computed exit.
   Includes preconditions from per-exit-specs (inventory, flags, etc.)."
  [from-room direction to-room per-fn]
  (let [spec (get-computed-exit from-room per-fn)
        preconds (:preconditions spec {})
        required-inventory (get preconds :inventory #{})
        required-flags (get preconds :flags #{})]
    {:id (keyword (str (name from-room) "->" (name to-room)))
     :type :movement
     :preconditions
     {:here from-room
      :inventory required-inventory
      :flags required-flags}
     :effects
     {:flags-set #{}
      :flags-clear #{}
      :inventory-add #{}
      :inventory-remove #{}
      :new-location to-room}
     :cost 1
     :reversible? (not (:one-way? spec true))
     :commands [(name direction)]
     :computed per-fn
     :max-inventory (get preconds :max-inventory nil)
     :notes (:notes spec)}))

(defn extract-movement-actions
  "Extract all movement actions from game state.
   Returns map of action-id -> action.

   Handles both regular exits and known computed (:per) exits
   from the computed-exit-destinations map."
  [game-state]
  (let [;; Extract regular movement actions
        regular-actions
        (reduce-kv
         (fn [actions room-id room-def]
           (let [exits (extract-room-exits room-def)
                 move-actions (keep #(generate-movement-action room-id (:direction %) %) exits)]
             (reduce (fn [m action] (assoc m (:id action) action))
                     actions
                     move-actions)))
         {}
         (:rooms game-state))

        ;; Extract actions for known computed exits
        computed-actions
        (reduce-kv
         (fn [actions room-id room-def]
           (let [exits (:exits room-def {})]
             (reduce-kv
              (fn [m direction exit-def]
                (if (and (map? exit-def) (:per exit-def))
                  ;; This is a computed exit - check if we know the destination
                  (let [per-fn (:per exit-def)
                        dest (get computed-exit-destinations [room-id per-fn])]
                    (if dest
                      (let [action (generate-computed-exit-action room-id direction dest per-fn)]
                        (assoc m (:id action) action))
                      m))
                  m))
              actions
              exits)))
         {}
         (:rooms game-state))]

    (merge regular-actions computed-actions)))

;; =============================================================================
;; Take Action Extraction
;; =============================================================================

(defn takeable?
  "Check if an object can be taken (has :take flag)."
  [obj-def]
  (contains? (:flags obj-def #{}) :take))

(defn find-object-room
  "Find what room an object is in (recursively if in container)."
  [game-state obj-id]
  (let [location (get-in game-state [:objects obj-id :in])]
    (cond
      ;; In a room directly
      (contains? (:rooms game-state) location)
      location

      ;; In a container - find container's location
      (keyword? location)
      (find-object-room game-state location)

      ;; In player inventory
      (= location :adventurer)
      nil

      :else nil)))

(defn generate-take-action
  "Generate a take action for an object.
   Includes flag requirements for reaching the object's room."
  [game-state obj-id obj-def]
  (let [room (find-object-room game-state obj-id)
        size (get obj-def :size 0)
        ;; Use parser-friendly object name instead of raw ID
        parser-name (object-id->parser-name game-state obj-id)
        ;; Get flags required to reach this room (e.g., :low-tide for atlantis-room)
        room-flags (flags-required-for-room room)]
    (when room
      {:id (keyword (str "take-" (name obj-id)))
       :type :take
       :preconditions
       {:here room
        :inventory #{}
        :flags room-flags}
       :effects
       {:flags-set #{}
        :flags-clear #{}
        :inventory-add #{obj-id}
        :inventory-remove #{}
        :removes-from-room obj-id}
       :cost 1
       :reversible? true
       :commands [(str "take " parser-name)]
       :object obj-id
       :weight size})))

;; Objects that need special handling (flag-gated, puzzles, etc.)
;; These are excluded from auto-generation and defined in puzzle-actions
(def special-take-objects
  "Objects that shouldn't have auto-generated take actions.
   These require puzzle solutions or flags to obtain."
  #{:pot-of-gold        ; Requires rainbow-flag (wave sceptre first)
    :silver-chalice     ; Obtained by killing thief (kill-thief-with-* actions)
    :garlic             ; In brown-sack, needs "open sack" first (use :get-garlic)
    :large-emerald      ; In buoy, needs "take buoy" then "open buoy" (use :open-buoy)
    :brass-bauble       ; Created when canary sings in forest (use :wind-canary)
    :clockwork-canary   ; In treasure-room after thief opens egg (use :take-clockwork-canary)
    :platinum-bar       ; Only visible after saying "echo" in loud-room (use :take-platinum-bar)
    :crystal-trident})  ; In atlantis-room, requires :low-tide to reach

(defn extract-take-actions
  "Extract take actions for all takeable objects.
   Excludes objects in special-take-objects (handled in puzzle-actions).
   Returns map of action-id -> action."
  [game-state]
  (reduce-kv
   (fn [actions obj-id obj-def]
     (if (and (takeable? obj-def)
              (not (contains? special-take-objects obj-id)))
       (if-let [action (generate-take-action game-state obj-id obj-def)]
         (assoc actions (:id action) action)
         actions)
       actions))
   {}
   (:objects game-state)))

;; =============================================================================
;; Deposit Action Generation
;; =============================================================================

(def treasures
  "List of all treasures in Zork I with their value when deposited.
   Note: IDs must match actual object :id values in objects.clj"
  #{:egg :clockwork-canary :brass-bauble :gold-coffin :sceptre
    :ivory-torch :crystal-trident :jade-figurine :sapphire-bracelet
    :huge-diamond :bag-of-coins :crystal-skull :trunk-of-jewels
    :large-emerald :painting :pot-of-gold :platinum-bar :scarab
    :silver-chalice})

(defn generate-deposit-action
  "Generate a PRIMITIVE action to put a treasure in the trophy case.

   IMPORTANT: This is now a single-step primitive that requires the
   trophy case to already be open. Use :open-trophy-case first.

   If game-state is provided, uses parser-friendly object names.
   Otherwise falls back to deriving name from object ID."
  ([treasure-id]
   (generate-deposit-action nil treasure-id))
  ([game-state treasure-id]
   (let [deposit-flag (keyword (str (name treasure-id) "-in-trophy-case"))
         ;; Use parser-friendly name if game-state available
         parser-name (if game-state
                       (object-id->parser-name game-state treasure-id)
                       ;; Fallback: derive from ID (e.g., :bag-of-coins -> "coins")
                       (last (str/split (name treasure-id) #"-")))]
     {:id (keyword (str "deposit-" (name treasure-id)))
      :type :deposit
      :preconditions
      {:here :living-room
       :inventory #{treasure-id}
       :flags #{}
       :open-containers #{:trophy-case}}  ; Requires case to be open!
      :effects
      {:flags-set #{deposit-flag}
       :flags-clear #{}
       :inventory-add #{}
       :inventory-remove #{treasure-id}
       :deposits treasure-id}
      :cost 1 ; Just the put command (case already open)
      :reversible? true
      :commands [(str "put " parser-name " in case")]
      :treasure treasure-id})))

(defn generate-deposit-actions
  "Generate deposit actions for all treasures.
   Returns map of action-id -> action."
  ([]
   (generate-deposit-actions nil))
  ([game-state]
   (reduce (fn [m treasure-id]
             (let [action (generate-deposit-action game-state treasure-id)]
               (assoc m (:id action) action)))
           {}
           treasures)))

;; =============================================================================
;; Puzzle/Combat Actions (Manual Definitions)
;; =============================================================================

(def puzzle-actions
  "Manually defined puzzle and combat actions.
   These require specific knowledge about game mechanics."
  {;; =========================================================================
   ;; LIGHT SOURCE ACTIONS
   ;; Must turn on lantern before entering dark areas
   ;; =========================================================================

   :turn-on-lantern
   {:id :turn-on-lantern
    :type :setup
    :preconditions
    {:inventory #{:brass-lantern}
     :flags #{}}
    :effects
    {:flags-set #{:lantern-on}
     :flags-clear #{}
     :inventory-add #{}
     :inventory-remove #{}}
    :cost 1
    :reversible? true
    :commands ["turn on lantern"]}

   ;; =========================================================================
   ;; DOOR/ACCESS SETUP ACTIONS
   ;; These must be performed before certain movements are possible
   ;; =========================================================================

   :open-kitchen-window
   {:id :open-kitchen-window
    :type :setup
    :preconditions
    {:here :behind-house
     :inventory #{}
     :flags #{}}
    :effects
    {:flags-set #{:window-open}
     :flags-clear #{}
     :inventory-add #{}
     :inventory-remove #{}
     :opens-container :window}
    :cost 1
    :reversible? true
    :commands ["open window"]}

   ;; Trophy case - PRIMITIVE action to open
   ;; Separate from deposit so we can batch multiple deposits
   :open-trophy-case
   {:id :open-trophy-case
    :type :setup
    :preconditions
    {:here :living-room
     :inventory #{}
     :flags #{}}
    :effects
    {:flags-set #{}
     :flags-clear #{}
     :inventory-add #{}
     :inventory-remove #{}
     :opens-container :trophy-case}
    :cost 1
    :reversible? true
    :commands ["open case"]}

   :move-rug
   {:id :move-rug
    :type :setup
    :preconditions
    {:here :living-room
     :inventory #{}
     :flags #{}}
    :effects
    {:flags-set #{:rug-moved}
     :flags-clear #{}
     :inventory-add #{}
     :inventory-remove #{}}
    :cost 1
    :reversible? false  ; Can't un-reveal the trap door
    :commands ["move rug"]}

   :open-trap-door
   {:id :open-trap-door
    :type :setup
    :preconditions
    {:here :living-room
     :inventory #{}
     :flags #{:rug-moved}}  ; Must move rug first!
    :effects
    {:flags-set #{:trap-door-open}
     :flags-clear #{}
     :inventory-add #{}
     :inventory-remove #{}}
    :cost 1
    :reversible? true
    :commands ["open trap door"]}

   ;; =========================================================================
   ;; GARLIC (bat protection)
   ;; =========================================================================
   ;; Garlic is in the brown sack in the kitchen.
   ;; Having garlic in inventory protects from the bat in bat-room.
   ;; Without garlic, the bat will grab the player and drop them at a random
   ;; location, disrupting all navigation.

   :get-garlic
   {:id :get-garlic
    :type :take
    :preconditions
    {:here :kitchen
     :inventory #{}
     :flags #{}}
    :effects
    {:flags-set #{}
     :flags-clear #{}
     :inventory-add #{:garlic}
     :inventory-remove #{}}
    :cost 2  ; open sack + take garlic
    :reversible? true
    :commands ["open sack" "take garlic"]
    :notes "Garlic protects from bat in bat-room (squeaky-room area)"}

   ;; =========================================================================
   ;; BRASS BAUBLE (wind canary in forest)
   ;; =========================================================================
   ;; The clockwork canary must be wound in a forest room to attract a songbird.
   ;; The songbird drops the brass bauble. Canary is obtained from the jeweled egg
   ;; (which the thief opens for you if you drop it near him).

   :wind-canary
   {:id :wind-canary
    :type :take
    :preconditions
    {:here :forest-path  ; Any forest room works, but forest-path is central
     :inventory #{:clockwork-canary}
     :flags #{}}
    :effects
    {:flags-set #{}
     :flags-clear #{}
     :inventory-add #{:brass-bauble}
     :inventory-remove #{}}
    :cost 2  ; wind canary + take bauble
    :reversible? false
    :commands ["wind canary" "take bauble"]}

   ;; Note: Going DOWN through trap door causes it to close and bar!
   ;; This is a ONE-WAY transition. You cannot return via trap door.
   ;; To return to living room from underground, must use:
   ;; cyclops-room -> strange-passage -> living-room (requires :magic-flag)

   ;; =========================================================================
   ;; COMBAT ACTIONS
   ;; =========================================================================

   :kill-troll
   {:id :kill-troll
    :type :combat
    :preconditions
    {:here :troll-room
     :inventory #{:sword}
     :flags #{}}
    :effects
    {:flags-set #{:troll-flag}
     :flags-clear #{}
     :inventory-add #{}
     :inventory-remove #{}}
    ;; Markov chain analysis at score 0 (DEF2A table):
    ;; - Expected attacks: 6.11
    ;; - Adding ~2 moves for retry overhead from possible player deaths
    :cost 8
    :reversible? false
    :combat
    {:action "attack troll with sword"
     :enemy :troll
     :enemy-strength 2
     :victory-flag :troll-flag
     :expected-attacks 5                 ; 2D simulation mean: 4.5
     :std-dev 3                          ; 2D simulation: 2.8
     :max-attacks 20                     ; Abort threshold
     :retreat-dir "south"
     :retry-actions ["wait" "north"]}
    ;; 10 attacks (pessimistic buffer for actual gameplay)
    :commands ["attack troll with sword" "attack troll with sword"
               "attack troll with sword" "attack troll with sword"
               "attack troll with sword" "attack troll with sword"
               "attack troll with sword" "attack troll with sword"
               "attack troll with sword" "attack troll with sword"]}

   :scare-cyclops
   {:id :scare-cyclops
    :type :puzzle
    :preconditions
    {:here :cyclops-room
     :inventory #{}
     :flags #{:troll-flag}}
    :effects
    {:flags-set #{:magic-flag :cyclops-flag}
     :flags-clear #{}
     :inventory-add #{}
     :inventory-remove #{}}
    :cost 1
    :reversible? false
    :commands ["ulysses"]}

   ;; Press yellow button in maintenance room to enable bolt turning
   ;; ZIL: Yellow button sets GATE-FLAG which allows bolt to turn
   :press-yellow-button
   {:id :press-yellow-button
    :type :puzzle
    :preconditions
    {:here :maintenance-room
     :inventory #{}
     :flags #{}}
    :effects
    {:flags-set #{:gate-flag}
     :flags-clear #{}
     :inventory-add #{}
     :inventory-remove #{}}
    :cost 1
    :reversible? false
    :commands ["push yellow button"]}

   :open-dam
   {:id :open-dam
    :type :puzzle
    :preconditions
    {:here :dam-room  ; Bolt is on dam's control panel (room ID is :dam-room)
     :inventory #{:wrench}
     :flags #{:gate-flag}}  ; Yellow button must be pressed first!
    :effects
    {:flags-set #{:dam-opened}
     :flags-clear #{}
     :inventory-add #{}
     :inventory-remove #{}}
    :cost 1
    :reversible? true
    :commands ["turn bolt with wrench"]}

   :tie-rope
   {:id :tie-rope
    :type :puzzle
    :preconditions
    {:here :dome-room
     :inventory #{:rope}
     :flags #{}}
    :effects
    {:flags-set #{:dome-flag}
     :flags-clear #{}
     :inventory-add #{}
     :inventory-remove #{:rope}}
    :cost 1
    :reversible? false
    :commands ["tie rope to railing"]}

   :exorcism
   {:id :exorcism
    :type :puzzle
    :preconditions
    {:here :entrance-to-hades
     ;; Note: Use actual object IDs, not short names
     :inventory #{:brass-bell :black-book :candles :matchbook}
     :flags #{}}
    :effects
    {:flags-set #{:lld-flag}
     :flags-clear #{}
     :inventory-add #{}
     :inventory-remove #{}}
    :cost 4
    :reversible? false
    :commands ["ring bell" "light match" "light candles with match" "read book"]}

   :wave-sceptre
   {:id :wave-sceptre
    :type :puzzle
    :preconditions
    {:here :end-of-rainbow
     :inventory #{:sceptre}
     :flags #{}}
    :effects
    {:flags-set #{:rainbow-flag}
     :flags-clear #{}
     :inventory-add #{:pot-of-gold}
     :inventory-remove #{}}
    :cost 1
    :reversible? false
    :commands ["wave sceptre"]}

   :enter-boat
   {:id :enter-boat
    :type :puzzle
    :preconditions
    {:here :dam-base
     :inventory #{}
     :flags #{:boat-inflated}}  ; Must inflate boat first!
    :effects
    {:flags-set #{:in-boat}
     :flags-clear #{}
     :inventory-add #{}
     :inventory-remove #{}}
    :cost 1
    :reversible? true
    :commands ["enter boat"]}

   :wait-for-drain
   {:id :wait-for-drain
    :type :puzzle
    :preconditions
    {:inventory #{}
     :flags #{:dam-opened}}
    :effects
    {:flags-set #{:low-tide}
     :flags-clear #{}
     :inventory-add #{}
     :inventory-remove #{}}
    :cost 8
    :reversible? false
    :commands ["wait" "wait" "wait" "wait" "wait" "wait" "wait" "wait"]}

   :inflate-boat
   {:id :inflate-boat
    :type :puzzle
    :preconditions
    {:here :dam-base
     :inventory #{:pump}
     :flags #{}}
    :effects
    {:flags-set #{:boat-inflated}
     :flags-clear #{}
     :inventory-add #{}
     :inventory-remove #{}}
    :cost 1
    :reversible? false
    :commands ["inflate boat with pump"]}

   :open-buoy
   {:id :open-buoy
    :type :puzzle
    :preconditions
    {:inventory #{:buoy}
     :flags #{}}
    :effects
    {:flags-set #{}
     :flags-clear #{}
     :inventory-add #{:large-emerald}
     :inventory-remove #{}}
    :cost 1
    :reversible? false
    :commands ["open buoy"]}

   ;; ==========================================================================
   ;; EGG PUZZLE (thief interaction)
   ;; ==========================================================================
   ;; The egg CANNOT be opened by the player - any attempt breaks it!
   ;; The ONLY way to open it safely is via the thief:
   ;; 1. Give egg to thief (or let him steal it)
   ;; 2. Thief takes egg to treasure room and deposits it
   ;; 3. When depositing, thief opens egg safely (sets :egg-solve flag, :open flag on egg)
   ;; 4. Kill thief, take the now-open egg
   ;; 5. Canary is inside the open egg
   ;;
   ;; Note: The thief is non-deterministic - he may not deposit immediately.
   ;; Must wait for him to actually deposit the egg.

   ;; Phase 1: Give egg to thief (or let him steal it)
   ;; The thief wanders, so we might encounter him anywhere underground
   ;; For planning, we model this as happening in treasure-room (his home base)
   :give-egg-to-thief
   {:id :give-egg-to-thief
    :type :puzzle
    :preconditions
    {:here :treasure-room
     :inventory #{:egg}
     :flags #{:cyclops-flag :thief-alive}}  ; Must reach treasure room AND thief must be alive
    :effects
    {:flags-set #{:thief-has-egg}
     :flags-clear #{}
     :inventory-add #{}
     :inventory-remove #{:egg}}
    :cost 1
    :reversible? false
    :commands ["give egg to thief"]
    :notes "Thief takes egg, will eventually deposit and open it."}

   ;; Phase 2: Wait for thief to deposit the egg
   ;; This represents doing other tasks while the thief wanders and deposits
   ;; In practice: do other underground tasks, thief will deposit eventually
   :wait-for-egg-deposit
   {:id :wait-for-egg-deposit
    :type :puzzle
    :preconditions
    {:flags #{:thief-has-egg}}
    :effects
    {:flags-set #{:egg-solve}
     :flags-clear #{}
     :inventory-add #{}
     :inventory-remove #{}}
    :cost 10  ; Represents time spent doing other things
    :reversible? false
    :commands []  ; No explicit command - happens while doing other tasks
    :notes "Thief deposits egg in treasure room, opening it safely. Do other tasks while waiting."}

   ;; After thief has deposited (opened) the egg and been killed
   ;; The egg must be in inventory and open (egg-solve flag set)
   :take-canary-from-egg
   {:id :take-canary-from-egg
    :type :take
    :preconditions
    {:inventory #{:egg}
     :flags #{:egg-solve}}  ; Thief must have deposited/opened the egg
    :effects
    {:flags-set #{}
     :flags-clear #{}
     :inventory-add #{:clockwork-canary}
     :inventory-remove #{}}
    :cost 1
    :reversible? false
    :commands ["take canary from egg"]}

   ;; Simpler action: take canary directly from treasure room after thief is dead
   ;; The thief will have taken the egg, opened it, and the canary will be there
   :take-clockwork-canary
   {:id :take-clockwork-canary
    :type :take
    :preconditions
    {:here :treasure-room
     :flags #{:thief-dead :egg-solve}}  ; Thief must deposit egg (opens it) AND be dead
    :effects
    {:flags-set #{}
     :flags-clear #{}
     :inventory-add #{:clockwork-canary}
     :inventory-remove #{}}
    :cost 1
    :reversible? false
    :commands ["take canary"]}

   :dig-for-scarab
   {:id :dig-for-scarab
    :type :puzzle
    :preconditions
    {:here :sandy-cave
     :inventory #{:shovel}
     :flags #{}}
    :effects
    {:flags-set #{}
     :flags-clear #{}
     :inventory-add #{:scarab}
     :inventory-remove #{}}
    :cost 4  ; Multiple dig commands
    :reversible? false
    :commands ["dig sand" "dig sand" "dig sand" "dig sand"]}

   ;; ==========================================================================
   ;; THIEF COMBAT
   ;; ==========================================================================
   ;; Combat mechanics:
   ;; - Player fight-strength: 2 + (score/70), ranges from 2 (0 pts) to 7 (350 pts)
   ;; - Thief strength: 5
   ;; - At 0 points: strength 2 vs 5 = almost impossible to win
   ;; - At 210 points: strength 5 vs 5 = even match
   ;; - At 280+ points: strength 6+ vs 5 = advantage
   ;; - MUST have a weapon (sword or nasty knife)
   ;; - Nasty knife is thief's "best weapon" - gives player advantage
   ;; - IMPORTANT: Thief fights to the DEATH in treasure room (never flees)
   ;;
   ;; Combat probability (DEF3 tables - no instant kill, must wound to 0):
   ;; - DEF3A (score <70): 18% light wound, 18% serious wound = 0.54 dmg/attack
   ;; - DEF3B (score 70-210): 27% light, 27% serious = 0.81 dmg/attack
   ;; - DEF3C (score 280+): 40% light, 30% serious = 1.0 dmg/attack
   ;; - Need 5 damage total to kill thief
   ;; - Player death probability ~15-25% per round at low scores
   ;;
   ;; Thief drops: chalice, stiletto, and any treasures he stole
   ;; Note: Thief does NOT open egg when killed - egg must be given to thief,
   ;; who will open it when he deposits it in treasure room (sets :egg-solve)

   :kill-thief-with-sword
   {:id :kill-thief-with-sword
    :type :combat
    :preconditions
    {:here :treasure-room
     :inventory #{:sword}
     :flags #{:cyclops-flag}  ; Must reach treasure room
     :minimum-score 30}       ; Deposit some treasures first for better combat odds
    :effects
    {:flags-set #{:thief-dead}
     :flags-clear #{:thief-alive}  ; Thief is no longer alive after being killed
     :inventory-add #{:silver-chalice}  ; Chalice always dropped, stolen items vary
     :inventory-remove #{}}
    ;; Realistic cost from 2D Markov simulation (50k fights at score 0):
    ;; - Player wins: 41%, Player dies: 59%
    ;; - Avg fight length: 6.1 rounds (shorter than 1D Markov estimate)
    ;; - Retry cost: ~18 moves to respawn and navigate back
    ;; - E[total] = (6.1 + 0.59 * 18) / 0.41 ≈ 41 moves
    :cost 40
    :reversible? false
    :combat
    {:action "attack thief with sword"
     :enemy :thief
     :enemy-strength 5
     :victory-flag :thief-dead
     :expected-attacks 8                 ; Use 8 for conservative estimate (actual mean 6.2)
     :std-dev 4                          ; Bump up for 3σ coverage (actual 2.6)
     :max-attacks 25                     ; Abort threshold
     :fights-to-death true               ; No fleeing in treasure room
     :player-death-prob 0.59             ; 2D simulation: 59% at score 0
     :retry-cost 18                      ; Moves to return after death
     :recovery-command "take sword"      ; Thief can disarm us - pick up sword
     :recovery-interval 4                ; Insert recovery every N attacks
     :post-combat ["take chalice"]}      ; Chalice drops when thief dies
    ;; ~16 attacks + recovery commands for 99%+ success rate
    :commands ["attack thief with sword" "attack thief with sword"
               "attack thief with sword" "attack thief with sword"
               "take sword"
               "attack thief with sword" "attack thief with sword"
               "attack thief with sword" "attack thief with sword"
               "take sword"
               "attack thief with sword" "attack thief with sword"
               "attack thief with sword" "attack thief with sword"
               "take sword"
               "attack thief with sword" "attack thief with sword"
               "attack thief with sword" "attack thief with sword"
               "take chalice"]
    :notes "Sword has no advantage vs thief. Very difficult at low scores."}

   ;; Nasty knife is thief's weakness (his "best weapon" in ZIL terms)
   ;; Using it against him reduces his effective defense by 1 (strength 5 -> 4)
   ;; This shifts combat tables in player's favor
   :kill-thief-with-knife
   {:id :kill-thief-with-knife
    :type :combat
    :preconditions
    {:here :treasure-room
     :inventory #{:nasty-knife}
     :flags #{:cyclops-flag}
     :minimum-score 30}       ; Deposit some treasures first for better combat odds
    :effects
    {:flags-set #{:thief-dead}
     :flags-clear #{:thief-alive}  ; Thief is no longer alive after being killed
     :inventory-add #{:silver-chalice}
     :inventory-remove #{}}
    ;; Knife advantage analysis:
    ;; - At score 0: knife doesn't help much (both still DEF3A)
    ;; - Player death prob still ~59% (knife only affects thief's defense, not offense)
    ;; - At score 140+: knife shifts to DEF3B - significant advantage
    ;; - Slight discount from sword cost for potential high-score benefit
    :cost 38
    :reversible? false
    :combat
    {:action "attack thief with knife"
     :enemy :thief
     :enemy-strength 4                   ; Reduced from 5 due to knife advantage
     :victory-flag :thief-dead
     :expected-attacks 6                 ; Similar to sword at low score
     :std-dev 3                          ; Similar variance
     :max-attacks 20
     :fights-to-death true               ; No fleeing in treasure room
     :player-death-prob 0.59             ; Same as sword at low score
     :retry-cost 18
     :post-combat ["take chalice"]}      ; Chalice drops when thief dies
    ;; 12 attacks (pessimistic buffer for actual gameplay) + take chalice
    :commands ["attack thief with knife" "attack thief with knife"
               "attack thief with knife" "attack thief with knife"
               "attack thief with knife" "attack thief with knife"
               "attack thief with knife" "attack thief with knife"
               "attack thief with knife" "attack thief with knife"
               "attack thief with knife" "attack thief with knife"
               "take chalice"]
    :notes "Nasty knife reduces thief's defense. Recommended approach."}

   ;; Loud room puzzle:
   ;; - Room is deafening when dam gates open and water high
   ;; - Entering when deafening ejects you randomly
   ;; - Room is quiet when: loud-flag set OR (gates closed AND low-tide)
   ;; - Saying "echo" sets loud-flag and reveals gold bar
   ;; Safest approach: drain dam first (low-tide), then enter and say echo
   :say-echo
   {:id :say-echo
    :type :puzzle
    :preconditions
    {:here :loud-room
     :inventory #{}
     :flags #{:low-tide}}  ; Need low-tide to safely enter
    :effects
    {:flags-set #{:loud-flag}  ; Not loud-room-solved - use actual flag name
     :flags-clear #{}
     :inventory-add #{}
     :inventory-remove #{}}
    :cost 1
    :reversible? false
    :commands ["echo"]}

   :take-platinum-bar
   {:id :take-platinum-bar
    :type :take
    :preconditions
    {:here :loud-room
     :inventory #{}
     :flags #{:loud-flag}}  ; Echo puzzle must be solved first
    :effects
    {:flags-set #{}
     :flags-clear #{}
     :inventory-add #{:platinum-bar}
     :inventory-remove #{}}
    :cost 1
    :reversible? true
    :commands ["take bar"]}

   ;; ==========================================================================
   ;; ATLANTIS ROOM - TRIDENT
   ;; ==========================================================================
   ;; The trident is in atlantis-room, which can only be reached when the
   ;; reservoir is drained (low-tide flag). The dam must be opened first.
   :take-crystal-trident
   {:id :take-crystal-trident
    :type :take
    :preconditions
    {:here :atlantis-room
     :inventory #{}
     :flags #{:low-tide}}  ; Must drain reservoir first
    :effects
    {:flags-set #{}
     :flags-clear #{}
     :inventory-add #{:crystal-trident}
     :inventory-remove #{}}
    :cost 1
    :reversible? true
    :commands ["take trident"]}

   ;; ==========================================================================
   ;; COAL MINE PUZZLE (very complex multi-step puzzle!)
   ;; ==========================================================================
   ;; The coal mine puzzle requires:
   ;; 1. Garlic (for bat room protection)
   ;; 2. Brass lantern (only non-flame light, safe in gas room)
   ;; 3. Ivory torch (provides light at bottom of shaft - but is a flame!)
   ;; 4. Screwdriver (to turn machine switch)
   ;;
   ;; Sequence:
   ;; 1. Put torch + screwdriver in basket at shaft-room
   ;; 2. Lower basket (now lower-shaft is lit)
   ;; 3. Navigate through mine WITH LANTERN ONLY (torch would explode in gas room)
   ;; 4. Get coal from coal-mine-4
   ;; 5. Return to shaft-room via gas room (safe with lantern)
   ;; 6. Put coal in basket, lower it
   ;; 7. Navigate to ladder-bottom, then west
   ;; 8. DROP ALL to fit through narrow crack
   ;; 9. Now in lower-shaft (lit by torch in basket)
   ;; 10. Put coal in machine, close, turn switch with screwdriver, open, take diamond
   ;; 11. Put diamond in basket, raise it
   ;; 12. Go back through crack, pick up items
   ;; 13. Return to shaft-room, get diamond and torch from basket
   ;;
   ;; CRITICAL CONSTRAINTS:
   ;; - Gas room: torch/candles/match = instant death
   ;; - Bat room: need garlic or bat steals items
   ;; - Narrow crack: must be empty-handed
   ;; - Lower shaft: dark unless basket has torch

   :setup-basket
   {:id :setup-basket
    :type :puzzle
    :preconditions
    {:here :shaft-room
     :inventory #{:ivory-torch :screwdriver}
     :flags #{}}
    :effects
    {:flags-set #{:basket-has-light :basket-has-screwdriver}
     :flags-clear #{}
     :inventory-add #{}
     :inventory-remove #{:ivory-torch :screwdriver}}
    :cost 2
    :reversible? true
    :commands ["put torch in basket" "put screwdriver in basket"]}

   :lower-basket
   {:id :lower-basket
    :type :puzzle
    :preconditions
    {:here :shaft-room
     :inventory #{}
     :flags #{:basket-has-light}}  ; Need light source in basket for lower shaft
    :effects
    {:flags-set #{:basket-lowered :lower-shaft-lit}
     :flags-clear #{}
     :inventory-add #{}
     :inventory-remove #{}}
    :cost 1
    :reversible? true
    :commands ["lower basket"]}

   :put-coal-in-basket
   {:id :put-coal-in-basket
    :type :puzzle
    :preconditions
    {:here :shaft-room
     :inventory #{:coal}
     :flags #{}}
    :effects
    {:flags-set #{:basket-has-coal}
     :flags-clear #{}
     :inventory-add #{}
     :inventory-remove #{:coal}}
    :cost 1
    :reversible? false
    :commands ["put coal in basket"]}

   :raise-basket
   {:id :raise-basket
    :type :puzzle
    :preconditions
    {:here :shaft-room
     :inventory #{}
     :flags #{:basket-lowered}}
    :effects
    {:flags-set #{}
     :flags-clear #{:basket-lowered}
     :inventory-add #{}
     :inventory-remove #{}}
    :cost 1
    :reversible? true
    :commands ["raise basket"]}

   ;; The crack requires dropping everything
   :squeeze-through-crack
   {:id :squeeze-through-crack
    :type :puzzle
    :preconditions
    {:here :ladder-bottom
     :inventory #{}  ; Must be empty-handed!
     :flags #{:lower-shaft-lit}}  ; Need basket light at bottom
    :effects
    {:flags-set #{:at-lower-shaft}
     :flags-clear #{}
     :inventory-add #{}
     :inventory-remove #{}
     :new-location :lower-shaft}
    :cost 1
    :reversible? true
    :commands ["west"]}

   :get-diamond
   {:id :get-diamond
    :type :puzzle
    :preconditions
    {:here :lower-shaft
     :inventory #{}
     :flags #{:basket-has-coal :basket-has-screwdriver :basket-lowered}}
    :effects
    {:flags-set #{}
     :flags-clear #{:basket-has-coal}
     :inventory-add #{:huge-diamond}
     :inventory-remove #{}}
    :cost 6
    :reversible? false
    :commands ["take coal from basket" "take screwdriver from basket"
               "put coal in machine" "close lid" "turn switch with screwdriver"
               "open lid" "take diamond"]}

   :put-diamond-in-basket
   {:id :put-diamond-in-basket
    :type :puzzle
    :preconditions
    {:here :lower-shaft
     :inventory #{:huge-diamond}
     :flags #{}}
    :effects
    {:flags-set #{:basket-has-diamond}
     :flags-clear #{}
     :inventory-add #{}
     :inventory-remove #{:huge-diamond}}
    :cost 1
    :reversible? false
    :commands ["put diamond in basket"]}

   :retrieve-diamond-from-basket
   {:id :retrieve-diamond-from-basket
    :type :puzzle
    :preconditions
    {:here :shaft-room
     :inventory #{}
     :flags #{:basket-has-diamond}}
    :effects
    {:flags-set #{}
     :flags-clear #{:basket-has-diamond}
     :inventory-add #{:huge-diamond}
     :inventory-remove #{}}
    :cost 1
    :reversible? false
    :commands ["take diamond from basket"]}

   ;; =========================================================================
   ;; WIN CONDITION (enter stone barrow)
   ;; =========================================================================
   ;; Once all treasures are deposited (score = 350), the :won flag is set.
   ;; This unlocks the southwest path from west-of-house to stone-barrow.
   ;; Entering the barrow completes the game.

   :enter-stone-barrow
   {:id :enter-stone-barrow
    :type :puzzle
    :preconditions
    {:here :west-of-house
     :inventory #{}
     :flags #{:won}}  ; Set when score = 350 (all treasures deposited)
    :effects
    {:flags-set #{:finished}
     :flags-clear #{}
     :inventory-add #{}
     :inventory-remove #{}
     :new-location :stone-barrow}
    :cost 1
    :reversible? false
    :commands ["sw"]}

})

;; =============================================================================
;; Action Registry
;; =============================================================================

(defn build-action-registry
  "Build complete action registry from game state.
   Combines auto-extracted and manual actions."
  [game-state]
  (merge
   (extract-movement-actions game-state)
   (extract-take-actions game-state)
   (generate-deposit-actions game-state)
   puzzle-actions))

(defn actions-requiring-flag
  "Find all actions that require a specific flag."
  [registry flag]
  (filter #(contains? (get-in % [:preconditions :flags] #{}) flag)
          (vals registry)))

(defn actions-setting-flag
  "Find all actions that set a specific flag."
  [registry flag]
  (filter #(contains? (get-in % [:effects :flags-set] #{}) flag)
          (vals registry)))

(defn actions-at-location
  "Find all non-movement actions at a specific location."
  [registry room-id]
  (filter #(and (= (get-in % [:preconditions :here]) room-id)
                (not= (:type %) :movement))
          (vals registry)))

(defn movement-actions-from
  "Find all movement actions from a specific room."
  [registry room-id]
  (filter #(and (= (get-in % [:preconditions :here]) room-id)
                (= (:type %) :movement))
          (vals registry)))

;; =============================================================================
;; Action Validation
;; =============================================================================

(defn can-execute?
  "Check if an action can be executed given current state.
   State should have :here, :inventory, :flags, :open-containers keys."
  [action state]
  (let [preconditions (:preconditions action)]
    (and
     ;; Location check (if specified)
     (or (nil? (:here preconditions))
         (= (:here preconditions) (:here state)))
     ;; Inventory check
     (set/subset? (:inventory preconditions #{})
                  (:inventory state #{}))
     ;; Flags check
     (set/subset? (:flags preconditions #{})
                  (:flags state #{}))
     ;; Open containers check (for primitives that require containers to be open)
     (set/subset? (:open-containers preconditions #{})
                  (:open-containers state #{})))))

(defn apply-action-effects
  "Apply action effects to state.
   Returns updated state.

   Note: Arguments ordered for use with reduce: (reduce apply-action-effects state actions)"
  [state action]
  (let [effects (:effects action)
        deposited-item (:deposits effects)
        opened-container (:opens-container effects)
        base-state (-> state
                       (update :flags set/union (:flags-set effects #{}))
                       (update :flags set/difference (:flags-clear effects #{}))
                       (update :inventory set/union (:inventory-add effects #{}))
                       (update :inventory set/difference (:inventory-remove effects #{}))
                       (cond-> (:new-location effects) (assoc :here (:new-location effects)))
                       ;; Track opened containers
                       (cond-> opened-container
                         (update :open-containers (fnil conj #{}) opened-container)))
        ;; Track deposited items and update score for score-aware planning
        with-deposit (if deposited-item
                       (-> base-state
                           (update :deposited (fnil conj #{}) deposited-item)
                           ;; Score = trophy case value of deposited treasure
                           (update :score (fnil + 0)
                                   (get {:painting 6 :egg 5 :bag-of-coins 5 :crystal-trident 11
                                         :pot-of-gold 10 :silver-chalice 5 :sapphire-bracelet 5
                                         :jade-figurine 5 :platinum-bar 5 :clockwork-canary 4
                                         :huge-diamond 10 :large-emerald 10 :ivory-torch 6
                                         :gold-coffin 15 :sceptre 6 :crystal-skull 10
                                         :scarab 5 :brass-bauble 1 :trunk-of-jewels 5}
                                        deposited-item 0)))
                       base-state)
        ;; Set :won flag when all treasures are deposited (enables stone-barrow entry)
        all-deposited? (= (:deposited with-deposit #{}) treasures)]
    (cond-> with-deposit
      all-deposited? (update :flags conj :won))))

;; =============================================================================
;; Debug Utilities
;; =============================================================================

(defn summarize-registry
  "Print summary of action registry."
  [registry]
  (let [by-type (group-by :type (vals registry))]
    (println "\n=== Action Registry Summary ===")
    (doseq [[type actions] (sort-by first by-type)]
      (println (str "  " (name type) ": " (count actions) " actions")))
    (println "  Total:" (count registry) "actions")))

(defn print-action
  "Pretty print an action."
  [action]
  (println "\n=== Action:" (:id action) "===")
  (println "  Type:" (:type action))
  (println "  Cost:" (:cost action))
  (println "  Preconditions:")
  (when (:here (:preconditions action))
    (println "    Location:" (:here (:preconditions action))))
  (when (seq (:inventory (:preconditions action)))
    (println "    Items:" (:inventory (:preconditions action))))
  (when (seq (:flags (:preconditions action)))
    (println "    Flags:" (:flags (:preconditions action))))
  (println "  Effects:")
  (when (seq (:flags-set (:effects action)))
    (println "    Sets:" (:flags-set (:effects action))))
  (when (seq (:inventory-add (:effects action)))
    (println "    Adds:" (:inventory-add (:effects action))))
  (println "  Commands:" (:commands action)))
