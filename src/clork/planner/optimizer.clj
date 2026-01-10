(ns clork.planner.optimizer
  "Route optimization for Clork speedrun plans.

   Converts abstract action plans into optimized command sequences:
   - Merges adjacent movement actions into efficient routes
   - Optimizes multi-destination routes (traveling salesman approach)
   - Handles inventory constraints (deposit trips)
   - Generates executable command sequences
   - Validates paths by running them through the game engine"
  (:require [clojure.set :as set]
            [clork.planner.actions :as actions]
            [clork.planner.constraints :as constraints]
            [clojure.data.priority-map :refer [priority-map]]))

;; =============================================================================
;; Region-Based Treasure Grouping
;; =============================================================================

(def treasure-regions
  "Treasures grouped by region for efficient collection.
   Collecting treasures in the same region together reduces backtracking."
  {:surface #{:jeweled-egg}
   :house #{:painting}
   :maze #{:bag-of-coins}
   :dam-area #{:emerald :platinum-bar :jewel-encrusted-trunk}
   :temple #{:ivory-torch :sceptre}
   :hades #{:crystal-skull}
   :thief-lair #{:jade-figurine :silver-chalice :gold-coffin :clockwork-canary}
   :coal-mine #{:huge-diamond :scarab :sapphire-bracelet}  ; bracelet is in gas-room
   :rainbow #{:pot-of-gold}
   :loud-room #{:gold-bar}
   :cyclops #{:brass-bauble}
   :atlantis #{:crystal-trident}})

(def region-order
  "Recommended order to visit regions (minimizes backtracking)."
  [:surface :house :maze :dam-area :temple :hades
   :coal-mine :loud-room :cyclops :thief-lair :atlantis :rainbow])

(defn treasures-in-region
  "Get set of treasures in a region."
  [region]
  (get treasure-regions region #{}))

(defn region-for-treasure
  "Find which region a treasure is in."
  [treasure-id]
  (first (for [[region treasures] treasure-regions
               :when (contains? treasures treasure-id)]
           region)))

;; =============================================================================
;; A* Pathfinding (copied from main planner for independence)
;; =============================================================================

(defn find-path
  "A* pathfinding from start to goal.
   Returns vector of rooms or nil if no path."
  [graph start goal]
  (if (= start goal)
    [start]
    (loop [open-set (priority-map start 0)
           came-from {}
           g-score {start 0}]
      (if (empty? open-set)
        nil
        (let [[current _] (first open-set)]
          (if (= current goal)
            (loop [path (list goal)
                   node goal]
              (if-let [prev (came-from node)]
                (recur (cons prev path) prev)
                (vec path)))
            (let [open-set (dissoc open-set current)
                  neighbors (get graph current #{})
                  tentative-g (inc (get g-score current))]
              (let [[new-open new-came-from new-g-score]
                    (reduce
                     (fn [[open cf gs] neighbor]
                       (if (< tentative-g (get gs neighbor Integer/MAX_VALUE))
                         [(assoc open neighbor tentative-g)
                          (assoc cf neighbor current)
                          (assoc gs neighbor tentative-g)]
                         [open cf gs]))
                     [open-set came-from g-score]
                     neighbors)]
                (recur new-open new-came-from new-g-score)))))))))

;; =============================================================================
;; Navigation Command Generation
;; =============================================================================

(defn build-nav-graph
  "Build navigation graph from game state.
   Includes both regular exits and known computed exits.

   Options:
   - :available-flags - set of flags currently available. Flag-gated exits
     are excluded unless their required flag is in this set. Default #{} (conservative).
   - :inventory-size - current number of items in inventory. Exits with :max-inventory
     constraints are excluded if inventory exceeds the limit. Default nil (no check)."
  [game-state & {:keys [available-flags inventory-size]
                 :or {available-flags #{} inventory-size nil}}]
  (reduce-kv
   (fn [graph room-id room-def]
     (let [exits (:exits room-def {})
           ;; Get destinations from regular exits (excluding flag-gated ones we can't use)
           regular-destinations
           (reduce-kv
            (fn [s dir exit]
              (let [target (actions/parse-exit exit dir)
                    dest (:to target)
                    ;; Check room-definition level flags
                    required-flags (get-in target [:requires :flags] #{})
                    ;; Also check door-gated-exits map for this transition
                    door-gate (get actions/door-gated-exits [room-id dest])
                    door-flags (get door-gate :flags #{})
                    all-required-flags (set/union required-flags door-flags)
                    ;; Only include if no flag required, or we have all required flags
                    flags-ok? (or (empty? all-required-flags)
                                  (set/subset? all-required-flags available-flags))]
                (if (and dest flags-ok?)
                  (conj s dest)
                  s)))
            #{}
            exits)
           ;; Add destinations from known computed exits (with flag AND inventory checks)
           computed-destinations
           (reduce-kv
            (fn [s dir exit]
              (if (and (map? exit) (:per exit))
                (let [per-fn (:per exit)
                      spec (actions/get-computed-exit room-id per-fn)
                      dest (get actions/computed-exit-destinations [room-id per-fn])
                      ;; Check flag constraints
                      required-flags (get-in spec [:preconditions :flags] #{})
                      flags-ok? (or (empty? required-flags)
                                    (set/subset? required-flags available-flags))
                      ;; Check inventory constraints (e.g., chimney max 2 items)
                      max-inv (get-in spec [:preconditions :max-inventory])
                      inv-ok? (or (nil? max-inv)
                                  (nil? inventory-size)
                                  (<= inventory-size max-inv))]
                  (if (and dest flags-ok? inv-ok?)
                    (conj s dest)
                    s))
                s))
            #{}
            exits)
           ;; Add destinations from teleport edges (e.g., prayer at altar)
           teleport-destinations
           (reduce-kv
            (fn [s [from to] spec]
              (if (= from room-id)
                (let [required-flags (get spec :flags #{})
                      flags-ok? (or (empty? required-flags)
                                    (set/subset? required-flags available-flags))]
                  (if flags-ok?
                    (conj s to)
                    s))
                s))
            #{}
            actions/teleport-edges)]
       (assoc graph room-id (set/union regular-destinations
                                       computed-destinations
                                       teleport-destinations))))
   {}
   (:rooms game-state)))

(defn build-room-graph-with-directions
  "Build graph that tracks direction for each connection.
   Includes both regular exits and known computed exits.

   Options:
   - :available-flags - set of flags currently available. Flag-gated exits
     are excluded unless their required flag is in this set. Default #{} (conservative).
   - :inventory-size - current number of items in inventory. Exits with :max-inventory
     constraints are excluded if inventory exceeds the limit. Default nil (no check)."
  [game-state & {:keys [available-flags inventory-size]
                 :or {available-flags #{} inventory-size nil}}]
  (reduce-kv
   (fn [graph room-id room-def]
     (let [exits (:exits room-def {})
           ;; Get direction map from regular exits (excluding flag-gated ones we can't use)
           regular-dir-map
           (reduce-kv
            (fn [m dir exit]
              (let [parsed (actions/parse-exit exit dir)
                    dest (:to parsed)
                    ;; Check room-definition level flags
                    required-flags (get-in parsed [:requires :flags] #{})
                    ;; Also check door-gated-exits map for this transition
                    door-gate (get actions/door-gated-exits [room-id dest])
                    door-flags (get door-gate :flags #{})
                    all-required-flags (set/union required-flags door-flags)
                    flags-ok? (or (empty? all-required-flags)
                                  (set/subset? all-required-flags available-flags))]
                (if (and dest flags-ok?)
                  (assoc m dest dir)
                  m)))
            {}
            exits)
           ;; Add direction map from known computed exits (with flag AND inventory checks)
           computed-dir-map
           (reduce-kv
            (fn [m dir exit]
              (if (and (map? exit) (:per exit))
                (let [per-fn (:per exit)
                      spec (actions/get-computed-exit room-id per-fn)
                      dest (get actions/computed-exit-destinations [room-id per-fn])
                      ;; Check flag constraints
                      required-flags (get-in spec [:preconditions :flags] #{})
                      flags-ok? (or (empty? required-flags)
                                    (set/subset? required-flags available-flags))
                      ;; Check inventory constraints (e.g., chimney max 2 items)
                      max-inv (get-in spec [:preconditions :max-inventory])
                      inv-ok? (or (nil? max-inv)
                                  (nil? inventory-size)
                                  (<= inventory-size max-inv))]
                  (if (and dest flags-ok? inv-ok?)
                    (assoc m dest dir)
                    m))
                m))
            {}
            exits)
           ;; Add direction map from teleport edges (e.g., prayer at altar)
           ;; Uses the command as a keyword (e.g., :pray) which path-to-directions
           ;; will convert to the string "pray"
           teleport-dir-map
           (reduce-kv
            (fn [m [from to] spec]
              (if (= from room-id)
                (let [required-flags (get spec :flags #{})
                      flags-ok? (or (empty? required-flags)
                                    (set/subset? required-flags available-flags))
                      cmd (keyword (get spec :command))]
                  (if flags-ok?
                    (assoc m to cmd)
                    m))
                m))
            {}
            actions/teleport-edges)]
       (assoc graph room-id (merge regular-dir-map computed-dir-map teleport-dir-map))))
   {}
   (:rooms game-state)))

(defn path-to-directions
  "Convert a path of rooms to direction commands."
  [room-graph path]
  (when (and path (> (count path) 1))
    (loop [directions []
           remaining path]
      (if (< (count remaining) 2)
        directions
        (let [from (first remaining)
              to (second remaining)
              dir-map (get room-graph from {})
              dir (get dir-map to)]
          (if dir
            (recur (conj directions dir) (rest remaining))
            nil))))))

;; =============================================================================
;; Door/Setup Requirements for Navigation
;; =============================================================================

(def door-setup-commands
  "Commands required before traversing certain room transitions.
   Format: {[from-room to-room] {:commands [...] :skip-if-flags #{...}}}

   When :skip-if-flags is provided, the commands are skipped if ALL of those
   flags are already achieved (meaning the setup has already been done)."
  {[:behind-house :kitchen] {:commands ["open window"]
                             :skip-if-flags #{}}  ; Window stays open
   [:living-room :cellar] {:commands ["move rug" "open trap door"]
                           :skip-if-flags #{:rug-moved :trap-door-open}}})

(def conditional-one-way-transitions
  "Transitions that are one-way UNTIL certain flags are set.
   Format: {[from-room to-room] {:unlocked-by #{flags...}}}"
  {[:living-room :cellar] {:unlocked-by #{:magic-flag :grating-unlocked}
                           :description "Trap door bars until alternate exit found"}})

(defn transition-is-one-way?
  "Check if a transition is currently one-way given achieved flags.
   Returns false if any unlocking flag has been achieved."
  ([from-room to-room]
   ;; Without flag info, assume worst case (one-way)
   (contains? conditional-one-way-transitions [from-room to-room]))
  ([from-room to-room achieved-flags]
   (if-let [constraint (get conditional-one-way-transitions [from-room to-room])]
     ;; One-way unless we have an unlocking flag
     (empty? (clojure.set/intersection (:unlocked-by constraint) achieved-flags))
     false)))

(def alternate-return-routes
  "When you can't return the way you came, use these routes.
   Format: {blocked-return-to alternative-route-via}"
  {:living-room {:via :strange-passage
                 :requires #{:cyclops-flag}
                 :from-rooms #{:cyclops-room :treasure-room :strange-passage}}})

(def underground-rooms
  "Rooms that are 'underground' - below the one-way trap door.
   Once you enter these, you cannot return via trap door."
  #{:cellar :troll-room :east-of-chasm :gallery :studio
    :east-west-passage :round-room :narrow-passage :mirror-room-south
    :mirror-room-north :winding-passage :mine-entrance :squeaky-room
    :bat-room :shaft-room :smelly-room :gas-room :coal-mine
    :maze-1 :maze-2 :maze-3 :maze-4 :maze-5 :maze-6 :maze-7 :maze-8
    :maze-9 :maze-10 :maze-11 :maze-12 :maze-13 :maze-14 :maze-15
    :dead-end :grating-room :cyclops-room :strange-passage :treasure-room
    :loud-room :deep-canyon :damp-cave :white-cliffs-beach-north
    :white-cliffs-beach-south :dam :dam-base :dam-lobby :maintenance-room
    :chasm-room :ns-passage :dome-room :torch-room :temple
    :egypt-room :altar :cave :cave-north :twisting-passage
    :engravings-cave :north-south-crawlway :west-of-chasm
    :north-temple :south-temple})

(defn room-is-underground?
  "Check if a room is underground (below trap door)."
  [room]
  (contains? underground-rooms room))

(defn door-commands-for-transition
  "Get door/setup commands needed for a room transition.

   If available-flags is provided and contains ALL skip-if-flags for this
   transition, returns empty (the setup has already been done)."
  ([from-room to-room]
   (door-commands-for-transition from-room to-room #{}))
  ([from-room to-room available-flags]
   (if-let [entry (get door-setup-commands [from-room to-room])]
     (let [{:keys [commands skip-if-flags]} entry]
       (if (and (seq skip-if-flags)
                (set/subset? skip-if-flags available-flags))
         []  ; All flags achieved, skip commands
         commands))
     [])))

(defn generate-navigation
  "Generate navigation commands from current location to destination.
   Includes door-opening commands when needed.

   Options:
   - :blocked-edges - set of [from to] pairs to exclude from pathfinding
   - :available-flags - set of flags currently available. Flag-gated exits
     are only used if their required flag is in this set. Default #{} (conservative).
   - :inventory-size - current number of items in inventory. Computed exits with
     :max-inventory constraints are excluded if inventory exceeds limit."
  [game-state current-room dest-room & {:keys [blocked-edges available-flags inventory-size]
                                         :or {blocked-edges #{} available-flags #{} inventory-size nil}}]
  (let [base-graph (build-nav-graph game-state
                                     :available-flags available-flags
                                     :inventory-size inventory-size)
        ;; Remove blocked edges from graph
        graph (reduce (fn [g [from to]]
                        (if (contains? (get g from #{}) to)
                          (update g from disj to)
                          g))
                      base-graph
                      blocked-edges)
        room-graph (build-room-graph-with-directions game-state
                                                      :available-flags available-flags
                                                      :inventory-size inventory-size)
        path (find-path graph current-room dest-room)]
    (when path
      (let [;; Generate direction commands with door handling
            commands-with-doors
            (loop [remaining path
                   commands []]
              (if (< (count remaining) 2)
                commands
                (let [from (first remaining)
                      to (second remaining)
                      ;; Pass available-flags to skip commands for already-achieved setup
                      door-cmds (door-commands-for-transition from to available-flags)
                      dir-map (get room-graph from {})
                      dir (get dir-map to)]
                  (if dir
                    (recur (rest remaining)
                           (-> commands
                               (into door-cmds)
                               (conj (name dir))))
                    ;; No direction found - shouldn't happen
                    commands))))]
        {:path path
         :commands commands-with-doors
         :moves (count commands-with-doors)
         :has-one-way? (some (fn [[from to]]
                               (transition-is-one-way? from to))
                             (partition 2 1 path))}))))

;; =============================================================================
;; Route Optimization (Nearest Neighbor)
;; =============================================================================

(defn nearest-neighbor-route
  "Compute route using nearest neighbor heuristic.
   Returns destinations in optimized order."
  [game-state start-room destinations]
  (let [graph (build-nav-graph game-state)]
    (loop [current start-room
           remaining (set destinations)
           route []]
      (if (empty? remaining)
        route
        (let [distances (for [dest remaining
                              :let [path (find-path graph current dest)]
                              :when path]
                          {:dest dest :distance (dec (count path))})
              nearest (first (sort-by :distance distances))]
          (if nearest
            (recur (:dest nearest)
                   (disj remaining (:dest nearest))
                   (conj route (:dest nearest)))
            ;; Some destinations unreachable
            (into route remaining)))))))

;; =============================================================================
;; Plan to Commands Conversion
;; =============================================================================

(defn action-location
  "Get the location where an action must be performed."
  [action]
  (get-in action [:preconditions :here]))

(defn expand-action-to-commands
  "Expand an action to its command strings."
  [action]
  (or (:commands action) []))

;; =============================================================================
;; Combat Execution (handles RNG and retry logic)
;; =============================================================================

(defn expand-combat-action
  "Expand a combat action into an executable command block.

   Combat actions have a :combat spec with:
   - :action           - The attack command
   - :enemy            - Enemy keyword for state checking
   - :victory-flag     - Flag set when enemy dies
   - :expected-attacks - Expected number of attacks (new format)
   - :expected-rounds  - [min max] typical range (old format)
   - :max-attacks      - Abort threshold (new format)
   - :max-rounds       - Abort threshold for timeout (old format)
   - :retreat-dir      - Direction to flee for RNG reset
   - :retry-actions    - Commands to change RNG state before retry

   Returns a command block that can be interpreted by the executor:
   {:type :combat-loop
    :action \"attack X with Y\"
    :check-victory (fn [state] ...)
    :max-rounds N
    :on-timeout {:retreat \"dir\" :reset [\"cmd\"...] :retry true}}"
  [action]
  (if-let [combat (:combat action)]
    ;; Return structured combat block
    {:type :combat-loop
     :action (:action combat)
     :enemy (:enemy combat)
     :victory-flag (:victory-flag combat)
     ;; Support both old and new field names
     :max-rounds (or (:max-attacks combat) (:max-rounds combat) 15)
     :max-attacks (or (:max-attacks combat) (:max-rounds combat) 15)
     :expected-rounds (:expected-rounds combat)
     :expected-attacks (:expected-attacks combat)
     :std-dev (:std-dev combat)           ; Standard deviation for attack count calculation
     :on-timeout {:retreat (:retreat-dir combat)
                  :reset (:retry-actions combat [])
                  :retry true}
     :score-requirement (:score-requirement combat 0)
     :recovery-command (:recovery-command combat)  ; Command to recover from disarming (e.g., "take sword")
     :recovery-interval (:recovery-interval combat)  ; Insert recovery every N attacks
     :post-combat (:post-combat combat)}  ; Commands to run after victory (e.g., take chalice)
    ;; Fallback to simple commands
    {:type :simple
     :commands (expand-action-to-commands action)}))

(defn generate-combat-commands
  "Generate combat commands for transcript output.

   For simple output (non-interactive), generates expected number of attacks
   plus a few extra for variance. Real execution would use the loop construct.

   Supports both old format (expected-rounds [min max]) and new format (expected-attacks n).

   Options:
   - :mode :optimistic  - Use lower bound of expected attacks
   - :mode :pessimistic - Use upper bound + buffer (default)
   - :mode :with-retry  - Include retreat/retry sequence"
  [combat-spec & {:keys [mode] :or {mode :pessimistic}}]
  (let [{:keys [action expected-rounds expected-attacks std-dev max-rounds max-attacks
                on-timeout post-combat recovery-command recovery-interval]} combat-spec
        ;; Support both old format [min max] and new format (single number + std-dev)
        [min-attacks base-expected] (cond
                                      expected-attacks [expected-attacks expected-attacks]
                                      expected-rounds expected-rounds
                                      :else [5 10])  ; fallback
        ;; Use std-dev for pessimistic calculation (mean + 2σ for 95% coverage)
        sigma (or std-dev 2)  ; default σ=2 if not specified
        max-total (or max-attacks max-rounds 30)
        attack-count (case mode
                       :optimistic min-attacks
                       :pessimistic (int (Math/ceil (+ base-expected (* 2 sigma))))  ; mean + 2σ
                       :with-retry max-total)
        ;; Generate attack commands, interleaving recovery commands if specified
        ;; Recovery commands handle being disarmed (e.g., "take sword" after thief knocks it away)
        attacks (if (and recovery-command recovery-interval)
                  ;; Interleave recovery command every N attacks
                  (vec (mapcat (fn [i]
                                 (if (and (pos? i) (zero? (mod i recovery-interval)))
                                   [recovery-command action]
                                   [action]))
                               (range attack-count)))
                  ;; Simple attack list
                  (vec (repeat attack-count action)))
        ;; Add post-combat commands (e.g., "take chalice" after killing thief)
        post-cmds (or post-combat [])]
    (if (= mode :with-retry)
      ;; Include retry sequence after attacks
      (-> attacks
          (conj (:retreat on-timeout))
          (into (:reset on-timeout))
          (into (repeat 5 action))
          (into post-cmds))
      (into attacks post-cmds))))

(defn action-is-combat?
  "Check if an action is a combat action with loop spec."
  [action]
  (and (= :combat (:type action))
       (some? (:combat action))))

(defn expand-action-for-transcript
  "Expand an action to commands suitable for transcript testing.
   Handles combat loops specially."
  [action & {:keys [combat-mode] :or {combat-mode :pessimistic}}]
  (if (action-is-combat? action)
    (let [combat-spec (expand-combat-action action)]
      (generate-combat-commands combat-spec :mode combat-mode))
    (expand-action-to-commands action)))

(defn action-sets-flag?
  "Check if an action sets a specific flag."
  [action flag]
  (contains? (get-in action [:effects :flags-set] #{}) flag))

(defn plan-to-command-sequence
  "Convert a plan to an executable command sequence.

   Parameters:
   - game-state: Current game state for navigation
   - plan: Sequence of actions
   - start-room: Starting location
   - opts: {:combat-mode :optimistic|:pessimistic|:with-retry
            :initial-flags #{flags already achieved before this plan}
            :initial-inventory #{items already in inventory}}

   Tracks underground state to properly route around the barred trap door:
   - After going down through trap door, cellar->living-room is blocked
   - Until magic-flag or grating-unlocked is achieved

   Also tracks inventory to avoid routes with inventory constraints (e.g., chimney
   requires <= 2 items).

   Returns:
   {:commands [\"cmd\" ...]
    :total-moves n
    :by-action [{:action :id :commands [...]}]
    :combat-actions [{:action :id :spec {...}}]}"
  [game-state plan start-room & {:keys [combat-mode initial-flags initial-inventory]
                                  :or {combat-mode :pessimistic
                                       initial-flags #{}
                                       initial-inventory #{}}}]
  ;; Determine initial underground state based on initial-flags
  ;; If we have rug-moved + trap-door-open, we've been underground
  (let [been-underground? (and (contains? initial-flags :rug-moved)
                               (contains? initial-flags :trap-door-open))]
    (loop [remaining plan
           current-room start-room
           all-commands []
           by-action []
           combat-actions []
           ;; Track underground state - start as underground if flags indicate we've been there
           underground? been-underground?
           has-return-unlock? (or (contains? initial-flags :magic-flag)
                                  (contains? initial-flags :grating-unlocked))
           ;; Track available flags (starts with initial-flags, accumulates as actions set flags)
           available-flags initial-flags
           ;; Track current inventory (starts with initial-inventory)
           current-inventory initial-inventory]
    (if (empty? remaining)
      {:commands all-commands
       :total-moves (count all-commands)
       :by-action by-action
       :combat-actions combat-actions}
      (let [action (first remaining)
            dest-room (action-location action)

            ;; Calculate blocked edges based on current state
            ;; Once you go down through the trap door, it bars itself PERMANENTLY.
            ;; Having magic-flag or grating-unlocked doesn't unbar the trap door -
            ;; those flags just open alternative routes (strange-passage, grating).
            ;; The trap door is blocked in BOTH directions forever after first use:
            ;; - Can't go UP from cellar (bars block you)
            ;; - Can't go DOWN from living-room (trap door closed and barred)
            blocked-edges (if underground?
                            #{[:cellar :living-room]
                              [:living-room :cellar]}
                            #{})

            ;; Generate navigation with current inventory size
            ;; This excludes routes with inventory constraints we can't meet
            nav (when (and dest-room (not= dest-room current-room))
                  (generate-navigation game-state current-room dest-room
                                       :blocked-edges blocked-edges
                                       :available-flags available-flags
                                       :inventory-size (count current-inventory)))
            nav-commands (or (:commands nav) [])

            ;; Use combat-aware expansion
            action-commands (expand-action-for-transcript action :combat-mode combat-mode)
            combined (into nav-commands action-commands)

            ;; Track combat actions separately
            combat-info (when (action-is-combat? action)
                          {:action (:id action)
                           :spec (expand-combat-action action)})

            ;; Update underground state
            ;; We become underground when navigation goes through trap door
            went-underground? (some #{"down"} nav-commands)
            new-underground? (or underground? went-underground?)

            ;; Update return unlock state
            ;; magic-flag or grating-unlocked allows return via trap door
            just-unlocked? (or (action-sets-flag? action :magic-flag)
                               (action-sets-flag? action :grating-unlocked))
            new-has-unlock? (or has-return-unlock? just-unlocked?)

            ;; Update available flags with flags set by this action
            new-flags (get-in action [:effects :flags-set] #{})
            new-available-flags (set/union available-flags new-flags)

            ;; Update inventory based on action effects
            items-added (get-in action [:effects :inventory-add] #{})
            items-removed (get-in action [:effects :inventory-remove] #{})
            new-inventory (-> current-inventory
                              (set/union items-added)
                              (set/difference items-removed))]

        (recur (rest remaining)
               (or dest-room current-room)
               (into all-commands combined)
               (conj by-action {:action (:id action)
                                :from current-room
                                :to dest-room
                                :nav-commands nav-commands
                                :action-commands action-commands
                                :is-combat? (action-is-combat? action)})
               (if combat-info
                 (conj combat-actions combat-info)
                 combat-actions)
               new-underground?
               new-has-unlock?
               new-available-flags
               new-inventory))))))  ;; Extra paren for outer let

;; =============================================================================
;; Deposit Trip Insertion
;; =============================================================================

(defn insert-deposit-trips
  "Insert deposit trips when inventory gets too full.

   Takes a plan and inserts deposit actions when carrying
   too many items."
  [game-state plan max-items]
  (loop [remaining plan
         current-inventory #{}
         result []
         current-room :west-of-house]
    (if (empty? remaining)
      result
      (let [action (first remaining)
            new-items (get-in action [:effects :inventory-add] #{})
            after-action (set/union current-inventory new-items)]

        (if (> (count after-action) max-items)
          ;; Need deposit trip
          (let [;; Create deposit actions for current inventory
                deposit-actions (map #(actions/generate-deposit-action %)
                                     current-inventory)]
            (recur remaining  ; Re-process current action after deposit
                   #{}        ; Inventory now empty
                   (into result deposit-actions)
                   :living-room))  ; After deposit we're at living room

          ;; Continue without deposit
          (recur (rest remaining)
                 after-action
                 (conj result action)
                 (or (get-in action [:effects :new-location])
                     (action-location action)
                     current-room)))))))

;; =============================================================================
;; Full Route Optimization
;; =============================================================================

(defn optimize-treasure-route
  "Optimize route for collecting multiple treasures.

   Returns:
   {:route [rooms to visit]
    :commands [command strings]
    :treasures [treasures in collection order]
    :total-moves n}"
  [game-state treasures-to-collect]
  (let [;; Get locations for each treasure
        treasure-locations (into {}
                                 (for [t treasures-to-collect]
                                   [t (actions/find-object-room game-state t)]))
        ;; Filter out treasures we can't locate
        locatable (filter #(treasure-locations %) treasures-to-collect)

        ;; Get unique destination rooms
        dest-rooms (set (vals treasure-locations))

        ;; Optimize route
        optimized-route (nearest-neighbor-route game-state :west-of-house dest-rooms)

        ;; Build commands
        graph (build-nav-graph game-state)
        room-graph (build-room-graph-with-directions game-state)]

    (loop [rooms optimized-route
           current :west-of-house
           all-commands []
           collected []]
      (if (empty? rooms)
        {:route optimized-route
         :commands all-commands
         :treasures collected
         :total-moves (count all-commands)}

        (let [dest (first rooms)
              nav (generate-navigation game-state current dest)
              nav-cmds (or (:commands nav) [])
              ;; Find treasures at this location
              treasures-here (filter #(= (treasure-locations %) dest) locatable)
              take-cmds (mapv #(str "take " (name %)) treasures-here)]
          (recur (rest rooms)
                 dest
                 (-> all-commands
                     (into nav-cmds)
                     (into take-cmds))
                 (into collected treasures-here)))))))

;; =============================================================================
;; Complete Speedrun Command Generation
;; =============================================================================

(defn generate-speedrun-commands
  "Generate complete speedrun command sequence.

   This is the main entry point for generating an optimized
   speedrun from a high-level plan.

   Options:
   - :initial-flags - flags already achieved before this plan
   - :initial-inventory - items already in inventory"
  [game-state plan & {:keys [initial-flags initial-inventory]
                       :or {initial-flags #{} initial-inventory #{}}}]
  (let [;; Convert plan to commands
        cmd-seq (plan-to-command-sequence game-state plan :west-of-house
                                           :initial-flags initial-flags
                                           :initial-inventory initial-inventory)

        ;; Calculate statistics
        by-type (group-by #(get-in % [:action] :unknown) (:by-action cmd-seq))]

    {:commands (:commands cmd-seq)
     :total-moves (:total-moves cmd-seq)
     :action-breakdown by-type}))

;; =============================================================================
;; Command Execution Simulation
;; =============================================================================

(defn simulate-commands
  "Simulate executing commands to validate a plan.
   Returns final state after simulation."
  [initial-state commands]
  ;; Simplified simulation - just tracks moves
  {:moves (count commands)
   :commands commands})

;; =============================================================================
;; Debug Utilities
;; =============================================================================

(defn print-route
  "Pretty print an optimized route."
  [{:keys [route commands treasures total-moves]}]
  (println "\n=== Optimized Route ===")
  (println "Rooms:" (count route))
  (println "Commands:" total-moves)
  (println "\nRoute:")
  (doseq [room route]
    (println "  ->" room))
  (println "\nTreasures collected:" (count treasures))
  (doseq [t treasures]
    (println "  -" (name t))))

(defn print-command-sequence
  "Print command sequence with action context."
  [{:keys [commands by-action total-moves]}]
  (println "\n=== Command Sequence (" total-moves "moves) ===")
  (doseq [{:keys [action from to nav-commands action-commands]} by-action]
    (when (or (seq nav-commands) (seq action-commands))
      (println (str "\n# " action))
      (when (and from to (not= from to))
        (println (str "  Navigate: " from " -> " to)))
      (when (seq nav-commands)
        (doseq [cmd nav-commands]
          (println (str "    " cmd))))
      (when (seq action-commands)
        (doseq [cmd action-commands]
          (println (str "    > " cmd)))))))

(defn compare-routes
  "Compare two routes by total moves."
  [route1 route2]
  (println "\n=== Route Comparison ===")
  (println "Route 1:" (:total-moves route1) "moves")
  (println "Route 2:" (:total-moves route2) "moves")
  (let [diff (- (:total-moves route1) (:total-moves route2))]
    (cond
      (pos? diff) (println (str "Route 2 is " diff " moves shorter"))
      (neg? diff) (println (str "Route 1 is " (- diff) " moves shorter"))
      :else (println "Routes are equal length"))))

;; =============================================================================
;; Plan Validation
;; =============================================================================
;; Note: Actual validation via game execution is in clork.planner.validator
;; These functions provide the interface for validated plan generation.

(defn validate-command-sequence
  "Validate a command sequence by executing it through the game engine.

   Parameters:
   - game-state: Initialized game state to start from
   - command-seq: Output from plan-to-command-sequence

   Returns:
   {:valid? boolean
    :validations [{:action id :valid? bool ...}]
    :first-failure (if any) {:action id :error {...}}
    :final-state game-state (if valid)}

   Requires clork.planner.validator to be loaded."
  [game-state command-seq]
  ;; Lazy require to avoid circular dependency
  (require 'clork.planner.validator)
  (let [validate-fn (resolve 'clork.planner.validator/validate-plan-navigations)]
    (validate-fn game-state (:by-action command-seq))))

(defn plan-to-validated-command-sequence
  "Convert a plan to commands AND validate each navigation segment.

   Like plan-to-command-sequence, but actually runs each navigation
   through the game engine to verify it works.

   Parameters:
   - game-state: Initialized game state (used for both nav graph AND validation)
   - plan: Sequence of actions
   - start-room: Starting location
   - opts: {:combat-mode :optimistic|:pessimistic|:with-retry
            :initial-flags #{flags already achieved before this plan}
            :initial-inventory #{items already in inventory}}

   Returns:
   {:commands [...]
    :total-moves n
    :by-action [...]
    :combat-actions [...]
    :validation {:valid? bool :validations [...] :first-failure {...}}}

   Throws assertion error if validation fails."
  [game-state plan start-room & {:keys [combat-mode initial-flags initial-inventory]
                                  :or {combat-mode :pessimistic
                                       initial-flags #{}
                                       initial-inventory #{}}}]
  (let [;; Generate command sequence
        cmd-seq (plan-to-command-sequence game-state plan start-room
                                          :combat-mode combat-mode
                                          :initial-flags initial-flags
                                          :initial-inventory initial-inventory)
        ;; Validate navigations
        validation (validate-command-sequence game-state cmd-seq)]
    (when-not (:valid? validation)
      (throw (ex-info "Plan validation failed"
                      {:type :validation-failed
                       :first-failure (:first-failure validation)
                       :command-sequence cmd-seq})))
    (assoc cmd-seq :validation validation)))

;; =============================================================================
;; Location Dependency Detection
;; =============================================================================

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
   #{:river-1 :river-2 :river-3 :river-4 :river-5}})

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

(defn find-flags-for-path
  "Find what additional flags are needed to reach dest-room from start-room.

   Parameters:
   - game-state: Game state with room definitions
   - start-room: Starting location
   - dest-room: Target location
   - available-flags: Flags already available

   Returns:
   {:reachable? boolean
    :required-flags #{flags needed but not available}
    :path path-if-reachable}"
  [game-state start-room dest-room available-flags]
  ;; First try with current flags
  (let [graph (build-nav-graph game-state :available-flags available-flags)
        path (find-path graph start-room dest-room)]
    (if path
      {:reachable? true
       :required-flags #{}
       :path path}
      ;; No path found - check what flags would help
      ;; Try adding flags progressively to see which ones enable the path
      (let [all-flags (set (keys flag-gated-regions))
            missing-flags (set/difference all-flags available-flags)
            ;; Try with each additional flag to see which enables path
            helpful-flags
            (filter (fn [flag]
                      (let [test-flags (conj available-flags flag)
                            test-graph (build-nav-graph game-state :available-flags test-flags)
                            test-path (find-path test-graph start-room dest-room)]
                        (some? test-path)))
                    missing-flags)]
        (if (seq helpful-flags)
          {:reachable? false
           :required-flags (set helpful-flags)
           :path nil}
          ;; No single flag helps - might need multiple or path doesn't exist
          ;; Try with all flags as sanity check
          (let [full-graph (build-nav-graph game-state :available-flags all-flags)
                full-path (find-path full-graph start-room dest-room)]
            (if full-path
              ;; Path exists with all flags - return required region flags
              {:reachable? false
               :required-flags (flags-required-for-room dest-room)
               :path nil}
              ;; Path doesn't exist even with all flags - truly unreachable
              {:reachable? false
               :required-flags #{}
               :path nil
               :error :no-path})))))))

(defn location-dependencies
  "Get all flag dependencies for reaching a location.
   Returns set of flags that must be achieved before going to this room."
  [game-state dest-room]
  (let [start-room :west-of-house  ; Game always starts here
        result (find-flags-for-path game-state start-room dest-room #{})]
    (:required-flags result #{})))

;; =============================================================================
;; Validated Execution
;; =============================================================================

(defn annotate-command-sequence
  "Add expected state annotations to a command sequence.

   Takes the output of plan-to-command-sequence and annotates each command
   with expected outcomes (room, inventory requirements, etc).

   Returns vector of {:command \"cmd\" :expected {...}}."
  [game-state commands by-action start-room]
  (let [;; Build a map of command index -> action info from by-action
        action-bounds
        (loop [actions by-action
               cmd-idx 0
               bounds []]
          (if (empty? actions)
            bounds
            (let [action (first actions)
                  nav-count (count (:nav-commands action))
                  action-count (count (:action-commands action))
                  total (+ nav-count action-count)
                  end-idx (+ cmd-idx total)]
              (recur (rest actions)
                     end-idx
                     (conj bounds {:action action
                                   :start-idx cmd-idx
                                   :nav-end-idx (+ cmd-idx nav-count)
                                   :end-idx end-idx})))))]

    ;; Annotate each command
    (loop [idx 0
           current-room start-room
           annotated []]
      (if (>= idx (count commands))
        annotated
        (let [cmd (nth commands idx)
              ;; Find which action this command belongs to
              action-info (first (filter #(and (>= idx (:start-idx %))
                                               (< idx (:end-idx %)))
                                         action-bounds))
              ;; Determine expected room after this command
              is-nav? (and action-info (< idx (:nav-end-idx action-info)))
              dest-room (when action-info (:to (:action action-info)))
              expected-room (if (and is-nav? dest-room
                                     (= idx (dec (:nav-end-idx action-info))))
                              ;; Last nav command - expect to be at destination
                              dest-room
                              ;; Mid-navigation or action - less strict
                              nil)
              ;; Build annotation
              annotation {:command cmd
                          :index idx
                          :expected {:room expected-room
                                     :action-id (when action-info
                                                  (get-in action-info [:action :action]))}}
              ;; Update current room if this completes navigation
              new-room (or expected-room current-room)]
          (recur (inc idx) new-room (conj annotated annotation)))))))

(defn validate-state
  "Check if actual game state matches expected state.

   Returns {:valid? bool :errors [...]} where errors describe mismatches.

   Checks:
   - Room matches expected (if specified)
   - Player is alive
   - Required items still in inventory
   - No unexpected location (e.g., bat grabbed us)"
  [actual-state expected & {:keys [required-inventory]}]
  (let [errors (atom [])
        player-id (:player actual-state)]

    ;; Check player death
    (when (:player-dead actual-state)
      (swap! errors conj {:type :player-dead
                          :message "Player has died"}))

    ;; Check expected room
    (when-let [expected-room (:room expected)]
      (when (and expected-room (not= (:here actual-state) expected-room))
        (swap! errors conj {:type :wrong-room
                            :expected expected-room
                            :actual (:here actual-state)
                            :message (str "Expected room " expected-room
                                          " but at " (:here actual-state))})))

    ;; Check required inventory items
    (when required-inventory
      (doseq [item required-inventory]
        (let [item-loc (get-in actual-state [:objects item :in])]
          (when (not= item-loc player-id)
            (swap! errors conj {:type :missing-item
                                :item item
                                :actual-location item-loc
                                :message (str "Expected " item " in inventory but it's at " item-loc)})))))

    {:valid? (empty? @errors)
     :errors @errors}))

(defn run-with-validation
  "Execute a command sequence with state validation.

   Parameters:
   - execute-fn: (fn [state command] [new-state output]) - command executor
   - initial-state: Starting game state
   - annotated-commands: Output of annotate-command-sequence
   - opts: {:critical-items #{items that must stay in inventory once acquired}
            :on-error :continue|:stop (default :stop)
            :verbose? bool}

   Tracks which items have been acquired and only validates them after pickup.

   Returns:
   {:success? bool
    :final-state state
    :commands-run n
    :errors [{:index n :error {...}}]
    :events [{:index n :type :kill|:deposit|:pickup :details ...}]}"
  [execute-fn initial-state annotated-commands & {:keys [critical-items on-error verbose?]
                                                   :or {critical-items #{:sword :brass-lantern}
                                                        on-error :stop
                                                        verbose? false}}]
  (loop [state initial-state
         remaining annotated-commands
         idx 0
         errors []
         events []
         ;; Track items we've acquired and should keep
         acquired-items #{}]
    (if (empty? remaining)
      ;; Success - ran all commands
      {:success? (empty? errors)
       :final-state state
       :commands-run idx
       :errors errors
       :events events}

      (let [{:keys [command expected]} (first remaining)
            [new-state output] (execute-fn state command)
            player-id (:player new-state)

            ;; Track item pickups - add to acquired-items when we take critical items
            picked-up (when (re-find #"^take " command)
                        (let [item-name (second (re-find #"take (\S+)" command))
                              item-kw (keyword item-name)]
                          (when (and (critical-items item-kw)
                                     (= player-id (get-in new-state [:objects item-kw :in])))
                            item-kw)))
            new-acquired (if picked-up (conj acquired-items picked-up) acquired-items)

            ;; Only validate items we've already acquired
            ;; This prevents false positives before we pick up sword/lamp
            validation (validate-state new-state expected
                                       :required-inventory new-acquired)

            ;; Detect events (kills, deposits, pickups)
            event (cond
                    (re-find #"(?i)breathes his last" output)
                    {:index idx :type :kill
                     :target (cond (re-find #"troll" output) :troll
                                   (re-find #"thief" output) :thief
                                   :else :unknown)}

                    (and (re-find #"put .* in case" command)
                         (re-find #"Done" output))
                    {:index idx :type :deposit :command command}

                    picked-up
                    {:index idx :type :pickup :item picked-up}

                    :else nil)]

        (when (and verbose? event)
          (println (format "%3d. %s: %s" idx command (:type event))))

        (if (and (not (:valid? validation))
                 (= on-error :stop))
          ;; Validation failed and we're stopping on errors
          {:success? false
           :final-state new-state
           :commands-run idx
           :errors (conj errors {:index idx
                                 :command command
                                 :validation-errors (:errors validation)})
           :events events
           :stopped-at {:index idx :command command :reason (:errors validation)}}

          ;; Continue (either valid or on-error is :continue)
          (recur new-state
                 (rest remaining)
                 (inc idx)
                 (if (:valid? validation)
                   errors
                   (conj errors {:index idx
                                 :command command
                                 :validation-errors (:errors validation)}))
                 (if event (conj events event) events)
                 new-acquired))))))

(defn run-speedrun-validated
  "High-level function to run a speedrun with full validation.

   Parameters:
   - execute-fn: Command executor (fn [state cmd] [new-state output])
   - initial-state: Starting game state
   - cmd-seq: Output of plan-to-command-sequence
   - start-room: Starting room

   Returns detailed result with success/failure info and diagnostics."
  [execute-fn initial-state cmd-seq start-room]
  (let [annotated (annotate-command-sequence
                   initial-state
                   (:commands cmd-seq)
                   (:by-action cmd-seq)
                   start-room)]
    (run-with-validation execute-fn initial-state annotated
                         :critical-items #{:sword :brass-lantern}
                         :on-error :stop
                         :verbose? true)))
