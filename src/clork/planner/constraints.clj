(ns clork.planner.constraints
  "Constraint system for GOAP planning.

   Tracks and enforces:
   - Inventory weight limits
   - Light requirements for dark rooms
   - One-way path dependencies (collect items before committing)
   - Door state requirements"
  (:require [clojure.set :as set]))

;; =============================================================================
;; Inventory Constraints
;; =============================================================================

(def max-inventory-weight
  "Maximum weight player can carry.
   Based on ZIL LOAD-ALLOWED = 100."
  100)

(def object-weights
  "Default weights for objects. Extracted from objects.clj :size fields.
   Objects without :size default to 0."
  {:sword 30
   :brass-lantern 15
   :rope 10
   :garlic 4
   :nasty-knife 10
   :jeweled-egg 8
   :bird 3
   :coal 20
   :sceptre 3
   :coffin 55
   :torch 20
   :trident 20
   :skull 15
   :trunk 35
   :bar 10
   :emerald 6
   :painting 15
   :pot-of-gold 15
   :figurine 8
   :bracelet 5
   :diamond 6
   :coins 10
   :scarab 5
   :bauble 5
   :shovel 15
   :buoy 10
   :pump 10
   :matchbook 2
   :bell 10
   :book 10
   :candles 4
   :map 2
   :gunk 6
   :wrench 10
   :screwdriver 4
   :key 2})

(defn get-object-weight
  "Get weight of an object from game state."
  [game-state obj-id]
  (or (get-in game-state [:objects obj-id :size])
      (get object-weights obj-id)
      0))

(defn inventory-weight
  "Calculate total weight of items in a collection."
  [game-state items]
  (reduce + 0 (map #(get-object-weight game-state %) items)))

(defn can-carry?
  "Check if player can carry an additional item without exceeding weight limit."
  [game-state current-inventory new-item]
  (let [current-weight (inventory-weight game-state current-inventory)
        new-weight (get-object-weight game-state new-item)]
    (<= (+ current-weight new-weight) max-inventory-weight)))

(defn must-drop-for
  "Calculate what items must be dropped to pick up new-item.
   Returns set of items to drop (preferring non-essential items)."
  [game-state current-inventory new-item essential-items]
  (let [target-weight (get-object-weight game-state new-item)
        droppable (set/difference (set current-inventory) essential-items)
        sorted-droppable (sort-by #(get-object-weight game-state %) > droppable)]
    (loop [to-drop #{}
           freed 0
           remaining sorted-droppable]
      (if (or (>= freed target-weight) (empty? remaining))
        to-drop
        (let [item (first remaining)
              weight (get-object-weight game-state item)]
          (recur (conj to-drop item)
                 (+ freed weight)
                 (rest remaining)))))))

;; =============================================================================
;; Light Requirements
;; =============================================================================

(def dark-rooms
  "Set of rooms that require a light source.
   Entering these without light causes death (grue attack)."
  #{:cellar :troll-room :maze-1 :maze-2 :maze-3 :maze-4 :maze-5 :maze-6
    :maze-7 :maze-8 :maze-9 :maze-10 :maze-11 :maze-12 :maze-13 :maze-14 :maze-15
    :east-west-passage :round-room :narrow-passage :dome-room :torch-room
    :egypt-room :engravings-cave :mine-entrance :coal-mine-1 :coal-mine-2
    :coal-mine-3 :coal-mine-4 :shaft-room :smelly-room :gas-room :bat-room
    :entrance-to-hades :land-of-living-dead :tiny-cave :mirror-room-1 :mirror-room-2
    :cold-passage :slide-room :machine-room :cyclops-room :treasure-room :studio
    :gallery :east-of-chasm :ns-passage :deep-canyon :dam-room :reservoir
    :reservoir-north :reservoir-south :loud-room :damp-cave :atlantis-room
    :winding-passage :north-temple :south-temple :dead-end-1 :dead-end-2
    :grating-room :dam-base :dam-lobby :maintenance-room :chasm-room})

(def light-sources
  "Items that provide light when in inventory."
  #{:brass-lantern :ivory-torch :candles :matchbook})

(defn requires-light?
  "Check if a room requires a light source."
  [room-id]
  (contains? dark-rooms room-id))

(defn has-light?
  "Check if inventory contains an active light source."
  [inventory]
  ;; Simplified: assumes lantern/torch are on when carried
  (or (contains? inventory :brass-lantern)
      (contains? inventory :ivory-torch)))

(defn light-constraint-satisfied?
  "Check if light constraints are satisfied for entering a room."
  [room-id inventory]
  (or (not (requires-light? room-id))
      (has-light? inventory)))

;; =============================================================================
;; One-Way Path Constraints
;; =============================================================================

(def one-way-paths
  "Map of one-way path sources to their destinations and items that become unreachable.
   Format: {from-room {:to to-room :blocks-access-to #{rooms...}}}

   These are critical for planning - must collect items from blocked areas first."
  {;; Maze diodes - one-way chutes
   :maze-2 {:to :maze-4
            :blocks-access-to #{}}
   :maze-7 {:to :dead-end-1
            :blocks-access-to #{}}
   :maze-9 {:to :maze-11
            :blocks-access-to #{}}
   :maze-12 {:to :maze-5
             :blocks-access-to #{}}

   ;; Coal mine slide - major one-way
   :slide-room {:to :cellar
                :blocks-access-to #{:coal-mine-4 :gas-room :shaft-room}
                :items-behind #{:huge-diamond :scarab :coal}}

   ;; Boat landing - can only go downstream
   ;; Sandy beach to white cliffs beach is one-way (no way back up river)
   :sandy-beach {:to :white-cliffs-beach
                 :items-behind #{:buoy :scarab}}})

(def items-before-oneway
  "Items that MUST be collected before taking certain one-way paths.
   Format: {:oneway-id #{:required-items}}"
  {:slide-to-cellar #{:huge-diamond :scarab}
   :boat-downstream #{:buoy :emerald :scarab}})

(defn items-blocked-by-oneway
  "Return items that become unreachable after taking a one-way path."
  [from-room]
  (get-in one-way-paths [from-room :items-behind] #{}))

(defn validate-oneway-safe
  "Validate that all items behind a one-way path have been collected.
   Returns {:safe? bool :missing #{items}}"
  [from-room inventory-collected]
  (let [blocked (items-blocked-by-oneway from-room)
        missing (set/difference blocked (set inventory-collected))]
    {:safe? (empty? missing)
     :missing missing}))

;; =============================================================================
;; Door Constraints
;; =============================================================================

(def door-keys
  "Map of doors to their opening requirements.
   Some doors need keys, others can be opened directly."
  {:trap-door {:opens-with :hand :from #{:living-room}}
   :kitchen-window {:opens-with :hand :from #{:behind-house}}
   :grate {:opens-with :skeleton-key :or-flag :grate-revealed}})

(defn door-openable?
  "Check if a door can be opened given inventory and current room."
  [game-state door-id inventory current-room]
  (let [door-info (get door-keys door-id)]
    (cond
      ;; No special requirements
      (nil? door-info) true

      ;; Hand-openable from specific locations
      (= (:opens-with door-info) :hand)
      (contains? (:from door-info) current-room)

      ;; Requires key
      (keyword? (:opens-with door-info))
      (contains? inventory (:opens-with door-info))

      :else false)))

;; =============================================================================
;; Composite Constraint Checking
;; =============================================================================

(defn check-action-constraints
  "Check all constraints for executing an action.
   Returns {:valid? bool :violations [...messages...]}"
  [game-state action current-state]
  (let [violations (atom [])]
    ;; Check light constraint for movement to dark rooms
    (when (= (:type action) :movement)
      (let [dest (get-in action [:effects :new-location])]
        (when (and dest
                   (requires-light? dest)
                   (not (has-light? (:inventory current-state))))
          (swap! violations conj "Cannot enter dark room without light"))))

    ;; Check inventory weight for take actions
    (when (= (:type action) :take)
      (let [obj (:object action)]
        (when (and obj
                   (not (can-carry? game-state (:inventory current-state) obj)))
          (swap! violations conj (str "Cannot carry " (name obj) " - too heavy")))))

    ;; Check one-way path safety
    (when (and (= (:type action) :movement)
               (get one-way-paths (:here current-state)))
      (let [validation (validate-oneway-safe (:here current-state)
                                             (:collected current-state #{}))]
        (when-not (:safe? validation)
          (swap! violations conj
                 (str "One-way path warning: missing items " (:missing validation))))))

    {:valid? (empty? @violations)
     :violations @violations}))

;; =============================================================================
;; Planning State
;; =============================================================================

;; Trophy case values for each treasure (points awarded on deposit)
;; Used for score-aware planning (deposit easy treasures before hard combat)
(def treasure-tvalues
  "Trophy case values for treasures. From objects.clj :tvalue fields.
   Score affects combat: fight-strength = 2 + (score / 70), max 7
   At score 140, strength 4 vs thief's 5 shifts from DEF3A to DEF3B tables."
  {:jeweled-scarab 5
   :painting 6
   :gold-coffin 15
   :sceptre 6
   :ivory-torch 6
   :crystal-skull 10
   :jewel-encrusted-trunk 5
   :huge-diamond 10
   :large-emerald 10
   :pot-of-gold 10
   :clockwork-canary 4
   :brass-bauble 1
   :jade-figurine 5
   :crystal-trident 11
   :sapphire-bracelet 5
   :silver-chalice 5
   :egg 5
   :bag-of-coins 5
   :platinum-bar 5})

(defn treasure-value
  "Get the trophy case value for a treasure."
  [treasure-id]
  (get treasure-tvalues treasure-id 0))

(defn score-for-deposits
  "Calculate total score from deposited treasures."
  [deposited]
  (reduce + 0 (map treasure-value deposited)))

(defn fight-strength
  "Calculate player's fighting strength from score.
   Formula: 2 + (score / 70), clamped to 2-7"
  [score]
  (min 7 (max 2 (+ 2 (int (/ score 70))))))

(defn initial-planning-state
  "Create initial state for planning."
  [game-state]
  {:here :west-of-house
   :inventory #{}
   :flags #{}
   :collected #{}  ; All items ever collected (for one-way validation)
   :deposited #{} ; Items in trophy case
   :score 0})      ; Current score (from deposits)

(defn planning-state-after-action
  "Update planning state after executing an action."
  [state action]
  (let [effects (:effects action)
        deposited-item (:deposits effects)]
    (-> state
        ;; Update location
        (cond-> (:new-location effects)
          (assoc :here (:new-location effects)))
        ;; Update inventory
        (update :inventory set/union (:inventory-add effects #{}))
        (update :inventory set/difference (:inventory-remove effects #{}))
        ;; Update flags
        (update :flags set/union (:flags-set effects #{}))
        (update :flags set/difference (:flags-clear effects #{}))
        ;; Track collected items (for one-way validation)
        (update :collected set/union (:inventory-add effects #{}))
        ;; Track deposited items and update score
        (cond-> deposited-item
          (-> (update :deposited conj deposited-item)
              (update :score + (treasure-value deposited-item)))))))

;; =============================================================================
;; Hazard Constraints
;; =============================================================================

(def hazard-rooms
  "Rooms with special hazards that require specific conditions to survive."
  {:gas-room {:hazard :explosion
              :triggers #{:ivory-torch :candles :matchbook}  ; Items with :flame flag
              :safe-with #{:brass-lantern}  ; Lantern has no :flame flag (battery-powered)
              :message "Entering with a flame source (torch/candles/match) causes explosion"}
   :bat-room {:hazard :bat-attack
              :safe-with #{:garlic}
              :message "Bat will steal items unless you have garlic"}})

(def rooms-behind-bat
  "Rooms that can ONLY be reached by passing through bat-room.
   Any route to these rooms requires garlic to avoid bat interference.

   Topology:
   squeeky-room -> bat-room -> shaft-room -> smelly-room -> gas-room -> coal-mine..."
  #{:shaft-room :smelly-room :gas-room
    :coal-mine :coal-mine-1 :coal-mine-2 :coal-mine-3 :coal-mine-4
    :mine-1 :mine-2 :mine-3 :mine-4
    :ladder-top :ladder-bottom :timber-room :lower-shaft})

(defn requires-bat-passage?
  "Check if reaching a room requires passing through bat-room.
   Returns true if garlic is needed to safely navigate to this room."
  [room-id]
  (or (= room-id :bat-room)
      (contains? rooms-behind-bat room-id)))

(defn room-hazard
  "Get hazard info for a room, or nil if no hazard."
  [room-id]
  (get hazard-rooms room-id))

(defn hazard-triggered?
  "Check if a hazard would be triggered given current inventory."
  [room-id inventory]
  (when-let [hazard (room-hazard room-id)]
    (and (seq (clojure.set/intersection inventory (:triggers hazard #{})))
         (empty? (clojure.set/intersection inventory (:safe-with hazard #{}))))))

(defn safe-for-room?
  "Check if inventory is safe for entering a room."
  [room-id inventory]
  (let [hazard (room-hazard room-id)]
    (or (nil? hazard)
        (not (hazard-triggered? room-id inventory)))))

;; =============================================================================
;; Light Source Management
;; =============================================================================

(def lantern-max-turns
  "Maximum turns the brass lantern can be on before dying."
  330)  ; Approximate

(defn light-source-active?
  "Check if a specific light source is active (on and functional)."
  [game-state light-id]
  (case light-id
    :brass-lantern (and (contains? (:inventory game-state #{}) :brass-lantern)
                        (get game-state :lantern-on false)
                        (> (get game-state :lantern-fuel lantern-max-turns) 0))
    :ivory-torch (contains? (:inventory game-state #{}) :ivory-torch)
    :candles (and (contains? (:inventory game-state #{}) :candles)
                  (get game-state :candles-lit false))
    false))

(defn available-light-sources
  "Get set of light sources currently available in inventory."
  [inventory]
  (clojure.set/intersection inventory light-sources))

(defn light-lost?
  "Check if player has lost their light source (robbed by thief, etc.)
   Returns true if in dark room without light."
  [room-id inventory]
  (and (requires-light? room-id)
       (not (has-light? inventory))))

;; =============================================================================
;; Combat Score Requirements
;; =============================================================================

(defn fight-strength
  "Calculate player's fight strength from score.
   Formula: 2 + (score / 70), clamped to 2-7 range.
   At 0 points: 2, at 350 points: 7."
  [score]
  (min 7 (max 2 (+ 2 (int (/ score 70))))))

(def combat-requirements
  "Score requirements for reliable combat success.
   Format: {:enemy {:strength n :recommended-score s :min-score m}}"
  {:thief {:strength 5
           :recommended-score 210  ; Even match (strength 5 vs 5)
           :reliable-score 280     ; Advantage (strength 6 vs 5)
           :notes "Nasty knife gives advantage, sword is harder"}
   :troll {:strength 2
           :recommended-score 0    ; Even weak player can beat troll
           :reliable-score 70      ; Strength 3 vs 2
           :notes "Sword required"}})

(defn can-reliably-defeat?
  "Check if player can reliably defeat an enemy at current score."
  [enemy-id score]
  (let [enemy (get combat-requirements enemy-id)]
    (when enemy
      (>= score (:reliable-score enemy 0)))))

(defn combat-difficulty
  "Return combat difficulty: :impossible, :risky, :even, :advantage, :easy"
  [enemy-id score]
  (let [enemy (get combat-requirements enemy-id)
        player-str (fight-strength score)
        enemy-str (:strength enemy 5)]
    (cond
      (nil? enemy) :unknown
      (< player-str (- enemy-str 2)) :impossible
      (< player-str enemy-str) :risky
      (= player-str enemy-str) :even
      (> player-str enemy-str) :advantage
      :else :easy)))

;; =============================================================================
;; Item Utility Tracking
;; =============================================================================

(def item-uses
  "Map of items to when they're needed.
   After all uses exhausted, item can be dropped."
  {:sword #{:kill-troll}  ; Only needed for troll (thief can use knife)
   :rope #{:tie-rope}     ; One-time use at dome
   :wrench #{:open-dam}   ; Only needed at maintenance room
   :shovel #{:dig-for-scarab}  ; Only needed at sandy cave
   :hand-pump #{:inflate-boat}  ; Only needed once
   :matchbook #{:exorcism}      ; Needed for candles
   :skeleton-key #{:open-grate} ; Needed for grate
   :bell #{:exorcism}
   :book #{:exorcism}
   :candles #{:exorcism}
   :sceptre #{:wave-sceptre}})  ; Needed at rainbow

(defn item-still-needed?
  "Check if an item is still needed for any remaining action."
  [item-id remaining-actions]
  (let [uses (get item-uses item-id #{})]
    (some uses remaining-actions)))

(defn droppable-items
  "Get items that are no longer needed and can be dropped."
  [inventory completed-actions remaining-actions]
  (set (filter #(not (item-still-needed? % remaining-actions)) inventory)))

(defn suggest-drops
  "Suggest items to drop when inventory is full."
  [inventory completed-actions remaining-actions target-count]
  (let [droppable (droppable-items inventory completed-actions remaining-actions)
        sorted (sort-by #(get object-weights % 0) > droppable)]
    (take target-count sorted)))

;; =============================================================================
;; Recovery Actions
;; =============================================================================

(def recovery-strategies
  "Strategies for recovering from bad states."
  {:lost-in-dark
   {:detect (fn [state] (light-lost? (:here state) (:inventory state)))
    :recover "Navigate to surface or find alternate light source"
    :actions [:go-to-surface :find-torch]}

   :robbed-by-thief
   {:detect (fn [state] (and (:thief-stole state)
                             (not (:thief-dead state))))
    :recover "Kill thief to recover stolen items"
    :actions [:go-to-treasure-room :kill-thief]}

   :trapped-by-oneway
   {:detect (fn [state] false)  ; Complex detection
    :recover "Restart or use alternate route"
    :actions []}})

;; =============================================================================
;; Goal State Builder
;; =============================================================================

(defn build-goal-state
  "Build a goal state for planning.

   Examples:
   (build-goal-state {:deposited #{:painting}})
   (build-goal-state {:flags #{:troll-flag} :here :cyclops-room})
   (build-goal-state {:deposited #{:painting :jeweled-egg} :flags #{:troll-flag}})"
  [{:keys [deposited flags here inventory] :or {deposited #{} flags #{} inventory #{}}}]
  {:deposited deposited
   :flags flags
   :here here
   :inventory inventory})

(defn minimal-goal
  "Create a minimal test goal - just one treasure deposited."
  [treasure-id]
  (build-goal-state {:deposited #{treasure-id}}))

(defn flag-goal
  "Create a goal to achieve specific flags."
  [& flag-ids]
  (build-goal-state {:flags (set flag-ids)}))

(defn location-goal
  "Create a goal to reach a location with optional flags."
  [room-id & {:keys [flags] :or {flags #{}}}]
  (build-goal-state {:here room-id :flags flags}))

;; =============================================================================
;; Debug Utilities
;; =============================================================================

(defn print-constraints-for-room
  "Print all constraints affecting a room."
  [room-id]
  (println "\n=== Constraints for" room-id "===")
  (println "  Dark room:" (requires-light? room-id))
  (when-let [oneway (get one-way-paths room-id)]
    (println "  One-way to:" (:to oneway))
    (println "  Blocks access to:" (:blocks-access-to oneway))
    (println "  Items behind:" (:items-behind oneway))))

(defn print-inventory-status
  "Print current inventory weight status."
  [game-state inventory]
  (let [weight (inventory-weight game-state inventory)
        remaining (- max-inventory-weight weight)]
    (println "\n=== Inventory Status ===")
    (println "  Items:" (count inventory))
    (println "  Weight:" weight "/" max-inventory-weight)
    (println "  Remaining capacity:" remaining)))
