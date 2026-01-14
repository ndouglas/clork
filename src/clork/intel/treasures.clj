(ns clork.intel.treasures
  "Treasure metadata for speedrun planning.

   This module provides comprehensive treasure information needed by the planner:
   - Locations and accessibility requirements
   - Dependencies (what must be done to obtain each treasure)
   - Point values for prioritization
   - Container relationships (e.g., canary inside egg)

   Key functions:
   - treasure-accessible?: Check if a treasure can currently be obtained
   - treasures-requiring-flag: Find treasures needing a specific flag
   - treasure-dependencies: Get the dependency chain for a treasure"
  (:require [clork.game-state :as gs]
            [clork.intel.routing :as routing]))

;;; ---------------------------------------------------------------------------
;;; TREASURE DEFINITIONS
;;; ---------------------------------------------------------------------------

(def treasure-metadata
  "Complete metadata for all 19 treasures in Zork I.
   Each treasure specifies:
   - :location - Default room where treasure is found
   - :value - Points earned when placed in trophy case
   - :requires - Flags or conditions needed to access
   - :container - If treasure is inside another object
   - :created-by - If treasure is created by an action (not just picked up)
   - :puzzle - Puzzle that unlocks access to this treasure
   - :notes - Special handling notes"

  {:egg
   {:location :up-a-tree
    :value 5   ; 5 points for egg, but contains canary worth 6
    :requires #{}
    :container nil
    :created-by nil
    :puzzle nil
    :notes "Can be opened by thief or carefully with tools. Contains canary."}

   :clockwork-canary
   {:location nil  ; Inside the egg
    :value 6
    :requires #{:egg-opened}
    :container :egg
    :created-by nil
    :puzzle :thief-expedition  ; Collected as part of thief expedition
    :notes "Must open egg via thief. Drop egg in troll-room, thief steals it, kill thief in treasure-room."}

   :painting
   {:location :gallery
    :value 6   ; TVALUE from ZIL
    :requires #{}
    :container nil
    :created-by nil
    :puzzle nil
    :notes "Heavy painting (size 15), takes inventory space."}

   :platinum-bar
   {:location :loud-room
    :value 10
    :requires #{:loud-flag}
    :container nil
    :created-by nil
    :puzzle :loud-room-echo
    :notes "Cannot be taken until loud-flag is set (say 'echo')."}

   :ivory-torch
   {:location :torch-room
    :value 14
    :requires #{:dome-flag}
    :container nil
    :created-by nil
    :puzzle :dome-rope
    :notes "Must tie rope at dome to reach torch room."}

   :gold-coffin
   {:location :egypt-room
    :value 10
    :requires #{}
    :container nil
    :created-by nil
    :puzzle nil
    :notes "Very heavy. Has sceptre inside if opened."}

   :sceptre
   {:location nil  ; Inside coffin
    :value 4
    :requires #{}
    :container :gold-coffin
    :created-by nil
    :puzzle nil
    :notes "Open coffin to get. Used to make rainbow solid."}

   :jade-figurine
   {:location :bat-room
    :value 5
    :requires #{}
    :container nil
    :created-by nil
    :puzzle nil
    :notes "Bat room requires light. Beware of bat transportation."}

   :sapphire-bracelet
   {:location :gas-room
    :value 5
    :requires #{}  ; Just need light and to reach gas room
    :container nil
    :created-by nil
    :puzzle nil
    :notes "Gas room - must turn off flame sources before entering."}

   :huge-diamond
   {:location nil  ; Created inside machine by coal-to-diamond puzzle
    :value 10
    :requires #{:coal-machine-used}
    :container :machine  ; Created inside the machine, must open to retrieve
    :created-by {:action :turn-machine-switch
                 :precondition :coal-in-machine}
    :puzzle :coal-to-diamond
    :notes "Put coal in machine, turn switch. Coal becomes huge diamond. Must open machine to retrieve."}

   :bag-of-coins
   {:location :maze-5  ; In the maze with skeleton
    :value 10
    :requires #{}
    :container nil
    :created-by nil
    :puzzle nil
    :notes "In maze near skeleton. Need to navigate maze."}

   :crystal-skull
   {:location :land-of-living-dead
    :value 10
    :requires #{:lld-flag}
    :container nil
    :created-by nil
    :puzzle :exorcism
    :notes "Must complete exorcism to access Land of Living Dead."}

   :crystal-trident
   {:location :atlantis-room
    :value 11  ; TVALUE from ZIL (trophy case value)
    :requires #{:gates-open}  ; Need dam open for reservoir access
    :container nil
    :created-by nil
    :puzzle :dam-open
    :notes "Atlantis accessible via reservoir when dam gates open."}

   :trunk-of-jewels
   {:location :reservoir
    :value 15
    :requires #{:low-tide}  ; Dam must be open AND time passed
    :container nil
    :created-by nil
    :puzzle :reservoir-low-tide
    :notes "Visible only at low tide after dam gates opened."}

   :pot-of-gold
   {:location :end-of-rainbow
    :value 10
    :requires #{:rainbow-flag}
    :container nil
    :created-by nil
    :puzzle :rainbow-solid
    :notes "Must wave sceptre to make rainbow solid."}

   :brass-bauble
   {:location :forest-path  ; Appears here after wind-canary puzzle
    :value 1
    :requires #{:canary-sung}  ; Need to wind canary first
    :container nil
    :created-by {:action :wind-canary
                 :location :forest-path}
    :puzzle :wind-canary
    :notes "Wind canary in forest to summon songbird which drops bauble."}


   :jeweled-scarab
   {:location :sandy-cave
    :value 5
    :requires #{:boat-ready}  ; Need boat to reach
    :container nil
    :created-by nil
    :puzzle :boat-ready
    :notes "Must use boat to reach sandy beach/cave via river."}

   :silver-chalice
   {:location :treasure-room
    :value 10
    :requires #{}  ; Just need to navigate underground
    :container nil
    :created-by nil
    :puzzle nil
    :notes "In treasure room. Thief may interfere if fighting."}

   :large-emerald
   {:location nil  ; Inside buoy at river-4
    :value 5
    :requires #{:boat-ready}  ; Buoy is in river, needs boat to reach
    :container :buoy
    :created-by nil
    :puzzle :buoy-collection  ; Separate puzzle for taking/opening buoy
    :notes "Inside buoy at river-4, must take buoy while in boat."}})

;;; ---------------------------------------------------------------------------
;;; TREASURE QUERY FUNCTIONS
;;; ---------------------------------------------------------------------------

(defn all-treasures
  "Get list of all treasure IDs."
  []
  (keys treasure-metadata))

(defn treasure-value
  "Get point value of a treasure."
  [treasure-id]
  (:value (get treasure-metadata treasure-id)))

(defn treasure-location
  "Get default location of a treasure (nil if created or in container)."
  [treasure-id]
  (:location (get treasure-metadata treasure-id)))

(defn treasure-requires
  "Get flags required to access a treasure."
  [treasure-id]
  (:requires (get treasure-metadata treasure-id) #{}))

(defn treasure-container
  "Get container that holds this treasure (nil if not in container)."
  [treasure-id]
  (:container (get treasure-metadata treasure-id)))

(defn treasure-puzzle
  "Get puzzle that unlocks this treasure (nil if none)."
  [treasure-id]
  (:puzzle (get treasure-metadata treasure-id)))

(defn treasures-in-container
  "Find treasures contained in an object."
  [container-id]
  (->> treasure-metadata
       (filter (fn [[_id meta]] (= (:container meta) container-id)))
       (map first)))

(defn treasures-requiring-flag
  "Find treasures that require a specific flag."
  [flag]
  (->> treasure-metadata
       (filter (fn [[_id meta]] (contains? (:requires meta) flag)))
       (map first)))

(defn treasures-requiring-puzzle
  "Find treasures unlocked by a specific puzzle."
  [puzzle-id]
  (->> treasure-metadata
       (filter (fn [[_id meta]] (= (:puzzle meta) puzzle-id)))
       (map first)))

(defn created-treasures
  "Get treasures that are created (not just picked up)."
  []
  (->> treasure-metadata
       (filter (fn [[_id meta]] (some? (:created-by meta))))
       (map first)))

;;; ---------------------------------------------------------------------------
;;; TREASURE ACCESSIBILITY
;;; ---------------------------------------------------------------------------

(defn treasure-accessible?
  "Check if a treasure is currently accessible in game state.
   Returns {:accessible bool :reason string :missing-flags #{} :missing-puzzle kw}"
  [game-state treasure-id]
  (let [meta (get treasure-metadata treasure-id)
        required-flags (:requires meta)
        current-flags (routing/extract-available-flags game-state)
        missing-flags (clojure.set/difference required-flags current-flags)
        container (:container meta)
        ;; Check if container is open (if treasure is in one)
        container-open (or (nil? container)
                           (gs/set-thing-flag? game-state container :open))
        ;; Check if treasure exists (for created treasures)
        created-by (:created-by meta)
        treasure-exists (or (nil? created-by)
                            ;; Check if treasure is in game (not in limbo)
                            (not= :limbo (gs/get-thing-loc-id game-state treasure-id)))]
    (cond
      ;; Missing required flags
      (seq missing-flags)
      {:accessible false
       :reason (str "Missing flags: " (clojure.string/join ", " (map name missing-flags)))
       :missing-flags missing-flags
       :missing-puzzle (:puzzle meta)}

      ;; Container not open
      (not container-open)
      {:accessible false
       :reason (str "Container " (name container) " not open")
       :missing-flags #{}
       :missing-puzzle nil
       :container container}

      ;; Treasure doesn't exist yet (needs to be created)
      (not treasure-exists)
      {:accessible false
       :reason "Treasure not yet created"
       :missing-flags #{}
       :missing-puzzle nil
       :created-by created-by}

      :else
      {:accessible true
       :reason "Treasure is accessible"
       :missing-flags #{}
       :missing-puzzle nil})))

(defn accessible-treasures
  "Get all treasures currently accessible in game state."
  [game-state]
  (->> (all-treasures)
       (filter #(:accessible (treasure-accessible? game-state %)))))

(defn inaccessible-treasures
  "Get all treasures not currently accessible, with reasons."
  [game-state]
  (->> (all-treasures)
       (map (fn [t] [t (treasure-accessible? game-state t)]))
       (filter (fn [[_ status]] (not (:accessible status))))
       (into {})))

;;; ---------------------------------------------------------------------------
;;; TREASURE DEPENDENCY ANALYSIS
;;; ---------------------------------------------------------------------------

(defn treasure-dependencies
  "Get the full dependency chain for obtaining a treasure.
   Returns {:flags-needed #{} :puzzles-needed [] :items-needed [] :containers-to-open []}"
  [treasure-id]
  (let [meta (get treasure-metadata treasure-id)
        flags (:requires meta)
        puzzle (:puzzle meta)
        container (:container meta)
        created-by (:created-by meta)]
    {:treasure treasure-id
     :flags-needed flags
     :puzzles-needed (if puzzle [puzzle] [])
     :containers-to-open (if container [container] [])
     :created-by created-by
     :location (:location meta)
     :value (:value meta)}))

(defn treasure-collection-order
  "Suggest optimal order to collect treasures based on dependencies.
   Treasures with fewer dependencies come first."
  [treasures]
  (sort-by (fn [t]
             (let [deps (treasure-dependencies t)]
               (+ (count (:flags-needed deps))
                  (count (:puzzles-needed deps))
                  (count (:containers-to-open deps))
                  (if (:created-by deps) 1 0))))
           treasures))

(defn total-treasure-value
  "Calculate total point value of a set of treasures."
  [treasures]
  (reduce + 0 (map treasure-value treasures)))

;;; ---------------------------------------------------------------------------
;;; PUZZLE-TREASURE RELATIONSHIPS
;;; ---------------------------------------------------------------------------

(defn puzzles-for-treasures
  "Get all puzzles needed to collect a set of treasures."
  [treasures]
  (->> treasures
       (map treasure-puzzle)
       (filter some?)
       (set)))

(defn flags-for-treasures
  "Get all flags needed to collect a set of treasures."
  [treasures]
  (->> treasures
       (mapcat treasure-requires)
       (set)))

(defn treasure-unlock-chain
  "For a treasure requiring a puzzle, get the full unlock chain.
   Example: :crystal-skull needs :exorcism which needs items at various locations."
  [treasure-id]
  (let [meta (get treasure-metadata treasure-id)
        puzzle (:puzzle meta)]
    (if puzzle
      {:treasure treasure-id
       :requires-puzzle puzzle
       :puzzle-flags (:requires meta)
       :treasure-location (:location meta)}
      {:treasure treasure-id
       :no-puzzle-needed true
       :location (:location meta)
       :requires (:requires meta)})))

;;; ---------------------------------------------------------------------------
;;; SCORE CALCULATIONS
;;; ---------------------------------------------------------------------------

(def max-treasure-score
  "Maximum possible score from treasures."
  (total-treasure-value (all-treasures)))

(defn treasures-by-value
  "Get treasures sorted by value (highest first)."
  []
  (sort-by #(- (treasure-value %)) (all-treasures)))

(defn treasures-by-efficiency
  "Get treasures sorted by value/difficulty ratio.
   Difficulty = number of dependencies."
  []
  (sort-by (fn [t]
             (let [deps (treasure-dependencies t)
                   difficulty (+ 1  ; Base difficulty
                                 (count (:flags-needed deps))
                                 (* 2 (count (:puzzles-needed deps)))
                                 (if (:created-by deps) 3 0))]
               (- (/ (treasure-value t) difficulty))))
           (all-treasures)))

;;; ---------------------------------------------------------------------------
;;; INVENTORY WEIGHT UTILITIES
;;; ---------------------------------------------------------------------------

(defn object-weight
  "Get weight/size of an object in game state.
   Returns 0 if object has no size defined."
  [game-state object-id]
  (get-in game-state [:objects object-id :size] 0))

(defn inventory-weight
  "Calculate total weight of player's inventory."
  [game-state]
  (let [inventory (gs/get-contents game-state :adventurer)]
    (reduce + 0 (map #(object-weight game-state %) inventory))))

(defn inventory-capacity
  "Get the player's maximum carrying capacity.
   In Zork, this is typically 100 units."
  []
  100)

(defn inventory-remaining-capacity
  "Calculate remaining carrying capacity."
  [game-state]
  (- (inventory-capacity) (inventory-weight game-state)))

(defn can-carry?
  "Check if player can carry an additional object."
  [game-state object-id]
  (let [obj-weight (object-weight game-state object-id)
        remaining (inventory-remaining-capacity game-state)]
    (<= obj-weight remaining)))

(defn treasures-that-fit
  "Filter treasures to those that can fit in remaining inventory.
   Returns treasures sorted by value (highest first)."
  [game-state treasures]
  (let [remaining (inventory-remaining-capacity game-state)]
    (->> treasures
         (filter (fn [t]
                   (let [obj-weight (object-weight game-state t)]
                     (<= obj-weight remaining))))
         (sort-by #(- (treasure-value %))))))

(defn treasures-by-value-weight-ratio
  "Get treasures sorted by value/weight ratio (best value per weight first).
   Useful for optimizing limited inventory capacity."
  [game-state]
  (sort-by (fn [t]
             (let [weight (max 1 (object-weight game-state t))
                   value (treasure-value t)]
               (- (/ value weight))))
           (all-treasures)))

(defn optimal-treasure-set
  "Find the optimal set of treasures that fit in remaining capacity.
   Uses greedy algorithm: take highest value/weight ratio items first.
   Returns {:treasures [...] :total-value N :total-weight N}"
  [game-state available-treasures]
  (let [remaining (inventory-remaining-capacity game-state)
        sorted (sort-by (fn [t]
                          (let [weight (max 1 (object-weight game-state t))
                                value (treasure-value t)]
                            (- (/ value weight))))
                        available-treasures)]
    (loop [treasures sorted
           selected []
           total-weight 0
           total-value 0]
      (if (empty? treasures)
        {:treasures selected
         :total-value total-value
         :total-weight total-weight
         :remaining-capacity (- remaining total-weight)}
        (let [t (first treasures)
              weight (object-weight game-state t)
              value (treasure-value t)]
          (if (<= (+ total-weight weight) remaining)
            (recur (rest treasures)
                   (conj selected t)
                   (+ total-weight weight)
                   (+ total-value value))
            (recur (rest treasures)
                   selected
                   total-weight
                   total-value)))))))
