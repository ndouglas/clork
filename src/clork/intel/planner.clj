(ns clork.intel.planner
  "Stage 6: The Speedrun Planner

   Generates and executes optimal speedrun plans for Zork I.

   The planner works by:
   1. Determining which treasures to collect
   2. Computing which puzzles must be solved to access them
   3. Ordering goals optimally (TSP with constraints)
   4. Generating action sequences
   5. Executing with replanning on failure

   Key functions:
   - plan-speedrun: Generate complete speedrun plan
   - execute-speedrun: Execute plan with replanning
   - plan-treasure-collection: Plan for specific treasures"
  (:require [clork.game-state :as gs]
            [clork.intel.transition :as transition]
            [clork.intel.affordances :as aff]
            [clork.intel.goals :as goals]
            [clork.intel.routing :as routing]
            [clork.intel.puzzles :as puzzles]
            [clork.intel.treasures :as treasures]))

;;; ---------------------------------------------------------------------------
;;; GOAL TYPES
;;; ---------------------------------------------------------------------------
;;; The planner works with high-level goals that get expanded into actions.

(defn make-goal
  "Create a goal of the given type with parameters."
  [goal-type & {:as params}]
  (assoc params :type goal-type))

(defn treasure-goal
  "Create a goal to collect a treasure."
  [treasure-id]
  (make-goal :collect-treasure :object treasure-id))

(defn puzzle-goal
  "Create a goal to solve a puzzle."
  [puzzle-id]
  (make-goal :solve-puzzle :puzzle puzzle-id))

(defn item-goal
  "Create a goal to acquire an item."
  [item-id]
  (make-goal :acquire-item :object item-id))

(defn location-goal
  "Create a goal to reach a location."
  [room-id]
  (make-goal :reach-location :room room-id))

(defn deposit-goal
  "Create a goal to deposit treasures in trophy case."
  [treasures]
  (make-goal :deposit-treasures :treasures treasures))

(defn light-source-goal
  "Create a goal to turn on a light source."
  [light-id]
  (make-goal :activate-light :object light-id))

;;; ---------------------------------------------------------------------------
;;; SHARP WEAPON HANDLING
;;; ---------------------------------------------------------------------------
;;; Sharp weapons puncture the inflated boat when boarding while held.
;;; To safely board: PUT weapons IN boat first, THEN board.

(def sharp-weapons
  "Objects that puncture the boat when boarding while held."
  #{:sceptre :knife :sword :rusty-knife :axe :stiletto})

(defn held-sharp-weapons
  "Get sharp weapons currently in inventory."
  [game-state]
  (let [inventory (set (gs/get-contents game-state :adventurer))]
    (clojure.set/intersection inventory sharp-weapons)))

(defn generate-safe-boarding-actions
  "Generate actions to safely board the inflated boat.
   Sharp weapons must be PUT IN boat before boarding."
  [game-state]
  (let [sharp-held (held-sharp-weapons game-state)
        put-actions (for [w sharp-held]
                      {:verb :put :direct-object w :indirect-object :inflated-boat})
        board-action {:verb :board :direct-object :inflated-boat}]
    (concat put-actions [board-action])))

;;; ---------------------------------------------------------------------------
;;; PUZZLE REQUIREMENT ANALYSIS
;;; ---------------------------------------------------------------------------

(def access-puzzles
  "Puzzles needed to access different areas of the game.
   Maps area keywords to required puzzle chains."
  {:house [:kitchen-window]           ; Enter house from outside
   :underground [:kitchen-window      ; Must enter house first
                 :trap-door-open]     ; Then open trap door
   :cellar [:kitchen-window :trap-door-open]
   :troll-area [:kitchen-window :trap-door-open :troll-battle]
   :cyclops-area [:kitchen-window :trap-door-open :troll-battle :cyclops-puzzle]
   :living-room-return [:kitchen-window :trap-door-open :troll-battle :cyclops-puzzle]
   :grating-access [:grating-unlock]
   :boat-area [:kitchen-window :trap-door-open :boat-ready]})

(def location-to-area
  "Maps specific rooms to their required access area."
  {;; House (surface access via kitchen window)
   :living-room :house
   :kitchen :house
   :attic :house
   ;; Underground via trap door
   :cellar :underground
   :torch-room :underground
   :dome-room :underground
   :loud-room :underground
   :egypt-room :underground
   :land-of-living-dead :underground
   :entrance-to-hades :underground
   :gallery :underground
   :bat-room :underground
   :gas-room :underground
   :reservoir :underground
   :atlantis-room :underground
   ;; Troll area (past troll)
   :troll-room :troll-area
   :maze-1 :troll-area
   :maze-2 :troll-area
   :maze-3 :troll-area
   :maze-4 :troll-area
   :maze-5 :troll-area
   ;; Cyclops area
   :cyclops-room :cyclops-area
   :strange-passage :cyclops-area
   :treasure-room :cyclops-area
   ;; Grating access (from forest)
   :grating-room :grating-access
   ;; Surface locations (no puzzle needed for basic access)
   :up-a-tree nil  ; Directly reachable
   :end-of-rainbow nil  ; Reachable but treasure needs rainbow-flag
   ;; Boat access - special handling
   :sandy-beach :boat-area})

(defn compute-access-puzzles
  "Compute puzzles needed to access a set of locations."
  [locations]
  (let [areas (set (keep location-to-area locations))
        puzzle-chains (mapcat access-puzzles areas)]
    (set puzzle-chains)))

(defn compute-required-puzzles
  "Compute which puzzles must be solved to collect given treasures.

   Returns a set of puzzle-ids that unlock access to the treasures.
   Includes both treasure-specific puzzles AND access puzzles."
  [treasures]
  (let [;; Direct puzzles from treasure metadata
        direct-puzzles (treasures/puzzles-for-treasures treasures)
        ;; Puzzles required by flags
        required-flags (treasures/flags-for-treasures treasures)
        flag-puzzles (set (mapcat puzzles/puzzles-for-flag required-flags))
        ;; Access puzzles needed to reach treasure locations
        locations (set (keep treasures/treasure-location treasures))
        access (compute-access-puzzles locations)
        ;; Combine all puzzles
        all-puzzles (clojure.set/union direct-puzzles flag-puzzles access)]
    all-puzzles))

(defn compute-puzzle-dependencies
  "Compute puzzle dependency order.
   Some puzzles require other puzzles to be solved first.

   Returns puzzles in execution order (dependencies first)."
  [game-state puzzle-ids]
  (puzzles/puzzle-dependency-order game-state (vec puzzle-ids)))

(defn compute-item-requirements
  "Compute items needed for a set of puzzles.

   Returns set of item-ids that must be acquired.
   Excludes treasures - those are collected during treasure collection."
  [puzzle-ids]
  (let [all-treasures (set (treasures/all-treasures))
        puzzle-items (->> puzzle-ids
                          (mapcat puzzles/puzzle-items-needed)
                          (set))]
    ;; Filter out treasures - they're collected separately
    (clojure.set/difference puzzle-items all-treasures)))

;;; ---------------------------------------------------------------------------
;;; LIGHTWEIGHT PLANNING STATE
;;; ---------------------------------------------------------------------------
;;; Instead of full game state simulation (slow), we track just enough
;;; to compute virtual flags for routing:
;;; - :here - current location
;;; - :open-objects - for virtual door flags
;;; - :game-flags - from puzzle postconditions

(defn initial-planning-state
  "Create initial planning state from game state.
   Includes :inventory to track items for weight calculations."
  [game-state]
  {:here (:here game-state)
   :open-objects (set (for [[obj-id _] (:objects game-state)
                            :when (gs/set-thing-flag? game-state obj-id :open)]
                        obj-id))
   :game-flags (routing/extract-available-flags game-state)
   :inventory (set (gs/get-contents game-state :adventurer))})

(defn planning-flags
  "Compute available flags for routing from planning state.

   Combines game flags with virtual door flags computed from open-objects."
  [planning-state]
  (let [{:keys [open-objects game-flags]} planning-state
        ;; Compute virtual door flags from open objects
        door-flags (set (for [[door-obj flag-name] routing/door-objects
                              :when (contains? open-objects door-obj)]
                          flag-name))]
    (clojure.set/union game-flags door-flags)))

(defn apply-puzzle-effects
  "Update planning state based on puzzle postconditions and consumed items.

   In addition to processing postconditions, this function removes items
   that are given away during puzzle execution (e.g., lunch given to cyclops)."
  [planning-state puzzle-id]
  (let [puzzle (puzzles/get-puzzle puzzle-id)
        postconds (:postconditions puzzle)
        steps (:steps puzzle)
        ;; Find items that are given away in puzzle steps
        consumed-items (set (for [step steps
                                  :let [action (:action step)]
                                  :when (= (:verb action) :give)]
                              (:direct-object action)))
        ;; Apply postcondition effects
        state-after-postconds
        (reduce
          (fn [state postcond]
            (case (:type postcond)
              :flag-set (update state :game-flags conj (:flag postcond))
              :object-flag (if (= (:flag postcond) :open)
                             (update state :open-objects conj (:object postcond))
                             state)
              :at-location (assoc state :here (:room postcond))
              state))
          planning-state
          postconds)]
    ;; Remove consumed items from inventory
    (update state-after-postconds :inventory #(apply disj % consumed-items))))

(defn apply-move-effect
  "Update planning state location."
  [planning-state destination]
  (assoc planning-state :here destination))

(defn apply-open-effect
  "Update planning state to mark object as open."
  [planning-state object-id]
  (update planning-state :open-objects conj object-id))

(defn apply-acquire-effect
  "Update planning state to add item to inventory."
  [planning-state item-id]
  (update planning-state :inventory conj item-id))

(defn apply-drop-effect
  "Update planning state to remove item from inventory."
  [planning-state item-id]
  (update planning-state :inventory disj item-id))

(defn planning-state-to-gs-view
  "Create a game-state view from planning state for weight calculations.
   Modifies the game-state's objects to reflect planned inventory.

   IMPORTANT: This function both ADDS items that should be in inventory
   AND REMOVES items that were consumed (like lunch given to cyclops).
   This ensures weight calculations are accurate."
  [game-state planning-state]
  (let [{:keys [here inventory]} planning-state
        planned-inv (set inventory)
        ;; Get current game state inventory
        current-inv (set (gs/get-contents game-state :adventurer))
        ;; Items to remove from adventurer (consumed)
        to-remove (clojure.set/difference current-inv planned-inv)
        ;; Items to add to adventurer (acquired)
        to-add (clojure.set/difference planned-inv current-inv)]
    (as-> (assoc game-state :here here) gs
      ;; Remove consumed items (move to limbo)
      (reduce (fn [g item-id]
                (assoc-in g [:objects item-id :in] :limbo))
              gs to-remove)
      ;; Add acquired items to adventurer
      (reduce (fn [g item-id]
                (assoc-in g [:objects item-id :in] :adventurer))
              gs to-add))))

;;; ---------------------------------------------------------------------------
;;; INVENTORY WEIGHT MANAGEMENT
;;; ---------------------------------------------------------------------------
;;; The planner must track inventory weight to avoid exceeding the 100-unit limit.

(def ^:private load-limit
  "Maximum weight the player can carry."
  100)

(defn- current-inventory-weight
  "Calculate total weight of player's inventory."
  [game-state]
  (gs/weight game-state :adventurer))

(defn- item-weight
  "Get weight of a single item."
  [game-state item-id]
  (let [obj (gs/get-thing game-state item-id)]
    (or (:size obj) 0)))

(defn- can-take-item?
  "Check if player can take an item given current weight."
  [game-state item-id]
  (let [current-weight (current-inventory-weight game-state)
        new-weight (item-weight game-state item-id)]
    (<= (+ current-weight new-weight) load-limit)))

(def ^:private essential-items
  "Items that should never be dropped (always needed)."
  #{:brass-lantern})

(defn- items-needed-for-puzzle
  "Get items required for a specific puzzle."
  [puzzle-id]
  (if-let [puzzle (puzzles/get-puzzle puzzle-id)]
    (set (:required-items puzzle))
    #{}))

(defn- all-future-required-items
  "Compute all items needed for remaining puzzles.
   This is a simple heuristic - in practice, items like sword
   are only needed for troll battle."
  [remaining-puzzles]
  (reduce
    (fn [items puzzle-id]
      (clojure.set/union items (items-needed-for-puzzle puzzle-id)))
    essential-items
    remaining-puzzles))

(defn- droppable-items
  "Get items in inventory that can be safely dropped.
   Returns items sorted by weight (heaviest first)."
  [game-state required-items]
  (let [inventory (gs/get-contents game-state :adventurer)
        droppable (remove (fn [item]
                            (or (contains? required-items item)
                                (contains? essential-items item)))
                          inventory)]
    ;; Sort by weight descending (drop heaviest first)
    (sort-by #(- (item-weight game-state %)) droppable)))

(defn- generate-drop-actions-for-weight
  "Generate drop actions to make room for a new item.
   Returns {:actions [...] :freed-weight N} or nil if impossible."
  [game-state item-id required-items]
  (let [current-weight (current-inventory-weight game-state)
        new-item-weight (item-weight game-state item-id)
        needed-space (- (+ current-weight new-item-weight) load-limit)]
    (if (<= needed-space 0)
      ;; No drops needed
      {:actions [] :freed-weight 0}
      ;; Need to drop items
      (let [droppable (droppable-items game-state required-items)]
        (loop [items droppable
               actions []
               freed 0]
          (if (or (empty? items) (>= freed needed-space))
            (if (>= freed needed-space)
              {:actions actions :freed-weight freed}
              ;; Can't free enough weight
              nil)
            (let [item (first items)
                  weight (item-weight game-state item)]
              (recur (rest items)
                     (conj actions {:verb :drop :direct-object item})
                     (+ freed weight)))))))))

;;; ---------------------------------------------------------------------------
;;; GOAL ORDERING (TSP-STYLE OPTIMIZATION)
;;; ---------------------------------------------------------------------------

(defn goal-location
  "Get the location associated with a goal."
  [game-state goal]
  (case (:type goal)
    :collect-treasure
    (let [obj (:object goal)]
      (or (treasures/treasure-location obj)
          (gs/get-thing-loc-id game-state obj)))

    :solve-puzzle
    (:execution-location (puzzles/get-puzzle (:puzzle goal)))

    :acquire-item
    (gs/get-thing-loc-id game-state (:object goal))

    :reach-location
    (:room goal)

    :deposit-treasures
    :living-room

    nil))

(defn distance-between-goals
  "Compute distance between two goals (for TSP)."
  [game-state available-flags goal1 goal2]
  (let [loc1 (goal-location game-state goal1)
        loc2 (goal-location game-state goal2)]
    (if (or (nil? loc1) (nil? loc2))
      999  ; Unknown location, penalize
      (let [path (routing/shortest-path game-state loc1 loc2
                                        :available-flags available-flags)]
        (if path
          (:distance path)
          999)))))

(defn greedy-tsp-order
  "Order goals using greedy nearest-neighbor TSP.
   Starts from current location, repeatedly picks nearest unvisited goal."
  [game-state available-flags goals start-location]
  (loop [remaining (set goals)
         ordered []
         current-loc start-location]
    (if (empty? remaining)
      ordered
      (let [;; Find nearest goal
            distances (for [g remaining]
                        (let [goal-loc (goal-location game-state g)
                              dist (if goal-loc
                                     (let [path (routing/shortest-path
                                                  game-state current-loc goal-loc
                                                  :available-flags available-flags)]
                                       (if path (:distance path) 999))
                                     999)]
                          [g dist goal-loc]))
            [nearest-goal _ next-loc] (first (sort-by second distances))]
        (recur (disj remaining nearest-goal)
               (conj ordered nearest-goal)
               (or next-loc current-loc))))))

(defn- puzzle-requires-treasure?
  "Check if a puzzle requires any treasures as items."
  [puzzle-id]
  (let [all-treasures (set (treasures/all-treasures))
        required-items (set (puzzles/puzzle-items-needed puzzle-id))]
    (seq (clojure.set/intersection all-treasures required-items))))

(defn- puzzle-required-treasures
  "Get treasures required by a puzzle."
  [puzzle-id]
  (let [all-treasures (set (treasures/all-treasures))
        required-items (set (puzzles/puzzle-items-needed puzzle-id))]
    (clojure.set/intersection all-treasures required-items)))

(defn- find-item-room
  "Find the actual room location of an item by traversing container chain.
   Returns the room keyword, or nil if not in a room."
  [game-state item-id]
  (loop [current item-id]
    (let [loc (gs/get-thing-loc-id game-state current)]
      (cond
        ;; Item is held by adventurer
        (= loc :adventurer) nil
        ;; Location is a room
        (get-in game-state [:rooms loc]) loc
        ;; Location is an object (container) - continue traversing
        (get-in game-state [:objects loc]) (recur loc)
        ;; Unknown location
        :else nil))))

(defn- item-reachable-with-flags?
  "Check if an item's location is reachable with given flags.
   Uses routing to determine accessibility. Traverses containers to find actual room."
  [game-state item-id available-flags]
  (let [room (find-item-room game-state item-id)
        start :west-of-house]  ; Starting location for planning
    (if (nil? room)
      ;; Item is held, in limbo, or can't find room - treat as accessible
      (= (gs/get-thing-loc-id game-state item-id) :adventurer)
      (let [path (routing/shortest-path game-state start room
                                        :available-flags available-flags)]
        (some? path)))))

(defn- partition-items-by-accessibility
  "Partition items into pre-troll, post-troll, and post-cyclops groups.
   Pre-troll items are reachable with just house access flags.
   Post-troll items require troll-flag.
   Post-cyclops items require cyclops-flag (via strange-passage)."
  [game-state items]
  (let [pre-troll-flags #{:kitchen-window-open :trap-door-open :rug-moved}
        post-troll-flags (conj pre-troll-flags :troll-flag)
        ;; After cyclops, strange-passage opens and we can reach treasure-room area
        post-cyclops-flags (clojure.set/union post-troll-flags
                                               #{:cyclops-flag :magic-flag :dome-flag})]
    (reduce
      (fn [acc item]
        (cond
          ;; Reachable without troll
          (item-reachable-with-flags? game-state item pre-troll-flags)
          (update acc :pre-troll conj item)
          ;; Reachable with troll
          (item-reachable-with-flags? game-state item post-troll-flags)
          (update acc :post-troll conj item)
          ;; Reachable after cyclops
          (item-reachable-with-flags? game-state item post-cyclops-flags)
          (update acc :post-cyclops conj item)
          ;; Not reachable (might need boat or other special access)
          :else
          (update acc :other conj item)))
      {:pre-troll [] :post-troll [] :post-cyclops [] :other []}
      items)))

(defn- treasure-requires-puzzle?
  "Check if a treasure requires a puzzle to be solved before collection.
   Returns the puzzle-id or nil."
  [treasure-id]
  (treasures/treasure-puzzle treasure-id))

(defn- treasure-container-treasure?
  "Check if a treasure is inside another treasure (container).
   Returns the container treasure or nil."
  [treasure-id]
  (let [container (treasures/treasure-container treasure-id)]
    (when (and container (contains? (set (treasures/all-treasures)) container))
      container)))

(defn- order-treasure-puzzles
  "Order treasure-requiring puzzles based on dependencies.
   If puzzle A requires a treasure that depends on puzzle B, B comes first.

   Example: wind-canary needs clockwork-canary, which needs egg-opening.
   So egg-opening must come before wind-canary.

   Returns puzzles in dependency order."
  [puzzle-ids]
  ;; Build dependency map: puzzle -> set of puzzles it depends on
  (let [deps-for-puzzle
        (fn [puzzle-id]
          ;; What treasures does this puzzle need?
          (let [required-treasures (set (puzzles/puzzle-items-needed puzzle-id))]
            ;; For each treasure, what puzzle unlocks it?
            (set (keep treasure-requires-puzzle? required-treasures))))

        all-deps (into {} (for [p puzzle-ids]
                            [p (deps-for-puzzle p)]))

        ;; Simple topological sort: puzzles with no deps first
        no-deps (filter #(empty? (get all-deps %)) puzzle-ids)
        has-deps (remove (set no-deps) puzzle-ids)]
    ;; Return puzzles with no deps first, then those with deps
    (concat no-deps has-deps)))

(defn optimize-goal-order
  "Order goals for execution.

   Uses a simple strategy (fast):
   - Access puzzles first (kitchen-window, trap-door-open)
   - Pre-troll items (reachable without passing troll)
   - Light source activation
   - Troll battle (if needed)
   - Post-troll items (reachable only after passing troll)
   - Cyclops puzzle (if needed, since lunch is pre-troll)
   - Post-cyclops items (temple items requiring strange-passage)
   - Standard puzzles (remaining puzzles that don't require treasures)
   - Treasure-requiring puzzles with their prereq and unlocked treasures interleaved
   - Standard treasures in value order (highest first)
   - Deposit at end

   IMPORTANT: Treasures with puzzle dependencies are collected AFTER their puzzle.
   Example: clockwork-canary needs egg-opening, so: egg → egg-opening → canary → wind-canary → bauble

   TSP optimization is expensive and disabled by default.
   For true optimization, use the routing module's TSP functions."
  [game-state puzzles treasures items]
  (let [;; Access puzzles in correct order (kitchen-window before trap-door)
        access-puzzle-order [:kitchen-window :trap-door-open]
        access-puzzle-set (set access-puzzle-order)
        puzzle-set (set puzzles)
        access-puzzles (filter #(contains? puzzle-set %) access-puzzle-order)

        ;; Separate troll-battle and cyclops-puzzle from other puzzles
        has-troll-battle (contains? puzzle-set :troll-battle)
        has-cyclops-puzzle (contains? puzzle-set :cyclops-puzzle)

        ;; Separate puzzles that require treasures from those that don't
        special-puzzles (conj access-puzzle-set :troll-battle :cyclops-puzzle)
        remaining-puzzles (remove special-puzzles puzzles)
        treasure-requiring-puzzles (filter puzzle-requires-treasure? remaining-puzzles)
        standard-puzzles (remove (set treasure-requiring-puzzles) remaining-puzzles)

        ;; Treasures that REQUIRE a puzzle to be accessible
        ;; e.g., clockwork-canary requires egg-opening
        treasures-with-puzzle-deps (set (filter treasure-requires-puzzle? treasures))

        ;; Treasures required BY puzzles, EXCLUDING those that need puzzles themselves
        ;; e.g., egg is required by egg-opening (and has no puzzle dependency)
        ;; But canary is required by wind-canary AND needs egg-opening - handle specially
        all-treasures-for-puzzles (set (mapcat puzzle-required-treasures treasure-requiring-puzzles))
        ;; Remove treasures that have their own puzzle deps - they'll be collected after their puzzle
        treasures-for-puzzles (clojure.set/difference all-treasures-for-puzzles
                                                       treasures-with-puzzle-deps)

        ;; Standard treasures: no puzzle prereq, not required by puzzles
        standard-treasures (remove (clojure.set/union treasures-for-puzzles
                                                       treasures-with-puzzle-deps)
                                   treasures)

        ;; Partition items by accessibility (before/after troll/cyclops)
        {:keys [pre-troll post-troll post-cyclops other]}
        (partition-items-by-accessibility game-state items)

        ;; Create goals
        access-puzzle-goals (map puzzle-goal access-puzzles)
        pre-troll-item-goals (map item-goal pre-troll)
        light-goal (when (some #{:brass-lantern} pre-troll)
                     (light-source-goal :brass-lantern))
        troll-battle-goal (when has-troll-battle [(puzzle-goal :troll-battle)])
        post-troll-item-goals (map item-goal post-troll)
        cyclops-puzzle-goal (when has-cyclops-puzzle [(puzzle-goal :cyclops-puzzle)])
        post-cyclops-item-goals (map item-goal post-cyclops)
        other-item-goals (map item-goal other)

        ;; Standard puzzles and their unlocked treasures
        ;; For each standard puzzle, add the puzzle goal followed by treasures it unlocks
        standard-puzzle-chain
        (mapcat (fn [puzzle-id]
                  (let [;; What treasures does this puzzle unlock?
                        unlocked (filter #(= (treasure-requires-puzzle? %) puzzle-id)
                                         treasures-with-puzzle-deps)]
                    (concat [(puzzle-goal puzzle-id)]
                            (map treasure-goal unlocked))))
                standard-puzzles)

        ;; Order treasure-requiring puzzles by their dependencies
        ordered-treasure-puzzles (order-treasure-puzzles treasure-requiring-puzzles)

        ;; For each treasure-requiring puzzle, interleave:
        ;; 1. Prereq treasures (that don't have their own puzzle deps)
        ;; 2. The puzzle itself
        ;; 3. Treasures unlocked by the puzzle
        treasure-puzzle-chain
        (mapcat (fn [puzzle-id]
                  (let [;; What treasures does this puzzle need?
                        prereqs (set (puzzles/puzzle-items-needed puzzle-id))
                        ;; Only include prereqs that are in treasures-for-puzzles
                        ;; (i.e., don't have their own puzzle deps)
                        available-prereqs (filter #(contains? treasures-for-puzzles %) prereqs)
                        ;; What treasures does this puzzle unlock?
                        unlocked (filter #(= (treasure-requires-puzzle? %) puzzle-id)
                                         treasures-with-puzzle-deps)]
                    (concat (map treasure-goal available-prereqs)
                            [(puzzle-goal puzzle-id)]
                            (map treasure-goal unlocked))))
                ordered-treasure-puzzles)

        ;; Standard treasures (no special dependencies)
        sorted-standard-treasures (sort-by #(- (treasures/treasure-value %)) standard-treasures)
        standard-treasure-goals (map treasure-goal sorted-standard-treasures)]

    ;; Final ordering
    (concat access-puzzle-goals
            pre-troll-item-goals
            (when light-goal [light-goal])
            troll-battle-goal
            post-troll-item-goals
            cyclops-puzzle-goal
            post-cyclops-item-goals
            other-item-goals
            standard-puzzle-chain      ; Puzzles with their unlocked treasures
            treasure-puzzle-chain
            standard-treasure-goals
            [(deposit-goal treasures)])))

;;; ---------------------------------------------------------------------------
;;; PLAN GENERATION
;;; ---------------------------------------------------------------------------

(defn- find-room-location
  "Find the actual room location of an item by traversing container chain.
   Returns {:room room-id :containers [container-ids-to-open]}"
  [game-state item-id]
  (loop [current item-id
         containers []]
    (let [loc (gs/get-thing-loc-id game-state current)]
      (cond
        ;; Item is held by adventurer
        (= loc :adventurer) {:room nil :containers [] :held true}
        ;; Location is a room
        (get-in game-state [:rooms loc]) {:room loc :containers containers}
        ;; Location is an object (container) - continue traversing
        (get-in game-state [:objects loc])
        (recur loc (conj containers loc))
        ;; Unknown location (limbo, nil, etc.)
        :else {:room nil :containers []}))))

(defn route-actions
  "Generate movement actions to travel a route."
  [path]
  (for [room path]
    {:verb :walk :direct-object room}))

(defn generate-route-to
  "Generate actions to move from current location to target.
   Returns nil if already at target or unreachable."
  [game-state target-room available-flags]
  (let [here (:here game-state)]
    (when (not= here target-room)
      (let [path (routing/shortest-path game-state here target-room
                                        :available-flags available-flags)]
        (when path
          ;; Convert path to direction-based movement actions
          (routing/path-to-actions game-state (:path path)
                                   :available-flags available-flags))))))

(defn- remaining-puzzle-ids
  "Extract puzzle IDs from remaining goals."
  [remaining-goals]
  (->> remaining-goals
       (filter #(= (:type %) :solve-puzzle))
       (map :puzzle)
       (set)))

(defn- compute-required-items
  "Compute items that must not be dropped (still needed for remaining puzzles)."
  [remaining-goals]
  (let [puzzle-ids (remaining-puzzle-ids remaining-goals)]
    (all-future-required-items puzzle-ids)))

(defn generate-goal-actions
  "Generate actions to achieve a single goal.
   Returns {:actions [...] :expected-state game-state}.

   remaining-goals is optional - used for inventory weight management."
  [game-state goal available-flags & [remaining-goals]]
  (case (:type goal)
    :collect-treasure
    (let [treasure (:object goal)
          deps (treasures/treasure-dependencies treasure)
          containers (:containers-to-open deps)
          ;; Get location: treasure location, or container location if in container
          location (or (treasures/treasure-location treasure)
                       (when (seq containers)
                         (let [container (first containers)]
                           (or (treasures/treasure-location container)
                               (gs/get-thing-loc-id game-state container))))
                       (gs/get-thing-loc-id game-state treasure))
          route (when location
                  (generate-route-to game-state location available-flags))
          ;; Generate open actions for containers that aren't already open
          open-actions (for [container containers
                             :when (not (gs/set-thing-flag? game-state container :open))]
                         {:verb :open :direct-object container})
          take-action {:verb :take :direct-object treasure}]
      (if (and location (nil? route) (not= (:here game-state) location))
        {:actions []
         :description (str "Collect " (name treasure))
         :error {:type :unreachable :treasure treasure :location location}}
        {:actions (concat (or route []) open-actions [take-action])
         :description (str "Collect " (name treasure))}))

    :solve-puzzle
    (let [puzzle-id (:puzzle goal)
          puzzle (puzzles/get-puzzle puzzle-id)
          ;; Check if puzzle is already solved (all postconditions satisfied)
          already-solved (:all-satisfied (puzzles/verify-postconditions game-state puzzle-id))]
      (if already-solved
        ;; Puzzle already solved, no actions needed
        {:actions []
         :description (str "Solve " (name puzzle-id) " (already solved)")}
        ;; Puzzle not yet solved, generate route and actions
        (let [location (:execution-location puzzle)
              route (when location
                      (generate-route-to game-state location available-flags))
              ;; Extract actions from steps, preserving repeat-until metadata
              puzzle-actions (for [step (:steps puzzle)]
                               (let [action (:action step)]
                                 (if (:repeat-until step)
                                   (assoc action
                                          :repeat-until (:repeat-until step)
                                          :max-attempts (:max-attempts step 10))
                                   action)))]
          ;; Check if route failed when we need to move
          (if (and location (nil? route) (not= (:here game-state) location))
            {:actions []
             :description (str "Solve " (name puzzle-id))
             :error {:type :unreachable :puzzle puzzle-id :location location}}
            {:actions (concat (or route []) puzzle-actions)
             :description (str "Solve " (name puzzle-id))}))))

    :acquire-item
    (let [item (:object goal)
          ;; Find actual room by traversing containers
          {:keys [room containers held]} (find-room-location game-state item)
          route (when room
                  (generate-route-to game-state room available-flags))
          ;; Generate open actions for all containers in the chain
          open-actions (for [container containers
                             :when (not (gs/set-thing-flag? game-state container :open))]
                         {:verb :open :direct-object container})
          take-action {:verb :take :direct-object item}
          ;; Check if we need to drop items for weight
          required-items (compute-required-items (or remaining-goals []))
          ;; Also keep the item we're about to take as required
          required-with-target (conj required-items item)
          drop-result (generate-drop-actions-for-weight game-state item required-with-target)
          drop-actions (when drop-result (:actions drop-result))]
      (cond
        ;; Already held
        held
        {:actions []
         :description (str "Acquire " (name item))}
        ;; Check if route failed (item is unreachable)
        (and room (nil? route) (not= (:here game-state) room))
        {:actions []
         :description (str "Acquire " (name item))
         :error {:type :unreachable :item item :location room}}
        ;; No room found (in limbo or missing)
        (nil? room)
        {:actions []
         :description (str "Acquire " (name item))
         :error {:type :not-found :item item}}
        ;; Can't free enough weight
        (and (not (can-take-item? game-state item)) (nil? drop-result))
        {:actions []
         :description (str "Acquire " (name item))
         :error {:type :too-heavy :item item
                 :current-weight (current-inventory-weight game-state)
                 :item-weight (item-weight game-state item)}}
        ;; Normal case - include drop actions if needed
        :else
        {:actions (concat (or route [])
                          (or drop-actions [])
                          open-actions
                          [take-action])
         :description (str "Acquire " (name item))}))

    :reach-location
    (let [room (:room goal)
          route (generate-route-to game-state room available-flags)]
      {:actions (or route [])
       :description (str "Go to " (name room))})

    :deposit-treasures
    (let [treasures (:treasures goal)
          route (generate-route-to game-state :living-room available-flags)
          ;; Open trophy case if not already open
          open-action (when-not (gs/set-thing-flag? game-state :trophy-case :open)
                        {:verb :open :direct-object :trophy-case})
          deposit-actions (for [t treasures]
                            {:verb :put :direct-object t :indirect-object :trophy-case})]
      {:actions (concat (or route [])
                        (when open-action [open-action])
                        deposit-actions)
       :description "Deposit treasures"})

    :enter-barrow
    ;; Endgame: go to west-of-house and enter stone barrow
    ;; Requires :won flag (set by depositing enough treasures)
    (let [route-to-house (generate-route-to game-state :west-of-house
                                            (conj available-flags :won))
          enter-action {:verb :walk :direct-object :sw}]
      {:actions (concat (or route-to-house []) [enter-action])
       :description "Enter the stone barrow (victory)"})

    :activate-light
    ;; Turn on a light source
    (let [light-id (:object goal)
          lamp-on-action {:verb :lamp-on :direct-object light-id}]
      {:actions [lamp-on-action]
       :description (str "Turn on " (name light-id))})

    :boat-expedition
    ;; Complete boat expedition to collect river treasures
    ;; Includes: inflate boat, PUT sharp weapons in boat, board, launch,
    ;; navigate river, collect buoy/emerald, land, get scarab, walk back
    (let [{:keys [treasures]} goal
          wants-emerald (some #{:large-emerald} treasures)
          wants-scarab (some #{:jeweled-scarab} treasures)

          ;; Route to dam-base
          route-to-dam (generate-route-to game-state :dam-base available-flags)

          ;; Inflate boat first
          inflate-action {:verb :inflate :direct-object :inflatable-boat :indirect-object :pump}

          ;; PUT sharp weapons IN boat (safe), then board
          sharp-held (held-sharp-weapons game-state)
          put-sharp-actions (for [w sharp-held]
                              {:verb :put :direct-object w :indirect-object :inflated-boat})
          board-action {:verb :board :direct-object :inflated-boat}
          launch-action {:verb :launch}

          ;; River flow: daemon moves boat, we wait
          ;; river-1 → river-2 → river-3 → river-4 (6 waits typical)
          wait-actions (repeat 6 {:verb :wait})

          ;; At river-4: take buoy if want emerald
          buoy-actions (when wants-emerald
                         [{:verb :take :direct-object :buoy}
                          {:verb :open :direct-object :buoy}
                          {:verb :take :direct-object :large-emerald}])

          ;; Land at sandy-beach (east from river-4)
          land-action {:verb :walk :direct-object :east}

          ;; Disembark from boat
          disembark-action {:verb :disembark}

          ;; If want scarab: sandy-beach → sandy-cave → back
          scarab-actions (when wants-scarab
                           [{:verb :walk :direct-object :ne}
                            {:verb :take :direct-object :jeweled-scarab}
                            {:verb :walk :direct-object :sw}])

          ;; Retrieve sharp weapons from boat after landing (take them back)
          retrieve-actions (for [w sharp-held]
                             {:verb :take :direct-object w})]

      {:actions (vec (concat (or route-to-dam [])
                             [inflate-action]
                             put-sharp-actions
                             [board-action launch-action]
                             wait-actions
                             buoy-actions
                             [land-action disembark-action]
                             scarab-actions
                             retrieve-actions))
       :description "Boat expedition for river treasures"})

    ;; Unknown goal type
    {:actions []
     :description (str "Unknown goal: " (:type goal))}))

(defn generate-plan
  "Generate executable action sequence from ordered goals.

   Uses lightweight planning state to track location, open containers,
   and game flags. Returns errors for unreachable goals.

   Options:
   - :simulate? - if true, simulate action execution (slow, default false)"
  [game-state ordered-goals & {:keys [simulate?] :or {simulate? false}}]
  (let [initial-pstate (initial-planning-state game-state)]
    (loop [goals ordered-goals
           all-actions []
           planning-state initial-pstate
           errors []]
      (if (empty? goals)
        {:actions all-actions
         :goal-count (count ordered-goals)
         :action-count (reduce + 0 (map #(count (:actions %)) all-actions))
         :errors (when (seq errors) errors)}
        (let [goal (first goals)
              remaining-goals (rest goals)
              available-flags (planning-flags planning-state)
              ;; Create a game-state view with planned location and inventory
              gs-view (planning-state-to-gs-view game-state planning-state)
              {:keys [actions description error]}
              (generate-goal-actions gs-view goal available-flags remaining-goals)

              ;; Update planning state based on goal type and effects
              ;; IMPORTANT: Only update location if route succeeded (no error)
              new-pstate
              (if error
                ;; Don't update planning state on error - we didn't actually move
                planning-state
                (case (:type goal)
                  :solve-puzzle
                  (let [puzzle (puzzles/get-puzzle (:puzzle goal))
                        exec-loc (:execution-location puzzle)]
                    (-> planning-state
                        (cond-> exec-loc (apply-move-effect exec-loc))
                        (apply-puzzle-effects (:puzzle goal))))

                  :collect-treasure
                  (let [treasure (:object goal)
                        deps (treasures/treasure-dependencies treasure)
                        containers (:containers-to-open deps)
                        ;; Same location logic as generate-goal-actions:
                        ;; treasure location, or container location if in container
                        location (or (treasures/treasure-location treasure)
                                     (when (seq containers)
                                       (let [container (first containers)]
                                         (or (treasures/treasure-location container)
                                             (gs/get-thing-loc-id game-state container))))
                                     (gs/get-thing-loc-id game-state treasure))]
                    (-> planning-state
                        (cond-> location (apply-move-effect location))
                        (update :open-objects into containers)))

                  :reach-location
                  (apply-move-effect planning-state (:room goal))

                  :deposit-treasures
                  (apply-move-effect planning-state :living-room)

                  :enter-barrow
                  (apply-move-effect planning-state :stone-barrow)

                  :acquire-item
                  (let [item (:object goal)
                        {:keys [room containers]} (find-room-location game-state item)
                        ;; Extract dropped items from actions to update inventory
                        dropped-items (->> actions
                                           (filter #(= (:verb %) :drop))
                                           (map :direct-object))]
                    (-> planning-state
                        (cond-> room (apply-move-effect room))
                        (update :open-objects into containers)
                        ;; Remove dropped items from inventory
                        (update :inventory #(apply disj % dropped-items))
                        ;; Add acquired item to inventory
                        (apply-acquire-effect item)))

                  :boat-expedition
                  ;; Boat expedition ends at sandy-beach, adds boat treasures to inventory
                  (let [boat-treasures (:treasures goal)]
                    (-> planning-state
                        (apply-move-effect :sandy-beach)
                        (update :inventory into boat-treasures)))

                  planning-state))]
          (recur (rest goals)
                 (conj (vec all-actions) {:phase description :actions actions})
                 new-pstate
                 (if error (conj errors error) errors)))))))

;;; ---------------------------------------------------------------------------
;;; SPEEDRUN PLANNING
;;; ---------------------------------------------------------------------------

(defn endgame-goal
  "Create a goal to enter the stone barrow (victory)."
  []
  (make-goal :enter-barrow))

(defn plan-speedrun
  "Generate optimal speedrun plan to collect all treasures.

   Returns:
   {:goals [...] - ordered high-level goals
    :plan {...} - detailed action plan
    :estimated-moves N - estimated move count}"
  [game-state]
  (let [;; Get all treasures
        all-treasures (treasures/all-treasures)

        ;; Partition treasures into boat-dependent and standard
        boat-treasures (set (filter #(contains? (treasures/treasure-requires %) :boat-ready)
                                    all-treasures))
        non-boat-treasures (remove boat-treasures all-treasures)

        ;; Exclude canary-related treasures (require complex thief timing)
        ;; The thief must open the egg, which requires multi-turn thief AI interaction
        ;; TODO: Implement thief timing support for egg-opening
        thief-excluded #{:clockwork-canary :brass-bauble}
        standard-treasures (remove thief-excluded non-boat-treasures)

        ;; Filter to accessible treasures (some may be unreachable)
        accessible (filter #(:accessible (treasures/treasure-accessible? game-state %))
                           standard-treasures)

        ;; What puzzles do we need for standard treasures?
        treasure-puzzles (compute-required-puzzles standard-treasures)

        ;; Add cyclops puzzle for return to living room
        ;; (needed to deposit treasures after underground collection)
        ;; Also add rainbow-solid if we have boat treasures (needed for walk-back path)
        base-puzzles (conj treasure-puzzles :cyclops-puzzle)
        all-puzzles (if (seq boat-treasures)
                      (conj base-puzzles :rainbow-solid)
                      base-puzzles)
        ordered-puzzles (compute-puzzle-dependencies game-state all-puzzles)

        ;; What items do we need for puzzles?
        puzzle-items (compute-item-requirements all-puzzles)

        ;; Essential items always needed for speedrun:
        ;; - brass-lantern: light source for underground exploration
        ;; - garlic: protects against bat kidnapping in bat-room
        ;; - pump: needed to inflate boat (if boat treasures)
        essential-items (if (seq boat-treasures)
                          #{:brass-lantern :garlic :pump}
                          #{:brass-lantern :garlic})
        required-items (clojure.set/union puzzle-items essential-items)

        ;; Optimize goal order for standard treasures
        ordered-goals (optimize-goal-order game-state
                                           ordered-puzzles
                                           standard-treasures
                                           required-items)

        ;; Remove the deposit goal (last item) so we can insert boat expedition before it
        ;; optimize-goal-order always ends with deposit-goal
        goals-without-deposit (vec (butlast ordered-goals))
        deposit (last ordered-goals)

        ;; Add boat expedition goal if we have boat treasures
        ;; This goes AFTER rainbow-solid (which is in ordered-goals) and BEFORE deposit
        boat-expedition-goals (when (seq boat-treasures)
                                [(make-goal :boat-expedition :treasures (vec boat-treasures))])

        ;; Recombine: standard goals + boat expedition + deposit + endgame
        all-goals (concat goals-without-deposit
                          boat-expedition-goals
                          [deposit]
                          [(endgame-goal)])

        ;; Generate plan
        plan (generate-plan game-state (vec all-goals))

        ;; Count total actions
        total-actions (reduce + 0 (map #(count (:actions %)) (:actions plan)))

        ;; Total treasures = standard + boat
        total-treasures (+ (count standard-treasures) (count boat-treasures))]

    {:goals (vec all-goals)
     :puzzles-required (count all-puzzles)
     :items-required (count required-items)
     :treasures-targeted total-treasures
     :treasures-excluded {:thief-timing (count thief-excluded)}
     :plan plan
     :estimated-moves total-actions}))

(defn plan-treasure-collection
  "Plan collection of specific treasures.

   Returns plan for collecting just the specified treasures."
  [game-state treasures]
  (let [;; What puzzles unlock these treasures?
        required-puzzles (compute-required-puzzles treasures)
        ordered-puzzles (compute-puzzle-dependencies game-state required-puzzles)

        ;; What items do we need?
        required-items (compute-item-requirements required-puzzles)

        ;; Optimize goal order
        ordered-goals (optimize-goal-order game-state
                                           ordered-puzzles
                                           treasures
                                           required-items)

        ;; Generate plan
        plan (generate-plan game-state ordered-goals)]

    {:goals ordered-goals
     :plan plan}))

;;; ---------------------------------------------------------------------------
;;; PLAN EXECUTION
;;; ---------------------------------------------------------------------------

(defn- action-expected-to-change-state?
  "Check if an action is expected to change state. Some actions like :wait
   may not change anything but are still valid."
  [action]
  (let [verb (:verb action)]
    ;; These verbs are expected to change state
    (contains? #{:go :walk :take :drop :put :open :close :move :turn :tie :inflate
                 :attack :give :wind :lamp-on :lamp-off :enter :board :launch
                 :land :ring :read :wave :unlock :odysseus :echo}
               verb)))

(defn- action-goal-already-achieved?
  "Check if an action's goal state is already achieved.
   This handles idempotent actions where no change is OK."
  [game-state action]
  (let [verb (:verb action)
        obj (:direct-object action)
        iobj (:indirect-object action)]
    (case verb
      ;; Open is successful if object is already open
      :open (gs/set-thing-flag? game-state obj :open)
      ;; Close is successful if object is already closed (not open)
      :close (not (gs/set-thing-flag? game-state obj :open))
      ;; Lamp-on is successful if object is already lit
      :lamp-on (gs/set-thing-flag? game-state obj :on)
      ;; Lamp-off is successful if object is already off
      :lamp-off (not (gs/set-thing-flag? game-state obj :on))
      ;; Enter window is successful if we're inside the house
      :enter (#{:kitchen :living-room :attic} (:here game-state))
      ;; Take is successful if we already have the object
      :take (= (gs/get-thing-loc-id game-state obj) :adventurer)
      ;; Move actions: rug, and dam buttons
      :move (cond
              ;; Move rug is successful if rug is already moved
              (= obj :rug) (gs/game-flag? game-state :rug-moved)
              ;; Yellow button controls dam gates
              (= obj :yellow-button) (gs/game-flag? game-state :gates-open)
              ;; Brown button controls low-tide timer (gate-flag)
              (= obj :brown-button) (gs/game-flag? game-state :gate-flag)
              :else false)
      ;; Echo is successful if loud room is already quiet (loud-flag set)
      :echo (gs/game-flag? game-state :loud-flag)
      ;; Odysseus command is successful if cyclops is already gone
      :odysseus (gs/game-flag? game-state :cyclops-flag)
      ;; Give is successful if:
      ;; 1. For lunch to cyclops: cyclops is already fed (cyclops-flag)
      ;; 2. Or we don't have the item to give (already gave it)
      :give (let [have-item (= (gs/get-thing-loc-id game-state obj) :adventurer)]
              (or (not have-item)  ; Don't have item - already gave it
                  (and (= obj :lunch) (= iobj :cyclops)
                       (gs/game-flag? game-state :cyclops-flag))))
      ;; Turn bolt is successful if gates are already open
      :turn (and (= obj :bolt) (= iobj :wrench)
                 (gs/game-flag? game-state :gates-open))
      ;; Put is successful if object is already in the container
      ;; (or we don't have the object to put - was dropped or never collected)
      :put (let [obj-loc (gs/get-thing-loc-id game-state obj)]
             (or (= obj-loc iobj)  ; Already in container
                 (not= obj-loc :adventurer)))  ; Don't have it - skip
      ;; Default: goal not already achieved
      false)))

(defn execute-action
  "Execute a single action and return result.
   Verifies that actions expected to change state actually did something,
   unless the action's goal is already achieved (idempotent actions).
   Returns {:success bool :game-state gs :error string}"
  [game-state action]
  (try
    (let [result (transition/step game-state action)
          diff (:diff result)
          changed (transition/changed? diff)
          ;; Check if goal was already achieved (no change needed)
          already-achieved (action-goal-already-achieved? (:game-state result) action)]
      (if (or changed
              already-achieved
              (not (action-expected-to-change-state? action)))
        {:success true
         :game-state (:game-state result)
         :changes (:changes result)
         :diff diff}
        ;; Action didn't change anything but was expected to
        {:success false
         :game-state (:game-state result)
         :error (str "Action had no effect: " action)
         :diff diff}))
    (catch Exception e
      {:success false
       :game-state game-state
       :error (.getMessage e)})))

(defn- repeat-until-satisfied?
  "Check if a repeat-until condition is satisfied."
  [game-state condition]
  (case (:type condition)
    :flag-set
    (gs/game-flag? game-state (:flag condition))
    ;; Default: check using puzzle precondition checker
    (:satisfied (puzzles/check-puzzle-precond game-state condition))))

(defn- execute-repeat-action
  "Execute an action with repeat-until logic.
   Returns {:success bool :game-state gs :attempts N :error string}"
  [game-state action]
  (let [condition (:repeat-until action)
        max-attempts (:max-attempts action 10)
        base-action (dissoc action :repeat-until :max-attempts)]
    (loop [current-state game-state
           attempts 0]
      (cond
        ;; Condition already satisfied
        (repeat-until-satisfied? current-state condition)
        {:success true
         :game-state current-state
         :attempts attempts}

        ;; Max attempts reached
        (>= attempts max-attempts)
        {:success false
         :game-state current-state
         :attempts attempts
         :error (str "Max attempts (" max-attempts ") reached without satisfying: " condition)}

        :else
        (let [result (execute-action current-state base-action)]
          (if (:success result)
            ;; Check if condition now satisfied
            (if (repeat-until-satisfied? (:game-state result) condition)
              {:success true
               :game-state (:game-state result)
               :attempts (inc attempts)}
              ;; Keep repeating
              (recur (:game-state result) (inc attempts)))
            ;; Action failed, but for combat we may need to retry
            ;; Combat can "fail" (no effect) if RNG is bad, but we should keep trying
            (if (#{:attack} (:verb base-action))
              ;; Retry combat even on "no effect"
              (recur (:game-state result) (inc attempts))
              ;; Non-combat action failed
              {:success false
               :game-state (:game-state result)
               :attempts (inc attempts)
               :error (:error result)})))))))

(defn execute-phase
  "Execute a phase (group of actions for a goal).
   Handles repeat-until actions by looping until condition is satisfied.
   Returns {:success bool :game-state gs :actions-completed N :error string}"
  [game-state phase]
  (let [actions (:actions phase)]
    (loop [remaining actions
           current-state game-state
           completed 0]
      (if (empty? remaining)
        {:success true
         :game-state current-state
         :actions-completed completed
         :phase-name (:phase phase)}
        (let [action (first remaining)
              ;; Check if this is a repeat-until action
              result (if (:repeat-until action)
                       (execute-repeat-action current-state action)
                       (execute-action current-state action))]
          (if (:success result)
            (recur (rest remaining)
                   (:game-state result)
                   (+ completed (or (:attempts result) 1)))
            ;; Action failed
            {:success false
             :game-state current-state
             :actions-completed completed
             :failed-action action
             :error (:error result)
             :phase-name (:phase phase)}))))))

(defn execute-speedrun
  "Execute a speedrun plan, replanning on failure.

   Options:
   - :max-replans - maximum replan attempts (default 5)
   - :verbose - print progress (default false)

   Returns:
   {:success bool
    :final-state game-state
    :total-moves N
    :replans N
    :treasures-collected [...]}"
  [game-state plan & {:keys [max-replans verbose]
                      :or {max-replans 5 verbose false}}]
  (loop [phases (:actions (:plan plan))
         current-state game-state
         total-moves 0
         replans 0]
    (if (empty? phases)
      ;; Success - all phases completed
      {:success true
       :final-state current-state
       :total-moves total-moves
       :replans replans
       :treasures-collected (gs/get-contents current-state :trophy-case)}

      (let [phase (first phases)
            _ (when verbose (println "Executing:" (:phase phase)))
            result (execute-phase current-state phase)]
        (if (:success result)
          ;; Phase succeeded, continue
          (recur (rest phases)
                 (:game-state result)
                 (+ total-moves (:actions-completed result))
                 replans)

          ;; Phase failed - attempt replan
          (if (>= replans max-replans)
            {:success false
             :final-state (:game-state result)
             :total-moves total-moves
             :replans replans
             :error (str "Max replans exceeded. Last error: " (:error result))}

            (do
              (when verbose
                (println "Replanning after failure:" (:error result)))
              (let [new-plan (plan-speedrun (:game-state result))]
                (recur (:actions (:plan new-plan))
                       (:game-state result)
                       total-moves
                       (inc replans))))))))))

;;; ---------------------------------------------------------------------------
;;; PLAN ANALYSIS / DEBUGGING
;;; ---------------------------------------------------------------------------

(defn summarize-plan
  "Get a human-readable summary of a plan."
  [plan]
  (let [phases (:actions (:plan plan))
        ;; Compute total actions from phases if :estimated-moves not present
        computed-actions (reduce + 0 (map #(count (:actions %)) phases))
        total-actions (or (:estimated-moves plan) computed-actions)]
    {:total-phases (count phases)
     :total-actions total-actions
     :puzzles (:puzzles-required plan)
     :items (:items-required plan)
     :treasures (:treasures-targeted plan)
     :phases (map (fn [p]
                    {:name (:phase p)
                     :action-count (count (:actions p))})
                  phases)}))

(def ^:private direction-keywords
  "Valid direction keywords that can appear in walk actions."
  #{:north :south :east :west :up :down :ne :nw :se :sw
    :n :s :e :w :u :d :enter :exit :land :in :out})

(defn validate-plan
  "Check if a plan is valid (all locations reachable, items available, etc.).
   Returns {:valid bool :issues [...]}."
  [game-state plan]
  (let [phases (:actions (:plan plan))
        issues (atom [])]
    ;; Check each phase
    (doseq [phase phases]
      (doseq [action (:actions phase)]
        (when (= :walk (:verb action))
          ;; Check if destination exists (skip direction-based walks)
          (let [dest (:direct-object action)]
            (when-not (or (contains? direction-keywords dest)
                          (get-in game-state [:rooms dest]))
              (swap! issues conj {:type :invalid-room :room dest}))))))
    {:valid (empty? @issues)
     :issues @issues}))

(defn plan-stats
  "Get statistics about a plan."
  [plan]
  (let [phases (:actions (:plan plan))
        all-actions (mapcat :actions phases)]
    {:total-moves (count all-actions)
     :walk-moves (count (filter #(= :walk (:verb %)) all-actions))
     :take-moves (count (filter #(= :take (:verb %)) all-actions))
     :puzzle-moves (count (filter #(#{:ring :light :read :wave :tie :turn
                                       :unlock :open :move :attack :say}
                                     (:verb %)) all-actions))
     :phases (count phases)}))
