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
            [clork.intel.treasures :as treasures]
            [clork.random :as random]))

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
   Includes :inventory to track items for weight calculations.

   NOTE: We remove :empty-handed from initial flags because it's a dynamic
   condition that depends on planned inventory, computed in planning-flags."
  [game-state]
  {:here (:here game-state)
   :open-objects (set (for [[obj-id _] (:objects game-state)
                            :when (gs/set-thing-flag? game-state obj-id :open)]
                        obj-id))
   ;; Remove :empty-handed - will be computed dynamically in planning-flags
   :game-flags (disj (routing/extract-available-flags game-state) :empty-handed)
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
   that are given away during puzzle execution (e.g., lunch given to cyclops).

   Special handling for trap door mechanics:
   - troll-battle: The route to troll-room goes through cellar, which triggers
     the trap door to close and bar behind you (touch flag set).
   - After having magic-flag (cyclops puzzle), player can exit via strange-passage
     which does NOT clear touch flag (unlike chimney exit which does).
   - With touch flag set, the trap door can be reopened from living-room and
     won't auto-close again on descent.
   - For planning simplicity, we mark the trap door as closed after troll-battle.
     Routes will use alternatives (maze path) or could be enhanced to reopen it."
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
          postconds)
        ;; Remove consumed items from inventory
        state-after-consumed
        (update state-after-postconds :inventory #(apply disj % consumed-items))]
    ;; Trap door closes when entering cellar during troll-battle route
    (if (= puzzle-id :troll-battle)
      (update state-after-consumed :open-objects disj :trap-door)
      state-after-consumed)))

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
  "Calculate total weight of player's inventory.
   Sums the weight of all items carried, NOT including adventurer's base size."
  [game-state]
  (let [inventory (gs/get-contents game-state :adventurer)]
    (reduce + 0 (map #(gs/weight game-state %) inventory))))

(defn- item-weight
  "Get weight of a single item including its contents.
   Uses gs/weight which includes nested contents (e.g., canary inside egg).
   This must match current-inventory-weight which also uses gs/weight."
  [game-state item-id]
  (gs/weight game-state item-id))

(defn- can-take-item?
  "Check if player can take an item given current weight."
  [game-state item-id]
  (let [current-weight (current-inventory-weight game-state)
        new-weight (item-weight game-state item-id)]
    (<= (+ current-weight new-weight) load-limit)))

(def ^:private essential-items
  "Items that should never be dropped (always needed).
   - :brass-lantern - light source needed throughout
   - :sword - needed for thief-expedition (kill thief to open egg)
   - :egg - needed for thief-expedition (give to thief, contains canary)"
  #{:brass-lantern :sword :egg})

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

        ;; First, group ALL items by which standard puzzle needs them
        ;; This is done BEFORE accessibility partitioning so we can acquire items
        ;; just-in-time before their puzzle, regardless of accessibility
        all-items-by-puzzle (group-by (fn [item]
                                        (first (filter (fn [puzzle-id]
                                                         (let [puzzle (puzzles/get-puzzle puzzle-id)
                                                               req-items (set (:required-items puzzle))]
                                                           (contains? req-items item)))
                                                       standard-puzzles)))
                                      items)
        ;; Items that ARE needed by a standard puzzle (will be acquired just-in-time)
        puzzle-assigned-items (set (mapcat val (dissoc all-items-by-puzzle nil)))
        ;; Items NOT needed by any standard puzzle (use accessibility partitioning)
        unassigned-items-list (get all-items-by-puzzle nil [])

        ;; Partition only UNASSIGNED items by accessibility (before/after troll/cyclops)
        {:keys [pre-troll post-troll post-cyclops other]}
        (partition-items-by-accessibility game-state unassigned-items-list)

        ;; Create goals
        access-puzzle-goals (map puzzle-goal access-puzzles)
        pre-troll-item-goals (map item-goal pre-troll)
        light-goal (when (some #{:brass-lantern} pre-troll)
                     (light-source-goal :brass-lantern))
        troll-battle-goal (when has-troll-battle [(puzzle-goal :troll-battle)])
        post-troll-item-goals (map item-goal post-troll)
        cyclops-puzzle-goal (when has-cyclops-puzzle [(puzzle-goal :cyclops-puzzle)])
        post-cyclops-item-goals (map item-goal post-cyclops)

        ;; Items in "other" that aren't assigned to a puzzle
        unassigned-other-items other

        ;; Standard puzzles with their required items and unlocked treasures
        ;; For each standard puzzle: acquire items, solve puzzle, collect treasures
        ;; Items are acquired just-in-time from ALL categories (not just "other")
        standard-puzzle-chain
        (mapcat (fn [puzzle-id]
                  (let [;; Items needed for this puzzle (from ALL items, not just "other")
                        puzzle-items (get all-items-by-puzzle puzzle-id [])
                        puzzle-item-goals (map item-goal puzzle-items)
                        ;; What treasures does this puzzle unlock?
                        unlocked (filter #(= (treasure-requires-puzzle? %) puzzle-id)
                                         treasures-with-puzzle-deps)]
                    (concat puzzle-item-goals
                            [(puzzle-goal puzzle-id)]
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
        standard-treasure-goals (map treasure-goal sorted-standard-treasures)

        ;; Goals for unassigned items in "other" category
        ;; (not needed by any puzzle and not accessible pre/post-troll/cyclops)
        unassigned-item-goals (map item-goal unassigned-other-items)]

    ;; Final ordering
    ;; Note: "other" items are now acquired just-in-time with their puzzles
    ;; (included in standard-puzzle-chain), not all at once before puzzles.
    (concat access-puzzle-goals
            pre-troll-item-goals
            (when light-goal [light-goal])
            troll-battle-goal
            post-troll-item-goals
            cyclops-puzzle-goal
            post-cyclops-item-goals
            unassigned-item-goals      ; Items not needed by any puzzle
            standard-puzzle-chain      ; Items + puzzle + treasures, grouped
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
          take-action {:verb :take :direct-object treasure}
          ;; Special handling for gas-room treasure (sapphire-bracelet)
          ;; Must extinguish flames before entering or room explodes
          gas-room-prep (when (= location :gas-room)
                          [{:verb :lamp-off :direct-object :candles}])
          ;; Weight management: drop non-essential items if needed
          required-items (compute-required-items (or remaining-goals []))
          ;; Also keep the treasure we're about to take as required
          required-with-target (conj required-items treasure)
          drop-result (generate-drop-actions-for-weight game-state treasure required-with-target)
          drop-actions (when drop-result (:actions drop-result))]
      (cond
        ;; Can't reach treasure location
        (and location (nil? route) (not= (:here game-state) location))
        {:actions []
         :description (str "Collect " (name treasure))
         :error {:type :unreachable :treasure treasure :location location}}
        ;; Can't free enough weight to take the treasure
        (and (not (can-take-item? game-state treasure)) (nil? drop-result))
        {:actions []
         :description (str "Collect " (name treasure))
         :error {:type :too-heavy :treasure treasure
                 :current-weight (current-inventory-weight game-state)
                 :treasure-weight (item-weight game-state treasure)}}
        ;; Normal case - include drop actions if needed
        :else
        {:actions (concat gas-room-prep (or drop-actions []) (or route []) open-actions [take-action])
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
                                   action)))
              ;; No special post-puzzle actions needed
              ;; (egg is now given to thief in treasure-room instead of dropped here)
              post-puzzle-actions nil]
          ;; Check if route failed when we need to move
          (if (and location (nil? route) (not= (:here game-state) location))
            {:actions []
             :description (str "Solve " (name puzzle-id))
             :error {:type :unreachable :puzzle puzzle-id :location location}}
            {:actions (concat (or route []) puzzle-actions post-puzzle-actions)
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

          ;; River flow: daemon moves boat with EVERY action (not just wait!)
          ;; river-1 → river-2 → river-3 → river-4 (3 waits needed)
          ;; After launch, we're at river-1. Each action moves boat downstream.
          ;; Must manage timing carefully to get treasures before going over falls!
          ;;
          ;; TIMING:
          ;; - 3 waits → river-4 (buoy is here)
          ;; - If want buoy: take buoy → river-5, then land immediately → shore
          ;; - If don't want buoy: land east → sandy-beach (scarab nearby)
          ;;
          ;; Note: Can't get BOTH buoy AND scarab easily since they require
          ;; landing at different locations. For simplicity, we get both when
          ;; possible by adjusting the path.

          ;; Strategy: Get buoy first (it's in river-4), then land at river-5,
          ;; then walk to sandy-cave for scarab if needed.
          wait-actions (repeat 3 {:verb :wait})

          ;; At river-4: take buoy (boat advances to river-5)
          ;; Must land IMMEDIATELY after or go over falls!
          take-buoy-action (when wants-emerald
                             {:verb :take :direct-object :buoy})

          ;; Land at shore from river-5 (east) - do this BEFORE opening buoy!
          ;; Or if no buoy, land at sandy-beach from river-4 (east)
          land-action {:verb :walk :direct-object :east}

          ;; Disembark from boat
          disembark-action {:verb :disembark}

          ;; Now safe on land - open buoy and take emerald
          open-buoy-actions (when wants-emerald
                              [{:verb :open :direct-object :buoy}
                               {:verb :take :direct-object :large-emerald}])

          ;; If landed at shore (from river-5 with buoy), need to walk to sandy-cave
          ;; shore → sandy-beach (north) → sandy-cave (ne)
          ;; If landed at sandy-beach directly (river-4 no buoy), just ne to sandy-cave
          scarab-route-from-shore (when (and wants-scarab wants-emerald)
                                    ;; Landed at shore, need to go north to sandy-beach first
                                    [{:verb :walk :direct-object :north}])

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
                             (when take-buoy-action [take-buoy-action])
                             [land-action disembark-action]
                             open-buoy-actions
                             scarab-route-from-shore
                             scarab-actions
                             retrieve-actions))
       :description "Boat expedition for river treasures"})

    :thief-expedition
    ;; Complete thief sequence: take egg, give to thief, kill thief, get treasures, wind canary
    ;; The egg is NOT collected early - the thief daemon steals it during exploration
    ;; and deposits it in treasure-room. We take it from there and give it to him.
    ;; Silver-chalice is also in treasure-room and requires thief dead to take
    ;;
    ;; IMPORTANT: We must WALK into treasure-room (not use TREASURE teleport)
    ;; Walking triggers the room's m-enter handler which makes the thief fight properly.
    ;; Using TREASURE bypasses m-enter so thief wanders away instead of defending.
    ;;
    ;; Sequence:
    ;; 1. Enter treasure-room (walk up from cyclops-room)
    ;; 2. Take egg (thief deposited it here)
    ;; 3. Weaken thief (combat to reduce his strength)
    ;; 4. Give egg to thief
    ;; 5. Kill thief (egg opens when he dies with it)
    ;; 6. Take canary, egg, chalice
    ;; 7. Go to forest, wind canary for bauble
    (let [;; Route to cyclops-room first (requires cyclops-flag)
          ;; Then walk UP to enter treasure-room properly (triggers m-enter)
          route-to-cyclops (generate-route-to game-state :cyclops-room available-flags)
          walk-up {:verb :walk :direct-object :up}

          ;; Take the egg - thief deposited it in treasure-room
          take-egg-initial {:verb :take :direct-object :egg}

          ;; Attack thief several times to weaken him before giving the egg
          ;; This prevents him from killing us while we're trying to give the egg
          ;; We use plain attack actions (not repeat-until) since we just want N attacks
          weaken-thief (repeat 8 {:verb :attack
                                  :direct-object :thief
                                  :indirect-object :sword})

          ;; Give the egg to the weakened thief - he'll take it
          give-egg {:verb :give :direct-object :egg :indirect-object :thief}

          ;; Kill the thief (repeat attack until egg-opened)
          ;; Thief opens the egg when he dies with it in his possession
          attack-actions [{:verb :attack
                          :direct-object :thief
                          :indirect-object :sword
                          :repeat-until {:type :flag-set :flag :egg-opened}
                          :max-attempts 30}]

          ;; Take treasures from treasure-room after thief is dead:
          ;; - egg (thief dropped it when he died, 5 pts)
          ;; - canary (inside egg, 6 pts)
          ;; - silver-chalice (10 pts, was blocked by thief)
          take-egg {:verb :take :direct-object :egg}
          take-canary {:verb :take :direct-object :clockwork-canary}
          take-chalice {:verb :take :direct-object :silver-chalice}

          ;; Drop the sword - we don't need it after killing the thief
          ;; and we need inventory space for the bauble (inventory is often near limit)
          drop-sword {:verb :drop :direct-object :sword}

          ;; Route FROM treasure-room TO forest-path for wind-canary
          ;; Note: we use routing directly since game-state :here is not treasure-room at plan time
          forest-path (routing/shortest-path game-state :treasure-room :forest-path
                                             :available-flags available-flags)
          route-to-forest (when forest-path
                            (routing/path-to-actions game-state (:path forest-path)
                                                     :available-flags available-flags))

          ;; Wind the canary to summon songbird and get bauble
          wind-canary {:verb :wind :direct-object :clockwork-canary}

          ;; Take the bauble
          take-bauble {:verb :take :direct-object :brass-bauble}]

      {:actions (vec (concat (or route-to-cyclops [])
                             [walk-up]  ; Enter treasure-room properly (triggers m-enter)
                             [take-egg-initial]  ; Take egg from treasure-room (thief deposited it)
                             weaken-thief  ; Weaken thief first so he doesn't kill us
                             [give-egg]  ; Give egg to weakened thief
                             attack-actions  ; Finish thief off (egg opens when he dies)
                             [take-egg take-canary take-chalice]
                             [drop-sword]  ; Free inventory space for bauble
                             (or route-to-forest [])
                             [wind-canary take-bauble]))
       :description "Thief expedition for egg, canary, chalice, and bauble"})

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
                                     (gs/get-thing-loc-id game-state treasure))
                        ;; Extract dropped items from actions to update inventory
                        dropped-items (->> actions
                                           (filter #(= (:verb %) :drop))
                                           (map :direct-object))]
                    (-> planning-state
                        (cond-> location (apply-move-effect location))
                        (update :open-objects into containers)
                        ;; Remove dropped items from inventory
                        (update :inventory #(apply disj % dropped-items))
                        ;; Add collected treasure to inventory
                        (update :inventory conj treasure)))

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

                  :thief-expedition
                  ;; Thief expedition ends at forest-path, adds treasures to inventory
                  ;; egg, canary, chalice (from treasure-room) and bauble (from forest-path)
                  ;; Also sets egg-opened and canary-sung flags
                  ;; NOTE: We drop the sword during thief expedition (for inventory space)
                  (-> planning-state
                      (apply-move-effect :forest-path)
                      (update :inventory into [:egg :clockwork-canary :silver-chalice :brass-bauble])
                      (update :inventory disj :sword)  ; Dropped during thief expedition
                      (update :flags conj :egg-opened :canary-sung))

                  planning-state))]
          (recur (rest goals)
                 (conj (vec all-actions) {:phase description
                                          :actions actions
                                          :expected-location (:here planning-state)})
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

        ;; Thief treasures need special handling (egg, canary, bauble, chalice)
        ;; The egg is dropped in troll-room for thief to steal, then recovered
        ;; after killing the thief along with canary. Bauble is created by winding canary.
        ;; Silver-chalice is in treasure-room where thief blocks access until dead.
        thief-treasures #{:egg :clockwork-canary :brass-bauble :silver-chalice}

        ;; Standard treasures = all except boat and thief (handled specially)
        non-boat-treasures (remove boat-treasures all-treasures)
        standard-treasures (remove thief-treasures non-boat-treasures)

        ;; Filter to accessible treasures (some may be unreachable)
        accessible (filter #(:accessible (treasures/treasure-accessible? game-state %))
                           standard-treasures)

        ;; What puzzles do we need for standard treasures?
        treasure-puzzles (compute-required-puzzles standard-treasures)

        ;; Add cyclops puzzle for return to living room and treasure-room access
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
        ;; - egg: collected early so thief daemon will steal it during exploration
        ;;        and deposit in treasure-room (NOT included in early deposit!)
        ;; - pump: needed to inflate boat (if boat treasures)
        essential-items (cond-> #{:brass-lantern :garlic :egg}
                          (seq boat-treasures) (conj :pump))
        required-items (clojure.set/union puzzle-items essential-items)

        ;; Optimize goal order - but DEFER most treasure collection
        ;; Pass empty treasures to optimize-goal-order so puzzles are done first
        puzzle-goals (optimize-goal-order game-state
                                          ordered-puzzles
                                          []  ; No treasures during puzzle phase
                                          required-items)

        ;; Remove the empty deposit goal from puzzle-goals
        goals-without-deposit (vec (butlast puzzle-goals))

        ;; STRATEGY: Collect and deposit treasures in two phases to minimize theft risk
        ;; Phase 1: Quick collection + deposit to raise score before thief fight
        ;; Phase 2: Remaining treasures after thief is dead
        ;;
        ;; The thief is easier at higher scores. By depositing some treasures first,
        ;; we increase our score before the thief expedition.
        ;; After killing the thief, remaining treasures are safe to collect.

        ;; Thief expedition collects egg, canary, chalice, and bauble
        thief-expedition-goal (make-goal :thief-expedition)

        ;; Boat expedition collects river treasures
        boat-expedition-goals (when (seq boat-treasures)
                                [(make-goal :boat-expedition :treasures (vec boat-treasures))])

        ;; Collect ALL standard treasures, then immediately deposit
        ;; The key is to minimize moves between collection and deposit
        standard-treasure-goals (map treasure-goal standard-treasures)
        standard-deposit (make-goal :deposit-treasures :treasures (vec standard-treasures))

        ;; Final deposit for thief and boat treasures
        late-treasures (vec (concat thief-treasures boat-treasures))
        final-deposit (make-goal :deposit-treasures :treasures late-treasures)

        ;; Final ordering:
        ;; 1. All puzzles and item acquisition (no treasures yet - minimizes theft)
        ;; 2. Standard treasures → immediate deposit (raises score quickly)
        ;; 3. Thief expedition (easier at higher score, gets egg/canary/chalice/bauble)
        ;; 4. Boat expedition (if needed)
        ;; 5. Final deposit (thief + boat treasures)
        ;; 6. Victory
        all-goals (concat goals-without-deposit
                          standard-treasure-goals
                          [standard-deposit]  ; Deposit immediately after collection
                          [thief-expedition-goal]
                          boat-expedition-goals
                          [final-deposit]
                          [(endgame-goal)])

        ;; Generate plan
        plan (generate-plan game-state (vec all-goals))

        ;; Count total actions
        total-actions (reduce + 0 (map #(count (:actions %)) (:actions plan)))

        ;; Total treasures = standard + thief + boat
        total-treasures (+ (count standard-treasures)
                           (count thief-treasures)
                           (count boat-treasures))]

    {:goals (vec all-goals)
     :puzzles-required (count all-puzzles)
     :items-required (count required-items)
     :treasures-targeted total-treasures
     :treasures-excluded {}  ; No more excluded treasures!
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
    ;; NOTE: :attack is NOT included because combat can involve misses
    ;; that don't change state but are still valid attempts
    (contains? #{:go :walk :take :drop :put :open :close :move :turn :tie :inflate
                 :give :wind :lamp-on :lamp-off :enter :board :launch
                 :land :ring :read :wave :unlock :odysseus :echo}
               verb)))

(def ^:private direction-keywords
  "Direction keywords for walk actions."
  #{:north :south :east :west :up :down :ne :nw :se :sw
    :n :s :e :w :u :d :enter :exit :land :in :out})

(defn- action-effect-achieved?
  "Check if an action's intended effect was achieved in the post-state.
   This catches cases where daemons cause state changes but the primary
   action failed (e.g., take failed due to weight limits)."
  [before-state after-state action]
  (let [verb (:verb action)
        obj (:direct-object action)
        iobj (:indirect-object action)]
    (case verb
      ;; Take is successful if object is now in adventurer's inventory
      :take (= (gs/get-thing-loc-id after-state obj) :adventurer)
      ;; Put is successful if object is now in the container
      :put (= (gs/get-thing-loc-id after-state obj) iobj)
      ;; Drop is successful if object is now in current room
      :drop (= (gs/get-thing-loc-id after-state obj) (:here after-state))
      ;; Walk is successful if:
      ;; - For room teleports: player is now at the destination room
      ;; - For directions: player actually moved (location changed)
      :walk (if (contains? direction-keywords obj)
              ;; Direction-based walk - success only if we actually moved
              ;; If we're blocked (by troll, closed door, etc), the action failed
              (not= (:here before-state) (:here after-state))
              ;; Room teleport - must be at the destination
              (= (:here after-state) obj))
      ;; For other actions, default to true (rely on changed? check)
      true)))

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
      ;; Drop is successful if we don't have the object
      ;; (was already dropped or never collected - skip the planned drop)
      :drop (not= (gs/get-thing-loc-id game-state obj) :adventurer)
      ;; Default: goal not already achieved
      false)))

(defn- player-died?
  "Check if the player died during an action.
   Death is indicated by:
   - :deaths counter increased
   - Score decreased (death penalty is -10)
   - Location changed to resurrection point (forest-1)"
  [before-state after-state]
  (let [deaths-before (get before-state :deaths 0)
        deaths-after (get after-state :deaths 0)]
    (> deaths-after deaths-before)))

(defn execute-action
  "Execute a single action and return result.
   Verifies that actions expected to change state actually did something,
   unless the action's goal is already achieved (idempotent actions).
   Also detects player death (e.g., killed by troll) and reports failure.
   Returns {:success bool :game-state gs :error string}"
  [game-state action]
  (try
    (let [result (transition/step game-state action)
          diff (:diff result)
          changed (transition/changed? diff)
          new-state (:game-state result)
          ;; Check if player died during this action
          died (player-died? game-state new-state)
          ;; Check if goal was already achieved (no change needed)
          already-achieved (action-goal-already-achieved? new-state action)
          ;; Check if the action's intended effect was achieved
          ;; This catches cases where daemons cause state changes but the
          ;; primary action failed (e.g., take failed due to weight limits)
          effect-achieved (action-effect-achieved? game-state new-state action)]
      (cond
        ;; Player died - this is always a failure
        died
        {:success false
         :game-state new-state
         :error (str "Player died during action: " action)
         :died true
         :diff diff}

        ;; Action succeeded - must check effect was actually achieved
        ;; (daemon-caused changes don't count if the primary action failed)
        (and (or changed
                 already-achieved
                 (not (action-expected-to-change-state? action)))
             effect-achieved)
        {:success true
         :game-state new-state
         :changes (:changes result)
         :diff diff}

        ;; Action didn't achieve its intended effect
        :else
        {:success false
         :game-state new-state
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

(def ^:private combat-seeds
  "Known-good RNG seeds for deterministic combat.
   These seeds have been tested to produce quick victories.
   Seeds 1, 2, 3, etc. work well for thief combat.
   Seed 42 tends to cause player death against thief, so it's later in list."
  [1 2 3 7 13 17 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97
   100 999 12345 98765 11111 22222 33333 44444 55555 66666 77777 42])

(defn- execute-repeat-action
  "Execute an action with repeat-until logic.
   For combat actions, uses deterministic RNG seeds to ensure consistent results.
   Returns {:success bool :game-state gs :attempts N :error string}"
  [game-state action]
  (let [condition (:repeat-until action)
        max-attempts (:max-attempts action 10)
        base-action (dissoc action :repeat-until :max-attempts)
        is-combat? (#{:attack} (:verb base-action))]

    ;; For combat, try known-good seeds for deterministic results
    (if is-combat?
      ;; Try each seed until combat succeeds
      (loop [seeds combat-seeds
             best-result nil]
        (if (empty? seeds)
          ;; No seed worked - return best result or error
          (or best-result
              {:success false
               :game-state game-state
               :attempts 0
               :error "All combat seeds failed"})

          ;; Try this seed
          (let [seed (first seeds)]
            (random/init! seed)
            (let [result
                  (loop [current-state game-state
                         attempts 0]
                    (cond
                      ;; Condition satisfied
                      (repeat-until-satisfied? current-state condition)
                      {:success true
                       :game-state current-state
                       :attempts attempts}

                      ;; Max attempts reached
                      (>= attempts max-attempts)
                      {:success false
                       :game-state current-state
                       :attempts attempts
                       :error (str "Max attempts (" max-attempts ") reached")}

                      :else
                      (let [action-result (execute-action current-state base-action)]
                        (cond
                          ;; Player died
                          (:died action-result)
                          {:success false
                           :game-state (:game-state action-result)
                           :attempts (inc attempts)
                           :error (:error action-result)
                           :died true}

                          ;; Keep trying
                          :else
                          (recur (:game-state action-result) (inc attempts))))))]

              ;; If this seed worked, use it
              (if (:success result)
                result
                ;; Try next seed
                (recur (rest seeds)
                       (or best-result result))))))))

      ;; Non-combat: standard repeat-until logic
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
            (cond
              ;; Player died - stop immediately
              (:died result)
              {:success false
               :game-state (:game-state result)
               :attempts (inc attempts)
               :error (:error result)
               :died true}

              ;; Action succeeded
              (:success result)
              (if (repeat-until-satisfied? (:game-state result) condition)
                {:success true
                 :game-state (:game-state result)
                 :attempts (inc attempts)}
                ;; Keep repeating
                (recur (:game-state result) (inc attempts)))

              ;; Non-combat action failed
              :else
              {:success false
               :game-state (:game-state result)
               :attempts (inc attempts)
               :error (:error result)}))))))

(defn execute-phase
  "Execute a phase (group of actions for a goal).
   Handles repeat-until actions by looping until condition is satisfied.
   Verifies we're at the expected location before executing.
   Returns {:success bool :game-state gs :actions-completed N :error string}"
  [game-state phase]
  (let [actions (:actions phase)
        expected-loc (:expected-location phase)
        actual-loc (:here game-state)]
    ;; Verify location if expected-location is set and actions exist
    (if (and expected-loc
             (seq actions)
             (not= expected-loc actual-loc))
      ;; Location mismatch - fail immediately with clear error
      {:success false
       :game-state game-state
       :actions-completed 0
       :error (str "Location mismatch: expected " (name expected-loc)
                   " but at " (name actual-loc))
       :expected-location expected-loc
       :actual-location actual-loc
       :phase-name (:phase phase)}
      ;; Location OK (or no expected location) - execute actions
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
               :phase-name (:phase phase)})))))))

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
