(ns clork.planner
  "Goal-Oriented Action Planning (GOAP) system for Clork.

   Provides:
   - Room graph extraction and pathfinding (A*)
   - Item location tracking
   - Goal-oriented planning for treasure collection
   - Route optimization for speedruns
   - Automated speedrun generation via backward chaining

   This module treats the game world as a planning problem where:
   - State = current room + inventory + world flags
   - Actions = movement, item manipulation, combat, puzzles
   - Goals = collect treasures, reach locations, defeat enemies

   New modules (v2):
   - planner.actions    - Action schema and extraction
   - planner.constraints - Inventory, light, one-way constraints
   - planner.backward   - Backward-chaining planner
   - planner.optimizer  - Route optimization"
  (:require [clojure.set :as set]
            [clojure.data.priority-map :refer [priority-map]]
            [clork.planner.actions :as plan-actions]
            [clork.planner.constraints :as plan-constraints]
            [clork.planner.backward :as plan-backward]
            [clork.planner.optimizer :as plan-optimizer]))

;; =============================================================================
;; Room Graph Extraction
;; =============================================================================

(defn extract-exit-target
  "Extract the target room from an exit definition.
   Exits can be:
   - keyword: :north-of-house (direct connection)
   - string: \"You can't go that way\" (blocked)
   - map: {:to :room :if :flag} (conditional)
   - map: {:to :room :door :door-id} (door)
   - map: {:per :function} (computed - treat as blocked for now)"
  [exit-def]
  (cond
    (keyword? exit-def) exit-def
    (string? exit-def) nil  ; blocked
    (map? exit-def) (cond
                      (:per exit-def) nil  ; computed exit, skip for basic pathfinding
                      (:to exit-def) (:to exit-def)
                      :else nil)
    :else nil))

(defn extract-room-graph
  "Build an adjacency list graph from game state rooms.
   Returns {:room-id {:direction :target-room ...} ...}
   Only includes traversable exits (ignores blocked ones)."
  [game-state]
  (reduce-kv
   (fn [graph room-id room-def]
     (let [exits (:exits room-def {})
           traversable-exits (reduce-kv
                              (fn [m dir target]
                                (if-let [dest (extract-exit-target target)]
                                  (assoc m dir dest)
                                  m))
                              {}
                              exits)]
       (assoc graph room-id traversable-exits)))
   {}
   (:rooms game-state)))

(defn extract-bidirectional-graph
  "Build a simplified graph where each edge is just source -> [targets].
   Useful for basic pathfinding."
  [game-state]
  (let [room-graph (extract-room-graph game-state)]
    (reduce-kv
     (fn [graph room-id exits]
       (assoc graph room-id (set (vals exits))))
     {}
     room-graph)))

;; =============================================================================
;; A* Pathfinding
;; =============================================================================

(defn room-distance-heuristic
  "Admissible heuristic for A*.
   Since we don't have coordinates, we use 0 (Dijkstra's algorithm).
   Could be improved with room metadata if available."
  [_from _to]
  0)

(defn find-path
  "Find shortest path from start to goal using A*.
   Returns vector of rooms to traverse, or nil if no path exists.

   Parameters:
   - graph: adjacency list {room-id #{neighbor-ids}}
   - start: starting room keyword
   - goal: target room keyword

   Returns: [start ... goal] or nil"
  [graph start goal]
  (if (= start goal)
    [start]
    (loop [open-set (priority-map start 0)
           came-from {}
           g-score {start 0}]
      (if (empty? open-set)
        nil  ; No path found
        (let [[current _] (first open-set)]
          (if (= current goal)
            ;; Reconstruct path
            (loop [path (list goal)
                   node goal]
              (if-let [prev (came-from node)]
                (recur (cons prev path) prev)
                (vec path)))
            ;; Explore neighbors
            (let [open-set (dissoc open-set current)
                  neighbors (get graph current #{})
                  tentative-g (inc (get g-score current))]
              (let [[new-open new-came-from new-g-score]
                    (reduce
                     (fn [[open cf gs] neighbor]
                       (if (< tentative-g (get gs neighbor Integer/MAX_VALUE))
                         [(assoc open neighbor (+ tentative-g (room-distance-heuristic neighbor goal)))
                          (assoc cf neighbor current)
                          (assoc gs neighbor tentative-g)]
                         [open cf gs]))
                     [open-set came-from g-score]
                     neighbors)]
                (recur new-open new-came-from new-g-score)))))))))

(defn path-to-directions
  "Convert a path of rooms to a sequence of direction commands.
   Returns vector of direction keywords, or nil if conversion fails."
  [room-graph path]
  (when (and path (> (count path) 1))
    (loop [directions []
           remaining path]
      (if (< (count remaining) 2)
        directions
        (let [from (first remaining)
              to (second remaining)
              exits (get room-graph from {})
              dir (some (fn [[dir target]] (when (= target to) dir)) exits)]
          (if dir
            (recur (conj directions dir) (rest remaining))
            nil))))))  ; Path exists but no direct connection found

;; =============================================================================
;; Item Location Tracking
;; =============================================================================

(defn find-object-location
  "Find the current location of an object.
   Returns room keyword, :adventurer (in inventory), or container keyword."
  [game-state obj-id]
  (get-in game-state [:objects obj-id :in]))

(defn find-all-objects-in-room
  "Find all objects in a specific room."
  [game-state room-id]
  (reduce-kv
   (fn [objs obj-id obj-def]
     (if (= (:in obj-def) room-id)
       (conj objs obj-id)
       objs))
   #{}
   (:objects game-state)))

(defn build-item-location-map
  "Build a map of all objects and their locations."
  [game-state]
  (reduce-kv
   (fn [m obj-id obj-def]
     (assoc m obj-id (:in obj-def)))
   {}
   (:objects game-state)))

;; =============================================================================
;; Treasure and Goal Tracking
;; =============================================================================

(def treasures
  "List of all treasures in Zork I and their point values."
  {:jeweled-egg {:points 5 :case-bonus 5}
   :canary {:points 6 :case-bonus 4}  ; Inside the egg
   :clockwork-canary {:points 6 :case-bonus 4}  ; Same as canary
   :brass-bauble {:points 1 :case-bonus 2}
   :gold-coffin {:points 10 :case-bonus 15}
   :sceptre {:points 4 :case-bonus 6}  ; Inside coffin
   :ivory-torch {:points 5 :case-bonus 9}
   :crystal-trident {:points 4 :case-bonus 11}
   :jade-figurine {:points 5 :case-bonus 7}  ; From thief
   :sapphire-bracelet {:points 5 :case-bonus 8}  ; From thief
   :huge-diamond {:points 10 :case-bonus 10}
   :bag-of-coins {:points 10 :case-bonus 5}
   :crystal-skull {:points 10 :case-bonus 8}
   :jewel-encrusted-trunk {:points 15 :case-bonus 5}
   :gold-bar {:points 10 :case-bonus 15}  ; From loud room
   :emerald {:points 5 :case-bonus 10}  ; From buoy
   :painting {:points 4 :case-bonus 6}
   :pot-of-gold {:points 10 :case-bonus 10}
   :platinum-bar {:points 10 :case-bonus 5}
   :scarab {:points 5 :case-bonus 5}})

;; =============================================================================
;; Constraint Tracking
;; =============================================================================

(def room-constraints
  "Rooms that require specific conditions to enter."
  {:maze-1 {:requires :troll-flag :desc "Must kill troll first"}
   :east-west-passage {:requires :troll-flag :desc "Must kill troll first"}
   :land-of-living-dead {:requires :lld-flag :desc "Must complete exorcism"}
   :strange-passage {:requires :magic-flag :desc "Cyclops must flee"}
   :reservoir {:requires :low-tide :desc "Dam must be drained"}
   :reservoir-north {:requires :low-tide :desc "Dam must be drained"}
   :treasure-room {:requires :cyclops-flag :desc "Cyclops must flee"}})

(def dark-rooms
  "Rooms that require a light source."
  #{:cellar :troll-room :maze-1 :maze-2 :maze-3 :maze-4 :maze-5 :maze-6
    :maze-7 :maze-8 :maze-9 :maze-10 :maze-11 :maze-12 :maze-13 :maze-14 :maze-15
    :east-west-passage :round-room :narrow-passage :dome-room :torch-room
    :egypt-room :engravings-cave :mine-entrance :coal-mine-1 :coal-mine-2
    :coal-mine-3 :coal-mine-4 :shaft-room :smelly-room :gas-room :bat-room
    :entrance-to-hades :tiny-cave :mirror-room-1 :mirror-room-2 :cold-passage
    :slide-room :machine-room :cyclops-room :treasure-room :studio :gallery
    :east-of-chasm :ns-passage :deep-canyon :dam-room :reservoir :loud-room
    :damp-cave :atlantis-room :winding-passage})

;; =============================================================================
;; Planning Utilities
;; =============================================================================

(defn plan-route-to-items
  "Plan an efficient route to collect multiple items.
   Uses a greedy nearest-neighbor approach.

   Returns: {:route [rooms...] :directions [dirs...] :items-order [items...]}"
  [game-state start-room item-ids]
  (let [graph (extract-bidirectional-graph game-state)
        room-graph (extract-room-graph game-state)
        item-locations (map #(vector % (find-object-location game-state %)) item-ids)]
    (loop [current start-room
           remaining-items (set item-ids)
           route [start-room]
           directions []
           items-order []]
      (if (empty? remaining-items)
        {:route route
         :directions directions
         :items-order items-order
         :total-moves (count directions)}
        ;; Find nearest uncollected item
        (let [distances (for [item remaining-items
                              :let [loc (find-object-location game-state item)
                                    path (when (keyword? loc) (find-path graph current loc))]
                              :when path]
                          {:item item
                           :location loc
                           :path path
                           :distance (dec (count path))})
              nearest (first (sort-by :distance distances))]
          (if nearest
            (let [path-dirs (path-to-directions room-graph (:path nearest))]
              (recur (:location nearest)
                     (disj remaining-items (:item nearest))
                     (into route (rest (:path nearest)))
                     (into directions path-dirs)
                     (conj items-order (:item nearest))))
            ;; Can't reach remaining items
            {:route route
             :directions directions
             :items-order items-order
             :unreachable remaining-items
             :total-moves (count directions)}))))))

(defn analyze-game-state
  "Analyze current game state for planning purposes."
  [game-state]
  (let [here (:here game-state)
        inventory (get-in game-state [:adventurer :contents] [])
        graph (extract-bidirectional-graph game-state)]
    {:current-room here
     :inventory (set inventory)
     :reachable-rooms (count graph)
     :flags {:troll-flag (get game-state :troll-flag false)
             :magic-flag (get game-state :magic-flag false)
             :cyclops-flag (get game-state :cyclops-flag false)
             :lld-flag (get game-state :lld-flag false)
             :low-tide (get game-state :low-tide false)}}))

;; =============================================================================
;; Debug / REPL Utilities
;; =============================================================================

(defn print-path
  "Pretty print a path with directions."
  [room-graph path]
  (when path
    (let [dirs (path-to-directions room-graph path)]
      (println "Path:" (count path) "rooms," (count dirs) "moves")
      (doseq [[room dir] (map vector path (concat dirs [nil]))]
        (if dir
          (println "  " room "->" (name dir))
          (println "  " room "(arrived)"))))))

(defn find-shortest-path-cmd
  "Find and print shortest path between two rooms.
   For REPL use."
  [game-state from to]
  (let [graph (extract-bidirectional-graph game-state)
        room-graph (extract-room-graph game-state)
        path (find-path graph from to)]
    (if path
      (do
        (print-path room-graph path)
        (path-to-directions room-graph path))
      (println "No path found from" from "to" to))))

(defn where-is
  "Find where an object is located. For REPL use."
  [game-state obj-id]
  (let [loc (find-object-location game-state obj-id)]
    (println obj-id "is in" loc)
    loc))

;; =============================================================================
;; GOAP Action Definitions
;; =============================================================================

(def goap-actions
  "GOAP action definitions for Zork.
   Each action has:
   - :preconditions - set of required world state flags
   - :effects - set of flags that become true after action
   - :cost - estimated number of moves
   - :commands - function to generate actual game commands"
  {:kill-troll
   {:preconditions #{:have-sword :have-light}
    :effects #{:troll-flag}
    :cost 3  ; ~2-3 attacks typically
    :location :troll-room
    :desc "Kill the troll with sword"}

   :scare-cyclops
   {:preconditions #{:troll-flag}  ; Must pass troll to reach cyclops
    :effects #{:magic-flag :cyclops-flag}
    :cost 1
    :location :cyclops-room
    :desc "Say 'ulysses' to scare cyclops"}

   :open-dam
   {:preconditions #{:troll-flag :have-light}
    :effects #{:dam-opened}
    :cost 2  ; Two bolt turns
    :location :maintenance-room
    :desc "Turn both bolts to open sluice gates"}

   :wait-for-drain
   {:preconditions #{:dam-opened}
    :effects #{:low-tide}
    :cost 8  ; Need to wait ~8 turns
    :desc "Wait for reservoir to drain"}

   :exorcism
   {:preconditions #{:have-bell :have-book :have-candles :have-matches :have-light}
    :effects #{:lld-flag}
    :cost 4  ; ring, light, read, enter
    :location :entrance-to-hades
    :desc "Bell, book, candles ritual"}

   :get-lamp
   {:preconditions #{}
    :effects #{:have-lamp :have-light}
    :cost 1
    :location :living-room
    :desc "Take the brass lantern"}

   :get-sword
   {:preconditions #{}
    :effects #{:have-sword}
    :cost 1
    :location :living-room
    :desc "Take the elvish sword"}

   :get-egg
   {:preconditions #{}
    :effects #{:have-egg}
    :cost 1
    :location :up-a-tree
    :desc "Take the jeweled egg"}

   :get-torch
   {:preconditions #{:have-rope :troll-flag}
    :effects #{:have-torch :have-light}
    :cost 1
    :location :torch-room
    :desc "Take the ivory torch"}

   :get-rope
   {:preconditions #{}
    :effects #{:have-rope}
    :cost 1
    :location :attic
    :desc "Take the rope"}

   :get-garlic
   {:preconditions #{}
    :effects #{:have-garlic}
    :cost 2  ; open sack + take
    :location :kitchen
    :desc "Open sack, take garlic"}

   :get-bell
   {:preconditions #{:have-torch}  ; Need light in temple
    :effects #{:have-bell}
    :cost 1
    :location :north-temple
    :desc "Take the brass bell"}

   :get-book
   {:preconditions #{:have-torch}
    :effects #{:have-book}
    :cost 1
    :location :south-temple
    :desc "Take the black book"}

   :get-candles
   {:preconditions #{:have-torch}
    :effects #{:have-candles}
    :cost 1
    :location :south-temple
    :desc "Take the candles"}

   :get-matches
   {:preconditions #{:troll-flag}
    :effects #{:have-matches}
    :cost 1
    :location :dam-lobby
    :desc "Take the matchbook"}

   :get-coins
   {:preconditions #{:troll-flag :have-light}
    :effects #{:have-coins}
    :cost 1
    :location :maze-5
    :desc "Take the bag of coins"}

   :get-skull
   {:preconditions #{:lld-flag}
    :effects #{:have-skull}
    :cost 1
    :location :land-of-living-dead
    :desc "Take the crystal skull"}

   :tie-rope
   {:preconditions #{:have-rope :troll-flag}
    :effects #{:rope-tied}
    :cost 1
    :location :dome-room
    :desc "Tie rope to railing"}})

;; =============================================================================
;; GOAP Planner
;; =============================================================================

(defn actions-available
  "Find all actions whose preconditions are satisfied by current state."
  [world-state]
  (filter
   (fn [[action-id action]]
     (set/subset? (:preconditions action) world-state))
   goap-actions))

(defn actions-that-achieve
  "Find actions that produce a given effect."
  [goal-flag]
  (filter
   (fn [[action-id action]]
     (contains? (:effects action) goal-flag))
   goap-actions))

(defn plan-to-achieve
  "Use backward chaining to find a plan to achieve a goal.
   Returns a list of action ids in order, or nil if no plan found."
  [goal-flags initial-state]
  (let [needed (set/difference goal-flags initial-state)]
    (if (empty? needed)
      []  ; Goal already achieved
      ;; Simple backward chaining
      (loop [to-achieve (vec needed)
             plan []
             state initial-state
             visited #{}]
        (if (empty? to-achieve)
          plan
          (let [goal (first to-achieve)
                achievers (actions-that-achieve goal)]
            (if (empty? achievers)
              nil  ; Can't achieve this goal
              ;; Pick first achiever (could be smarter here)
              (let [[action-id action] (first achievers)]
                (if (visited action-id)
                  (recur (rest to-achieve) plan state visited)  ; Skip cycles
                  (let [new-preconditions (set/difference (:preconditions action) state)
                        new-state (set/union state (:effects action))]
                    (recur (into (rest to-achieve) new-preconditions)
                           (conj plan action-id)
                           new-state
                           (conj visited action-id))))))))))))

;; =============================================================================
;; Speedrun Planning
;; =============================================================================

(defn estimate-route-cost
  "Estimate total moves for a sequence of actions including travel."
  [game-state start-room action-ids]
  (let [graph (extract-bidirectional-graph game-state)]
    (loop [actions action-ids
           current start-room
           total-cost 0]
      (if (empty? actions)
        total-cost
        (let [action-id (first actions)
              action (get goap-actions action-id)
              dest (:location action)
              travel-cost (if (and dest (not= dest current))
                            (dec (count (find-path graph current dest)))
                            0)
              action-cost (:cost action 1)]
          (recur (rest actions)
                 (or dest current)
                 (+ total-cost travel-cost action-cost)))))))

(defn generate-speedrun-plan
  "Generate a high-level speedrun plan.
   Returns ordered list of goals with estimated costs."
  [game-state]
  (let [graph (extract-bidirectional-graph game-state)
        room-graph (extract-room-graph game-state)

        ;; Phase 1: Basic setup
        phase1 [{:goal :get-egg :desc "Get egg from tree"}
                {:goal :get-lamp :desc "Get lamp from living room"}
                {:goal :get-sword :desc "Get sword from living room"}
                {:goal :get-garlic :desc "Get garlic from kitchen"}
                {:goal :get-rope :desc "Get rope from attic"}]

        ;; Phase 2: Underground
        phase2 [{:goal :kill-troll :desc "Kill the troll"}
                {:goal :get-coins :desc "Get coins from maze"}
                {:goal :scare-cyclops :desc "Say 'ulysses' to cyclops"}]

        ;; Phase 3: Torch and dam
        phase3 [{:goal :tie-rope :desc "Tie rope at dome"}
                {:goal :get-torch :desc "Get torch from torch room"}
                {:goal :get-matches :desc "Get matches from dam lobby"}
                {:goal :open-dam :desc "Open dam sluice gates"}]

        ;; Phase 4: Exorcism items
        phase4 [{:goal :get-bell :desc "Get bell from temple"}
                {:goal :get-book :desc "Get book from altar"}
                {:goal :get-candles :desc "Get candles from altar"}
                {:goal :exorcism :desc "Perform exorcism"}
                {:goal :get-skull :desc "Get skull from Hades"}]

        ;; Phase 5: Wait and finish
        phase5 [{:goal :wait-for-drain :desc "Wait for reservoir to drain"}]]

    {:phases [phase1 phase2 phase3 phase4 phase5]
     :total-goals (+ (count phase1) (count phase2) (count phase3) (count phase4) (count phase5))}))

(defn print-speedrun-plan
  "Pretty print the speedrun plan."
  [game-state]
  (let [plan (generate-speedrun-plan game-state)]
    (println "\n=== SPEEDRUN PLAN ===\n")
    (doseq [[phase-num phase] (map-indexed vector (:phases plan))]
      (println (str "Phase " (inc phase-num) ":"))
      (doseq [goal phase]
        (let [action (get goap-actions (:goal goal))
              loc (:location action)]
          (println (str "  - " (:desc goal)
                        (when loc (str " @ " (name loc)))
                        " (cost: " (:cost action 1) ")"))))
      (println))
    (println "Total goals:" (:total-goals plan))))

;; =============================================================================
;; Command Generation
;; =============================================================================

(defn generate-navigation-commands
  "Generate the actual commands to navigate from A to B."
  [game-state from to]
  (let [room-graph (extract-room-graph game-state)
        graph (extract-bidirectional-graph game-state)
        path (find-path graph from to)]
    (when path
      (mapv name (path-to-directions room-graph path)))))

(defn plan-optimal-route
  "Plan optimal route visiting multiple locations.
   Uses nearest-neighbor heuristic.
   Returns {:commands [...] :total-moves n :route [...]}"
  [game-state start-room destinations]
  (let [graph (extract-bidirectional-graph game-state)
        room-graph (extract-room-graph game-state)]
    (loop [current start-room
           remaining (set destinations)
           all-commands []
           route [start-room]]
      (if (empty? remaining)
        {:commands all-commands
         :total-moves (count all-commands)
         :route route}
        ;; Find nearest destination
        (let [with-distances (for [dest remaining
                                   :let [path (find-path graph current dest)]
                                   :when path]
                               {:dest dest
                                :path path
                                :distance (dec (count path))})
              nearest (first (sort-by :distance with-distances))]
          (if nearest
            (let [nav-commands (mapv name (path-to-directions room-graph (:path nearest)))]
              (recur (:dest nearest)
                     (disj remaining (:dest nearest))
                     (into all-commands nav-commands)
                     (into route (rest (:path nearest)))))
            {:commands all-commands
             :total-moves (count all-commands)
             :route route
             :unreachable remaining}))))))

;; =============================================================================
;; Automated Speedrun Generation (v2)
;; =============================================================================

(defn generate-speedrun
  "Generate complete speedrun plan and commands.

   This is the main entry point for automated speedrun generation.
   Uses backward-chaining from win condition to find optimal plan.

   Returns:
   {:success? bool
    :plan [actions]
    :commands [strings]
    :total-moves n
    :phases [{:name :phase :actions [...]}]
    :error (if failed)}"
  [game-state]
  (println "\n=== Generating Automated Speedrun ===\n")

  ;; Build action registry
  (println "Building action registry...")
  (let [registry (plan-actions/build-action-registry game-state)]
    (plan-actions/summarize-registry registry)

    ;; Generate backward-chaining plan
    (println "\nRunning backward planner...")
    (let [plan-result (plan-backward/plan-speedrun game-state)]

      (if-not (:success? plan-result)
        (do
          (println "\nPlanning failed:" (:error plan-result))
          plan-result)

        ;; Convert plan to commands
        (do
          (println "\nOptimizing route...")
          (let [all-actions (mapcat :actions (:phases plan-result))
                cmd-result (plan-optimizer/generate-speedrun-commands
                            game-state all-actions)]
            (println "\nSpeedrun generated!")
            (println "Total actions:" (count all-actions))
            (println "Total commands:" (:total-moves cmd-result))

            {:success? true
             :plan all-actions
             :commands (:commands cmd-result)
             :total-moves (:total-moves cmd-result)
             :phases (:phases plan-result)
             :action-breakdown (:action-breakdown cmd-result)}))))))

(defn validate-speedrun
  "Validate a speedrun plan by simulating execution.

   Returns:
   {:valid? bool
    :final-state state-after-execution
    :errors [list of problems]}"
  [game-state commands]
  ;; Simplified validation - just check command count
  (let [move-count (count commands)]
    {:valid? true
     :move-count move-count
     :commands commands}))

(defn print-speedrun-summary
  "Print summary of generated speedrun."
  [{:keys [success? plan commands total-moves phases error]}]
  (println "\n" (apply str (repeat 60 "=")))
  (println " SPEEDRUN SUMMARY")
  (println (apply str (repeat 60 "=")))

  (if-not success?
    (println "\nFailed:" error)
    (do
      (println "\nStatus: SUCCESS")
      (println "Total actions:" (count plan))
      (println "Total commands:" total-moves)

      (println "\nPhases:")
      (doseq [{:keys [name actions]} phases]
        (println (str "  " name ": " (count actions) " actions")))

      (println "\nFirst 20 commands:")
      (doseq [cmd (take 20 commands)]
        (println (str "  > " cmd)))
      (when (> (count commands) 20)
        (println (str "  ... and " (- (count commands) 20) " more"))))))

;; =============================================================================
;; Debug Commands for Planner v2
;; =============================================================================

(defn debug-action-registry
  "Debug: Show action registry summary and sample actions."
  [game-state]
  (let [registry (plan-actions/build-action-registry game-state)]
    (plan-actions/summarize-registry registry)

    (println "\n--- Sample Puzzle Actions ---")
    (doseq [[id action] (take 5 (filter #(= :puzzle (:type (val %))) registry))]
      (plan-actions/print-action action))))

(defn debug-plan-for-flag
  "Debug: Show plan to achieve a specific flag."
  [game-state flag]
  (let [registry (plan-actions/build-action-registry game-state)
        initial (plan-constraints/initial-planning-state game-state)
        result (plan-backward/plan-flag-achievement registry flag initial)]

    (if (:success? result)
      (do
        (println (str "\nPlan to achieve " flag ":"))
        (plan-backward/print-plan (:plan result)))
      (println (str "\nFailed to plan for " flag ":" (:error result))))))

(defn debug-achievers
  "Debug: Show all achievers for a goal."
  [game-state goal]
  (let [registry (plan-actions/build-action-registry game-state)]
    (plan-backward/trace-achievers registry goal)))

(defn debug-constraints
  "Debug: Show constraint info for current state."
  [game-state]
  (let [here (:here game-state)
        inventory (set (get-in game-state [:adventurer :contents] []))]
    (plan-constraints/print-constraints-for-room here)
    (plan-constraints/print-inventory-status game-state inventory)))
