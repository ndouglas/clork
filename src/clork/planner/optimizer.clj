(ns clork.planner.optimizer
  "Route optimization for Clork speedrun plans.

   Converts abstract action plans into optimized command sequences:
   - Merges adjacent movement actions into efficient routes
   - Optimizes multi-destination routes (traveling salesman approach)
   - Handles inventory constraints (deposit trips)
   - Generates executable command sequences"
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
   :thief-lair #{:jade-figurine :sapphire-bracelet :gold-coffin :clockwork-canary}
   :coal-mine #{:huge-diamond :scarab}
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
  "Build navigation graph from game state."
  [game-state]
  (reduce-kv
   (fn [graph room-id room-def]
     (let [exits (:exits room-def {})
           destinations (reduce-kv
                         (fn [s dir exit]
                           (let [target (actions/parse-exit exit dir)]
                             (if (:to target)
                               (conj s (:to target))
                               s)))
                         #{}
                         exits)]
       (assoc graph room-id destinations)))
   {}
   (:rooms game-state)))

(defn build-room-graph-with-directions
  "Build graph that tracks direction for each connection."
  [game-state]
  (reduce-kv
   (fn [graph room-id room-def]
     (let [exits (:exits room-def {})
           dir-map (reduce-kv
                    (fn [m dir exit]
                      (let [parsed (actions/parse-exit exit dir)]
                        (if (:to parsed)
                          (assoc m (:to parsed) dir)
                          m)))
                    {}
                    exits)]
       (assoc graph room-id dir-map)))
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

(defn generate-navigation
  "Generate navigation commands from current location to destination."
  [game-state current-room dest-room]
  (let [graph (build-nav-graph game-state)
        room-graph (build-room-graph-with-directions game-state)
        path (find-path graph current-room dest-room)]
    (when path
      (let [dirs (path-to-directions room-graph path)]
        {:path path
         :commands (mapv name dirs)
         :moves (count dirs)}))))

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

(defn plan-to-command-sequence
  "Convert a plan to an executable command sequence.

   Parameters:
   - game-state: Current game state for navigation
   - plan: Sequence of actions
   - start-room: Starting location

   Returns:
   {:commands [\"cmd\" ...]
    :total-moves n
    :by-action [{:action :id :commands [...]}]}"
  [game-state plan start-room]
  (loop [remaining plan
         current-room start-room
         all-commands []
         by-action []]
    (if (empty? remaining)
      {:commands all-commands
       :total-moves (count all-commands)
       :by-action by-action}
      (let [action (first remaining)
            dest-room (action-location action)
            nav (when (and dest-room (not= dest-room current-room))
                  (generate-navigation game-state current-room dest-room))
            nav-commands (or (:commands nav) [])
            action-commands (expand-action-to-commands action)
            combined (into nav-commands action-commands)]
        (recur (rest remaining)
               (or dest-room current-room)
               (into all-commands combined)
               (conj by-action {:action (:id action)
                                :from current-room
                                :to dest-room
                                :nav-commands nav-commands
                                :action-commands action-commands}))))))

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
   speedrun from a high-level plan."
  [game-state plan]
  (let [;; Convert plan to commands
        cmd-seq (plan-to-command-sequence game-state plan :west-of-house)

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
