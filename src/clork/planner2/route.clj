(ns clork.planner2.route
  "Route Optimizer for the Prep-Optimized Planner.

   Computes optimal visitation order for treasures using TSP-like
   algorithms, with prep action insertion at optimal points.

   Key features:
   - All-pairs shortest paths using Floyd-Warshall
   - Nearest-neighbor TSP heuristic with 2-opt improvement
   - Prep action insertion based on geographic proximity
   - Inventory constraint handling

   See IMPLEMENTATION_PLAN.md for the full architecture."
  (:require [clork.planner2.navigate :as nav]
            [clork.planner2.observe :as obs]
            [clork.planner2.prep :as prep]
            [clork.planner2.deps :as deps]
            [clojure.set :as set]))

;;; ---------------------------------------------------------------------------
;;; ALL-PAIRS SHORTEST PATHS
;;; ---------------------------------------------------------------------------

(defn floyd-warshall
  "Compute all-pairs shortest paths using Floyd-Warshall algorithm.
   Returns {:dist {[from to] distance} :next {[from to] next-room}}."
  [graph]
  (let [rooms (keys graph)
        n (count rooms)
        room->idx (zipmap rooms (range))
        idx->room (zipmap (range) rooms)
        ;; Initialize distance matrix
        dist (reduce
              (fn [d room]
                (let [i (room->idx room)]
                  (reduce
                   (fn [d' neighbor]
                     (let [j (room->idx neighbor)]
                       (assoc d' [i j] 1)))
                   (assoc d [i i] 0)
                   (get graph room #{}))))
              {}
              rooms)
        ;; Initialize next matrix (for path reconstruction)
        next-room (reduce
                   (fn [nx room]
                     (let [i (room->idx room)]
                       (reduce
                        (fn [nx' neighbor]
                          (let [j (room->idx neighbor)]
                            (assoc nx' [i j] j)))
                        nx
                        (get graph room #{}))))
                   {}
                   rooms)]
    ;; Floyd-Warshall main loop
    (loop [k 0
           dist dist
           next-room next-room]
      (if (>= k n)
        ;; Convert back to room keywords
        {:dist (into {}
                     (for [[[i j] d] dist]
                       [[(idx->room i) (idx->room j)] d]))
         :next (into {}
                     (for [[[i j] nx] next-room]
                       [[(idx->room i) (idx->room j)] (idx->room nx)]))}
        (let [[new-dist new-next]
              (reduce
               (fn [[d nx] i]
                 (reduce
                  (fn [[d' nx'] j]
                    (let [d-ij (get d [i j] Integer/MAX_VALUE)
                          d-ik (get d [i k] Integer/MAX_VALUE)
                          d-kj (get d [k j] Integer/MAX_VALUE)
                          d-via-k (if (or (= d-ik Integer/MAX_VALUE)
                                          (= d-kj Integer/MAX_VALUE))
                                    Integer/MAX_VALUE
                                    (+ d-ik d-kj))]
                      (if (< d-via-k d-ij)
                        [(assoc d' [i j] d-via-k)
                         (assoc nx' [i j] (get nx [i k]))]
                        [d' nx'])))
                  [d nx]
                  (range n)))
               [dist next-room]
               (range n))]
          (recur (inc k) new-dist new-next))))))

(defn compute-distances
  "Compute distance matrix for a set of rooms.
   Returns map of [from to] -> distance."
  [game-state rooms]
  (let [graph (nav/build-room-graph game-state)
        result (floyd-warshall graph)]
    (:dist result)))

(defn distance
  "Get distance between two rooms from precomputed distances.
   Returns Integer/MAX_VALUE if unreachable."
  [distances from to]
  (get distances [from to] Integer/MAX_VALUE))

;;; ---------------------------------------------------------------------------
;;; ROOM LOCATIONS FOR TREASURES/PREPS
;;; ---------------------------------------------------------------------------

(def treasure-locations
  "Map of treasure -> room where it can be found."
  {:egg :up-a-tree
   :painting :gallery
   :bag-of-coins :maze-5
   :platinum-bar :loud-room
   :ivory-torch :torch-room
   :sceptre :torch-room
   :jade-figurine :bat-room
   :sapphire-bracelet :gas-room
   :pot-of-gold :end-of-rainbow
   :crystal-skull :land-of-living-dead
   :trunk :thiefs-lair
   :clockwork-canary :thiefs-lair  ; thief opens egg
   :gold-coffin :egypt-room
   :huge-diamond :machine-room
   :silver-chalice :maze-7  ; dead thief
   :brass-bauble :maze-9
   :zorkmid-coins :dead-end-2
   :china-figurine :maze-6
   :sapphire :atlantis-room})

(defn treasure-location
  "Get the room where a treasure can be found."
  [treasure]
  (get treasure-locations treasure))

(defn prep-location
  "Get the room where a prep action must be performed."
  [prep-id]
  (let [loc (prep/prep-location prep-id)]
    (if (set? loc)
      (first loc)  ; Pick first if multiple options
      loc)))

;;; ---------------------------------------------------------------------------
;;; TSP SOLVER - NEAREST NEIGHBOR
;;; ---------------------------------------------------------------------------

(defn nearest-neighbor-tsp
  "Solve TSP using nearest-neighbor heuristic.
   Returns ordered list of locations to visit."
  [distances start locations]
  (loop [current start
         remaining (set locations)
         route []]
    (if (empty? remaining)
      route
      (let [;; Find nearest unvisited location
            nearest (apply min-key
                           #(distance distances current %)
                           remaining)]
        (recur nearest
               (disj remaining nearest)
               (conj route nearest))))))

(defn route-length
  "Calculate total distance of a route."
  [distances start route]
  (if (empty? route)
    0
    (loop [prev start
           remaining route
           total 0]
      (if (empty? remaining)
        total
        (let [next (first remaining)
              d (distance distances prev next)]
          (if (= d Integer/MAX_VALUE)
            Integer/MAX_VALUE
            (recur next (rest remaining) (+ total d))))))))

;;; ---------------------------------------------------------------------------
;;; TSP SOLVER - 2-OPT IMPROVEMENT
;;; ---------------------------------------------------------------------------

(defn two-opt-swap
  "Perform a 2-opt swap: reverse the segment between i and j."
  [route i j]
  (let [before (subvec route 0 i)
        middle (vec (reverse (subvec route i (inc j))))
        after (subvec route (inc j))]
    (vec (concat before middle after))))

(defn two-opt-improve
  "Improve a route using 2-opt local search."
  [distances start route & {:keys [max-iterations] :or {max-iterations 100}}]
  (loop [best-route (vec route)
         best-length (route-length distances start route)
         iterations 0]
    (if (>= iterations max-iterations)
      best-route
      (let [n (count best-route)
            [improved? new-route new-length]
            (reduce
             (fn [[improved? r len] i]
               (reduce
                (fn [[imp? r' len'] j]
                  (let [swapped (two-opt-swap r' i j)
                        swapped-len (route-length distances start swapped)]
                    (if (< swapped-len len')
                      [true swapped swapped-len]
                      [imp? r' len'])))
                [improved? r len]
                (range (inc i) n)))
             [false best-route best-length]
             (range (dec n)))]
        (if improved?
          (recur new-route new-length (inc iterations))
          best-route)))))

(defn optimize-route
  "Find optimized route through locations using nearest-neighbor + 2-opt."
  [distances start locations]
  (let [initial (nearest-neighbor-tsp distances start locations)]
    (two-opt-improve distances start initial)))

;;; ---------------------------------------------------------------------------
;;; PREP INSERTION
;;; ---------------------------------------------------------------------------

(defn required-preps-for-treasures
  "Get all prep actions required for a set of treasures."
  [treasures]
  (deps/preps-for-treasures treasures))

(defn prep-insertion-point
  "Find the best point in a route to insert a prep action.
   Returns the index where the prep should be inserted."
  [distances route prep-id]
  (let [prep-loc (prep-location prep-id)]
    (if (nil? prep-loc)
      0  ; No specific location, do at start
      ;; Find position that minimizes total distance increase
      (let [candidates
            (for [i (range (inc (count route)))]
              (let [prev (if (zero? i) :living-room (nth route (dec i)))
                    next (if (= i (count route)) nil (nth route i))
                    ;; Distance to go: prev -> prep -> next
                    d-to-prep (distance distances prev prep-loc)
                    d-from-prep (if next
                                  (distance distances prep-loc next)
                                  0)
                    ;; Distance we're replacing: prev -> next
                    d-direct (if next
                               (distance distances prev next)
                               0)
                    ;; Extra distance for this insertion
                    extra (- (+ d-to-prep d-from-prep) d-direct)]
                {:index i :extra extra}))]
        (:index (apply min-key :extra candidates))))))

(defn insert-preps-into-route
  "Insert all required prep actions into an optimized route.
   Returns route with prep locations interleaved."
  [distances route preps]
  (let [;; Sort preps by their optimal insertion point
        prep-locs (for [p preps
                        :let [loc (prep-location p)]
                        :when loc]
                    {:prep p :location loc})
        ;; Convert route to treasure -> location format
        treasure-locs (vec (for [t route] (treasure-location t)))]
    ;; For now, just prepend prep locations that aren't already in route
    (let [route-locs (set treasure-locs)
          needed-prep-locs (remove #(route-locs (:location %)) prep-locs)]
      (concat (map :location needed-prep-locs) treasure-locs))))

;;; ---------------------------------------------------------------------------
;;; FULL ROUTE PLANNING
;;; ---------------------------------------------------------------------------

(defn plan-treasure-route
  "Plan an optimized route for collecting treasures.

   Returns:
   {:treasures [ordered treasures]
    :preps [required prep actions in order]
    :route [room visits in order]
    :total-distance estimated-moves}

   NOTE: Includes ALL requested treasures, even if currently unreachable.
   The executor will enable access through prep actions."
  [game-state treasures]
  (let [;; Get required preps
        preps (required-preps-for-treasures treasures)

        ;; Build distance matrix
        all-rooms (concat
                   (map treasure-location treasures)
                   (keep prep-location preps)
                   [:living-room])  ; Start/end point
        graph (nav/build-room-graph game-state)
        distances (:dist (floyd-warshall graph))

        ;; Get reachable treasures (for optimization)
        treasure-locs (map treasure-location treasures)
        reachable (filter #(< (distance distances :living-room %) Integer/MAX_VALUE)
                          treasure-locs)
        unreachable-locs (remove (set reachable) treasure-locs)

        ;; Optimize reachable treasure order
        optimized-locs (if (empty? reachable)
                         []
                         (optimize-route distances :living-room reachable))

        ;; Map back to treasures - include ALL treasures
        ;; Reachable ones in optimized order, unreachable ones at the end
        ;; NOTE: Multiple treasures can be at the same location, so use a multimap
        loc->treasures (reduce (fn [m t]
                                 (update m (treasure-location t) (fnil conj []) t))
                               {}
                               treasures)
        reachable-treasures (mapcat #(get loc->treasures % []) optimized-locs)
        unreachable-treasures (mapcat #(get loc->treasures % []) unreachable-locs)
        all-treasures (concat reachable-treasures unreachable-treasures)

        ;; Calculate distance (only for reachable part)
        total-dist (route-length distances :living-room optimized-locs)]

    {:treasures (vec all-treasures)
     :preps (vec preps)
     :route (vec (concat optimized-locs unreachable-locs))
     :total-distance total-dist}))

(defn plan-full-route
  "Plan a full speedrun route including preps and treasures.

   Returns a sequence of actions/locations in optimal order:
   [{:type :prep :id :troll-flag :location :troll-room}
    {:type :treasure :id :painting :location :gallery}
    ...]"
  [game-state treasures]
  (let [;; Get required preps in dependency order
        preps (required-preps-for-treasures treasures)

        ;; Build optimized treasure route
        plan (plan-treasure-route game-state treasures)

        ;; Interleave preps with treasures based on dependencies
        ;; For now, do all preps first then treasures
        prep-actions (for [p preps]
                       {:type :prep
                        :id p
                        :location (prep-location p)
                        :action (prep/prep-action p)})
        treasure-actions (for [t (:treasures plan)]
                           {:type :treasure
                            :id t
                            :location (treasure-location t)})]
    (concat prep-actions treasure-actions)))

;;; ---------------------------------------------------------------------------
;;; INVENTORY MANAGEMENT
;;; ---------------------------------------------------------------------------

(def max-inventory 7)  ; Typical adventure game limit

(defn items-needed-for-preps
  "Get all items needed for a set of prep actions."
  [preps]
  (let [all-reqs (mapcat prep/prep-requires preps)
        ;; Filter to just items (not flags)
        items (remove #(contains? prep/prep-actions %) all-reqs)]
    (set items)))

(defn items-at-location
  "Get items available at a location."
  [game-state location]
  ;; This would need to check actual game state
  ;; For now, return items from prep-item-locations
  (set (for [[item info] prep/prep-item-locations
             :when (= (:location info) location)]
         item)))

(defn plan-with-inventory
  "Plan route considering inventory constraints.
   May need multiple trips if too many items are needed."
  [game-state treasures]
  ;; Simplified version - just return basic plan
  ;; Full implementation would track inventory through route
  (plan-full-route game-state treasures))

;;; ---------------------------------------------------------------------------
;;; ANALYSIS / DEBUG
;;; ---------------------------------------------------------------------------

(defn estimate-turns
  "Estimate total turns for a route.
   Includes: movement, prep actions, treasure pickups, deposits."
  [route-plan]
  (let [movement (:total-distance route-plan)
        preps (count (:preps route-plan))
        treasures (count (:treasures route-plan))
        ;; Rough estimates
        prep-turns (* preps 3)  ; Average 3 turns per prep
        pickup-turns treasures
        deposit-turns (* treasures 2)]  ; Go to living room + put in case
    (+ movement prep-turns pickup-turns deposit-turns)))

(defn print-route-plan
  "Print a human-readable route plan."
  [plan]
  (println "=== Route Plan ===")
  (println "Treasures:" (count (:treasures plan)))
  (println "Preps needed:" (count (:preps plan)))
  (println "Estimated movement:" (:total-distance plan))
  (println "Estimated total turns:" (estimate-turns plan))
  (println)
  (println "Prep actions:")
  (doseq [p (:preps plan)]
    (println "  -" (name p) "at" (prep-location p)))
  (println)
  (println "Treasure order:")
  (doseq [t (:treasures plan)]
    (println "  -" (name t) "at" (treasure-location t))))

(defn compare-routes
  "Compare two route orderings and show which is better."
  [game-state route1 route2]
  (let [graph (nav/build-room-graph game-state)
        distances (:dist (floyd-warshall graph))
        locs1 (map treasure-location route1)
        locs2 (map treasure-location route2)
        len1 (route-length distances :living-room (vec locs1))
        len2 (route-length distances :living-room (vec locs2))]
    {:route1-length len1
     :route2-length len2
     :better (if (< len1 len2) :route1 :route2)
     :savings (Math/abs (long (- len1 len2)))}))
