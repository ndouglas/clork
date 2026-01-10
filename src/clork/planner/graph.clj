(ns clork.planner.graph
  "Graph analysis algorithms for speedrun planning.

   Provides:
   - All-pairs shortest path (Floyd-Warshall)
   - TSP solver (Held-Karp dynamic programming)
   - Graph structure analysis utilities"
  (:require [clojure.set :as set]))

;; =============================================================================
;; All-Pairs Shortest Path (Floyd-Warshall)
;; =============================================================================

(defn build-adjacency-map
  "Convert a navigation graph to adjacency map with distances.
   Input: {room #{neighbor1 neighbor2 ...}}
   Output: {[from to] distance} where distance is 1 for adjacent rooms"
  [nav-graph]
  (reduce-kv
   (fn [distances room neighbors]
     (reduce (fn [d neighbor]
               (assoc d [room neighbor] 1))
             distances
             neighbors))
   {}
   nav-graph))

(defn floyd-warshall
  "Compute all-pairs shortest paths using Floyd-Warshall algorithm.

   Input: nav-graph - {room #{neighbors...}}
   Output: {:distances {[from to] dist}
            :next {[from to] next-room-on-path}}

   Time: O(V³), Space: O(V²)
   For Zork's ~110 rooms, this is ~1.3M operations - very fast."
  [nav-graph]
  (let [rooms (vec (keys nav-graph))
        room-set (set rooms)
        n (count rooms)
        room->idx (zipmap rooms (range))
        idx->room (zipmap (range) rooms)

        ;; Initialize distance matrix
        ;; dist[i][j] = 1 if adjacent, infinity otherwise, 0 for self
        inf 999999  ; Use a large but safe integer

        ;; Use mutable int arrays for efficiency
        dist (make-array Integer/TYPE n n)
        next-hop (make-array Integer/TYPE n n)]

    ;; Initialize
    (dotimes [i n]
      (dotimes [j n]
        (aset-int dist i j (if (= i j) 0 inf))
        (aset-int next-hop i j -1)))

    ;; Set direct edges
    (doseq [[room neighbors] nav-graph
            :let [i (room->idx room)]
            neighbor neighbors
            :when (contains? room-set neighbor)
            :let [j (room->idx neighbor)]]
      (aset-int dist i j 1)
      (aset-int next-hop i j (int j)))

    ;; Floyd-Warshall main loop
    (dotimes [k n]
      (dotimes [i n]
        (dotimes [j n]
          (let [d-ik (aget dist i k)
                d-kj (aget dist k j)]
            (when (and (< d-ik inf) (< d-kj inf))
              (let [new-dist (+ d-ik d-kj)]
                (when (< new-dist (aget dist i j))
                  (aset-int dist i j new-dist)
                  (aset-int next-hop i j (aget next-hop i k)))))))))

    ;; Convert back to Clojure maps
    (let [distances (into {}
                          (for [i (range n)
                                j (range n)
                                :let [d (aget dist i j)]
                                :when (and (not= i j) (< d inf))]
                            [[(idx->room i) (idx->room j)] d]))
          next-hops (into {}
                          (for [i (range n)
                                j (range n)
                                :let [nh (aget next-hop i j)]
                                :when (>= nh 0)]
                            [[(idx->room i) (idx->room j)] (idx->room nh)]))]
      {:distances distances
       :next next-hops
       :rooms room-set})))

(defn get-distance
  "Get shortest distance between two rooms.
   Returns nil if unreachable."
  [apsp from to]
  (if (= from to)
    0
    (get (:distances apsp) [from to])))

(defn get-path
  "Reconstruct shortest path from Floyd-Warshall next-hop data.
   Returns vector of rooms including start and end, or nil if unreachable."
  [apsp from to]
  (if (= from to)
    [from]
    (if-let [next-room (get (:next apsp) [from to])]
      (loop [path [from]
             current from]
        (if (= current to)
          path
          (if-let [next (get (:next apsp) [current to])]
            (recur (conj path next) next)
            nil)))  ; Unreachable
      nil)))

;; =============================================================================
;; TSP Solver (Held-Karp Algorithm)
;; =============================================================================

(defn held-karp
  "Solve Traveling Salesman Problem using Held-Karp dynamic programming.

   Finds the minimum cost tour visiting all nodes exactly once and returning
   to the start.

   Input:
   - nodes: vector of node IDs to visit
   - start: starting node (must be in nodes)
   - dist-fn: (fn [from to] -> distance or nil if unreachable)

   Output:
   {:cost total-distance
    :tour [node1 node2 ... start]  ; optimal visiting order
    :reachable? true}

   Or {:reachable? false} if no valid tour exists.

   Time: O(n² 2ⁿ), Space: O(n 2ⁿ)
   For n=15 treasures: ~7.4M operations, ~500KB memory - very feasible."
  [nodes start dist-fn]
  (let [nodes (vec nodes)
        n (count nodes)
        node->idx (zipmap nodes (range))
        idx->node (zipmap (range) nodes)
        start-idx (node->idx start)
        inf Long/MAX_VALUE

        ;; dp[mask][i] = min cost to visit nodes in mask, ending at i
        ;; mask is a bitmask where bit i means node i is visited
        ;; We use a map for sparse storage
        dp (atom {})
        parent (atom {})]

    (when (nil? start-idx)
      (throw (ex-info "Start node not in nodes list" {:start start :nodes nodes})))

    ;; Base case: starting at start, only start visited
    (swap! dp assoc [(bit-shift-left 1 start-idx) start-idx] 0)

    ;; Fill DP table
    (doseq [mask (range 1 (bit-shift-left 1 n))
            :when (bit-test mask start-idx)  ; Start must be in mask
            last-idx (range n)
            :when (bit-test mask last-idx)
            :let [prev-cost (get @dp [mask last-idx] inf)]
            :when (< prev-cost inf)
            next-idx (range n)
            :when (not (bit-test mask next-idx))
            :let [d (dist-fn (idx->node last-idx) (idx->node next-idx))]
            :when d
            :let [new-mask (bit-or mask (bit-shift-left 1 next-idx))
                  new-cost (+ prev-cost d)
                  existing (get @dp [new-mask next-idx] inf)]]
      (when (< new-cost existing)
        (swap! dp assoc [new-mask next-idx] new-cost)
        (swap! parent assoc [new-mask next-idx] [mask last-idx])))

    ;; Find best final state (all visited, return to start)
    (let [full-mask (dec (bit-shift-left 1 n))
          ;; Find best ending point that can return to start
          best-end (reduce
                    (fn [best last-idx]
                      (if (= last-idx start-idx)
                        best  ; Skip start as ending (we return there)
                        (let [tour-cost (get @dp [full-mask last-idx] inf)
                              return-cost (or (dist-fn (idx->node last-idx) start) inf)]
                          (if (and (< tour-cost inf) (< return-cost inf))
                            (let [total (+ tour-cost return-cost)]
                              (if (or (nil? best) (< total (:cost best)))
                                {:cost total :last-idx last-idx}
                                best))
                            best))))
                    nil
                    (range n))]

      (if-not best-end
        {:reachable? false}

        ;; Reconstruct path
        (let [tour (loop [path [start-idx]
                          mask full-mask
                          current (:last-idx best-end)]
                     (if-let [[prev-mask prev-idx] (get @parent [mask current])]
                       (recur (conj path current) prev-mask prev-idx)
                       (conj path current)))]
          {:reachable? true
           :cost (:cost best-end)
           :tour (mapv idx->node (reverse tour))})))))

(defn nearest-neighbor-tsp
  "Fast approximate TSP using nearest neighbor heuristic.
   Good for quick estimates or when Held-Karp is too slow.

   Time: O(n²)"
  [nodes start dist-fn]
  (loop [tour [start]
         remaining (disj (set nodes) start)
         total-cost 0
         current start]
    (if (empty? remaining)
      ;; Return to start
      (let [return-cost (dist-fn current start)]
        (if return-cost
          {:reachable? true
           :cost (+ total-cost return-cost)
           :tour (conj tour start)}
          {:reachable? false}))
      ;; Find nearest unvisited
      (let [nearest (reduce
                     (fn [best node]
                       (let [d (dist-fn current node)]
                         (if (and d (or (nil? best) (< d (:dist best))))
                           {:node node :dist d}
                           best)))
                     nil
                     remaining)]
        (if-not nearest
          {:reachable? false}
          (recur (conj tour (:node nearest))
                 (disj remaining (:node nearest))
                 (+ total-cost (:dist nearest))
                 (:node nearest)))))))

;; =============================================================================
;; Graph Structure Analysis
;; =============================================================================

(defn find-articulation-points
  "Find articulation points (cut vertices) in the graph.
   These are rooms that, if removed, disconnect the graph.

   Uses Tarjan's algorithm."
  [nav-graph]
  (let [rooms (vec (keys nav-graph))
        n (count rooms)
        room->idx (zipmap rooms (range))

        ;; State for DFS
        visited (atom #{})
        disc (atom {})      ; Discovery time
        low (atom {})       ; Lowest reachable
        parent (atom {})
        ap (atom #{})       ; Articulation points
        time (atom 0)]

    (letfn [(dfs [u]
              (let [u-idx (room->idx u)
                    children (atom 0)]
                (swap! visited conj u)
                (swap! disc assoc u @time)
                (swap! low assoc u @time)
                (swap! time inc)

                (doseq [v (get nav-graph u #{})
                        :when (contains? (set rooms) v)]
                  (if (not (@visited v))
                    (do
                      (swap! children inc)
                      (swap! parent assoc v u)
                      (dfs v)
                      ;; Update low value
                      (swap! low assoc u (min (@low u) (@low v)))
                      ;; Check articulation point conditions
                      (when (or
                             ;; Root with 2+ children
                             (and (nil? (@parent u)) (> @children 1))
                             ;; Non-root where child can't reach ancestor
                             (and (@parent u) (>= (@low v) (@disc u))))
                        (swap! ap conj u)))
                    ;; Back edge
                    (when (not= v (@parent u))
                      (swap! low assoc u (min (@low u) (@disc v))))))))]

      ;; Run DFS from each unvisited node
      (doseq [room rooms
              :when (not (@visited room))]
        (dfs room)))

    @ap))

(defn find-one-way-edges
  "Find edges that only exist in one direction (one-way transitions).
   Returns set of [from to] pairs."
  [nav-graph]
  (let [edges (for [[room neighbors] nav-graph
                    neighbor neighbors]
                [room neighbor])]
    (set (filter (fn [[from to]]
                   (not (contains? (get nav-graph to #{}) from)))
                 edges))))

(defn reachable-from
  "Find all rooms reachable from a starting room."
  [nav-graph start]
  (loop [visited #{}
         frontier #{start}]
    (if (empty? frontier)
      visited
      (let [current (first frontier)
            neighbors (get nav-graph current #{})
            new-neighbors (set/difference neighbors visited #{current})]
        (recur (conj visited current)
               (set/union (disj frontier current) new-neighbors))))))

(defn compute-centrality
  "Compute betweenness centrality for each room.
   Rooms with high centrality appear on many shortest paths.

   Returns map of room -> centrality score."
  [apsp]
  (let [rooms (:rooms apsp)
        ;; For each pair (s,t), count how often each intermediate room v appears
        ;; on shortest paths from s to t
        centrality (atom (zipmap rooms (repeat 0)))]

    (doseq [s rooms
            t rooms
            :when (not= s t)
            :let [path (get-path apsp s t)]
            :when path
            ;; Count intermediate nodes (exclude s and t)
            v (rest (butlast path))]
      (swap! centrality update v inc))

    @centrality))

;; =============================================================================
;; Convenience Functions for Planner
;; =============================================================================

(defn make-distance-fn
  "Create a distance function from APSP data for use with TSP solvers."
  [apsp]
  (fn [from to]
    (get-distance apsp from to)))

(defn optimal-treasure-order
  "Given treasures to collect and a home base, find optimal visiting order.

   Input:
   - apsp: All-pairs shortest path data
   - treasures: seq of {:id :location} maps
   - home: room to start and end at (living-room for trophy case)

   Output:
   {:order [{:id :treasure-id :location :room} ...]
    :total-distance n
    :distances-between [[from to dist] ...]}

   Uses Held-Karp for optimal solution.
   Note: Multiple treasures at the same location are handled correctly."
  [apsp treasures home]
  (let [;; Build list of unique locations to visit
        locations (cons home (distinct (map :location treasures)))
        ;; Group treasures by location (handles co-located treasures like
        ;; silver-chalice and clockwork-canary both at treasure-room)
        treasures-by-location (group-by :location treasures)
        dist-fn (make-distance-fn apsp)

        ;; Solve TSP over unique locations
        result (held-karp (set locations) home dist-fn)]

    (if-not (:reachable? result)
      {:reachable? false}

      ;; Convert tour to treasure order, expanding co-located treasures
      (let [tour (:tour result)
            ;; Remove home from start and end, expand to all treasures at each location
            treasure-order (->> tour
                                (drop 1)        ; Remove starting home
                                (butlast)       ; Remove ending home
                                (mapcat #(get treasures-by-location %))
                                (filter some?)
                                vec)]
        {:reachable? true
         :order treasure-order
         :total-distance (:cost result)
         :tour (:tour result)}))))

(defn analyze-graph
  "Comprehensive graph analysis for planning.
   Returns all useful precomputed data."
  [nav-graph]
  (let [apsp (floyd-warshall nav-graph)
        articulation (find-articulation-points nav-graph)
        one-way (find-one-way-edges nav-graph)
        centrality (compute-centrality apsp)]
    {:apsp apsp
     :articulation-points articulation
     :one-way-edges one-way
     :centrality centrality
     :room-count (count (:rooms apsp))}))
