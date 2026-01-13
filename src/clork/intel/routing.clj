(ns clork.intel.routing
  "Stage 4: Routing Engine for optimal pathfinding.

   Provides:
   - Floyd-Warshall all-pairs shortest paths for TSP optimization
   - State-aware routing that respects flag requirements
   - Special edges (teleports, conditional passages)
   - Route generation for action sequences

   Key functions:
   - shortest-path: Find optimal path respecting state constraints
   - all-pairs-shortest-paths: Floyd-Warshall for TSP
   - route-to: Generate action sequence to reach destination
   - optimize-visit-order: TSP for visiting multiple locations"
  (:require [clork.game-state :as gs]
            [clork.debug.pathfind :as pathfind]
            [clojure.set :as set]))

;;; ---------------------------------------------------------------------------
;;; CONSTANTS
;;; ---------------------------------------------------------------------------

(def ^:const infinity
  "Large number representing infinite distance."
  Integer/MAX_VALUE)

;;; ---------------------------------------------------------------------------
;;; SPECIAL EDGES (TELEPORTS AND CONDITIONAL PASSAGES)
;;; ---------------------------------------------------------------------------
;;; These are edges that don't appear in the normal room exits.

(def special-edges
  "Special edges that represent teleports or flag-gated passages.
   Each edge is {:from room :to room :cost moves :via action :requires #{flags}}"
  [;; Prayer teleport: altar -> forest-1
   {:from :altar
    :to :forest-1
    :cost 1
    :via :pray
    :requires #{}}  ; Always available at altar

   ;; Mirror teleport: mirror-room-1 <-> mirror-room-2
   {:from :mirror-room-1
    :to :mirror-room-2
    :cost 1
    :via {:verb :look :object :mirror}
    :requires #{}}

   {:from :mirror-room-2
    :to :mirror-room-1
    :cost 1
    :via {:verb :look :object :mirror}
    :requires #{}}

   ;; Land of Living Dead access (requires lld-flag from exorcism)
   {:from :entrance-to-hades
    :to :land-of-living-dead
    :cost 1
    :via :south
    :requires #{:lld-flag}}

   ;; Troll room passages (require troll-flag)
   {:from :troll-room
    :to :east-west-passage
    :cost 1
    :via :east
    :requires #{:troll-flag}}

   {:from :east-west-passage
    :to :troll-room
    :cost 1
    :via :west
    :requires #{:troll-flag}}

   ;; Cyclops room up passage (requires cyclops-flag)
   {:from :cyclops-room
    :to :strange-passage
    :cost 1
    :via :up
    :requires #{:cyclops-flag}}

   ;; Strange passage east (magic wall opened by cyclops fleeing)
   {:from :cyclops-room
    :to :strange-passage
    :cost 1
    :via :east
    :requires #{:magic-flag}}

   ;; Dome room to torch room (requires dome-flag - rope tied)
   {:from :dome-room
    :to :torch-room
    :cost 1
    :via {:verb :climb-down :object :rope}
    :requires #{:dome-flag}}

   {:from :torch-room
    :to :dome-room
    :cost 1
    :via {:verb :climb-up :object :rope}
    :requires #{:dome-flag}}

   ;; Solid rainbow crossing (requires rainbow-flag)
   {:from :on-the-rainbow
    :to :end-of-rainbow
    :cost 1
    :via :west
    :requires #{:rainbow-flag}}

   {:from :end-of-rainbow
    :to :on-the-rainbow
    :cost 1
    :via :east
    :requires #{:rainbow-flag}}

   ;; Cellar to living room (trap door must be open)
   {:from :cellar
    :to :living-room
    :cost 1
    :via :up
    :requires #{:trap-door-open}}

   {:from :living-room
    :to :cellar
    :cost 1
    :via :down
    :requires #{:trap-door-open}}

   ;; Grating clearing to grating room
   {:from :grating-clearing
    :to :grating-room
    :cost 1
    :via :down
    :requires #{:grate-open}}

   {:from :grating-room
    :to :grating-clearing
    :cost 1
    :via :up
    :requires #{:grate-open}}])

(defn get-special-edges
  "Get special edges that are traversable with the given flags."
  [available-flags]
  (filter (fn [edge]
            (every? #(contains? available-flags %) (:requires edge)))
          special-edges))

;;; ---------------------------------------------------------------------------
;;; NAVIGATION GRAPH BUILDING
;;; ---------------------------------------------------------------------------

(defn- parse-exit-destination
  "Parse an exit definition to get destination.
   Returns room keyword or nil if not traversable.
   Checks :if condition against available-flags for conditional exits."
  [exit-def available-flags]
  (cond
    (keyword? exit-def) exit-def
    (string? exit-def) nil  ; Blocked
    (and (map? exit-def) (:to exit-def))
    ;; Conditional exit - check :if flag
    (let [required-flag (:if exit-def)]
      (if (or (nil? required-flag)
              (contains? available-flags required-flag))
        (:to exit-def)
        nil))  ; Flag not available, exit blocked
    :else nil))

(defn build-navigation-graph
  "Build weighted navigation graph from game state.

   Returns map of:
   {:edges [{:from room :to room :cost 1 :via dir}]
    :rooms #{all room ids}
    :adjacency {room -> #{neighbors}}}

   Options:
   - available-flags: Set of flags that are currently set (for special edges)"
  [game-state & {:keys [available-flags] :or {available-flags #{}}}]
  (let [rooms (:rooms game-state)
        room-ids (set (keys rooms))

        ;; Normal edges from room exits
        normal-edges
        (for [[room-id room] rooms
              [dir exit-def] (:exits room {})
              :let [dest (parse-exit-destination exit-def available-flags)]
              :when dest]
          {:from room-id :to dest :cost 1 :via dir})

        ;; Add special edges that are available with current flags
        special (get-special-edges available-flags)

        all-edges (concat normal-edges special)

        ;; Build adjacency map
        adjacency (reduce (fn [m {:keys [from to]}]
                            (update m from (fnil conj #{}) to))
                          {}
                          all-edges)]
    {:edges all-edges
     :rooms room-ids
     :adjacency adjacency}))

;;; ---------------------------------------------------------------------------
;;; FLOYD-WARSHALL ALL-PAIRS SHORTEST PATHS
;;; ---------------------------------------------------------------------------

(defn- init-distance-matrix
  "Initialize distance matrix for Floyd-Warshall.
   Returns {[from to] -> distance}"
  [edges rooms]
  (let [;; Start with infinity for all pairs
        base (into {} (for [from rooms, to rooms]
                        [[from to] (if (= from to) 0 infinity)]))
        ;; Set direct edges to cost 1
        with-edges (reduce (fn [m {:keys [from to cost]}]
                             (let [current (get m [from to] infinity)]
                               (assoc m [from to] (min current (or cost 1)))))
                           base
                           edges)]
    with-edges))

(defn- init-next-matrix
  "Initialize next-hop matrix for path reconstruction.
   Returns {[from to] -> next-hop-room}"
  [edges rooms]
  (let [;; Direct edges have next-hop = destination
        base (into {} (for [from rooms, to rooms]
                        [[from to] (when (= from to) from)]))
        with-edges (reduce (fn [m {:keys [from to]}]
                             (assoc m [from to] to))
                           base
                           edges)]
    with-edges))

(defn floyd-warshall
  "Compute all-pairs shortest paths using Floyd-Warshall algorithm.

   Returns {:dist {[from to] -> distance}
            :next {[from to] -> next-hop}
            :rooms #{room-ids}}"
  [graph]
  (let [{:keys [edges rooms]} graph
        room-vec (vec rooms)
        n (count room-vec)

        ;; Initialize matrices
        dist (init-distance-matrix edges rooms)
        next-hop (init-next-matrix edges rooms)

        ;; Run Floyd-Warshall
        [final-dist final-next]
        (reduce
          (fn [[d nx] k]
            (reduce
              (fn [[d' nx'] [i j]]
                (let [dik (get d' [i k] infinity)
                      dkj (get d' [k j] infinity)
                      dij (get d' [i j] infinity)
                      through-k (if (or (= dik infinity) (= dkj infinity))
                                  infinity
                                  (+ dik dkj))]
                  (if (< through-k dij)
                    [(assoc d' [i j] through-k)
                     (assoc nx' [i j] (get nx' [i k]))]
                    [d' nx'])))
              [d nx]
              (for [i room-vec, j room-vec] [i j])))
          [dist next-hop]
          room-vec)]
    {:dist final-dist
     :next final-next
     :rooms rooms}))

(defn get-distance
  "Get distance between two rooms from Floyd-Warshall result."
  [fw-result from to]
  (let [d (get-in fw-result [:dist [from to]] infinity)]
    (when (< d infinity) d)))

(defn reconstruct-path-fw
  "Reconstruct path between two rooms from Floyd-Warshall result.
   Returns sequence of room IDs (including start and end) or nil if no path."
  [fw-result from to]
  (let [{:keys [dist next]} fw-result]
    (if (= infinity (get dist [from to] infinity))
      nil
      (loop [path [from]
             current from]
        (if (= current to)
          path
          (let [next-room (get next [current to])]
            (if (nil? next-room)
              nil  ; No path
              (recur (conj path next-room) next-room))))))))

;;; ---------------------------------------------------------------------------
;;; STATE-AWARE SHORTEST PATH
;;; ---------------------------------------------------------------------------

(defn shortest-path
  "Find shortest path between rooms, respecting current state.

   Returns {:path [room-ids] :distance N :actions [actions]} or nil."
  [game-state from to & {:keys [available-flags] :or {available-flags #{}}}]
  (let [graph (build-navigation-graph game-state :available-flags available-flags)
        fw (floyd-warshall graph)
        path (reconstruct-path-fw fw from to)
        dist (get-distance fw from to)]
    (when path
      {:path path
       :distance dist
       :from from
       :to to})))

;;; ---------------------------------------------------------------------------
;;; ROUTE GENERATION (PATH TO ACTIONS)
;;; ---------------------------------------------------------------------------

(defn- find-edge
  "Find the edge connecting from -> to in the graph."
  [graph from to]
  (first (filter #(and (= (:from %) from) (= (:to %) to))
                 (:edges graph))))

(defn path-to-actions
  "Convert a path of rooms to a sequence of actions.

   Returns sequence of action maps like {:verb :walk :direct-object :north}"
  [game-state path & {:keys [available-flags] :or {available-flags #{}}}]
  (when (seq path)
    (let [graph (build-navigation-graph game-state :available-flags available-flags)]
      (for [[from to] (partition 2 1 path)
            :let [edge (find-edge graph from to)]
            :when edge]
        (let [via (:via edge)]
          (if (keyword? via)
            {:verb :walk :direct-object via}
            via))))))

(defn route-to
  "Generate complete route from current location to destination.

   Returns {:path [rooms] :actions [actions] :distance N :commands [strings]}"
  [game-state destination & {:keys [available-flags] :or {available-flags #{}}}]
  (let [start (:here game-state)
        result (shortest-path game-state start destination
                              :available-flags available-flags)]
    (when result
      (let [actions (path-to-actions game-state (:path result)
                                     :available-flags available-flags)
            commands (mapv #(if (keyword? (:direct-object %))
                              (name (:direct-object %))
                              (str (:verb %) " " (:direct-object %)))
                           actions)]
        (assoc result
               :actions (vec actions)
               :commands commands)))))

;;; ---------------------------------------------------------------------------
;;; TSP OPTIMIZATION FOR VISITING MULTIPLE LOCATIONS
;;; ---------------------------------------------------------------------------

(defn- tsp-nearest-neighbor
  "Simple nearest-neighbor heuristic for TSP.
   Returns ordered list of locations to visit."
  [fw-result start locations]
  (loop [current start
         remaining (set locations)
         order []]
    (if (empty? remaining)
      order
      (let [nearest (apply min-key
                           #(get-distance fw-result current %)
                           remaining)]
        (recur nearest
               (disj remaining nearest)
               (conj order nearest))))))

(defn- tsp-total-distance
  "Calculate total distance for a given order of visits."
  [fw-result start order return-to]
  (let [full-path (concat [start] order (when return-to [return-to]))]
    (reduce + 0
            (map (fn [[from to]]
                   (or (get-distance fw-result from to) infinity))
                 (partition 2 1 full-path)))))

(defn optimize-visit-order
  "Find near-optimal order to visit multiple locations.

   Uses nearest-neighbor heuristic with 2-opt improvement.

   Returns {:order [locations] :total-distance N :unreachable [locations]}"
  [game-state locations & {:keys [start return-to available-flags]
                           :or {available-flags #{}}}]
  (let [start-room (or start (:here game-state))
        graph (build-navigation-graph game-state :available-flags available-flags)
        fw (floyd-warshall graph)

        ;; Filter out unreachable locations
        reachable (filter #(some? (get-distance fw start-room %)) locations)
        unreachable (remove (set reachable) locations)

        ;; Use nearest-neighbor heuristic
        initial-order (tsp-nearest-neighbor fw start-room reachable)

        ;; Simple 2-opt improvement
        improved-order
        (if (< (count initial-order) 3)
          initial-order
          (loop [order (vec initial-order)
                 improved? true]
            (if (not improved?)
              order
              (let [n (count order)
                    ;; Try all 2-opt swaps
                    swaps (for [i (range (dec n))
                                j (range (inc i) n)]
                            [i j])
                    best-swap
                    (reduce
                      (fn [best [i j]]
                        (let [new-order (vec (concat
                                               (subvec order 0 i)
                                               (reverse (subvec order i (inc j)))
                                               (subvec order (inc j))))
                              new-dist (tsp-total-distance fw start-room new-order return-to)]
                          (if (< new-dist (:dist best))
                            {:order new-order :dist new-dist :improved true}
                            best)))
                      {:order order
                       :dist (tsp-total-distance fw start-room order return-to)
                       :improved false}
                      swaps)]
                (recur (:order best-swap) (:improved best-swap))))))]
    {:order improved-order
     :total-distance (tsp-total-distance fw start-room improved-order return-to)
     :unreachable unreachable
     :start start-room
     :return-to return-to}))

;;; ---------------------------------------------------------------------------
;;; CONVENIENCE FUNCTIONS
;;; ---------------------------------------------------------------------------

(defn distance-between
  "Get shortest distance between two rooms with given flags."
  [game-state from to & {:keys [available-flags] :or {available-flags #{}}}]
  (let [graph (build-navigation-graph game-state :available-flags available-flags)
        fw (floyd-warshall graph)]
    (get-distance fw from to)))

(defn reachable-from
  "Get all rooms reachable from a starting room with given flags."
  [game-state from & {:keys [available-flags] :or {available-flags #{}}}]
  (let [graph (build-navigation-graph game-state :available-flags available-flags)
        fw (floyd-warshall graph)]
    (set (for [room (:rooms graph)
               :when (some? (get-distance fw from room))]
           room))))

(defn unreachable-from
  "Get all rooms NOT reachable from a starting room with given flags."
  [game-state from & {:keys [available-flags] :or {available-flags #{}}}]
  (let [graph (build-navigation-graph game-state :available-flags available-flags)
        reachable (reachable-from game-state from :available-flags available-flags)]
    (clojure.set/difference (:rooms graph) reachable)))

(defn rooms-requiring-flag
  "Find rooms that become reachable when a flag is added."
  [game-state from flag & {:keys [available-flags] :or {available-flags #{}}}]
  (let [without-flag (reachable-from game-state from :available-flags available-flags)
        with-flag (reachable-from game-state from :available-flags (conj available-flags flag))]
    (clojure.set/difference with-flag without-flag)))

;;; ---------------------------------------------------------------------------
;;; TREASURE ROUTING (for speedrun planning)
;;; ---------------------------------------------------------------------------

(def treasure-locations
  "Map of treasure objects to their default locations.
   Used for planning treasure collection routes."
  {:jeweled-egg :up-a-tree
   :clockwork-canary :jeweled-egg  ; Inside egg
   :portrait :gallery
   :platinum-bar :loud-room
   :ivory-torch :torch-room
   :gold-coffin :egyptian-room
   :jade :bat-room
   :sapphire-bracelet :gas-room
   :diamond :machine  ; Created from coal
   :bag-of-coins :maze-5
   :crystal-skull :land-of-living-dead
   :jewel-encrusted-trident :atlantis-room
   :chalice :treasure-room
   :trunk-of-jewels :reservoir
   :crystal-sphere :top-of-the-world
   :torch :torch-room
   :pot-of-gold :end-of-rainbow
   :scarab :sandy-beach
   :bauble :gas-room
   :figurine :land-of-living-dead})

(defn plan-treasure-route
  "Plan optimal route to collect specified treasures and return them to trophy case.

   Returns {:order [treasures] :route [actions] :total-distance N}"
  [game-state treasures & {:keys [available-flags] :or {available-flags #{}}}]
  (let [;; Get treasure locations
        locs (map (fn [t] [t (get treasure-locations t)]) treasures)
        valid-locs (filter #(some? (second %)) locs)
        location-rooms (map second valid-locs)

        ;; Optimize visit order
        result (optimize-visit-order game-state location-rooms
                                     :start (:here game-state)
                                     :return-to :living-room
                                     :available-flags available-flags)]
    (assoc result
           :treasures (mapv first valid-locs)
           :locations (zipmap (map first valid-locs) (map second valid-locs)))))
