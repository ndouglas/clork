(ns clork.planner2.navigate
  "Navigation system for the reactive planner.

   Handles:
   - A* pathfinding
   - Room graph construction
   - Movement action selection"
  (:require [clork.planner2.observe :as obs]
            [clork.game-state :as gs]
            [clojure.data.priority-map :refer [priority-map]]))

;;; ---------------------------------------------------------------------------
;;; DOOR/GATE DEFINITIONS
;;; ---------------------------------------------------------------------------

(def door-blocked-exits
  "Exits that are blocked by doors that can be opened.
   Map of [from-room to-room] -> {:door <door-id> :open-action <action>}
   Set :one-way? true for doors that can only be opened from one side."
  {[:behind-house :kitchen] {:door :kitchen-window
                              :open-action {:verb :open :direct-object :kitchen-window}}
   [:living-room :cellar] {:door :trap-door
                           :open-action {:verb :open :direct-object :trap-door}
                           :pre-action {:verb :move :direct-object :rug}}
   ;; Trap door can't be opened from below - it's barred from above!
   [:cellar :living-room] {:door :trap-door
                           :one-way? true}})

(def one-way-blocked-exits
  "Exits that are completely blocked and cannot be traversed.
   Used for doors that can only be opened from one side."
  #{[:cellar :living-room]})

;;; ---------------------------------------------------------------------------
;;; DARK ROOMS
;;; ---------------------------------------------------------------------------

(def dark-rooms
  "Set of rooms that require a light source.
   NOTE: Living room, kitchen, and attic are lit by natural light.
   Only truly underground/enclosed rooms without windows need light."
  #{:cellar :east-of-chasm :gallery :studio
    ;; Underground
    :troll-room :east-west-passage :round-room :narrow-passage
    :mirror-room-south :mirror-room-north :small-cave :engravings-cave
    :dome-room :torch-room :temple :egyptian-room :altar
    :loud-room :damp-cave :white-cliffs-beach-north :white-cliffs-beach-south
    :frigid-river-1 :frigid-river-2 :frigid-river-3 :frigid-river-4 :frigid-river-5
    :dam-base :reservoir :stream-view :stream :underground-reservoir
    :north-south-passage :chasm :deep-canyon :reservoir-south
    :cyclops-room :strange-passage :treasure-room
    ;; Maze
    :maze-1 :maze-2 :maze-3 :maze-4 :maze-5 :maze-6 :maze-7 :maze-8 :maze-9
    :maze-10 :maze-11 :maze-12 :maze-13 :maze-14 :maze-15 :dead-end-maze
    ;; Coal mine
    :coal-mine-1 :coal-mine-2 :coal-mine-3 :coal-mine-4 :coal-mine-5
    :ladder-top :ladder-bottom :timber-room :drafty-room :lower-shaft
    :machine-room :gas-room :smelly-room
    ;; Hades
    :entrance-to-hades :land-of-living-dead
    ;; Other
    :atlantis-room :cave :cave-south :twisting-passage :north-temple :shaft-room
    :bat-room :squeeky-room :mine-entrance :slide-room})

(defn dark-room?
  "Check if a room is dark and requires light."
  [room-id]
  (contains? dark-rooms room-id))

(defn path-requires-light?
  "Check if a navigation path goes through any dark rooms."
  [path]
  (some dark-room? path))

(defn door-info
  "Get door info for a room transition, or nil."
  [from-room to-room]
  (get door-blocked-exits [from-room to-room]))

;;; ---------------------------------------------------------------------------
;;; ROOM GRAPH CONSTRUCTION
;;; ---------------------------------------------------------------------------

(defn- parse-exit-destination
  "Parse an exit definition to get the destination room.
   Returns the destination room-id or nil if blocked permanently."
  [exit-def]
  (cond
    (keyword? exit-def) exit-def
    (string? exit-def) nil  ; Blocked message
    (map? exit-def) (:to exit-def)  ; {:to :room :if :flag} or {:to :room :door :door}
    :else nil))

(defn build-room-graph
  "Build a navigation graph from game state.
   Returns map of room-id -> #{reachable-room-ids}.

   This builds a PERMISSIVE graph that includes door-blocked exits,
   assuming doors can be opened. Flag-gated exits are only included
   if the flag is already set. One-way blocked exits are excluded."
  [game-state]
  (reduce-kv
   (fn [graph room-id room-def]
     (let [exits (:exits room-def {})
           destinations
           (reduce-kv
            (fn [s dir exit-def]
              (let [dest (parse-exit-destination exit-def)]
                (cond
                  ;; No destination
                  (nil? dest) s

                  ;; One-way blocked exit - never include
                  (contains? one-way-blocked-exits [room-id dest])
                  s

                  ;; Flag-gated exit - only include if flag is set
                  (and (map? exit-def) (:if exit-def))
                  (if (get game-state (:if exit-def))
                    (conj s dest)
                    s)

                  ;; Door-gated exit - include (we can open doors)
                  (and (map? exit-def) (:door exit-def))
                  (conj s dest)

                  ;; Regular exit
                  :else
                  (conj s dest))))
            #{}
            exits)]
       (assoc graph room-id destinations)))
   {}
   (:rooms game-state)))

(defn build-room-graph-with-directions
  "Build a navigation graph that tracks direction for each connection.
   Returns map of room-id -> {dest-room-id -> direction}.

   Like build-room-graph, includes door-blocked exits but excludes one-way blocked."
  [game-state]
  (reduce-kv
   (fn [graph room-id room-def]
     (let [exits (:exits room-def {})
           dest-to-dir
           (reduce-kv
            (fn [m dir exit-def]
              (let [dest (parse-exit-destination exit-def)]
                (cond
                  (nil? dest) m

                  ;; One-way blocked exit - never include
                  (contains? one-way-blocked-exits [room-id dest])
                  m

                  ;; Flag-gated - only include if flag set
                  (and (map? exit-def) (:if exit-def))
                  (if (get game-state (:if exit-def))
                    (assoc m dest dir)
                    m)

                  ;; Door-gated or regular - include
                  :else
                  (assoc m dest dir))))
            {}
            exits)]
       (assoc graph room-id dest-to-dir)))
   {}
   (:rooms game-state)))

;;; ---------------------------------------------------------------------------
;;; A* PATHFINDING
;;; ---------------------------------------------------------------------------

(defn find-path
  "A* pathfinding from start to goal.
   Returns vector of rooms forming the path, or nil if no path exists."
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
                         [(assoc open neighbor tentative-g)
                          (assoc cf neighbor current)
                          (assoc gs neighbor tentative-g)]
                         [open cf gs]))
                     [open-set came-from g-score]
                     neighbors)]
                (recur new-open new-came-from new-g-score)))))))))

(defn path-to-directions
  "Convert a path of rooms to direction commands.
   Returns vector of directions, or nil if any step is not navigable."
  [game-state path]
  (when (and path (> (count path) 1))
    (let [dir-graph (build-room-graph-with-directions game-state)]
      (loop [directions []
             remaining path]
        (if (< (count remaining) 2)
          directions
          (let [from (first remaining)
                to (second remaining)
                dir-map (get dir-graph from {})
                dir (get dir-map to)]
            (if dir
              (recur (conj directions dir) (rest remaining))
              nil)))))))

;;; ---------------------------------------------------------------------------
;;; NAVIGATION PLANNING
;;; ---------------------------------------------------------------------------

(defn plan-navigation
  "Plan navigation from current room to destination.

   Returns:
   {:path [room-ids...]
    :directions [:north :east ...]
    :distance n
    :reachable? bool}

   Or nil if no path exists."
  [game-state dest-room]
  (let [current (obs/current-room game-state)
        graph (build-room-graph game-state)
        path (find-path graph current dest-room)]
    (when path
      (let [directions (path-to-directions game-state path)]
        {:path path
         :directions (or directions [])
         :distance (dec (count path))
         :reachable? (some? directions)}))))

(defn next-move-toward
  "Get the next direction to move toward a destination.
   Returns a direction keyword, or nil if not reachable."
  [game-state dest-room]
  (when-let [nav (plan-navigation game-state dest-room)]
    (first (:directions nav))))

;;; ---------------------------------------------------------------------------
;;; NAVIGATION ACTION SELECTION
;;; ---------------------------------------------------------------------------

(defn select-movement-action
  "Select a movement action to make progress toward a room goal.

   Returns an action map {:verb :go :direction <dir>} or nil if stuck."
  [game-state dest-room]
  (let [current (obs/current-room game-state)]
    (if (= current dest-room)
      nil  ; Already there
      (when-let [direction (next-move-toward game-state dest-room)]
        {:verb :go :direction direction}))))

;;; ---------------------------------------------------------------------------
;;; SPECIAL NAVIGATION HANDLING
;;; ---------------------------------------------------------------------------

;; Some rooms require special actions before entering

(defn pre-entry-actions
  "Get any actions required before navigating from one room to another.
   Returns vector of actions or nil."
  [game-state from-room to-room]
  (when-let [info (door-info from-room to-room)]
    (let [{:keys [door open-action pre-action]} info
          door-open? (obs/object-open? game-state door)
          ;; Check if pre-action is needed (e.g., move rug before trap door)
          pre-needed? (and pre-action
                           (case (:verb pre-action)
                             :move (not (obs/flag-set? game-state :rug-moved))
                             true))]
      (cond
        ;; Door already open, nothing needed
        door-open?
        nil

        ;; Need pre-action first (rug)
        pre-needed?
        [pre-action]

        ;; Just need to open door
        :else
        [open-action]))))

(defn full-navigation-sequence
  "Get the full sequence of actions to navigate to a destination.
   Includes any pre-entry requirements.

   Returns vector of actions or nil if not reachable."
  [game-state dest-room]
  (when-let [nav (plan-navigation game-state dest-room)]
    (let [path (:path nav)
          directions (:directions nav)]
      (loop [actions []
             path-remaining path
             dirs-remaining directions]
        (if (empty? dirs-remaining)
          actions
          (let [from (first path-remaining)
                to (second path-remaining)
                dir (first dirs-remaining)
                pre-actions (pre-entry-actions game-state from to)
                move-action {:verb :go :direction dir}]
            (recur (into actions (concat (or pre-actions []) [move-action]))
                   (rest path-remaining)
                   (rest dirs-remaining))))))))

;;; ---------------------------------------------------------------------------
;;; DISTANCE CALCULATIONS
;;; ---------------------------------------------------------------------------

(defn distance-to
  "Calculate distance from current room to destination.
   Returns number of moves, or nil if unreachable."
  [game-state dest-room]
  (when-let [nav (plan-navigation game-state dest-room)]
    (:distance nav)))

(defn reachable?
  "Check if a room is reachable from current location."
  [game-state dest-room]
  (some? (plan-navigation game-state dest-room)))

(defn nearest-room
  "Find the nearest room from a set of candidates.
   Returns [room-id distance] or nil if none reachable."
  [game-state room-candidates]
  (let [distances (for [room room-candidates
                        :let [dist (distance-to game-state room)]
                        :when dist]
                    [room dist])]
    (when (seq distances)
      (apply min-key second distances))))

;;; ---------------------------------------------------------------------------
;;; DEBUG
;;; ---------------------------------------------------------------------------

(defn print-path
  "Print a navigation plan."
  [game-state dest-room]
  (if-let [nav (plan-navigation game-state dest-room)]
    (do
      (println "\n=== Navigation Plan ===")
      (println "  From:" (obs/current-room game-state))
      (println "  To:" dest-room)
      (println "  Distance:" (:distance nav) "moves")
      (println "  Path:" (vec (:path nav)))
      (println "  Directions:" (vec (:directions nav))))
    (println "No path to" dest-room)))
