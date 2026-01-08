(ns clork.debug.pathfind
  "A* pathfinding for finding optimal routes between rooms.

   Usage:
   - $path <from> <to>     - Find shortest path between two rooms
   - $path here <to>       - Find path from current location
   - $path <to>            - Shorthand for $path here <to>
   - $reachable            - Show all rooms reachable from current location
   - $reachable <room>     - Show all rooms reachable from specified room"
  (:require [clork.utils :as utils]
            [clork.game-state :as gs]
            [clojure.string :as str]))

;;; ---------------------------------------------------------------------------
;;; GRAPH BUILDING
;;; ---------------------------------------------------------------------------

(def direction-names
  "Direction keywords to command strings."
  {:north "n" :south "s" :east "e" :west "w"
   :ne "ne" :nw "nw" :se "se" :sw "sw"
   :up "u" :down "d" :in "in" :out "out"
   :enter "enter" :exit "exit" :land "land"
   :northeast "ne" :northwest "nw" :southeast "se" :southwest "sw"})

(defn- parse-exit
  "Parse an exit definition to extract the destination room.
   Returns [dest-room-id requires-condition?] or nil if not traversable."
  [exit-def]
  (cond
    ;; Simple keyword - direct connection
    (keyword? exit-def)
    [exit-def false]

    ;; String - blocked/message, not traversable
    (string? exit-def)
    nil

    ;; Map with :to - conditional exit
    (and (map? exit-def) (:to exit-def))
    [(:to exit-def) (boolean (or (:if exit-def) (:door exit-def) (:per exit-def)))]

    ;; Unknown format
    :else nil))

(defn- get-room-neighbors
  "Get all neighboring rooms from a room's exits.
   Returns seq of [direction dest-room conditional?]"
  [room]
  (when-let [exits (:exits room)]
    (->> exits
         (keep (fn [[dir exit-def]]
                 (when-let [[dest conditional?] (parse-exit exit-def)]
                   [dir dest conditional?])))
         (into []))))

(defn build-graph
  "Build a graph of room connections from game state.
   Returns map of room-id -> [{:dir :to :conditional?} ...]"
  [game-state]
  (->> (:rooms game-state)
       (map (fn [[room-id room]]
              [room-id
               (mapv (fn [[dir dest cond?]]
                       {:dir dir :to dest :conditional? cond?})
                     (get-room-neighbors room))]))
       (into {})))

;;; ---------------------------------------------------------------------------
;;; A* PATHFINDING
;;; ---------------------------------------------------------------------------

(defn- reconstruct-path
  "Reconstruct path from came-from map."
  [came-from goal]
  (loop [current goal
         path []]
    (if-let [{:keys [from dir conditional?]} (get came-from current)]
      (recur from (cons {:room current :dir dir :conditional? conditional?} path))
      path)))

(defn find-path
  "Find shortest path from start to goal using BFS.
   Returns seq of {:room :dir :conditional?} or nil if no path exists.

   Options:
   - :allow-conditional? - if true, include conditional exits (default true)"
  [game-state start goal & {:keys [allow-conditional?] :or {allow-conditional? true}}]
  (let [graph (build-graph game-state)]
    (if (= start goal)
      []
      (loop [frontier (conj clojure.lang.PersistentQueue/EMPTY start)
             came-from {}
             visited #{start}]
        (if (empty? frontier)
          nil
          (let [current (peek frontier)
                frontier' (pop frontier)]
            (if (= current goal)
              (reconstruct-path came-from goal)
              (let [neighbors (get graph current [])
                    valid-neighbors (if allow-conditional?
                                      neighbors
                                      (filter #(not (:conditional? %)) neighbors))
                    new-neighbors (filter #(not (visited (:to %))) valid-neighbors)]
                (recur
                 (reduce #(conj %1 (:to %2)) frontier' new-neighbors)
                 (reduce (fn [m neighbor]
                           (assoc m (:to neighbor)
                                  {:from current
                                   :dir (:dir neighbor)
                                   :conditional? (:conditional? neighbor)}))
                         came-from new-neighbors)
                 (reduce #(conj %1 (:to %2)) visited new-neighbors))))))))))

(defn path-to-commands
  "Convert a path to a sequence of movement commands."
  [path]
  (mapv (fn [{:keys [dir]}]
          (get direction-names dir (name dir)))
        path))

;;; ---------------------------------------------------------------------------
;;; DEBUG COMMANDS
;;; ---------------------------------------------------------------------------

(defn- parse-keyword [s]
  (when s
    (if (str/starts-with? s ":")
      (keyword (subs s 1))
      (keyword s))))

(defn- format-path-step
  "Format a single path step, marking conditional exits with *"
  [{:keys [room dir conditional?]}]
  (str " -" (name dir) (when conditional? "*") "-> " (name room)))

(defn- show-path-result
  "Display path finding results."
  [game-state start goal path allow-conditional?]
  (let [commands (path-to-commands path)
        has-conditional? (some :conditional? path)]
    (-> game-state
        (utils/tell (str "Path from " (name start) " to " (name goal)
                         " (" (count path) " moves)"
                         (when (not allow-conditional?) " [unconditional only]")
                         ":\n"))
        (utils/tell (str "  Commands: " (str/join " " commands) "\n"))
        (utils/tell (str "  Route: " (name start)))
        ((fn [gs]
           (reduce (fn [g step]
                     (utils/tell g (format-path-step step)))
                   gs path)))
        (utils/tell "\n")
        ((fn [gs]
           (if has-conditional?
             (utils/tell gs "  (* = requires door/flag/condition)\n")
             gs))))))

(defn cmd-path
  "Find shortest path between two rooms.

   Usage:
     $path <to>              - Path from here (includes conditional exits)
     $path <from> <to>       - Path between two rooms

   Conditional exits (marked with *) require doors to be open,
   flags to be set, or other game conditions to be met."
  [game-state args]
  (cond
    (empty? args)
    (-> game-state
        (utils/tell "Usage: $path <destination>     - find path (may include conditional exits)\n")
        (utils/tell "       $path <from> <to>       - path between two rooms\n")
        (utils/tell "       $path! <destination>    - find path using ONLY open/unconditional exits\n")
        (utils/tell "\nConditional exits (* in output) require doors, flags, or conditions.\n"))

    (= 1 (count args))
    (let [goal (parse-keyword (first args))
          start (:here game-state)]
      (if-let [_goal-room (get-in game-state [:rooms goal])]
        (if-let [path (find-path game-state start goal :allow-conditional? true)]
          (show-path-result game-state start goal path true)
          (utils/tell game-state (str "No path found from " (name start)
                                       " to " (name goal) "\n")))
        (utils/tell game-state (str "Unknown room: " goal "\n"))))

    :else
    (let [from-arg (first args)
          start (if (= from-arg "here")
                  (:here game-state)
                  (parse-keyword from-arg))
          goal (parse-keyword (second args))]
      (cond
        (nil? (get-in game-state [:rooms start]))
        (utils/tell game-state (str "Unknown room: " start "\n"))

        (nil? (get-in game-state [:rooms goal]))
        (utils/tell game-state (str "Unknown room: " goal "\n"))

        :else
        (if-let [path (find-path game-state start goal :allow-conditional? true)]
          (show-path-result game-state start goal path true)
          (utils/tell game-state (str "No path found from " (name start)
                                       " to " (name goal) "\n")))))))

(defn cmd-path-strict
  "Find shortest path using ONLY unconditional exits.

   Usage:
     $path! <to>           - Path from here (unconditional only)
     $path! <from> <to>    - Path between two rooms (unconditional only)

   This excludes exits that require doors, flags, or special conditions."
  [game-state args]
  (cond
    (empty? args)
    (-> game-state
        (utils/tell "Usage: $path! <destination>\n")
        (utils/tell "       $path! <from> <to>\n")
        (utils/tell "\nFinds path using ONLY unconditional/open exits.\n"))

    (= 1 (count args))
    (let [goal (parse-keyword (first args))
          start (:here game-state)]
      (if-let [_goal-room (get-in game-state [:rooms goal])]
        (if-let [path (find-path game-state start goal :allow-conditional? false)]
          (show-path-result game-state start goal path false)
          (utils/tell game-state (str "No unconditional path from " (name start)
                                       " to " (name goal) "\n")))
        (utils/tell game-state (str "Unknown room: " goal "\n"))))

    :else
    (let [from-arg (first args)
          start (if (= from-arg "here")
                  (:here game-state)
                  (parse-keyword from-arg))
          goal (parse-keyword (second args))]
      (cond
        (nil? (get-in game-state [:rooms start]))
        (utils/tell game-state (str "Unknown room: " start "\n"))

        (nil? (get-in game-state [:rooms goal]))
        (utils/tell game-state (str "Unknown room: " goal "\n"))

        :else
        (if-let [path (find-path game-state start goal :allow-conditional? false)]
          (show-path-result game-state start goal path false)
          (utils/tell game-state (str "No unconditional path from " (name start)
                                       " to " (name goal) "\n")))))))

(defn cmd-reachable
  "Show all rooms reachable from a location.

   Usage:
     $reachable        - Rooms reachable from current location
     $reachable <room> - Rooms reachable from specified room"
  [game-state args]
  (let [start (if (empty? args)
                (:here game-state)
                (parse-keyword (first args)))]
    (if-let [_start-room (get-in game-state [:rooms start])]
      (let [graph (build-graph game-state)
            reachable (loop [frontier (conj clojure.lang.PersistentQueue/EMPTY start)
                             visited #{start}]
                        (if (empty? frontier)
                          visited
                          (let [current (peek frontier)
                                frontier' (pop frontier)
                                neighbors (get graph current [])
                                new-rooms (filter #(not (visited (:to %))) neighbors)]
                            (recur
                             (reduce #(conj %1 (:to %2)) frontier' new-rooms)
                             (reduce #(conj %1 (:to %2)) visited new-rooms)))))
            sorted-rooms (sort (map name reachable))]
        (-> game-state
            (utils/tell (str "Rooms reachable from " (name start)
                             " (" (count reachable) " total):\n"))
            (utils/tell (str "  " (str/join ", " sorted-rooms) "\n"))))
      (utils/tell game-state (str "Unknown room: " start "\n")))))

(defn cmd-route
  "Plan a multi-stop route through several locations.

   Usage:
     $route <room1> <room2> <room3> ...

   Shows the optimal path visiting all rooms in order."
  [game-state args]
  (if (< (count args) 2)
    (utils/tell game-state "Usage: $route <room1> <room2> [room3 ...]\n")
    (let [rooms (mapv parse-keyword args)
          invalid (filter #(nil? (get-in game-state [:rooms %])) rooms)]
      (if (seq invalid)
        (utils/tell game-state (str "Unknown rooms: " (str/join ", " (map name invalid)) "\n"))
        (loop [current (first rooms)
               remaining (rest rooms)
               total-path []
               total-commands []]
          (if (empty? remaining)
            (-> game-state
                (utils/tell (str "Route through " (count rooms) " rooms "
                                 "(" (count total-commands) " moves):\n"))
                (utils/tell (str "  Commands: " (str/join " " total-commands) "\n"))
                (utils/tell (str "  Stops: " (str/join " -> " (map name rooms)) "\n")))
            (let [next-room (first remaining)]
              (if-let [path (find-path game-state current next-room)]
                (recur next-room
                       (rest remaining)
                       (concat total-path path)
                       (concat total-commands (path-to-commands path)))
                (utils/tell game-state
                            (str "No path from " (name current)
                                 " to " (name next-room) "\n"))))))))))
