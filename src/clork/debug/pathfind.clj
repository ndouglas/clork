(ns clork.debug.pathfind
  "Pathfinding debug commands using the intel/routing engine.

   Usage:
   - $path <from> <to>     - Find shortest path between two rooms
   - $path here <to>       - Find path from current location
   - $path <to>            - Shorthand for $path here <to>
   - $reachable            - Show all rooms reachable from current location
   - $reachable <room>     - Show all rooms reachable from specified room
   - $route <room1> <room2> ... - Plan multi-stop route"
  (:require [clork.utils :as utils]
            [clork.game-state :as gs]
            [clork.intel.routing :as routing]
            [clojure.string :as str]))

;;; ---------------------------------------------------------------------------
;;; HELPER FUNCTIONS
;;; ---------------------------------------------------------------------------

(def direction-names
  "Direction keywords to command strings."
  {:north "n" :south "s" :east "e" :west "w"
   :ne "ne" :nw "nw" :se "se" :sw "sw"
   :up "u" :down "d" :in "in" :out "out"
   :enter "enter" :exit "exit" :land "land"
   :northeast "ne" :northwest "nw" :southeast "se" :southwest "sw"
   :pray "pray" :downstream "wait"})

(defn- parse-keyword [s]
  (when s
    (if (str/starts-with? s ":")
      (keyword (subs s 1))
      (keyword s))))

(defn- via-to-command
  "Convert a :via value to a command string."
  [via]
  (cond
    (keyword? via) (get direction-names via (name via))
    (map? via) (str (name (:verb via)) " " (name (:object via)))
    :else "?"))

(defn- format-path-step
  "Format a single path step."
  [from to via]
  (str " -" (via-to-command via) "-> " (name to)))

;;; ---------------------------------------------------------------------------
;;; DEBUG COMMANDS
;;; ---------------------------------------------------------------------------

(defn cmd-path
  "Find shortest path between two rooms.

   Usage:
     $path <to>              - Path from here to destination
     $path <from> <to>       - Path between two rooms
     $path! <to>             - Path using only currently-available routes

   Uses the routing engine which includes:
   - Flag-gated passages (when flags are set)
   - Door-gated passages (when doors are open)
   - Teleports (prayer, mirror)
   - Boat/river navigation (when boat is ready)

   By default, shows path assuming ALL conditions are met.
   Use $path! to see path with ONLY current game state."
  [game-state args]
  (cond
    (empty? args)
    (-> game-state
        (utils/tell "Usage: $path <destination>   - find path (all conditions available)\n")
        (utils/tell "       $path <from> <to>     - path between two rooms\n")
        (utils/tell "       $path! <destination>  - path using ONLY current flags/doors\n")
        (utils/tell "\nPath includes special edges: teleports, boat routes, flag-gated passages.\n"))

    (= 1 (count args))
    (let [goal (parse-keyword (first args))
          start (:here game-state)]
      (if-let [_goal-room (get-in game-state [:rooms goal])]
        ;; Use ALL flags for "best possible" path
        (let [all-flags #{:troll-flag :cyclops-flag :magic-flag :lld-flag
                          :rainbow-flag :dome-flag :low-tide :coffin-cure
                          :trap-door-open :grate-open :kitchen-window-open
                          :boat-ready :empty-handed}
              result (routing/shortest-path game-state start goal
                                            :available-flags all-flags)]
          (if result
            (let [graph (routing/build-navigation-graph game-state :available-flags all-flags)
                  path-pairs (partition 2 1 (:path result))]
              (-> game-state
                  (utils/tell (str "Path from " (name start) " to " (name goal)
                                   " (" (:distance result) " moves):\n"))
                  (utils/tell (str "  Route: " (name start)))
                  ((fn [gs]
                     (reduce (fn [g [from to]]
                               (let [edge (first (filter #(and (= (:from %) from)
                                                               (= (:to %) to))
                                                         (:edges graph)))]
                                 (utils/tell g (format-path-step from to (:via edge)))))
                             gs path-pairs)))
                  (utils/tell "\n")))
            (utils/tell game-state (str "No path found from " (name start)
                                         " to " (name goal) "\n"))))
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
        (let [all-flags #{:troll-flag :cyclops-flag :magic-flag :lld-flag
                          :rainbow-flag :dome-flag :low-tide :coffin-cure
                          :trap-door-open :grate-open :kitchen-window-open
                          :boat-ready :empty-handed}
              result (routing/shortest-path game-state start goal
                                            :available-flags all-flags)]
          (if result
            (let [graph (routing/build-navigation-graph game-state :available-flags all-flags)
                  path-pairs (partition 2 1 (:path result))]
              (-> game-state
                  (utils/tell (str "Path from " (name start) " to " (name goal)
                                   " (" (:distance result) " moves):\n"))
                  (utils/tell (str "  Route: " (name start)))
                  ((fn [gs]
                     (reduce (fn [g [from to]]
                               (let [edge (first (filter #(and (= (:from %) from)
                                                               (= (:to %) to))
                                                         (:edges graph)))]
                                 (utils/tell g (format-path-step from to (:via edge)))))
                             gs path-pairs)))
                  (utils/tell "\n")))
            (utils/tell game-state (str "No path found from " (name start)
                                         " to " (name goal) "\n"))))))))

(defn cmd-path-strict
  "Find shortest path using ONLY currently-available routes.

   Usage:
     $path! <to>           - Path from here (current flags only)
     $path! <from> <to>    - Path between two rooms (current flags only)

   This uses the actual current game state - only passages that
   are currently accessible (doors open, flags set, etc.)."
  [game-state args]
  (cond
    (empty? args)
    (-> game-state
        (utils/tell "Usage: $path! <destination>\n")
        (utils/tell "       $path! <from> <to>\n")
        (utils/tell "\nUses ONLY currently available routes (doors open, flags set).\n"))

    (= 1 (count args))
    (let [goal (parse-keyword (first args))
          start (:here game-state)]
      (if-let [_goal-room (get-in game-state [:rooms goal])]
        (let [current-flags (routing/extract-available-flags game-state)
              result (routing/shortest-path game-state start goal
                                            :available-flags current-flags)]
          (if result
            (let [graph (routing/build-navigation-graph game-state :available-flags current-flags)
                  path-pairs (partition 2 1 (:path result))]
              (-> game-state
                  (utils/tell (str "Path from " (name start) " to " (name goal)
                                   " (" (:distance result) " moves) [current state]:\n"))
                  (utils/tell (str "  Available flags: " (str/join ", " (map name current-flags)) "\n"))
                  (utils/tell (str "  Route: " (name start)))
                  ((fn [gs]
                     (reduce (fn [g [from to]]
                               (let [edge (first (filter #(and (= (:from %) from)
                                                               (= (:to %) to))
                                                         (:edges graph)))]
                                 (utils/tell g (format-path-step from to (:via edge)))))
                             gs path-pairs)))
                  (utils/tell "\n")))
            (utils/tell game-state (str "No path found from " (name start)
                                         " to " (name goal)
                                         " with current flags.\n"))))
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
        (let [current-flags (routing/extract-available-flags game-state)
              result (routing/shortest-path game-state start goal
                                            :available-flags current-flags)]
          (if result
            (let [graph (routing/build-navigation-graph game-state :available-flags current-flags)
                  path-pairs (partition 2 1 (:path result))]
              (-> game-state
                  (utils/tell (str "Path from " (name start) " to " (name goal)
                                   " (" (:distance result) " moves) [current state]:\n"))
                  (utils/tell (str "  Available flags: " (str/join ", " (map name current-flags)) "\n"))
                  (utils/tell (str "  Route: " (name start)))
                  ((fn [gs]
                     (reduce (fn [g [from to]]
                               (let [edge (first (filter #(and (= (:from %) from)
                                                               (= (:to %) to))
                                                         (:edges graph)))]
                                 (utils/tell g (format-path-step from to (:via edge)))))
                             gs path-pairs)))
                  (utils/tell "\n")))
            (utils/tell game-state (str "No path found from " (name start)
                                         " to " (name goal)
                                         " with current flags.\n"))))))))

(defn cmd-reachable
  "Show all rooms reachable from a location.

   Usage:
     $reachable         - Rooms reachable from current location
     $reachable <room>  - Rooms reachable from specified room
     $reachable! <room> - Reachable with ALL flags (best possible)"
  [game-state args]
  (let [;; Check if using ! variant for all-flags mode
        all-flags-mode? (and (seq args) (str/ends-with? (first args) "!"))
        clean-args (if all-flags-mode?
                     (cons (str/replace (first args) "!" "") (rest args))
                     args)
        start (if (or (empty? clean-args) (= (first clean-args) ""))
                (:here game-state)
                (parse-keyword (first clean-args)))
        flags (if all-flags-mode?
                #{:troll-flag :cyclops-flag :magic-flag :lld-flag
                  :rainbow-flag :dome-flag :low-tide :coffin-cure
                  :trap-door-open :grate-open :kitchen-window-open
                  :boat-ready :empty-handed}
                (routing/extract-available-flags game-state))]
    (if-let [_start-room (get-in game-state [:rooms start])]
      (let [reachable (routing/reachable-from game-state start :available-flags flags)
            sorted-rooms (sort (map name reachable))]
        (-> game-state
            (utils/tell (str "Rooms reachable from " (name start)
                             " (" (count reachable) " total)"
                             (when all-flags-mode? " [all flags]")
                             ":\n"))
            (utils/tell (str "  " (str/join ", " sorted-rooms) "\n"))))
      (utils/tell game-state (str "Unknown room: " start "\n")))))

(defn cmd-route
  "Plan a multi-stop route through several locations.

   Usage:
     $route <room1> <room2> <room3> ...

   Shows the optimal path visiting all rooms in order.
   Assumes all conditions are met (use $route! for current state only)."
  [game-state args]
  (if (< (count args) 2)
    (utils/tell game-state "Usage: $route <room1> <room2> [room3 ...]\n")
    (let [rooms (mapv parse-keyword args)
          invalid (filter #(nil? (get-in game-state [:rooms %])) rooms)]
      (if (seq invalid)
        (utils/tell game-state (str "Unknown rooms: " (str/join ", " (map name invalid)) "\n"))
        (let [all-flags #{:troll-flag :cyclops-flag :magic-flag :lld-flag
                          :rainbow-flag :dome-flag :low-tide :coffin-cure
                          :trap-door-open :grate-open :kitchen-window-open
                          :boat-ready :empty-handed}]
          (loop [current (first rooms)
                 remaining (rest rooms)
                 total-distance 0
                 all-commands []]
            (if (empty? remaining)
              (-> game-state
                  (utils/tell (str "Route through " (count rooms) " rooms "
                                   "(" total-distance " moves):\n"))
                  (utils/tell (str "  Stops: " (str/join " -> " (map name rooms)) "\n"))
                  (utils/tell (str "  Commands: " (str/join " " all-commands) "\n")))
              (let [next-room (first remaining)
                    result (routing/route-to
                             (assoc game-state :here current)
                             next-room
                             :available-flags all-flags)]
                (if result
                  (recur next-room
                         (rest remaining)
                         (+ total-distance (:distance result))
                         (concat all-commands (:commands result)))
                  (utils/tell game-state
                              (str "No path from " (name current)
                                   " to " (name next-room) "\n")))))))))))

(defn cmd-distance
  "Show distance between two rooms.

   Usage:
     $distance <from> <to>  - Distance in moves
     $distance <to>         - Distance from current location"
  [game-state args]
  (cond
    (empty? args)
    (utils/tell game-state "Usage: $distance <destination> or $distance <from> <to>\n")

    (= 1 (count args))
    (let [goal (parse-keyword (first args))
          start (:here game-state)]
      (if-let [_goal-room (get-in game-state [:rooms goal])]
        (let [all-flags #{:troll-flag :cyclops-flag :magic-flag :lld-flag
                          :rainbow-flag :dome-flag :low-tide :coffin-cure
                          :trap-door-open :grate-open :kitchen-window-open
                          :boat-ready :empty-handed}
              dist (routing/distance-between game-state start goal
                                             :available-flags all-flags)]
          (if dist
            (utils/tell game-state (str "Distance from " (name start)
                                         " to " (name goal) ": " dist " moves\n"))
            (utils/tell game-state (str (name goal) " is not reachable from "
                                         (name start) "\n"))))
        (utils/tell game-state (str "Unknown room: " goal "\n"))))

    :else
    (let [start (parse-keyword (first args))
          goal (parse-keyword (second args))]
      (cond
        (nil? (get-in game-state [:rooms start]))
        (utils/tell game-state (str "Unknown room: " start "\n"))

        (nil? (get-in game-state [:rooms goal]))
        (utils/tell game-state (str "Unknown room: " goal "\n"))

        :else
        (let [all-flags #{:troll-flag :cyclops-flag :magic-flag :lld-flag
                          :rainbow-flag :dome-flag :low-tide :coffin-cure
                          :trap-door-open :grate-open :kitchen-window-open
                          :boat-ready :empty-handed}
              dist (routing/distance-between game-state start goal
                                             :available-flags all-flags)]
          (if dist
            (utils/tell game-state (str "Distance from " (name start)
                                         " to " (name goal) ": " dist " moves\n"))
            (utils/tell game-state (str (name goal) " is not reachable from "
                                         (name start) "\n"))))))))
