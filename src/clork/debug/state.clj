(ns clork.debug.state
  "State inspection debug commands.

   Commands:
   - $debug state  - High-level game state overview
   - $debug here   - Current room in detail
   - $debug object - Object details
   - $debug room   - Room details
   - $debug tree   - Object containment hierarchy
   - $debug flags  - Flags on object/room
   - $debug find   - Search by name/synonym"
  (:require [clork.utils :as utils]
            [clork.game-state :as gs]
            [clork.flags :as flags]
            [clojure.string :as str]
            [clojure.pprint :as pp]))

;;; ---------------------------------------------------------------------------
;;; HELPERS
;;; ---------------------------------------------------------------------------

(defn- format-keyword
  "Format a keyword for display, removing the leading colon."
  [kw]
  (if (keyword? kw)
    (name kw)
    (str kw)))

(defn- format-flags
  "Format a set of flags for display."
  [thing]
  (let [flag-keys (filter #(and (keyword? %)
                                (true? (get thing %)))
                          (keys thing))]
    (if (empty? flag-keys)
      "none"
      (str/join ", " (map format-keyword (sort flag-keys))))))

(defn- tell-line
  "Output a line with key-value formatting."
  [game-state label value]
  (utils/tell game-state (str "  " label ": " value "\n")))

(defn- parse-thing-id
  "Parse a thing ID from string argument. Returns keyword."
  [s]
  (when s
    (if (str/starts-with? s ":")
      (keyword (subs s 1))
      (keyword s))))

;;; ---------------------------------------------------------------------------
;;; $debug state
;;; ---------------------------------------------------------------------------

(defn cmd-debug-state
  "Show high-level game state."
  [game-state _args]
  (let [here-id (:here game-state)
        here (gs/get-thing game-state here-id)
        winner-id (:winner game-state)
        turn (:turn-number game-state 0)
        lit? (:lit game-state)
        verbose? (:verbose game-state)
        super-brief? (:super-brief game-state)
        object-count (count (:objects game-state))
        room-count (count (:rooms game-state))]
    (-> game-state
        (utils/tell "Game State:\n")
        (tell-line "Turn" turn)
        (tell-line "Location" (str here-id " (" (:desc here "?") ")"))
        (tell-line "Player" (:player game-state))
        (tell-line "Winner" winner-id)
        (tell-line "Lit" lit?)
        (tell-line "Verbose" verbose?)
        (tell-line "Super-brief" super-brief?)
        (tell-line "Objects" (str object-count " defined"))
        (tell-line "Rooms" (str room-count " defined")))))

;;; ---------------------------------------------------------------------------
;;; $debug here
;;; ---------------------------------------------------------------------------

(defn cmd-debug-here
  "Show current room in detail."
  [game-state _args]
  (let [here-id (:here game-state)
        room (gs/get-thing game-state here-id)
        contents (gs/get-contents game-state here-id)]
    (-> game-state
        (utils/tell (str "Current Room: " here-id "\n"))
        (tell-line "Description" (:desc room "?"))
        (tell-line "Flags" (format-flags room))
        (tell-line "Contents" (if (empty? contents)
                                "empty"
                                (str/join ", " (map format-keyword contents))))
        ;; Show any other interesting room properties
        ((fn [gs]
           (reduce (fn [gs [k v]]
                     (if (and (keyword? k)
                              (not (#{:id :desc :action :flags} k))
                              (not (true? v))
                              (not (false? v))
                              (not (nil? v)))
                       (tell-line gs (format-keyword k) (str v))
                       gs))
                   gs
                   room))))))

;;; ---------------------------------------------------------------------------
;;; $debug object <id>
;;; ---------------------------------------------------------------------------

(defn cmd-debug-object
  "Show object details."
  [game-state args]
  (if (empty? args)
    (utils/tell game-state "Usage: $debug object <id>\n")
    (let [obj-id (parse-thing-id (first args))
          obj (get-in game-state [:objects obj-id])]
      (if obj
        (-> game-state
            (utils/tell (str "Object: " obj-id "\n"))
            (tell-line "Description" (:desc obj "?"))
            (tell-line "Location" (or (:in obj) "nowhere"))
            (tell-line "Synonyms" (str/join ", " (or (:synonym obj) [])))
            (tell-line "Adjective" (or (:adjective obj) "none"))
            (tell-line "Flags" (format-flags obj))
            ;; Contents if container
            ((fn [gs]
               (let [contents (gs/get-contents gs obj-id)]
                 (if (not-empty contents)
                   (tell-line gs "Contains" (str/join ", " (map format-keyword contents)))
                   gs))))
            ;; Other properties
            ((fn [gs]
               (reduce (fn [gs [k v]]
                         (if (and (keyword? k)
                                  (not (#{:id :desc :in :synonym :adjective :action :flags} k))
                                  (not (true? v))
                                  (not (false? v))
                                  (not (nil? v)))
                           (tell-line gs (format-keyword k) (str v))
                           gs))
                       gs
                       obj))))
        (utils/tell game-state (str "Unknown object: " obj-id "\n"))))))

;;; ---------------------------------------------------------------------------
;;; $debug room <id>
;;; ---------------------------------------------------------------------------

(defn cmd-debug-room
  "Show room details."
  [game-state args]
  (if (empty? args)
    (utils/tell game-state "Usage: $debug room <id>\n")
    (let [room-id (parse-thing-id (first args))
          room (get-in game-state [:rooms room-id])]
      (if room
        (-> game-state
            (utils/tell (str "Room: " room-id "\n"))
            (tell-line "Description" (:desc room "?"))
            (tell-line "Flags" (format-flags room))
            (tell-line "Contents" (let [contents (gs/get-contents game-state room-id)]
                                    (if (empty? contents)
                                      "empty"
                                      (str/join ", " (map format-keyword contents)))))
            ;; Other properties
            ((fn [gs]
               (reduce (fn [gs [k v]]
                         (if (and (keyword? k)
                                  (not (#{:id :desc :action :flags} k))
                                  (not (true? v))
                                  (not (false? v))
                                  (not (nil? v)))
                           (tell-line gs (format-keyword k) (str v))
                           gs))
                       gs
                       room))))
        (utils/tell game-state (str "Unknown room: " room-id "\n"))))))

;;; ---------------------------------------------------------------------------
;;; $debug tree
;;; ---------------------------------------------------------------------------

(defn- tree-helper
  "Recursively build tree output."
  [game-state container-id depth max-depth]
  (if (>= depth max-depth)
    game-state
    (let [contents (gs/get-contents game-state container-id)
          indent (apply str (repeat (* depth 2) " "))]
      (reduce (fn [gs obj-id]
                (let [obj (gs/get-thing gs obj-id)
                      desc (or (:desc obj) (format-keyword obj-id))]
                  (-> gs
                      (utils/tell (str indent "- " obj-id " (" desc ")\n"))
                      (tree-helper obj-id (inc depth) max-depth))))
              game-state
              contents))))

(defn cmd-debug-tree
  "Show object containment hierarchy."
  [game-state args]
  (let [root (if (empty? args)
               (:here game-state)
               (parse-thing-id (first args)))
        thing (gs/get-thing game-state root)]
    (if thing
      (-> game-state
          (utils/tell (str "Containment tree from: " root "\n"))
          (tree-helper root 0 10))
      (utils/tell game-state (str "Unknown location: " root "\n")))))

;;; ---------------------------------------------------------------------------
;;; $debug flags <id>
;;; ---------------------------------------------------------------------------

(defn cmd-debug-flags
  "Show all flags on an object or room."
  [game-state args]
  (if (empty? args)
    (-> game-state
        (utils/tell "Usage: $debug flags <id>\n")
        (utils/tell "Known flags: ")
        (utils/tell (str/join ", " (map format-keyword (sort (keys flags/flags)))))
        (utils/tell "\n"))
    (let [thing-id (parse-thing-id (first args))
          thing (gs/get-thing game-state thing-id)]
      (if thing
        (let [set-flags (filter #(and (keyword? %)
                                      (true? (get thing %)))
                                (keys thing))
              unset-flags (filter #(and (keyword? %)
                                        (false? (get thing %)))
                                  (keys thing))]
          (-> game-state
              (utils/tell (str "Flags on " thing-id ":\n"))
              (tell-line "Set" (if (empty? set-flags)
                                 "none"
                                 (str/join ", " (map format-keyword (sort set-flags)))))
              (tell-line "Cleared" (if (empty? unset-flags)
                                     "none"
                                     (str/join ", " (map format-keyword (sort unset-flags)))))))
        (utils/tell game-state (str "Unknown thing: " thing-id "\n"))))))

;;; ---------------------------------------------------------------------------
;;; $debug find <name>
;;; ---------------------------------------------------------------------------

(defn- matches-name?
  "Check if thing matches search term."
  [thing term]
  (let [term-lower (str/lower-case term)
        id-str (format-keyword (:id thing))
        desc-lower (str/lower-case (or (:desc thing) ""))
        synonyms (map str/lower-case (or (:synonym thing) []))]
    (or (str/includes? id-str term-lower)
        (str/includes? desc-lower term-lower)
        (some #(str/includes? % term-lower) synonyms))))

(defn cmd-debug-find
  "Search for objects/rooms by name or synonym."
  [game-state args]
  (if (empty? args)
    (utils/tell game-state "Usage: $debug find <name>\n")
    (let [term (first args)
          matching-objects (->> (:objects game-state)
                                vals
                                (filter #(matches-name? % term))
                                (map :id))
          matching-rooms (->> (:rooms game-state)
                              vals
                              (filter #(matches-name? % term))
                              (map :id))]
      (-> game-state
          (utils/tell (str "Search results for \"" term "\":\n"))
          ((fn [gs]
             (if (empty? matching-objects)
               (utils/tell gs "  Objects: none\n")
               (utils/tell gs (str "  Objects: " (str/join ", " (map format-keyword matching-objects)) "\n")))))
          ((fn [gs]
             (if (empty? matching-rooms)
               (utils/tell gs "  Rooms: none\n")
               (utils/tell gs (str "  Rooms: " (str/join ", " (map format-keyword matching-rooms)) "\n")))))))))

;;; ---------------------------------------------------------------------------
;;; MAIN DEBUG DISPATCHER
;;; ---------------------------------------------------------------------------

(def subcommands
  {:state  {:handler cmd-debug-state  :help "High-level game state overview"}
   :here   {:handler cmd-debug-here   :help "Current room in detail"}
   :object {:handler cmd-debug-object :help "Show object details"}
   :room   {:handler cmd-debug-room   :help "Show room details"}
   :tree   {:handler cmd-debug-tree   :help "Object containment hierarchy"}
   :flags  {:handler cmd-debug-flags  :help "Show flags on object/room"}
   :find   {:handler cmd-debug-find   :help "Search by name/synonym"}})

(defn cmd-debug
  "Main $debug command dispatcher."
  [game-state args]
  (if (empty? args)
    ;; Show available subcommands
    (reduce (fn [gs [name info]]
              (utils/tell gs (str "  $debug " (format-keyword name) " - " (:help info) "\n")))
            (utils/tell game-state "$debug subcommands:\n")
            (sort-by first subcommands))
    ;; Dispatch to subcommand
    (let [subcmd (keyword (first args))
          sub-args (rest args)
          sub-info (get subcommands subcmd)]
      (if sub-info
        ((:handler sub-info) game-state sub-args)
        (utils/tell game-state (str "Unknown subcommand: " (first args) "\nType $debug for list.\n"))))))
