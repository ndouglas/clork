(ns clork.debug.manipulation
  "State manipulation debug commands (wizard mode).

   Commands:
   - $goto <room>      - Teleport to any room
   - $purloin <obj>    - Take any object (bypass checks)
   - $move <obj> <dest> - Move object to location
   - $flag <id> <flag> - Set a flag on object/room
   - $unflag <id> <flag> - Clear a flag
   - $frotz <obj>      - Make object give light
   - $seed <n>         - Seed the RNG for reproducible gameplay"
  (:require [clork.utils :as utils]
            [clork.game-state :as gs]
            [clork.flags :as flags]
            [clojure.string :as str]
            [clork.verbs-look :as verbs-look]
            [clork.random :as random]))

;;; ---------------------------------------------------------------------------
;;; HELPERS
;;; ---------------------------------------------------------------------------

(defn- parse-keyword
  "Parse a string to a keyword."
  [s]
  (when s
    (if (str/starts-with? s ":")
      (keyword (subs s 1))
      (keyword s))))

(defn- tell-action
  "Output an action message in wizard style."
  [game-state msg]
  (utils/tell game-state (str "*" msg "*\n")))

;;; ---------------------------------------------------------------------------
;;; $goto <room>
;;; ---------------------------------------------------------------------------

(defn cmd-goto
  "Teleport to any room."
  [game-state args]
  (if (empty? args)
    (-> game-state
        (utils/tell "Usage: $goto <room-id>\n")
        (utils/tell "Rooms: ")
        (utils/tell (str/join ", " (sort (map name (keys (:rooms game-state))))))
        (utils/tell "\n"))
    (let [room-id (parse-keyword (first args))
          room (get-in game-state [:rooms room-id])
          winner (:winner game-state)]
      (if room
        (let [gs (-> game-state
                     ;; Move the player object to the new room (tracked)
                     (gs/move-object winner room-id :debug-goto)
                     ;; Update HERE (tracked)
                     (gs/set-location room-id :debug-goto)
                     ;; Update LIT flag based on room and carried light sources
                     (as-> gs
                           (let [room-lit (contains? (or (:flags room) #{}) :lit)
                                 contents (gs/get-contents gs winner)
                                 has-light-on (some (fn [obj-id]
                                                      (and (gs/set-thing-flag? gs obj-id :light)
                                                           (gs/set-thing-flag? gs obj-id :on)))
                                                    contents)]
                             (assoc gs :lit (or room-lit has-light-on))))
                     (tell-action (str "Teleported to " room-id " (" (:desc room) ")")))
              ;; Call room action with M-ENTER if it exists
              ;; This ensures special behaviors like loud-room-mode are triggered
              room-action (:action room)
              gs (if room-action
                   (let [result (room-action gs :m-enter)]
                     (if (gs/use-default? result)
                       (gs/clear-use-default result)
                       result))
                   gs)]
          ;; Show room description
          (verbs-look/v-first-look gs))
        (utils/tell game-state (str "Unknown room: " room-id "\n"))))))

;;; ---------------------------------------------------------------------------
;;; $purloin <obj>
;;; ---------------------------------------------------------------------------

(defn cmd-purloin
  "Take any object, bypassing all accessibility checks."
  [game-state args]
  (if (empty? args)
    (-> game-state
        (utils/tell "Usage: $purloin <object-id>\n")
        (utils/tell "Objects: ")
        (utils/tell (str/join ", " (sort (map name (keys (:objects game-state))))))
        (utils/tell "\n"))
    (let [obj-id (parse-keyword (first args))
          obj (get-in game-state [:objects obj-id])
          winner (:winner game-state)]
      (if obj
        (-> game-state
            (gs/move-object obj-id winner :debug-purloin)
            (tell-action (str "Purloined " obj-id " (" (:desc obj) ")")))
        (utils/tell game-state (str "Unknown object: " obj-id "\n"))))))

;;; ---------------------------------------------------------------------------
;;; $move <obj> <dest>
;;; ---------------------------------------------------------------------------

(defn cmd-move
  "Move an object to any location."
  [game-state args]
  (cond
    (< (count args) 2)
    (utils/tell game-state "Usage: $move <object-id> <destination-id>\n")

    :else
    (let [obj-id (parse-keyword (first args))
          dest-id (parse-keyword (second args))
          obj (get-in game-state [:objects obj-id])
          dest (gs/get-thing game-state dest-id)]
      (cond
        (nil? obj)
        (utils/tell game-state (str "Unknown object: " obj-id "\n"))

        (nil? dest)
        (utils/tell game-state (str "Unknown destination: " dest-id "\n"))

        :else
        (-> game-state
            (gs/move-object obj-id dest-id :debug-move)
            (tell-action (str "Moved " obj-id " to " dest-id)))))))

;;; ---------------------------------------------------------------------------
;;; $flag <id> <flag>
;;; ---------------------------------------------------------------------------

(defn cmd-flag
  "Set a flag on an object or room."
  [game-state args]
  (cond
    (< (count args) 2)
    (-> game-state
        (utils/tell "Usage: $flag <thing-id> <flag-name>\n")
        (utils/tell "Known flags: ")
        (utils/tell (str/join ", " (sort (map name (keys flags/flag-names)))))
        (utils/tell "\n"))

    :else
    (let [thing-id (parse-keyword (first args))
          flag-name (parse-keyword (second args))
          thing (gs/get-thing game-state thing-id)]
      (cond
        (nil? thing)
        (utils/tell game-state (str "Unknown thing: " thing-id "\n"))

        (nil? (get flags/flag-names flag-name))
        (utils/tell game-state (str "Unknown flag: " flag-name "\n"))

        :else
        (-> game-state
            (gs/set-thing-flag thing-id flag-name)
            (tell-action (str "Set " flag-name " on " thing-id)))))))

;;; ---------------------------------------------------------------------------
;;; $unflag <id> <flag>
;;; ---------------------------------------------------------------------------

(defn cmd-unflag
  "Clear a flag on an object or room."
  [game-state args]
  (cond
    (< (count args) 2)
    (-> game-state
        (utils/tell "Usage: $unflag <thing-id> <flag-name>\n")
        (utils/tell "Known flags: ")
        (utils/tell (str/join ", " (sort (map name (keys flags/flag-names)))))
        (utils/tell "\n"))

    :else
    (let [thing-id (parse-keyword (first args))
          flag-name (parse-keyword (second args))
          thing (gs/get-thing game-state thing-id)]
      (cond
        (nil? thing)
        (utils/tell game-state (str "Unknown thing: " thing-id "\n"))

        (nil? (get flags/flag-names flag-name))
        (utils/tell game-state (str "Unknown flag: " flag-name "\n"))

        :else
        (-> game-state
            (gs/unset-thing-flag thing-id flag-name)
            (tell-action (str "Cleared " flag-name " on " thing-id)))))))

;;; ---------------------------------------------------------------------------
;;; $frotz <obj>
;;; ---------------------------------------------------------------------------

(defn cmd-frotz
  "Make an object give light. Classic Infocom wizard command."
  [game-state args]
  (if (empty? args)
    (utils/tell game-state "Usage: $frotz <object-id>\n")
    (let [obj-id (parse-keyword (first args))
          obj (get-in game-state [:objects obj-id])]
      (if obj
        (-> game-state
            (gs/set-thing-flag obj-id :light)
            (gs/set-thing-flag obj-id :on)
            (tell-action (str "Frotzing " obj-id " - it now glows!")))
        (utils/tell game-state (str "Unknown object: " obj-id "\n"))))))

;;; ---------------------------------------------------------------------------
;;; $where <obj>
;;; ---------------------------------------------------------------------------

(defn- get-location-chain
  "Get the chain of containers for an object up to a room."
  [game-state obj-id]
  (loop [current obj-id
         chain []]
    (let [thing (gs/get-thing game-state current)
          location (:in thing)]
      (if (or (nil? location)
              (get-in game-state [:rooms current]))
        ;; We've reached a room or no location
        (if (get-in game-state [:rooms current])
          (conj chain current)
          chain)
        ;; Keep going up
        (recur location (conj chain location))))))

(defn cmd-where
  "Find where an object is located, showing the full containment chain."
  [game-state args]
  (if (empty? args)
    (-> game-state
        (utils/tell "Usage: $where <object-id>\n")
        (utils/tell "Find where any object is located in the game world.\n"))
    (let [obj-id (parse-keyword (first args))
          obj (get-in game-state [:objects obj-id])]
      (if obj
        (let [location (:in obj)
              chain (get-location-chain game-state obj-id)
              desc (:desc obj "?")]
          (-> game-state
              (utils/tell (str obj-id " (" desc ")\n"))
              (utils/tell (str "  Location: " (or location "nowhere") "\n"))
              (utils/tell (str "  Chain: " (str/join " -> " (map name chain)) "\n"))))
        ;; Maybe it's a room?
        (if-let [room (get-in game-state [:rooms obj-id])]
          (utils/tell game-state (str obj-id " is a ROOM: " (:desc room) "\n"))
          (utils/tell game-state (str "Unknown object: " obj-id "\n")))))))

;;; ---------------------------------------------------------------------------
;;; $seed <n>
;;; ---------------------------------------------------------------------------

(defn cmd-seed
  "Seed the random number generator for reproducible gameplay."
  [game-state args]
  (if (empty? args)
    (let [seed-info (random/get-seed-info)]
      (-> game-state
          (utils/tell "Current RNG state:\n")
          (utils/tell (str "  Seed: " (:seed seed-info) "\n"))
          (utils/tell (str "  Calls: " (:call-count seed-info) "\n"))
          (utils/tell "Usage: $seed <number> - Set seed for reproducible randomness\n")))
    (let [seed (try (Long/parseLong (first args))
                    (catch NumberFormatException _ nil))]
      (if seed
        (do
          (random/init! seed)
          (tell-action game-state (str "RNG seeded with " seed " - randomness is now reproducible")))
        (utils/tell game-state "Invalid seed. Please provide a number.\n")))))

;;; ---------------------------------------------------------------------------
;;; COMMAND REGISTRATION
;;; ---------------------------------------------------------------------------
;;; These are registered directly in debug.clj as top-level $ commands.
