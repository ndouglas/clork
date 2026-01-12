(ns clork.planner2.observe
  "State observation layer for the reactive planner.

   Wraps clork.ml functions and adds additional queries needed for planning.
   All functions take game-state as first argument and return data, never modify state."
  (:require [clork.ml :as ml]
            [clork.game-state :as gs]))

;;; ---------------------------------------------------------------------------
;;; BASIC STATE QUERIES
;;; ---------------------------------------------------------------------------

(defn current-room
  "Get the current room ID."
  [game-state]
  (:here game-state))

(defn at-room?
  "Check if player is at a specific room."
  [game-state room-id]
  (= (:here game-state) room-id))

(defn inventory
  "Get set of item IDs in player's inventory (top-level only)."
  [game-state]
  (->> (ml/get-inventory game-state)
       (filter #(zero? (:depth %)))  ; Top-level only
       (map :id)
       set))

(defn inventory-all
  "Get set of ALL item IDs in player's inventory (including nested)."
  [game-state]
  (->> (ml/get-inventory game-state)
       (map :id)
       set))

(defn has-item?
  "Check if player has a specific item."
  [game-state item-id]
  (contains? (inventory-all game-state) item-id))

(defn visible-objects
  "Get set of object IDs visible in current room."
  [game-state]
  (->> (ml/get-visible-objects game-state)
       (map :id)
       set))

(defn object-visible?
  "Check if an object is visible in the current room."
  [game-state obj-id]
  (contains? (visible-objects game-state) obj-id))

;;; ---------------------------------------------------------------------------
;;; FLAG QUERIES
;;; ---------------------------------------------------------------------------

(defn flag-set?
  "Check if a game flag is set.

   Handles both:
   - Entity flags (on objects/rooms): :open, :on, :take, etc.
   - Global game state flags: :troll-flag, :cyclops-flag, etc."
  [game-state flag]
  ;; Check global game state first (e.g., :troll-flag is a top-level key)
  (or (get game-state flag)
      ;; Some flags are on the current room
      (gs/set-here-flag? game-state flag)))

(defn object-flag?
  "Check if a specific object has a flag set."
  [game-state obj-id flag]
  (gs/set-thing-flag? game-state obj-id flag))

(defn object-open?
  "Check if an object/container is open."
  [game-state obj-id]
  (gs/set-thing-flag? game-state obj-id :open))

(defn lantern-on?
  "Check if the lantern is turned on."
  [game-state]
  (gs/set-thing-flag? game-state :brass-lantern :on))

;;; ---------------------------------------------------------------------------
;;; LOCATION QUERIES
;;; ---------------------------------------------------------------------------

(defn object-location
  "Get the location of an object (room ID, container ID, or :adventurer for inventory).
   Returns nil if object doesn't exist."
  [game-state obj-id]
  (get-in game-state [:objects obj-id :in]))

(defn object-in-room?
  "Check if an object is in a specific room (directly or in a container there)."
  [game-state obj-id room-id]
  (loop [loc (object-location game-state obj-id)
         depth 0]
    (cond
      (nil? loc) false
      (= loc room-id) true
      (> depth 10) false  ; Prevent infinite loops
      (contains? (:rooms game-state) loc) false  ; In a different room
      :else (recur (object-location game-state loc) (inc depth)))))

(defn find-object-room
  "Find what room an object is in (traversing container hierarchy).
   Returns nil if in inventory or limbo."
  [game-state obj-id]
  (loop [loc (object-location game-state obj-id)
         depth 0]
    (cond
      (nil? loc) nil
      (= loc :limbo) nil
      (= loc :adventurer) nil  ; In inventory
      (= loc (:winner game-state)) nil  ; In inventory
      (contains? (:rooms game-state) loc) loc
      (> depth 10) nil
      :else (recur (object-location game-state loc) (inc depth)))))

;;; ---------------------------------------------------------------------------
;;; EXIT/NAVIGATION QUERIES
;;; ---------------------------------------------------------------------------

(defn available-exits
  "Get map of direction -> destination room from current room."
  [game-state]
  (ml/get-available-exits game-state))

(defn exit-available?
  "Check if a specific exit direction is available."
  [game-state direction]
  (contains? (available-exits game-state) direction))

(defn exit-destination
  "Get destination room for a direction, or nil if not available."
  [game-state direction]
  (get (available-exits game-state) direction))

;;; ---------------------------------------------------------------------------
;;; ACTION QUERIES
;;; ---------------------------------------------------------------------------

(defn valid-actions
  "Get all valid actions from current state."
  [game-state]
  (ml/valid-actions game-state))

(defn action-list
  "Get flat list of all valid actions as executable maps."
  [game-state]
  (ml/action-list game-state))

(defn can-take?
  "Check if an object can be taken from current location."
  [game-state obj-id]
  (let [actions (valid-actions game-state)
        obj-actions (get-in actions [:object-actions obj-id :verbs] [])]
    (some #{:take} obj-actions)))

(defn can-move?
  "Check if player can move in a direction."
  [game-state direction]
  (contains? (available-exits game-state) direction))

;;; ---------------------------------------------------------------------------
;;; GAME STATE QUERIES
;;; ---------------------------------------------------------------------------

(defn score
  "Get current score."
  [game-state]
  (:score game-state 0))

(defn moves
  "Get move count."
  [game-state]
  (:moves game-state 0))

(defn deaths
  "Get death count."
  [game-state]
  (:deaths game-state 0))

(defn player-alive?
  "Check if player is alive."
  [game-state]
  (and (not (:dead game-state))
       (< (:deaths game-state 0) 3)))

(defn game-won?
  "Check if player has won (all treasures deposited)."
  [game-state]
  (:won game-state false))

(defn game-finished?
  "Check if game is completely finished (entered barrow after winning)."
  [game-state]
  (:finished game-state false))

(defn game-over?
  "Check if game is in a terminal state."
  [game-state]
  (or (:quit game-state)
      (:finished game-state)
      (>= (:deaths game-state 0) 3)
      (:dead game-state)))

(defn lit?
  "Check if current room is lit."
  [game-state]
  (or (:lit game-state)
      (gs/set-here-flag? game-state :lit)))

;;; ---------------------------------------------------------------------------
;;; ENEMY QUERIES
;;; ---------------------------------------------------------------------------

(defn troll-dead?
  "Check if the troll has been killed."
  [game-state]
  (flag-set? game-state :troll-flag))

(defn cyclops-gone?
  "Check if the cyclops has been scared away."
  [game-state]
  (flag-set? game-state :cyclops-flag))

(defn thief-dead?
  "Check if the thief has been killed."
  [game-state]
  (flag-set? game-state :thief-dead))

(defn enemy-present?
  "Check if a specific enemy is in the current room."
  [game-state enemy-id]
  (let [enemy-loc (object-location game-state enemy-id)]
    (= enemy-loc (:here game-state))))

(defn troll-present?
  "Check if the troll is in the current room and alive."
  [game-state]
  (and (not (troll-dead? game-state))
       (enemy-present? game-state :troll)))

(defn thief-present?
  "Check if the thief is in the current room and alive."
  [game-state]
  (and (not (thief-dead? game-state))
       (enemy-present? game-state :thief)))

;;; ---------------------------------------------------------------------------
;;; CONTAINER QUERIES
;;; ---------------------------------------------------------------------------

(defn container-contents
  "Get set of object IDs directly inside a container."
  [game-state container-id]
  (->> (gs/get-contents game-state container-id)
       set))

(defn trophy-case-contents
  "Get set of items in the trophy case."
  [game-state]
  (container-contents game-state :trophy-case))

(defn item-deposited?
  "Check if a treasure is in the trophy case."
  [game-state treasure-id]
  (contains? (trophy-case-contents game-state) treasure-id))

;;; ---------------------------------------------------------------------------
;;; STATE SNAPSHOT FOR DEBUGGING
;;; ---------------------------------------------------------------------------

(defn snapshot
  "Get a complete snapshot of relevant game state for debugging.
   Returns a map with all key state information."
  [game-state]
  {:room (current-room game-state)
   :inventory (inventory game-state)
   :score (score game-state)
   :moves (moves game-state)
   :deaths (deaths game-state)
   :lit? (lit? game-state)
   :game-won? (game-won? game-state)
   :game-finished? (game-finished? game-state)
   :available-exits (available-exits game-state)
   :visible-objects (visible-objects game-state)
   :troll-dead? (troll-dead? game-state)
   :cyclops-gone? (cyclops-gone? game-state)
   :thief-dead? (thief-dead? game-state)
   :lantern-on? (lantern-on? game-state)})

(defn print-snapshot
  "Print a formatted snapshot of game state."
  [game-state]
  (let [s (snapshot game-state)]
    (println "\n=== Game State ===")
    (println "  Room:" (:room s))
    (println "  Inventory:" (or (seq (:inventory s)) "(empty)"))
    (println "  Score:" (:score s) "| Moves:" (:moves s) "| Deaths:" (:deaths s))
    (println "  Lit:" (:lit? s) "| Won:" (:game-won? s) "| Finished:" (:game-finished? s))
    (println "  Exits:" (keys (:available-exits s)))
    (println "  Visible:" (or (seq (:visible-objects s)) "(none)"))
    (println "  Troll:" (if (:troll-dead? s) "dead" "alive")
             "| Cyclops:" (if (:cyclops-gone? s) "gone" "present")
             "| Thief:" (if (:thief-dead? s) "dead" "alive"))))
