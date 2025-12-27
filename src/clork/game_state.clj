(ns clork.game-state
  "Game state management and flag operations."
  (:require [clork.parser.state :as parser-state]))

;;; ---------------------------------------------------------------------------
;;; PARSER CONSTANTS
;;; ---------------------------------------------------------------------------
;;; These are defined here (rather than in parser/) so they're available
;;; to verb_defs.clj which is loaded before the parser.

(def search-bits
  "Location bits controlling where to search for objects.

   ZIL Constants from gparser.zil lines 1032-1038."
  {:held     128   ; SH - search player's inventory
   :carried   64   ; SC - search containers player is carrying
   :in-room   32   ; SIR - search room floor
   :on-ground 16   ; SOG - search containers in room
   :take       8   ; STAKE - auto-take if not held
   :many       4   ; SMANY - allow 'all', 'everything'
   :have       2}) ; SHAVE - must already be holding

(def getflags
  "Flags for GET-OBJECT parsing state.

   ZIL: P-ALL, P-ONE, P-INHIBIT constants."
  {:all     1   ; Player said 'all' or 'everything'
   :one     2   ; Player said 'one' or 'a' (pick randomly)
   :inhibit 4}) ; Inhibit object search (for 'of' constructs)

;;; ---------------------------------------------------------------------------
;;; GAME STATE
;;; ---------------------------------------------------------------------------

(defn initial-game-state
  "Return an initial game state."
  []
  {
    :rooms {}
    :objects {}
    :i-candles 40
    :i-lantern 200
    ;; <GLOBAL HERE 0>
    :here :west-of-house
    :it :mailbox
    :lit false
    :adventurer :adventurer
    ;; <GLOBAL WINNER 0>
    :winner :adventurer
    :player :adventurer
    :verbose false
    :super-brief false
    :won false
    :parser (parser-state/initial-parser-state)
  })


;;; ---------------------------------------------------------------------------
;;; FLAG OPERATIONS
;;; ---------------------------------------------------------------------------
;;; Generic flag operations for objects, rooms, and other entities.
;;;
;;; Base functions:
;;;   set-flag, unset-flag, flag? - work with entity-type (:objects, :rooms)
;;;
;;; Polymorphic functions (auto-detect entity type):
;;;   set-thing-flag, unset-thing-flag, set-thing-flag?
;;;
;;; Current room convenience:
;;;   set-here-flag, unset-here-flag, set-here-flag?

(defn- resolve-entity-type
  "Resolve the entity type for a thing-id. Returns :objects, :rooms, or nil."
  [game-state thing-id]
  (cond
    (contains? (:objects game-state) thing-id) :objects
    (contains? (:rooms game-state) thing-id) :rooms
    (= (:player game-state) thing-id) :objects
    :else nil))

(defn set-flag
  "Sets a flag on an entity. entity-type is :objects or :rooms."
  [game-state entity-type entity-id flag]
  (assoc-in game-state [entity-type entity-id flag] true))

(defn unset-flag
  "Unsets a flag on an entity. entity-type is :objects or :rooms."
  [game-state entity-type entity-id flag]
  (assoc-in game-state [entity-type entity-id flag] false))

(defn flag?
  "Returns true if a flag is set on an entity. entity-type is :objects or :rooms."
  [game-state entity-type entity-id flag]
  (get-in game-state [entity-type entity-id flag] false))

;; Polymorphic functions that auto-detect entity type

(defn set-thing-flag
  "Sets a flag on a room or object."
  [game-state thing-id flag]
  (if-let [entity-type (resolve-entity-type game-state thing-id)]
    (let [actual-id (if (= (:player game-state) thing-id)
                      (:adventurer game-state)
                      thing-id)]
      (set-flag game-state entity-type actual-id flag))
    (throw (Exception. (str "Thing " thing-id " not found!")))))

(defn unset-thing-flag
  "Unsets a flag on room or object."
  [game-state thing-id flag]
  (if-let [entity-type (resolve-entity-type game-state thing-id)]
    (let [actual-id (if (= (:player game-state) thing-id)
                      (:adventurer game-state)
                      thing-id)]
      (unset-flag game-state entity-type actual-id flag))
    (throw (Exception. (str "Thing " thing-id " not found!")))))

(defn set-thing-flag?
  "Indicates whether a flag is set on a room or object."
  [game-state thing-id flag]
  (if-let [entity-type (resolve-entity-type game-state thing-id)]
    (let [actual-id (if (= (:player game-state) thing-id)
                      (:adventurer game-state)
                      thing-id)]
      (flag? game-state entity-type actual-id flag))
    (throw (Exception. (str "Thing " thing-id " not found!")))))

;; Current room convenience functions

(defn set-here-flag
  "Sets a flag on the current room."
  [game-state flag]
  (set-flag game-state :rooms (:here game-state) flag))

(defn unset-here-flag
  "Unsets a flag on the current room."
  [game-state flag]
  (unset-flag game-state :rooms (:here game-state) flag))

(defn set-here-flag?
  "Indicates whether a flag is set on the current room."
  [game-state flag]
  (flag? game-state :rooms (:here game-state) flag))

(defn get-thing
  "Get an object or room based on its ID."
  [game-state id]
  (or (get-in game-state [:rooms id])
      (get-in game-state [:objects id])))

(defn thing-name
  "Get the description/name of a thing (object or room)."
  [game-state id]
  (or (:desc (get-thing game-state id))
      (name id)))

(defn get-thing-loc-id
  "Return the ID of the object or room in which the specified id is located."
  [game-state thing-id]
  (:in (get-thing game-state thing-id)))

;; Alias for compatibility with parser code
(def get-thing-location get-thing-loc-id)

(defn get-contents
  "Return the IDs of all objects inside the given container."
  [game-state container-id]
  (->> (:objects game-state)
       (filter (fn [[_ obj]] (= (:in obj) container-id)))
       (map first)))

(defn verbose?
  "Indicate whether we should be operating in verbose mode."
  [game-state]
  (or (get game-state :verbose false) (not (set-here-flag? game-state :touch))))

(defn get-here
  "Return the current room's object."
  [game-state]
  (get-thing game-state (:here game-state)))

(defn get-winner
  "Return the WINNER object."
  [game-state]
  (get-thing game-state (:winner game-state)))

(defn get-winner-loc
  "Return the location of the winner."
  [game-state]
  (get-thing game-state (get-thing-loc-id game-state (:winner game-state))))

(defn get-winner-loc-id
  "Return the location ID of the winner."
  [game-state]
  (get-thing-loc-id game-state :winner))

(defn get-player
  "Return the PLAYER object."
  [game-state]
  (get-thing game-state (:player game-state)))

(defn add-room
  "Add a room to the game state"
  [game-state room]
  (assoc game-state :rooms
    (assoc (:rooms game-state) (:id room) room)))

(defn add-rooms
  "Add each of the list of rooms to the game state"
  [game-state rooms]
  (reduce add-room game-state rooms))

(defn add-object
  "Add an object to the game state"
  [game-state object]
  (assoc game-state :objects
    (assoc (:objects game-state) (:id object) object)))

(defn add-objects
  "Add each of the list of objects to the game state"
  [game-state objects]
  (reduce add-object game-state objects))

(defn game-state-copy
  "Set the value of one key in game-state to the value of another."
  [game-state source-key dest-key]
  (assoc-in game-state dest-key (get-in game-state source-key))
)

;;
;; <ROUTINE META-LOC (OBJ)
;; 	 <REPEAT ()
;; 		 <COND (<NOT .OBJ>
;; 			<RFALSE>)
;; 		       (<IN? .OBJ ,GLOBAL-OBJECTS>
;; 			<RETURN ,GLOBAL-OBJECTS>)>
;; 		 <COND (<IN? .OBJ ,ROOMS>
;; 			<RETURN .OBJ>)
;; 		       (T
;; 			<SET OBJ <LOC .OBJ>>)>>>

(defn meta-location
  "Return the 'meta-location' of the thing."
  [game-state thing]
  (cond
    (nil? thing)
      nil
    (contains? (:objects game-state) thing)
      :objects
    (contains? (:rooms game-state) thing)
      :rooms
    true
      (meta-location game-state (:in thing))))