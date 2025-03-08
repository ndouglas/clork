(in-ns 'clork.core)

(declare initial-parser-state)
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
    :parser (initial-parser-state)
  })


(defn set-obj-flag
  "Sets a flag on an object."
  [game-state obj-id flag]
  (assoc-in game-state [:objects obj-id flag] true))

(defn unset-obj-flag
  "Unsets a flag on an object."
  [game-state obj-id flag]
  (assoc-in game-state [:objects obj-id flag] false))

(defn set-obj-flag?
  "Indicates whether a flag is set on an object."
  [game-state obj-id flag]
  (get-in game-state [:objects obj-id flag] false))

(defn set-room-flag
  "Sets a flag on a room."
  [game-state room-id flag]
  (assoc-in game-state [:rooms room-id flag] true))

(defn unset-room-flag
  "Unsets a flag on a room."
  [game-state room-id flag]
  (assoc-in game-state [:rooms room-id flag] false))

(defn set-room-flag?
  "Indicates whether a flag is set on a room."
  [game-state room-id flag]
  (get-in game-state [:rooms room-id flag] false))

(defn set-adv-flag
  "Sets a flag on the adventurer."
  [game-state flag]
  (set-obj-flag game-state (:adventurer game-state) flag))

(defn unset-adv-flag
  "Unsets a flag on the adventurer."
  [game-state flag]
  (unset-obj-flag game-state (:adventurer game-state) flag))

(defn set-adv-flag?
  "Indicates whether a flag is set on the adventurer."
  [game-state flag]
  (set-obj-flag? game-state (:adventurer game-state) flag))

(defn set-thing-flag
  "Sets a flag on a room or object."
  [game-state thng-id flag]
  (cond
    (contains? (:objects game-state) thng-id)
      (set-obj-flag game-state thng-id flag)
    (contains? (:rooms game-state) thng-id)
      (set-room-flag game-state thng-id flag)
    (= (:player game-state) thng-id)
      (set-adv-flag game-state flag)
    true
      (throw (Exception. (str "Thing " thng-id " not found!")))))

(defn unset-thing-flag
  "Unsets a flag on room or object."
  [game-state thng-id flag]
  (cond
    (contains? (:objects game-state) thng-id)
      (unset-obj-flag game-state thng-id flag)
    (contains? (:rooms game-state) thng-id)
      (unset-room-flag game-state thng-id flag)
    (= (:player game-state) thng-id)
      (unset-adv-flag game-state flag)
    true
      (throw (Exception. (str "Thing " thng-id " not found!")))))

(defn set-thing-flag?
  "Indicates whether a flag is set on a room or object."
  [game-state thng-id flag]
  (cond
    (contains? (:objects game-state) thng-id)
      (set-obj-flag? game-state thng-id flag)
    (contains? (:rooms game-state) thng-id)
      (set-room-flag? game-state thng-id flag)
    (= (:player game-state) thng-id)
      (set-adv-flag? game-state flag)
    true
      (throw (Exception. (str "Thing " thng-id " not found!")))))

(defn set-here-flag
  "Sets a flag on the current room."
  [game-state flag]
  (set-room-flag game-state (:here game-state) flag))

(defn unset-here-flag
  "Unsets a flag on the current room."
  [game-state flag]
  (unset-room-flag game-state (:here game-state) flag))

(defn set-here-flag?
  "Indicates whether a flag is set on the current room."
  [game-state flag]
  (set-room-flag? game-state (:here game-state) flag))

(defn get-thing
  "Get an object or room based on its ID."
  [game-state id]
  (get-in game-state [:rooms id] (get-in game-state [:objects :id])))

(defn get-thing-loc-id
  "Return the ID of the object or room in which the specified id is located."
  [game-state thing-id]
  (:in (get-thing game-state thing-id)))

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