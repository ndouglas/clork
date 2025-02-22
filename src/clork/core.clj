(ns clork.core
  (:gen-class))

(require '[clojure.pprint :refer [pprint]])

(defn crlf
  "Print a carriage return and line feed."
  []
  (println ""))

;; <ROUTINE V-VERSION ("AUX" (CNT 17))
;; 	%<COND (<==? ,ZORK-NUMBER 1>
;; 		'<TELL "ZORK I: The Great Underground Empire|
;; Infocom interactive fiction - a fantasy story|
;; Copyright (c) 1981, 1982, 1983, 1984, 1985, 1986">)
;; 	       (<==? ,ZORK-NUMBER 2>
;; 		'<TELL "ZORK II: The Wizard of Frobozz|
;; Infocom interactive fiction - a fantasy story|
;; Copyright (c) 1981, 1982, 1983, 1986">)
;; 	       (<==? ,ZORK-NUMBER 3>
;; 		'<TELL "ZORK III: The Dungeon Master|
;; Infocom interactive fiction - a fantasy story|
;; Copyright 1982, 1983, 1984, 1986">)>
;; 	<TELL " Infocom, Inc. All rights reserved." CR>
;; 	<TELL "ZORK is a registered trademark of Infocom, Inc.|
;; Release ">
;; 	<PRINTN <BAND <GET 0 1> *3777*>>
;; 	<TELL " / Serial number ">
;; 	<REPEAT ()
;; 		<COND (<G? <SET CNT <+ .CNT 1>> 23>
;; 		       <RETURN>)
;; 		      (T
;; 		       <PRINTC <GETB 0 .CNT>>)>>
;; 	<CRLF>>


(defn v-version
  "Prints information about the current version of the game."
  []
  (do
    (println "ZORK I: The Great Underground Empire")
    (println "Infocom interactive fiction - a fantasy story")
    (println "Copyright (c) 1981, 1982, 1983, 1984, 1985, 1986 Infocom, Inc. All rights reserved.")
    (println "ZORK is a registered trademark of Infocom, Inc.")
    (println "Release 1 / Serial number 1")
  ))

(def initial-game-state {
  :rooms {},
  :objects {},
  :i-candles 40,
  :i-lantern 200,
  :here :west-of-house,
  :it :mailbox,
  :lit true,
  :adventurer :adventurer,
  :winner :adventurer,
  :player :adventurer,
})

(defn add-room
  "Add a room to the game state"
  [game-state room]
  (assoc game-state :rooms
    (assoc (get game-state :rooms) (get room :id) room)))

(defn add-rooms
  "Add each of the list of rooms to the game state"
  [game-state rooms]
  (reduce add-room game-state rooms))

(defn add-object
  "Add an object to the game state"
  [game-state object]
  (assoc game-state :objects
    (assoc (get game-state :objects) (get object :id) object)))

(defn add-objects
  "Add each of the list of objects to the game state"
  [game-state objects]
  (reduce add-object game-state objects))

(defn this-is-it
  "Sets 'it' to refer to the passed object"
  [game-state it]
  (assoc game-state :it it))

(defn get-default-flags
  "Get the default (all-off) flags set."
  []
  (let [default-flags {
    ;; The player can pick up and carry the object.
    :take false,
    ;; Tells the parser not to let the player implicitly take an object.
    :trytake false,
    ;; The object is a container; things can be put inside it, it can be opened
    ;; and closed, etc.
    :cont false,
    ;; The object is a door.
    :door false,
    ;; The object is a door or container, and is open.
    :open false,
    ;; The object is a surface, such as a table, desk, countertop, etc.
    :surface false,
    ;; Locked and can't be opened without proper equipment.
    :locked false,
    ;; The object is wearable (not necessarily being worn).
    :wear false,
    ;; The object is currently being worn.
    :worn false,
    ;; The object is readable (has a :text property).
    :read false,
    ;; The object is capable of being turned on and off.
    :light false,
    ;; The room is lit, or the object is providing light.
    :on false,
    ;; The object is a source of fire.
    :flame false,
    ;; The object is burnable.
    :burn false,
    ;; The object is transparent; objects inside it can be seen even if it is
    ;; closed.
    :trans false,
    ;; The room description is describing this object. Should be cleared once
    ;; taken.
    :ndesc false,
    ;; Tells the parser not to find this object; the bit would presumably be
    ;; cleared at some point.
    :invisible false,
    ;; For rooms, player has been to the room at least once. For objects, it
    ;; has been taken or otherwise disturbed by the player.
    :touch false,
    ;; Tells the parser to look as deeply into a container as it can in order
    ;; to find the referened object.
    :search false,
    ;; The object is a vehicle.
    :vehicle false,
    ;; The object is a character in the game.
    :person false,
    ;; The object is an actor who is a female.
    :female false,
    ;; Any verb default which prints an indefinite article before the :desc,
    ;; use "an" instead of "a".
    :vowel false,
    ;; The object's :desc doesn't work with articles, and they should be
    ;; omitted.
    :narticle false,
    ;; The object's :desc is a plural noun or noun phrase.
    :plural false,
    ;; Indicates that the room is dry land.
    :rland false,
    ;; The room is water rather than dry land.
    :rwater false,
    ;; The room is mid-air.
    :rair false,
    ;; This bit is used only in the syntax file.
    :kludge false,
    ;; This room is outdoors.
    :outside false,
    ;; An integral part of another object; can't be taken or dropped.
    :integral false,
    ;; The object is a body part.
    :part false,
    ;; "Take all" should not take this object.
    :nall false,
    ;; Found in vehicles. If the player drops an item, it stays in the vehicle
    ;; rather than into the room outside.
    :drop false,
    ;; Found in vehicles. Tells routines to say "in the vehicle" instead of "on
    ;; the vehicle."
    :in false,
  }]
  (set (for [[k v] default-flags :when v]
       k))
  ))

(defn set-flag
  "Sets a flag in the set of flags passed to it."
  [flag-set flag]
  (conj flag-set flag))

(defn is-flag-set?
  "Indicates whether a flag is set in the set of flags."
  [flag-set flag]
  (contains? flag-set flag)
)

(defn is-obj-flag-set?
  "Indicates whether a flag is set in the object's set of flags."
  [obj flag]
  (is-flag-set? (get obj :flags) flag)
)

(defn unset-flag
  "Unsets a flag in the set of flags passed to it."
  [flag-set flag]
  (disj flag-set flag))

(defn flags
  "Returns a new set of flags with the specified flags set."
  [& set-flags]
  (let [flag-set (get-default-flags)]
    (reduce set-flag flag-set set-flags)
  ))

(defn here-name
  "Returns the name of the room where the player currently is."
  [game-state]
  (get game-state :here)
)

(defn here-room
  "Returns the room where the player currently is."
  [game-state]
  (get (get game-state :rooms) (here-name game-state))
)

;; <ROOM WEST-OF-HOUSE
;;       (IN ROOMS)
;;       (DESC "West of House")
;;       (NORTH TO NORTH-OF-HOUSE)
;;       (SOUTH TO SOUTH-OF-HOUSE)
;;       (NE TO NORTH-OF-HOUSE)
;;       (SE TO SOUTH-OF-HOUSE)
;;       (WEST TO FOREST-1)
;;       (EAST "The door is boarded and you can't remove the boards.")
;;       (SW TO STONE-BARROW IF WON-FLAG)
;;       (IN TO STONE-BARROW IF WON-FLAG)
;;       (ACTION WEST-HOUSE)
;;       (FLAGS RLANDBIT ONBIT SACREDBIT)
;;       (GLOBAL WHITE-HOUSE BOARD FOREST)>

(def west-of-house {
  :id :west-of-house,
  :desc "West of House",
})

;; <OBJECT MAILBOX
;;	(IN WEST-OF-HOUSE)
;;	(SYNONYM MAILBOX BOX)
;;	(ADJECTIVE SMALL)
;;	(DESC "small mailbox")
;;	(FLAGS CONTBIT TRYTAKEBIT)
;;	(CAPACITY 10)
;;	(ACTION MAILBOX-F)>
;;

(def mailbox {
  :id :mailbox,
  :in :west-of-house,
  :synonym ["mailbox" "box"],
  :adjective "small",
  :desc "small mailbox",
  :flags (flags :cont, :trytake),
  :capacity 10,
})

;; <OBJECT ADVENTURER
;; 	(SYNONYM ADVENTURER)
;; 	(DESC "cretin")
;; 	(FLAGS NDESCBIT INVISIBLE SACREDBIT ACTORBIT)
;; 	(STRENGTH 0)
;; 	(ACTION 0)>

(def adventurer {
  :id :adventurer,
  :synonym ["adventurer"],
  :desc "cretin",
  :flags (flags :ndesc :invisible :sacred :actor),
  :strength 0,
  :action 0,
})

(defn go
  "The GO routine."
  []
  ;; Set special global variables
  (def game-state initial-game-state)

  ;; Add rooms.
  (def game-state (add-rooms game-state [
    west-of-house,
  ]))

  ;; Add objects.
  (def game-state (add-objects game-state [
    adventurer,
    mailbox,
  ]))

  ;; THIS-IS-IT sets "IT" to refer to the mailbox. So "OPEN IT" will open the
  ;; mailbox.
  (def game-state (this-is-it game-state :mailbox))

  ;; // If we haven't been here before, then show V-VERSION text.
	;; <COND (<NOT <FSET? ,HERE ,TOUCHBIT>>
	;;        <V-VERSION>
	;;        <CRLF>)>
  (if-not
    (is-obj-flag-set? (here-room game-state) :touch)
    (do
      (v-version)
      (crlf)))

  ;; Set interrupts, usually with the QUEUE or INT routines
  ;; Display an opening text/title screen
  ;; Call V-VERSION to show copyright information, release number, and serial number
  ;; Call V-LOOK to describe the current location
  ;; Call the MAIN-LOOP
  ;; (pprint game-state)
)

(defn -main
  "Main function for CLORK."
  [& args]
  (go))
