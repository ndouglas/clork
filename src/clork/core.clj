(ns clork.core
  (:gen-class))

(require '[clojure.pprint :refer [pprint]])

(load "utils")
(load "verbs")
(load "initial_game_state")
(load "flags")
(load "game_state")
(load "rooms")

;; <ROUTINE DESCRIBE-ROOM ("OPTIONAL" (LOOK? <>) "AUX" V? STR AV)
;; 	 <SET V? <OR .LOOK? ,VERBOSE>>
;; 	 <COND (<NOT ,LIT>
;; 		<TELL "It is pitch black.">
;; 		<COND (<NOT ,SPRAYED?>
;; 		       <TELL " You are likely to be eaten by a grue.">)>
;; 		<CRLF>
;; 		%<COND (<==? ,ZORK-NUMBER 3>
;; 			'<COND (<EQUAL? ,HERE ,DARK-2>
;; 		                <TELL
;; "The ground continues to slope upwards away from the lake. You can barely
;; detect a dim light from the east." CR>)>)
;; 		       (T
;; 			'<NULL-F>)>
;; 		<RFALSE>)>
;; 	 <COND (<NOT <FSET? ,HERE ,TOUCHBIT>>
;; 		<FSET ,HERE ,TOUCHBIT>
;; 		<SET V? T>)>
;; 	 %<COND (<==? ,ZORK-NUMBER 1>
;; 		 '<COND (<FSET? ,HERE ,MAZEBIT>
;; 		         <FCLEAR ,HERE ,TOUCHBIT>)>)
;; 		(T
;; 		 '<NULL-F>)>
;; 	 <COND (<IN? ,HERE ,ROOMS>
;; 		;"Was <TELL D ,HERE CR>"
;; 		<TELL D ,HERE>
;; 		<COND (<FSET? <SET AV <LOC ,WINNER>> ,VEHBIT>
;; 		       <TELL ", in the " D .AV>)>
;; 		<CRLF>)>
;; 	 <COND (%<COND (<==? ,ZORK-NUMBER 2>
;; 			'<OR .LOOK? <NOT ,SUPER-BRIEF> <EQUAL? ,HERE ,ZORK3>>)
;; 		       (ELSE
;; 			'<OR .LOOK? <NOT ,SUPER-BRIEF>>)>
;; 		<SET AV <LOC ,WINNER>>
;; 		;<COND (<FSET? .AV ,VEHBIT>
;; 		       <TELL "(You are in the " D .AV ".)" CR>)>
;; 		<COND (<AND .V? <APPLY <GETP ,HERE ,P?ACTION> ,M-LOOK>>
;; 		       <RTRUE>)
;; 		      (<AND .V? <SET STR <GETP ,HERE ,P?LDESC>>>
;; 		       <TELL .STR CR>)
;; 		      (T
;; 		       <APPLY <GETP ,HERE ,P?ACTION> ,M-FLASH>)>
;;   ------------------------------------------------------------
;; 		<COND (<AND <NOT <EQUAL? ,HERE .AV>> <FSET? .AV ,VEHBIT>>
;; 		       <APPLY <GETP .AV ,P?ACTION> ,M-LOOK>)>)>
;; 	 T>

(defn describe-room
  "Describes the room."
  ([game-state] (describe-room game-state false))
  ([game-state is-verbose]
    (let [is-verbose (or is-verbose (verbose? game-state))]
      (if
        (not (set-here-flag? game-state :lit))
          (println "It is pitch black. You are likely to be eaten by a grue."))
      (set-here-flag game-state :touch)
      (if
        (set-here-flag? game-state :maze)
        (unset-here-flag game-state :touch))
      (def loc (get-winner-loc game-state))
      (def loc-is-veh (get-in loc [:flags :vehicle] false))
      (if
        loc-is-veh
        (let [vehicle (get-in :desc)]
          (println (str "(You are in the " vehicle ".)"))
        )
      )
      (def act (:action (get-here game-state)))
      (cond
        (and is-verbose (some? act)) (act game-state :look)
        is-verbose (println (:ldesc (get-here game-state)))
        (some? act) (act game-state :flash)))
      (if
        (and
          loc-is-veh
          (not (= (:id loc) (:here game-state))))
        (act game-state :look)
      )
    ))

(defn describe-objects
  "Describes the objects in the room."
  ([game-state] (describe-objects game-state false))
  ([game-state look] game-state))

(defn v-look
  "Describes the room and any objects."
  [game-state]
  (if (describe-room game-state true)
    (describe-objects game-state true)))

(defn tell
  "Tell the player something, and return the game state."
  [game-state message]
  (print message)
  game-state
)

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

  ;; If we haven't been here before, then show V-VERSION text.
  (if-not
    (set-here-flag? game-state :touch)
    (do
      (v-version)
      (crlf)))

  ;; Set LIT to T, so everything is lit.
  (def game-state (set-here-flag game-state :lit))

  ;; Call V-LOOK to describe the current location
  (v-look game-state)

  ;; Call the MAIN-LOOP
  ;; (pprint game-state)
)

(defn -main
  "Main function for CLORK."
  [& args]
  (go))
