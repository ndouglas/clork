(in-ns 'clork.core)

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

;; // The infamous white house.
;; //
;; // If you've managed to get all of the trophies into the trophy case, then
;; // there is a secret path leading southwest into the forest.
;; //
;; // RARG is the "room argument"; when the player enters the room for the first
;; // time, or if VERBOSE is turned on, this would be equal to LOOK, and would
;; // print the description of the room.
;; //
;;
;; <ROUTINE WEST-HOUSE (RARG)
;; 	 <COND (<EQUAL? .RARG ,M-LOOK>
;; 		<TELL
;; "You are standing in an open field west of a white house, with a boarded
;; front door.">
;; 		<COND (,WON-FLAG
;; 		       <TELL
;; " A secret path leads southwest into the forest.">)>
;; 		<CRLF>)>>

(def west-of-house {
  :id :west-of-house,
  :desc "West of House",
  :action (fn [game-state rarg]
    (if
      (= rarg :look)
      (print "You are standing in an open field west of a white house, with a boarded front door."))
    (if
      (:won game-state)
      (print " A secret path leads southwest into the forest."))
    (crlf game-state)
  )
})
