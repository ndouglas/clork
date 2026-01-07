(ns clork.forest
  "Forest room actions and daemons.

   ZIL Reference: 1actions.zil
   - FOREST-ROOM (lines 3017-3022): Room action for forest rooms
   - I-FOREST-ROOM (lines 3009-3015): Daemon that occasionally prints songbird message
   - FOREST-ROOM? (lines 3005-3007): Helper to check if in forest room"
  (:require [clork.daemon :as daemon]
            [clork.utils :as utils]
            [clork.random :as random]
            [clork.game-state :as gs]))

;;; ---------------------------------------------------------------------------
;;; FOREST ROOM HELPER
;;; ---------------------------------------------------------------------------

(defn forest-room?
  "Check if the player is in a forest room.

   ZIL: FOREST-ROOM? (1actions.zil lines 3005-3007)
   <ROUTINE FOREST-ROOM? ()
     <OR <EQUAL? ,HERE ,FOREST-1 ,FOREST-2 ,FOREST-3>
         <EQUAL? ,HERE ,PATH ,UP-A-TREE>>>"
  [game-state]
  (let [here (:here game-state)]
    (contains? #{:forest-1 :forest-2 :forest-3 :forest-path :up-a-tree
                 :clearing :grating-clearing}
               here)))

;;; ---------------------------------------------------------------------------
;;; I-FOREST-ROOM DAEMON
;;; ---------------------------------------------------------------------------

(defn i-forest-room
  "Forest room daemon that occasionally prints songbird message.

   ZIL: I-FOREST-ROOM (1actions.zil lines 3009-3015)
   <ROUTINE I-FOREST-ROOM ()
     <COND (<NOT <FOREST-ROOM?>>
            <DISABLE <INT I-FOREST-ROOM>>
            <RFALSE>)
           (<PROB 15>
            <TELL \"You hear in the distance the chirping of a song bird.\" CR>)>>"
  [game-state]
  (cond
    ;; If not in forest room, disable daemon
    (not (forest-room? game-state))
    (daemon/disable game-state :i-forest-room)

    ;; 15% chance of hearing songbird
    ;; Use paragraph break to separate from previous action output
    (< (random/rand-int* 100) 15)
    (utils/tell game-state "\n\nYou hear in the distance the chirping of a song bird.")

    :else
    game-state))

;;; ---------------------------------------------------------------------------
;;; FOREST-ROOM ACTION
;;; ---------------------------------------------------------------------------

(defn forest-room-action
  "Room action for forest rooms.

   ZIL: FOREST-ROOM (1actions.zil lines 3017-3022)
   <ROUTINE FOREST-ROOM (RARG)
     <COND (<EQUAL? .RARG ,M-ENTER> <ENABLE <QUEUE I-FOREST-ROOM -1>>)
           (<EQUAL? .RARG ,M-BEG>
            <COND (<AND <VERB? CLIMB-FOO CLIMB-UP>
                        <EQUAL? ,PRSO ,TREE>>
                   <DO-WALK ,P?UP>)>)>>

   Arguments:
     game-state - current game state
     rarg - room action type (:m-enter, :m-beg, :m-look, etc.)"
  [game-state rarg]
  (case rarg
    ;; M-ENTER: Enable forest room daemon when entering
    :m-enter
    (daemon/enable game-state :i-forest-room)

    ;; Default: No special handling
    (gs/use-default game-state)))
