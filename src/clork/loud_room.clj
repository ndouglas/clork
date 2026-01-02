(ns clork.loud-room
  "Loud Room area implementation.

   The Loud Room contains the echo puzzle. When the reservoir is full and
   gates are open, the rushing water makes the room deafeningly loud.
   The solution is to say 'echo' which silences the room.

   ZIL Reference:
   - LOUD-ROOM-FCN in 1actions.zil (lines 1673-1741)
   - DEEP-CANYON-F in 1actions.zil (lines 1743-1758)
   - LOUD-FLAG global in 1actions.zil (line 1671)
   - LOUD-RUNS table in 1actions.zil (line 1274)"
  (:require [clork.utils :as utils]
            [clork.game-state :as gs]
            [clork.parser.state :as parser-state]
            [clork.debug.trace :as trace]))

;;; ---------------------------------------------------------------------------
;;; CONSTANTS
;;; ---------------------------------------------------------------------------

(def loud-runs
  "Rooms where player scrambles to when the room becomes too loud.
   ZIL: LOUD-RUNS = <LTABLE 0 DAMP-CAVE ROUND-ROOM DEEP-CANYON>"
  [:damp-cave :round-room :deep-canyon])

;;; ---------------------------------------------------------------------------
;;; HELPER FUNCTIONS
;;; ---------------------------------------------------------------------------

(defn room-is-quiet?
  "Check if the loud room is currently quiet.
   The room is quiet if:
   - LOUD-FLAG is set (player said 'echo'), OR
   - Gates are closed AND water is low (dam not releasing water)

   ZIL: <OR ,LOUD-FLAG <AND <NOT ,GATES-OPEN> ,LOW-TIDE>>"
  [game-state]
  (or (:loud-flag game-state)
      (and (not (:gates-open game-state))
           (:low-tide game-state))))

(defn room-is-deafening?
  "Check if the room is unbearably loud (player will be forced out).
   The room is deafening when gates are open AND water is not low.

   ZIL: <AND ,GATES-OPEN <NOT ,LOW-TIDE>>"
  [game-state]
  (and (:gates-open game-state)
       (not (:low-tide game-state))))

(defn pick-scramble-room
  "Pick a random room for the player to scramble to.
   ZIL: <GOTO <PICK-ONE ,LOUD-RUNS>>"
  []
  (rand-nth loud-runs))

;;; ---------------------------------------------------------------------------
;;; ROOM ACTION FUNCTIONS
;;; ---------------------------------------------------------------------------

(defn loud-room-action
  "Room action for LOUD-ROOM.

   Behavior depends on water state:
   - When quiet: Normal room behavior
   - When loud (gates open, water high): Special echo puzzle mode

   ZIL: LOUD-ROOM-FCN (1actions.zil lines 1673-1741)"
  [game-state rarg]
  (case rarg
    :look
    (let [quiet? (room-is-quiet? game-state)]
      (-> game-state
          (utils/tell "This is a large room with a ceiling which cannot be detected from the ground. There is a narrow passage from east to west and a stone stairway leading upward.")
          (cond->
            quiet?
            (utils/tell " The room is eerie in its quietness.")

            (not quiet?)
            (utils/tell " The room is deafeningly loud with an undetermined rushing sound. The sound seems to reverberate from all of the walls, making it difficult even to think."))
          (utils/crlf)))

    ;; M-END: Called at end of turn - check if player should be ejected
    :m-end
    (if (room-is-deafening? game-state)
      ;; Player is forced out of the room
      (let [escape-room (pick-scramble-room)]
        (-> game-state
            (utils/tell "It is unbearably loud here, with an ear-splitting roar seeming to come from all around you. There is a pounding in your head which won't stop. With a tremendous effort, you scramble out of the room.")
            (utils/crlf)
            (utils/crlf)
            (assoc :here escape-room)
            ;; Mark that we handled the action (don't process further)
            (assoc ::ejected true)))
      game-state)

    ;; M-ENTER: Called when entering room
    ;; When loud, this triggers the special echo puzzle input mode
    ;; For now, we'll handle this in the main loop by checking room state
    :m-enter
    (cond
      ;; Room is quiet - normal entry
      (room-is-quiet? game-state)
      game-state

      ;; Room is deafening - player will be ejected at end of turn
      (room-is-deafening? game-state)
      game-state

      ;; Room is loud but not deafening - enter special mode
      ;; (This shouldn't happen with current logic, but handle gracefully)
      :else
      game-state)

    ;; Default: no special handling
    game-state))

(defn deep-canyon-action
  "Room action for DEEP-CANYON.

   Shows water-related description based on dam state.

   ZIL: DEEP-CANYON-F (1actions.zil lines 1743-1758)"
  [game-state rarg]
  (case rarg
    :look
    (let [gates-open? (:gates-open game-state)
          low-tide? (:low-tide game-state)]
      (-> game-state
          (utils/tell "You are on the south edge of a deep canyon. Passages lead off to the east, northwest and southwest. A stairway leads down.")
          (cond->
            ;; Gates open, water high - loud roaring
            (and gates-open? (not low-tide?))
            (utils/tell " You can hear a loud roaring sound, like that of rushing water, from below.")

            ;; Gates closed, water low - quiet
            (and (not gates-open?) low-tide?)
            identity  ; Just newline, no extra text

            ;; Other states - normal flowing water sound
            (and (not (and gates-open? (not low-tide?)))
                 (not (and (not gates-open?) low-tide?)))
            (utils/tell " You can hear the sound of flowing water from below."))
          (utils/crlf)))

    ;; Default: no special handling
    game-state))

;;; ---------------------------------------------------------------------------
;;; ECHO VERB HANDLER
;;; ---------------------------------------------------------------------------

(defn v-echo
  "Handle the ECHO verb.
   In the Loud Room, saying 'echo' solves the puzzle.
   Elsewhere, it just echoes back what was said.

   ZIL: V-ECHO (gverbs.zil lines 542-564)"
  [game-state]
  (if (= (:here game-state) :loud-room)
    ;; In Loud Room - solve the puzzle!
    (-> game-state
        (assoc :loud-flag true)
        ;; Remove SACREDBIT from the platinum bar (can now be taken)
        (gs/unset-thing-flag :platinum-bar :sacred)
        (utils/tell "The acoustics of the room change subtly.")
        (utils/crlf))
    ;; Not in Loud Room - just echo
    (-> game-state
        (utils/tell "echo echo ...")
        (utils/crlf))))
