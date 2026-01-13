(ns clork.loud-room
  "Loud Room area implementation.

   The Loud Room contains the echo puzzle. When the reservoir is full and
   gates are open, the rushing water makes the room deafeningly loud.
   The solution is to say 'echo' which silences the room.

   Special Input Mode:
   When the room is loud (but not deafening), a special input mode is activated
   that only accepts limited commands: movement (W/E/U), ECHO, SAVE, RESTORE, QUIT.
   All other input is echoed back as if the room is repeating it.

   ZIL Reference:
   - LOUD-ROOM-FCN in 1actions.zil (lines 1673-1741)
   - DEEP-CANYON-F in 1actions.zil (lines 1743-1758)
   - LOUD-FLAG global in 1actions.zil (line 1671)
   - LOUD-RUNS table in 1actions.zil (line 1274)"
  (:require [clork.utils :as utils]
            [clork.game-state :as gs]
            [clork.parser.state :as parser-state]
            [clork.debug.trace :as trace]
            [clork.random :as random]
            [clork.verbs-look :as look]
            [clojure.string :as str]))

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

(defn special-mode-required?
  "Check if the special input mode should be active.
   Special mode is required when:
   - Room is NOT quiet (puzzle not solved, water not drained)
   - Room is NOT deafening (player isn't being kicked out)

   This is the 'loud but bearable' state where limited input is accepted."
  [game-state]
  (and (not (room-is-quiet? game-state))
       (not (room-is-deafening? game-state))))

(defn pick-scramble-room
  "Pick a random room for the player to scramble to.
   ZIL: <GOTO <PICK-ONE ,LOUD-RUNS>>"
  []
  (random/rand-nth* loud-runs))

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
          ;; Paragraph break after room description
          (utils/tell "\n\n")))

    ;; M-END: Called at end of turn - check if player should be ejected
    :m-end
    (if (room-is-deafening? game-state)
      ;; Player is forced out of the room
      ;; ZIL: <GOTO <PICK-ONE ,LOUD-RUNS>> - moves player AND describes new room
      (let [escape-room (pick-scramble-room)
            v-first-look (requiring-resolve 'clork.verbs-look/v-first-look)]
        (-> game-state
            (utils/tell "It is unbearably loud here, with an ear-splitting roar seeming to come from all around you. There is a pounding in your head which won't stop. With a tremendous effort, you scramble out of the room.")
            (utils/crlf)
            (utils/crlf)
            (gs/set-location escape-room :loud-room-ejection)
            (v-first-look)
            ;; Mark that we handled the action (don't process further)
            (assoc ::ejected true)))
      game-state)

    ;; M-ENTER: Called when entering room
    ;; When loud but not deafening, trigger the special echo puzzle input mode
    :m-enter
    (cond
      ;; Room is quiet - normal entry
      (room-is-quiet? game-state)
      game-state

      ;; Room is deafening - player will be ejected at end of turn
      (room-is-deafening? game-state)
      game-state

      ;; Room is loud but not deafening - enter special mode
      ;; ZIL: Lines 1700-1741 in LOUD-ROOM-FCN
      :else
      (-> game-state
          (assoc :loud-room-mode true)))

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

   ZIL: V-ECHO (gverbs.zil lines 542-564)
        LOUD-ROOM-FCN (1actions.zil lines 1732-1738)"
  [game-state]
  (if (= (:here game-state) :loud-room)
    ;; In Loud Room - solve the puzzle!
    ;; ZIL: Returns from special input mode, then game loop shows room
    (-> game-state
        (gs/set-game-flag :loud-flag)
        ;; Remove SACREDBIT from the platinum bar (can now be taken)
        (gs/unset-thing-flag :platinum-bar :sacred)
        (utils/tell "The acoustics of the room change subtly.\n\n")
        ;; After solving, show the room description (ZIL returns to main loop which does this)
        (look/v-look))
    ;; Not in Loud Room - just echo
    (-> game-state
        (utils/tell "echo echo ...")
        (utils/crlf))))

;;; ---------------------------------------------------------------------------
;;; SPECIAL INPUT MODE HANDLER
;;; ---------------------------------------------------------------------------
;;; ZIL: Lines 1700-1741 in LOUD-ROOM-FCN
;;; When the room is loud but not deafening, only limited commands work.

(defn- get-first-word
  "Extract the first meaningful word from input.
   If input starts with GO/WALK/RUN, get the direction word instead.
   If input starts with SAY, get the word being said."
  [input]
  (when input
    (let [words (str/split (str/trim (str/lower-case input)) #"\s+")
          first-word (first words)]
      (cond
        ;; GO WEST -> WEST, WALK EAST -> EAST, etc.
        (contains? #{"go" "walk" "run"} first-word)
        (second words)

        ;; SAY ECHO -> ECHO
        (= "say" first-word)
        (second words)

        :else first-word))))

(defn- goto-room
  "Move player to a room and exit special mode.
   Uses requiring-resolve to avoid circular dependency with verbs-look."
  [game-state room-id]
  (let [v-first-look (requiring-resolve 'clork.verbs-look/v-first-look)]
    (-> game-state
        (gs/set-location room-id :loud-room-exit)
        (dissoc :loud-room-mode)
        (v-first-look))))

(defn- solve-echo-puzzle
  "Solve the echo puzzle and exit special mode."
  [game-state]
  (-> game-state
      (gs/set-game-flag :loud-flag)
      (gs/unset-thing-flag :platinum-bar :sacred)
      (utils/tell "The acoustics of the room change subtly.")
      (utils/crlf)
      (dissoc :loud-room-mode)))

(defn- echo-input
  "Echo the player's input back (room is too loud for anything else).
   ZIL: V-ECHO in special mode just echoes the input."
  [game-state input]
  (-> game-state
      (utils/tell (str input " " input " ..."))
      (utils/crlf)))

(defn handle-special-input
  "Handle input in the loud room special mode.
   Only limited commands are recognized:
   - Movement: W/WEST, E/EAST, U/UP (to specific rooms)
   - Meta: SAVE, RESTORE, QUIT
   - Puzzle: ECHO (solves the puzzle)
   - Debug: $ commands (always allowed)
   - Everything else is echoed back.

   ZIL: LOUD-ROOM-FCN lines 1706-1741

   Returns [game-state handled?] where handled? is true if we processed it."
  [game-state input]
  (cond
    ;; Empty input
    (or (nil? input) (str/blank? input))
    [(utils/tell game-state "I beg your pardon?") true]

    ;; Debug commands always pass through
    (str/starts-with? (str/trim input) "$")
    [game-state false]  ; Let main loop handle it

    :else
    (let [word (get-first-word input)]
      (case word
        ;; Movement - exit to specific rooms
        ("w" "west")
        [(goto-room game-state :round-room) true]

        ("e" "east")
        [(goto-room game-state :damp-cave) true]

        ("u" "up")
        [(goto-room game-state :deep-canyon) true]

        ;; Meta commands - let main loop handle these
        ("save" "restore" "quit" "q")
        [game-state false]

        ;; BUG easter egg
        "bug"
        [(-> game-state
             (utils/tell "That's only your opinion.")
             (utils/crlf)) true]

        ;; ECHO - solve the puzzle!
        "echo"
        [(solve-echo-puzzle game-state) true]

        ;; LOOK - show room description (helpful for players)
        ("look" "l")
        [(-> game-state
             (loud-room-action :look)) true]

        ;; Everything else is echoed back
        [(echo-input game-state input) true]))))
