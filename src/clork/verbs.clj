(ns clork.verbs
  "Verb handler functions."
  (:require [clork.utils :as utils]
            [clork.game-state :as gs]
            [clork.parser.state :as parser-state]
            [clork.verbs-look :as verbs-look]))

;;; ---------------------------------------------------------------------------
;;; VERB HANDLERS
;;; ---------------------------------------------------------------------------
;;; Each verb has a handler function that performs the action.
;;; Handler functions take game-state and return updated game-state.
;;;
;;; ZIL Reference: gverbs.zil contains meta-verb handlers like V-VERSION,
;;; V-VERBOSE, V-BRIEF, V-SUPER-BRIEF.

(defn v-version
  "Prints information about the current version of the game.

   ZIL: V-VERSION in gverbs.zil"
  [game-state]
  (-> game-state
      (utils/tell "ZORK I: The Great Underground Empire\n")
      (utils/tell "Infocom interactive fiction - a fantasy story\n")
      (utils/tell "Copyright (c) 1981, 1982, 1983, 1984, 1985, 1986 Infocom, Inc. All rights reserved.\n")
      (utils/tell "ZORK is a registered trademark of Infocom, Inc.\n")
      (utils/tell "Release 1 / Serial number 1\n")
      (utils/tell "Clojure port by Nathan Douglas\n")))

(defn v-verbose
  "Turns on verbose mode - always describe rooms fully.

   ZIL: V-VERBOSE in gverbs.zil
     <ROUTINE V-VERBOSE ()
       <SETG VERBOSE T>
       <SETG SUPER-BRIEF <>>
       <TELL \"Maximum verbosity.\" CR>>"
  [game-state]
  (-> game-state
      (assoc :verbose true)
      (assoc :super-brief false)
      (utils/tell "Maximum verbosity.")))

(defn v-brief
  "Turns on brief mode - describe rooms only on first visit.

   ZIL: V-BRIEF in gverbs.zil
     <ROUTINE V-BRIEF ()
       <SETG VERBOSE <>>
       <SETG SUPER-BRIEF <>>
       <TELL \"Brief descriptions.\" CR>>"
  [game-state]
  (-> game-state
      (assoc :verbose false)
      (assoc :super-brief false)
      (utils/tell "Brief descriptions.")))

(defn v-super-brief
  "Turns on super-brief mode - never describe rooms automatically.

   ZIL: V-SUPER-BRIEF in gverbs.zil
     <ROUTINE V-SUPER-BRIEF ()
       <SETG SUPER-BRIEF T>
       <TELL \"Superbrief descriptions.\" CR>>"
  [game-state]
  (-> game-state
      (assoc :super-brief true)
      (utils/tell "Superbrief descriptions.")))

(defn v-inventory
  "Prints the player's inventory.

   ZIL: V-INVENTORY in gverbs.zil
     <ROUTINE V-INVENTORY ()
       <COND (<FIRST? ,WINNER> <PRINT-CONT ,WINNER>)
             (T <TELL \"You are empty-handed.\" CR>)>>"
  [game-state]
  (let [winner-id (:winner game-state)
        contents (gs/get-contents game-state winner-id)]
    (if (empty? contents)
      (utils/tell game-state "You are empty-handed.")
      (reduce (fn [state obj-id]
                (let [obj-name (gs/thing-name state obj-id)]
                  (utils/tell state (str "  A " obj-name "\n"))))
              (utils/tell game-state "You are carrying:\n")
              contents))))

;;; ---------------------------------------------------------------------------
;;; Constants for fight/health system
;;; ---------------------------------------------------------------------------
;;; ZIL: STRENGTH-MAX = 7, STRENGTH-MIN = 2
;;; Base fight strength is calculated from score, ranging from 2 to 7.

(def ^:private strength-max
  "Maximum base fighting strength. ZIL: STRENGTH-MAX = 7"
  7)

(def ^:private strength-min
  "Minimum base fighting strength. ZIL: STRENGTH-MIN = 2"
  2)

(defn- fight-strength
  "Calculate the player's current fighting ability.

   ZIL: FIGHT-STRENGTH routine in 1actions.zil
     S = STRENGTH-MIN + (SCORE / (SCORE-MAX / (STRENGTH-MAX - STRENGTH-MIN)))

   At 0 points: base = 2
   At 350 points: base = 7

   If adjust? is true (default), adds wound modifier.
   If adjust? is false, returns just the base (for survivability display)."
  ([game-state] (fight-strength game-state true))
  ([game-state adjust?]
   (let [score (get game-state :score 0)
         score-max (get game-state :score-max 350)
         ;; Calculate base strength from score
         ;; S = STRENGTH-MIN + (SCORE / (SCORE-MAX / (STRENGTH-MAX - STRENGTH-MIN)))
         ;; = 2 + (SCORE / (350 / 5)) = 2 + (SCORE / 70)
         divisor (/ score-max (- strength-max strength-min))
         base-strength (+ strength-min (int (/ score divisor)))
         ;; Clamp to valid range
         base-strength (min strength-max (max strength-min base-strength))
         wound-modifier (get-in game-state [:objects (:winner game-state) :strength] 0)]
     (if adjust?
       (+ base-strength wound-modifier)
       base-strength))))

(defn- wound-description
  "Return the description of wounds based on wound level.

   ZIL: Wound levels from V-DIAGNOSE:
     1 = light wound
     2 = serious wound
     3 = several wounds
     >3 = serious wounds"
  [wound-level]
  (cond
    (= wound-level 1) "a light wound"
    (= wound-level 2) "a serious wound"
    (= wound-level 3) "several wounds"
    (> wound-level 3) "serious wounds"))

(defn- survivability-description
  "Return description of how much more damage the player can take.

   ZIL: Based on remaining strength (RS = MS + WD):
     0 = expect death soon
     1 = killed by one more light wound
     2 = killed by a serious wound
     3 = survive one serious wound
     >3 = survive several wounds"
  [remaining-strength]
  (cond
    (<= remaining-strength 0) "expect death soon"
    (= remaining-strength 1) "be killed by one more light wound"
    (= remaining-strength 2) "be killed by a serious wound"
    (= remaining-strength 3) "survive one serious wound"
    (> remaining-strength 3) "survive several wounds"))

(defn v-diagnose
  "Reports the player's health status.

   ZIL: V-DIAGNOSE in 1actions.zil
     Reports current wounds, time to heal, survivability, and death count.

     <ROUTINE V-DIAGNOSE (\"AUX\" (MS <FIGHT-STRENGTH <>>)
                          (WD <GETP ,WINNER ,P?STRENGTH>) (RS <+ .MS .WD>))
       ...>"
  [game-state]
  (let [winner-id (:winner game-state)
        ;; WD = wound modifier (0 = healthy, negative = wounded)
        wound-modifier (get-in game-state [:objects winner-id :strength] 0)
        ;; Convert to positive wound level for display
        wound-level (- wound-modifier)
        ;; RS = remaining strength = base + wounds
        remaining-strength (fight-strength game-state)
        deaths (get game-state :deaths 0)]
    (-> game-state
        ;; Report wound status
        (as-> gs
              (if (zero? wound-level)
                (utils/tell gs "You are in perfect health.")
                (-> gs
                    (utils/tell "You have ")
                    (utils/tell (wound-description wound-level))
                    (utils/tell "."))))
        ;; Report survivability
        (utils/tell "\nYou can ")
        (utils/tell (survivability-description remaining-strength))
        (utils/tell ".")
        ;; Report death count if any
        (as-> gs
              (if (pos? deaths)
                (-> gs
                    (utils/tell "\nYou have been killed ")
                    (utils/tell (if (= deaths 1) "once" "twice"))
                    (utils/tell "."))
                gs)))))

;;; ---------------------------------------------------------------------------
;;; SCORING SYSTEM
;;; ---------------------------------------------------------------------------
;;; ZIL: V-SCORE, SCORE-UPD, SCORE-OBJ in gverbs.zil and 1actions.zil

(defn- player-rank
  "Return the player's rank based on their score.

   ZIL: Rank thresholds from V-SCORE:
     350 = Master Adventurer
     >330 = Wizard
     >300 = Master
     >200 = Adventurer
     >100 = Junior Adventurer
     >50 = Novice Adventurer
     >25 = Amateur Adventurer
     else = Beginner"
  [score]
  (cond
    (= score 350)  "Master Adventurer"
    (> score 330)  "Wizard"
    (> score 300)  "Master"
    (> score 200)  "Adventurer"
    (> score 100)  "Junior Adventurer"
    (> score 50)   "Novice Adventurer"
    (> score 25)   "Amateur Adventurer"
    :else          "Beginner"))

(defn v-score
  "Prints the player's current score and rank.

   ZIL: V-SCORE in 1actions.zil
     <ROUTINE V-SCORE (\"OPTIONAL\" (ASK? T))
       <TELL \"Your score is \">
       <TELL N ,SCORE>
       <TELL \" (total of 350 points), in \">
       <TELL N ,MOVES>
       ...>"
  [game-state]
  (let [score (get game-state :score 0)
        score-max (get game-state :score-max 350)
        moves (get game-state :moves 0)
        rank (player-rank score)]
    (-> game-state
        (utils/tell (str "Your score is " score " (total of " score-max " points), in "))
        (utils/tell (str moves (if (= moves 1) " move." " moves.")))
        (utils/tell (str "\nThis gives you the rank of " rank ".")))))

(defn score-upd
  "Update the player's score by a given amount.

   ZIL: SCORE-UPD in gverbs.zil
     <ROUTINE SCORE-UPD (NUM)
       <SETG BASE-SCORE <+ ,BASE-SCORE .NUM>>
       <SETG SCORE <+ ,SCORE .NUM>>
       ...>

   Returns the updated game-state."
  [game-state amount]
  (let [new-base-score (+ (get game-state :base-score 0) amount)
        new-score (+ (get game-state :score 0) amount)
        ;; Check for winning condition (score reaches max)
        score-max (get game-state :score-max 350)
        won? (and (= new-score score-max)
                  (not (:won game-state)))]
    (cond-> game-state
      true (assoc :base-score new-base-score)
      true (assoc :score new-score)
      won? (assoc :won true)
      won? (utils/tell "\nAn almost inaudible voice whispers in your ear, \"Look to your treasures for the final secret.\""))))

(defn score-obj
  "Score an object's treasure value (if any) and set its value to 0.

   ZIL: SCORE-OBJ in gverbs.zil
     <ROUTINE SCORE-OBJ (OBJ \"AUX\" TEMP)
       <COND (<G? <SET TEMP <GETP .OBJ ,P?VALUE>> 0>
              <SCORE-UPD .TEMP>
              <PUTP .OBJ ,P?VALUE 0>)>>

   Objects can only be scored once - after scoring, their :value is set to 0.
   Returns the updated game-state."
  [game-state obj-id]
  (let [value (get-in game-state [:objects obj-id :value] 0)]
    (if (pos? value)
      (-> game-state
          (score-upd value)
          (assoc-in [:objects obj-id :value] 0))
      game-state)))

;;; ---------------------------------------------------------------------------
;;; QUIT COMMAND
;;; ---------------------------------------------------------------------------
;;; ZIL: V-QUIT in gverbs.zil

(defn- yes?
  "Check if response is affirmative (Y, YES, y, yes).

   ZIL: YES? built-in routine"
  [response]
  (when response
    (let [r (clojure.string/lower-case (clojure.string/trim response))]
      (or (= r "y") (= r "yes")))))

;; Dynamic var to allow testing without actual readline
(def ^:dynamic *read-input-fn*
  "Function used to read user input. Can be rebound for testing."
  nil)

(defn- read-quit-confirmation
  "Read confirmation from user. Returns the response string."
  []
  (if *read-input-fn*
    (*read-input-fn*)
    ;; Use readline if available, otherwise standard input
    (do
      (print "Do you wish to leave the game? (Y is affirmative): ")
      (flush)
      (read-line))))

(defn v-quit
  "Quit the game after showing score and asking for confirmation.

   ZIL: V-QUIT in gverbs.zil
     <ROUTINE V-QUIT (\"AUX\" SCOR)
       <V-SCORE>
       <TELL \"Do you wish to leave the game? (Y is affirmative): \">
       <COND (<YES?>
              <QUIT>)
             (ELSE <TELL \"Ok.\" CR>)>>"
  [game-state]
  ;; First show the score (like ZIL does)
  (let [gs (v-score game-state)
        response (read-quit-confirmation)]
    (if (yes? response)
      (-> gs
          (utils/tell "\n")
          (assoc :quit true))
      (utils/tell gs "\nOk."))))

;;; ---------------------------------------------------------------------------
;;; OPEN COMMAND
;;; ---------------------------------------------------------------------------
;;; ZIL: V-OPEN in gverbs.zil

(defn- openable?
  "Returns true if the object can be opened (is a container or door)."
  [obj]
  (let [flags (or (:flags obj) #{})]
    (or (contains? flags :cont)
        (contains? flags :door))))

(defn- already-open?
  "Returns true if the object is already open."
  [obj]
  (contains? (or (:flags obj) #{}) :open))

(defn- add-flag
  "Add a flag to an object's flag set in game-state."
  [game-state obj-id flag]
  (let [current-flags (get-in game-state [:objects obj-id :flags] #{})]
    (assoc-in game-state [:objects obj-id :flags] (conj current-flags flag))))

(defn- container?
  "Returns true if the object is a container."
  [obj]
  (contains? (or (:flags obj) #{}) :cont))

(defn- transparent?
  "Returns true if the object is transparent."
  [obj]
  (contains? (or (:flags obj) #{}) :trans))

(defn- describe-contents
  "Describe the contents of a container that was just opened."
  [game-state obj-id]
  (let [contents (gs/get-contents game-state obj-id)
        visible (remove (fn [id]
                          (let [obj (gs/get-thing game-state id)
                                flags (or (:flags obj) #{})]
                            (contains? flags :invisible)))
                        contents)]
    (if (empty? visible)
      ""
      (let [descriptions (map (fn [id]
                                (let [obj (gs/get-thing game-state id)]
                                  (str "a " (:desc obj))))
                              visible)]
        (clojure.string/join ", " descriptions)))))

;;; ---------------------------------------------------------------------------
;;; TAKE COMMAND
;;; ---------------------------------------------------------------------------
;;; ZIL: V-TAKE and ITAKE in gverbs.zil, PRE-TAKE for preconditions

(def ^:private yuks
  "Humorous responses for trying to take non-takeable objects.
   ZIL: YUKS global in gverbs.zil"
  ["A valiant attempt."
   "You can't be serious."
   "An interesting idea..."
   "What a concept!"])

(defn- takeable?
  "Returns true if the object has the :take flag."
  [obj]
  (contains? (or (:flags obj) #{}) :take))

(defn- in-closed-container?
  "Returns true if the object is inside a closed container."
  [game-state obj-id]
  (let [loc-id (gs/get-thing-loc-id game-state obj-id)
        loc (gs/get-thing game-state loc-id)
        loc-flags (or (:flags loc) #{})]
    (and (contains? loc-flags :cont)
         (not (contains? loc-flags :open)))))

(defn- move-to-inventory
  "Move an object to the winner's (player's) inventory."
  [game-state obj-id]
  (let [winner (:winner game-state)]
    (assoc-in game-state [:objects obj-id :in] winner)))

(defn- already-holding?
  "Returns true if the player is already holding the object."
  [game-state obj-id]
  (let [winner (:winner game-state)
        obj-loc (gs/get-thing-loc-id game-state obj-id)]
    (= obj-loc winner)))

(defn v-take
  "Take an object and add it to inventory.

   ZIL: V-TAKE in gverbs.zil (line 1398)
     <ROUTINE V-TAKE ()
       <COND (<EQUAL? <ITAKE> T>
              <COND (<FSET? ,PRSO ,WEARBIT>
                     <TELL \"You are now wearing the \" D ,PRSO \".\" CR>)
                    (T
                     <TELL \"Taken.\" CR>)>)>>

   PRE-TAKE checks (line 1369):
   - Already holding: \"You already have that!\"
   - In closed container: \"You can't reach something that's inside a closed container.\"

   ITAKE checks (line 1916):
   - Not takeable (no TAKEBIT): pick from YUKS
   - In closed container: fail silently
   - Too heavy: \"Your load is too heavy\"
   - Too many items: \"You're holding too many things already!\"
   - Success: move to inventory, set TOUCHBIT"
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        obj (gs/get-thing game-state prso)
        desc (:desc obj)]
    (cond
      ;; Already holding it
      (already-holding? game-state prso)
      (utils/tell game-state "You already have that!")

      ;; In a closed container - can't reach it
      (in-closed-container? game-state prso)
      (utils/tell game-state "You can't reach something that's inside a closed container.")

      ;; Not takeable - respond with humor
      (not (takeable? obj))
      (utils/tell game-state (rand-nth yuks))

      ;; Success - take the object
      :else
      (let [state (-> game-state
                      (move-to-inventory prso)
                      (add-flag prso :touch))]
        (utils/tell state "Taken.")))))

;;; ---------------------------------------------------------------------------
;;; READ COMMAND
;;; ---------------------------------------------------------------------------
;;; ZIL: V-READ in gverbs.zil

(defn- readable?
  "Returns true if the object has the :read flag."
  [obj]
  (contains? (or (:flags obj) #{}) :read))

(defn v-read
  "Read text on an object.

   ZIL: V-READ in gverbs.zil (line 1159)
     <ROUTINE V-READ ()
       <COND (<NOT <FSET? ,PRSO ,READBIT>>
              <TELL \"How does one read a \" D ,PRSO \"?\" CR>)
             (T
              <TELL <GETP ,PRSO ,P?TEXT> CR>)>>"
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        obj (gs/get-thing game-state prso)
        desc (:desc obj)]
    (cond
      ;; Not readable
      (not (readable? obj))
      (utils/tell game-state (str "How does one read a " desc "?"))

      ;; Readable - print the text
      :else
      (if-let [text (:text obj)]
        (utils/tell game-state text)
        (utils/tell game-state "There's nothing written on it.")))))

;;; ---------------------------------------------------------------------------
;;; DROP COMMAND
;;; ---------------------------------------------------------------------------
;;; ZIL: V-DROP and IDROP in gverbs.zil

(defn- carrying?
  "Returns true if the player (winner) is carrying the object,
   either directly or in a container they're holding."
  [game-state obj-id]
  (let [winner (:winner game-state)
        obj-loc (gs/get-thing-loc-id game-state obj-id)]
    (or (= obj-loc winner)
        ;; Check if in a container the winner is holding
        (when obj-loc
          (= (gs/get-thing-loc-id game-state obj-loc) winner)))))

(defn- drop-to-room
  "Move an object to the current room."
  [game-state obj-id]
  (let [here (:here game-state)]
    (assoc-in game-state [:objects obj-id :in] here)))

(defn v-drop
  "Drop an object from inventory.

   ZIL: V-DROP in gverbs.zil (line 495)
     <ROUTINE V-DROP ()
       <COND (<IDROP>
              <TELL \"Dropped.\" CR>)>>

   IDROP (line 1982):
   - Check if not carrying: \"You're not carrying the X.\"
   - Check if in closed container: \"The X is closed.\"
   - Move to current room"
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        obj (gs/get-thing game-state prso)
        desc (:desc obj)]
    (cond
      ;; Not carrying it
      (not (carrying? game-state prso))
      (utils/tell game-state (str "You're not carrying the " desc "."))

      ;; Success - drop the object
      :else
      (let [state (drop-to-room game-state prso)]
        (utils/tell state "Dropped.")))))

;;; ---------------------------------------------------------------------------
;;; LOOK-INSIDE COMMAND
;;; ---------------------------------------------------------------------------
;;; ZIL: V-LOOK-INSIDE in gverbs.zil

(defn v-look-inside
  "Look inside an object (container or door).

   ZIL: V-LOOK-INSIDE in gverbs.zil (line 881)
     <ROUTINE V-LOOK-INSIDE ()
       <COND (<FSET? ,PRSO ,DOORBIT>
              <COND (<FSET? ,PRSO ,OPENBIT>
                     <TELL \"The \" D ,PRSO \" is open, but I can't tell what's beyond it.\">)
                    (T
                     <TELL \"The \" D ,PRSO \" is closed.\">)>
              <CRLF>)
             (<FSET? ,PRSO ,CONTBIT>
              <COND (<FSET? ,PRSO ,ACTORBIT>
                     <TELL \"There is nothing special to be seen.\" CR>)
                    (<SEE-INSIDE? ,PRSO>
                     <COND (<AND <FIRST? ,PRSO> <PRINT-CONT ,PRSO>>
                            <RTRUE>)
                           (T
                            <TELL \"The \" D ,PRSO \" is empty.\" CR>)>)
                    (T
                     <TELL \"The \" D ,PRSO \" is closed.\" CR>)>)
             (T
              <TELL \"You can't look inside a \" D ,PRSO \".\" CR>)>>"
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        obj (gs/get-thing game-state prso)
        desc (:desc obj)
        flags (or (:flags obj) #{})]
    (cond
      ;; Door
      (contains? flags :door)
      (if (contains? flags :open)
        (utils/tell game-state (str "The " desc " is open, but I can't tell what's beyond it."))
        (utils/tell game-state (str "The " desc " is closed.")))

      ;; Container
      (contains? flags :cont)
      (cond
        ;; Actor (NPC holding things) - special message
        (contains? flags :actor)
        (utils/tell game-state "There is nothing special to be seen.")

        ;; Can see inside (open or transparent)
        (or (contains? flags :open) (contains? flags :trans))
        (let [contents (gs/get-contents game-state prso)
              visible (remove (fn [id]
                                (let [o (gs/get-thing game-state id)
                                      oflags (or (:flags o) #{})]
                                  (contains? oflags :invisible)))
                              contents)]
          (if (empty? visible)
            (utils/tell game-state (str "The " desc " is empty."))
            ;; Print contents
            (let [content-strs (map (fn [id]
                                      (let [o (gs/get-thing game-state id)]
                                        (str "  A " (:desc o))))
                                    visible)]
              (-> game-state
                  (utils/tell (str "The " desc " contains:\n"))
                  (utils/tell (clojure.string/join "\n" content-strs))))))

        ;; Closed container
        :else
        (utils/tell game-state (str "The " desc " is closed.")))

      ;; Not a container or door
      :else
      (utils/tell game-state (str "You can't look inside a " desc ".")))))

;;; ---------------------------------------------------------------------------
;;; EXAMINE COMMAND
;;; ---------------------------------------------------------------------------
;;; ZIL: V-EXAMINE in gverbs.zil

(defn v-examine
  "Examine an object to learn more about it.

   ZIL: V-EXAMINE in gverbs.zil (line 639)
     <ROUTINE V-EXAMINE ()
       <COND (<GETP ,PRSO ,P?TEXT>
              <TELL <GETP ,PRSO ,P?TEXT> CR>)
             (<OR <FSET? ,PRSO ,CONTBIT>
                  <FSET? ,PRSO ,DOORBIT>>
              <V-LOOK-INSIDE>)
             (T
              <TELL \"There's nothing special about the \" D ,PRSO \".\" CR>)>>"
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        obj (gs/get-thing game-state prso)
        desc (:desc obj)
        flags (or (:flags obj) #{})]
    (cond
      ;; Has text property - show it
      (:text obj)
      (utils/tell game-state (:text obj))

      ;; Container or door - look inside
      (or (contains? flags :cont) (contains? flags :door))
      (v-look-inside game-state)

      ;; Default - nothing special
      :else
      (utils/tell game-state (str "There's nothing special about the " desc ".")))))

;;; ---------------------------------------------------------------------------
;;; OPEN COMMAND
;;; ---------------------------------------------------------------------------
;;; ZIL: V-OPEN in gverbs.zil

(defn v-open
  "Open a container or door.

   ZIL: V-OPEN in gverbs.zil
     <ROUTINE V-OPEN ()
       <COND (<FSET? ,PRSO ,CONTBIT>
              <COND (<FSET? ,PRSO ,OPENBIT>
                     <TELL \"It is already open.\" CR>)
                    (T
                     <FSET ,PRSO ,OPENBIT>
                     <FSET ,PRSO ,TOUCHBIT>
                     ...)>)
             (<FSET? ,PRSO ,DOORBIT>
              <COND (<FSET? ,PRSO ,OPENBIT>
                     <TELL \"It is already open.\" CR>)
                    (T
                     <FSET ,PRSO ,OPENBIT>
                     <TELL \"The \" D ,PRSO \" opens.\" CR>)>)
             (T
              <TELL \"You must tell me how to do that to a \" D ,PRSO \".\" CR>)>>"
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        obj (gs/get-thing game-state prso)
        desc (:desc obj)
        action-fn (:action obj)]
    ;; First try the object's action handler (like kitchen window)
    (if-let [result (when action-fn (action-fn game-state))]
      result
      ;; Object didn't handle it - default behavior
      (cond
        ;; Not openable
        (not (openable? obj))
        (utils/tell game-state (str "You must tell me how to do that to a " desc "."))

        ;; Already open
        (already-open? obj)
        (utils/tell game-state "It is already open.")

        ;; Container
        (container? obj)
        (let [state (-> game-state
                        (add-flag prso :open)
                        (add-flag prso :touch))
              contents (gs/get-contents state prso)
              visible (remove (fn [id]
                                (let [o (gs/get-thing state id)
                                      flags (or (:flags o) #{})]
                                  (contains? flags :invisible)))
                              contents)]
          (if (or (empty? visible) (transparent? obj))
            (utils/tell state "Opened.")
            (let [content-desc (describe-contents state prso)]
              (utils/tell state (str "Opening the " desc " reveals " content-desc ".")))))

        ;; Door
        :else
        (let [state (add-flag game-state prso :open)]
          (utils/tell state (str "The " desc " opens.")))))))

;;; ---------------------------------------------------------------------------
;;; MOVEMENT COMMANDS
;;; ---------------------------------------------------------------------------
;;; ZIL: V-WALK, DO-WALK, GOTO in gverbs.zil

(defn- get-exit
  "Get the exit definition for a direction from the current room.
   Returns nil if no exit, a keyword for room destination, or a string for blocked message."
  [game-state direction]
  (let [here (:here game-state)
        room (gs/get-thing game-state here)]
    (get-in room [:exits direction])))

(defn- room-lit?
  "Check if a room is lit (simple version for movement).

   A room is lit if it has the :lit flag in its :flags set.
   This is a simplified check - the full LIT? in validation.clj
   also searches for light sources, but that creates a cyclic dependency.

   Note: parser/validation.clj has a more complete lit? that also
   checks for carried light sources. That version is used during parsing."
  [game-state room-id]
  (let [room (gs/get-thing game-state room-id)
        room-flags (or (:flags room) #{})]
    (or (contains? room-flags :lit)
        (contains? room-flags :on)
        (gs/flag? game-state :rooms room-id :lit)
        ;; Also check for carried light source (simple check)
        (let [winner (:winner game-state)
              contents (gs/get-contents game-state winner)]
          (some (fn [obj-id]
                  (let [obj (gs/get-thing game-state obj-id)
                        obj-flags (or (:flags obj) #{})]
                    (and (contains? obj-flags :light)
                         (contains? obj-flags :on))))
                contents)))))

(defn- goto
  "Move the player to a new room and describe it.

   ZIL: GOTO routine in gverbs.zil (lines 2061-2110)
   - Move winner to new room
   - Update HERE global
   - Update LIT flag
   - Call V-FIRST-LOOK to describe room

   Returns updated game-state."
  [game-state room-id]
  (let [winner (:winner game-state)
        ;; Move the winner to the new room
        gs (assoc-in game-state [:objects winner :in] room-id)
        ;; Update HERE
        gs (assoc gs :here room-id)
        ;; Update LIT flag
        gs (assoc gs :lit (room-lit? gs room-id))]
    ;; Describe the room (V-FIRST-LOOK)
    (verbs-look/v-look gs)))

(defn v-walk
  "Move in a direction.

   ZIL: V-WALK in gverbs.zil (lines 1537-1597)
   Handles various exit types:
   - Simple exit: (NORTH TO KITCHEN) -> move to room
   - Blocked exit: (EAST \"The door is boarded.\") -> print message
   - Conditional exit: (SW TO BARROW IF WON-FLAG) -> check flag
   - Door exit: (WEST TO ROOM IF DOOR IS OPEN) -> check door

   For now we implement simple and blocked exits."
  [game-state]
  (let [;; Get direction from parser (stored in prso as a keyword)
        prso (parser-state/get-prso game-state)
        direction (if (keyword? prso) prso prso)]
    (cond
      ;; No direction specified
      (nil? direction)
      (utils/tell game-state "You must specify a direction to go.")

      :else
      (let [exit (get-exit game-state direction)]
        (cond
          ;; No exit in that direction
          (nil? exit)
          (utils/tell game-state "You can't go that way.")

          ;; Exit is a string (blocked message)
          (string? exit)
          (utils/tell game-state exit)

          ;; Exit is a map with conditions
          (map? exit)
          (let [{:keys [to if door]} exit]
            (cond
              ;; Conditional on a flag
              (and if (not (get game-state if)))
              (if-let [else-msg (:else exit)]
                (utils/tell game-state else-msg)
                (utils/tell game-state "You can't go that way."))

              ;; Conditional on a door being open
              (and door (not (contains? (or (:flags (gs/get-thing game-state door)) #{}) :open)))
              (let [door-obj (gs/get-thing game-state door)
                    door-desc (:desc door-obj)]
                (utils/tell game-state (str "The " door-desc " is closed.")))

              ;; All conditions met - go to the room
              :else
              (goto game-state to)))

          ;; Exit is a keyword (simple room destination)
          (keyword? exit)
          (goto game-state exit)

          ;; Unknown exit type
          :else
          (utils/tell game-state "You can't go that way."))))))

;;; ---------------------------------------------------------------------------
;;; THROUGH COMMAND
;;; ---------------------------------------------------------------------------
;;; ZIL: V-THROUGH in gverbs.zil
;;;   <ROUTINE V-THROUGH ("OPTIONAL" (OBJ <>) "AUX" M)
;;;     <COND (<AND <FSET? ,PRSO ,DOORBIT>
;;;                 <SET M <OTHER-SIDE ,PRSO>>>
;;;            <DO-WALK .M>
;;;            <RTRUE>)
;;;           ...>>
;;;
;;; Handles "enter X", "go through X", "walk in X" etc.
;;; First calls object's action handler; if not handled, falls back to
;;; door logic (walk to other side if DOORBIT set).

(defn v-through
  "Go through a door, window, or other passageway.

   ZIL: V-THROUGH in gverbs.zil

   First tries the object's action handler. If the object handles the verb,
   we're done. Otherwise, if the object has :door flag, try to walk through
   to the other side."
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        obj (gs/get-thing game-state prso)
        desc (:desc obj)
        flags (or (:flags obj) #{})
        action-fn (:action obj)]
    ;; First try the object's action handler
    (if-let [result (when action-fn (action-fn game-state))]
      ;; Object handled the verb
      result
      ;; Object didn't handle it - try default door behavior
      (cond
        ;; Object has door flag - find the other side and walk there
        (contains? flags :door)
        (let [here (:here game-state)
              ;; Find which room has an exit through this door
              rooms (:rooms game-state)
              ;; Look for exits that reference this door
              other-side (some (fn [[room-id room]]
                                 (when (not= room-id here)
                                   (some (fn [[dir exit]]
                                           (when (and (map? exit)
                                                      (= (:door exit) prso))
                                             (:to exit)))
                                         (:exits room))))
                               rooms)]
          (if (and other-side (contains? flags :open))
            (goto game-state other-side)
            (if (not (contains? flags :open))
              (utils/tell game-state (str "The " desc " is closed."))
              (utils/tell game-state "You can't go that way."))))

        ;; Not a door - can't go through it
        :else
        (utils/tell game-state "I don't know how to do that.")))))

;;; ---------------------------------------------------------------------------
;;; WALK-AROUND COMMAND
;;; ---------------------------------------------------------------------------
;;; ZIL: V-WALK-AROUND in gverbs.zil
;;;   <ROUTINE V-WALK-AROUND ()
;;;     <TELL "Use compass directions for movement." CR>>
;;;
;;; Default behavior just tells player to use compass directions.
;;; Objects like WHITE-HOUSE intercept this via their action handlers
;;; to implement "go around house" functionality.

(defn v-walk-around
  "Walk around an object.

   ZIL: V-WALK-AROUND in gverbs.zil

   First tries the object's action handler. If the object handles the verb
   (like the white house cycling through exterior rooms), we're done.
   Otherwise, just tell player to use compass directions."
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        obj (gs/get-thing game-state prso)
        action-fn (:action obj)]
    ;; First try the object's action handler
    (if-let [result (when action-fn (action-fn game-state))]
      ;; Object handled the verb
      result
      ;; Object didn't handle it - default message
      (utils/tell game-state "Use compass directions for movement."))))
