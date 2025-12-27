(ns clork.verbs
  "Verb handler functions."
  (:require [clork.utils :as utils]
            [clork.game-state :as gs]))

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
  (let [prso (get-in game-state [:parser :prso])
        obj (gs/get-thing game-state prso)
        desc (:desc obj)]
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
        (utils/tell state (str "The " desc " opens."))))))
