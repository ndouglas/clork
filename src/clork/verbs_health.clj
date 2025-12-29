(ns clork.verbs-health
  "Health, scoring, and quit command handlers.

   ZIL Reference: V-DIAGNOSE, V-SCORE, V-QUIT in gverbs.zil and 1actions.zil."
  (:require [clork.utils :as utils]
            [clork.game-state :as gs]))

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

(defn fight-strength
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
