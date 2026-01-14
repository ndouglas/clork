(ns clork.verbs-health
  "Health, scoring, and quit command handlers.

   ZIL Reference: V-DIAGNOSE, V-SCORE, V-QUIT in gverbs.zil and 1actions.zil."
  (:require [clork.utils :as utils]
            [clork.game-state :as gs]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))

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
  (if (zero? amount)
    game-state  ; No change
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
        won? (utils/tell "\nAn almost inaudible voice whispers in your ear, \"Look to your treasures for the final secret.\"")))))

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

(defn score-tvalue
  "Score an object's trophy case value (if any) and set its tvalue to 0.

   This is called when putting treasures in the trophy case. The :tvalue
   property represents additional points awarded for depositing treasures
   in the case (separate from the :value awarded on first interaction).

   Objects can only be scored once - after scoring, their :tvalue is set to 0.
   Returns the updated game-state."
  [game-state obj-id]
  (let [tvalue (get-in game-state [:objects obj-id :tvalue] 0)]
    (if (pos? tvalue)
      (-> game-state
          (score-upd tvalue)
          (assoc-in [:objects obj-id :tvalue] 0))
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
;;; RESTART COMMAND
;;; ---------------------------------------------------------------------------
;;; ZIL: V-RESTART in gverbs.zil

(defn- read-restart-confirmation
  "Read confirmation from user for restart. Returns the response string."
  []
  (if *read-input-fn*
    (*read-input-fn*)
    (do
      (print "Do you wish to restart? (Y is affirmative): ")
      (flush)
      (read-line))))

(defn v-restart
  "Restart the game after showing score and asking for confirmation.

   ZIL: V-RESTART in gverbs.zil
     <ROUTINE V-RESTART ()
       <V-SCORE T>
       <TELL \"Do you wish to restart? (Y is affirmative): \">
       <COND (<YES?>
              <TELL \"Restarting.\" CR>
              <RESTART>
              <TELL \"Failed.\" CR>)>>

   In our implementation, sets :restart flag which main-loop uses to
   restore from the initial game state stored in :restart-state."
  [game-state]
  ;; First show the score (like ZIL does with <V-SCORE T>)
  (let [gs (v-score game-state)
        response (read-restart-confirmation)]
    (if (yes? response)
      (-> gs
          (utils/tell "\nRestarting.")
          (assoc :restart true))
      (utils/tell gs "\nOk."))))

;;; ---------------------------------------------------------------------------
;;; SAVE/RESTORE COMMANDS
;;; ---------------------------------------------------------------------------
;;; ZIL: V-SAVE, V-RESTORE in gverbs.zil

(def ^:private default-save-file "clork.sav")

(defn- read-save-filename
  "Read save filename from user. Returns the response string or default."
  [prompt default]
  (if *read-input-fn*
    (let [response (*read-input-fn*)]
      (if (clojure.string/blank? response)
        default
        response))
    (do
      (print prompt)
      (print (str " [" default "]: "))
      (flush)
      (let [response (read-line)]
        (if (clojure.string/blank? response)
          default
          (clojure.string/trim response))))))

(defn- remove-functions
  "Recursively remove function values from a map.
   Functions (like :action handlers on objects) can't be serialized."
  [m]
  (cond
    (map? m)
    (reduce-kv (fn [acc k v]
                 (if (fn? v)
                   acc  ; Skip functions
                   (assoc acc k (remove-functions v))))
               {}
               m)
    (sequential? m)
    (mapv remove-functions m)
    (set? m)
    (set (map remove-functions m))
    :else m))

(defn- saveable-state
  "Extract the saveable portion of game state.
   Removes transient data like :restart-state to avoid nested copies.
   Removes functions that can't be serialized to EDN."
  [game-state]
  (-> game-state
      (dissoc :restart-state)
      (dissoc :script-config)
      ;; Remove any IO-related state
      (dissoc :transcript-file)
      (dissoc :transcript-writer)
      ;; Remove functions that can't be serialized
      remove-functions))

(defn- save-game-to-file
  "Save game state to file. Returns true on success, false on failure."
  [game-state filename]
  (try
    (let [save-data (saveable-state game-state)
          ;; Convert to EDN with pretty printing
          edn-str (pr-str save-data)]
      (spit filename edn-str)
      true)
    (catch Exception e
      (binding [*out* *err*]
        (println (str "Save failed: " (.getMessage e))))
      false)))

(defn- reattach-handlers
  "Re-attach function handlers from restart-state after restore.

   Functions (like :action handlers on objects/rooms and :handler on daemons)
   can't be serialized to EDN, so they must be re-attached from the canonical
   definitions stored in restart-state after loading a saved game."
  [restored-state restart-state]
  (-> restored-state
      ;; Re-attach object :action handlers
      (update :objects
              (fn [objects]
                (reduce-kv
                 (fn [acc obj-id _obj]
                   (if-let [action (get-in restart-state [:objects obj-id :action])]
                     (assoc-in acc [obj-id :action] action)
                     acc))
                 objects
                 objects)))
      ;; Re-attach room :action handlers
      (update :rooms
              (fn [rooms]
                (reduce-kv
                 (fn [acc room-id _room]
                   (if-let [action (get-in restart-state [:rooms room-id :action])]
                     (assoc-in acc [room-id :action] action)
                     acc))
                 rooms
                 rooms)))
      ;; Re-attach daemon :handler functions
      (update :daemons
              (fn [daemons]
                (reduce-kv
                 (fn [acc daemon-id _daemon]
                   (if-let [handler (get-in restart-state [:daemons daemon-id :handler])]
                     (assoc-in acc [daemon-id :handler] handler)
                     acc))
                 daemons
                 daemons)))))

(defn- restore-game-from-file
  "Restore game state from file. Returns game-state on success, nil on failure."
  [filename current-state]
  (try
    (when (.exists (io/file filename))
      (let [edn-str (slurp filename)
            restored (edn/read-string edn-str)
            restart-state (:restart-state current-state)]
        ;; Merge back in the non-saveable state and re-attach function handlers
        (-> restored
            (assoc :restart-state restart-state)
            (assoc :script-config (:script-config current-state))
            (reattach-handlers restart-state))))
    (catch Exception e
      (binding [*out* *err*]
        (println (str "Restore failed: " (.getMessage e))))
      nil)))

(defn v-save
  "Save the current game state to a file.

   ZIL: V-SAVE in gverbs.zil
     <ROUTINE V-SAVE ()
       <COND (<SAVE>
              <TELL \"Ok.\" CR>)
             (T
              <TELL \"Failed.\" CR>)>>

   Prompts for filename and saves game state as EDN."
  [game-state]
  (let [filename (read-save-filename "Enter save file name" default-save-file)]
    (if (save-game-to-file game-state filename)
      (utils/tell game-state "Ok.")
      (utils/tell game-state "Failed."))))

(defn v-restore
  "Restore game state from a saved file.

   ZIL: V-RESTORE in gverbs.zil
     <ROUTINE V-RESTORE ()
       <COND (<RESTORE>
              <TELL \"Ok.\" CR>
              <V-FIRST-LOOK>)
             (T
              <TELL \"Failed.\" CR>)>>

   Prompts for filename and restores game state from EDN file.
   On success, sets :restored flag which main-loop uses to show room."
  [game-state]
  (let [filename (read-save-filename "Enter restore file name" default-save-file)]
    (if-let [restored-state (restore-game-from-file filename game-state)]
      (-> restored-state
          (utils/tell "Ok.")
          ;; Set flag to trigger v-first-look in main loop
          (assoc :restored true))
      (utils/tell game-state "Failed."))))

;;; ---------------------------------------------------------------------------
;;; SCRIPT/UNSCRIPT COMMANDS
;;; ---------------------------------------------------------------------------
;;; ZIL: V-SCRIPT, V-UNSCRIPT in gverbs.zil

(def ^:private default-script-file "clork.txt")

(defn- read-script-filename
  "Read transcript filename from user. Returns the response string or default."
  [default]
  (if *read-input-fn*
    (let [response (*read-input-fn*)]
      (if (clojure.string/blank? response)
        default
        response))
    (do
      (print (str "Enter transcript file name [" default "]: "))
      (flush)
      (let [response (read-line)]
        (if (clojure.string/blank? response)
          default
          (clojure.string/trim response))))))

(defn v-script
  "Start transcription to a file.

   ZIL: V-SCRIPT in gverbs.zil
     <ROUTINE V-SCRIPT ()
       <PUT 0 8 <BOR <GET 0 8> 1>>
       <TELL \"Here begins a transcript of interaction with\" CR>
       <V-VERSION>
       <RTRUE>>

   Prompts for filename and starts writing all output to the transcript file."
  [game-state]
  (if (utils/script-enabled?)
    (utils/tell game-state "Transcription is already on.")
    (let [filename (read-script-filename default-script-file)]
      (if (utils/start-script! filename)
        (-> game-state
            (utils/tell "Here begins a transcript of interaction with")
            (utils/crlf)
            ;; Call v-version inline (can't forward-reference verbs-meta)
            (utils/tell "ZORK I: The Great Underground Empire\n")
            (utils/tell "Infocom interactive fiction - a fantasy story\n")
            (utils/tell "Copyright (c) 1981, 1982, 1983, 1984, 1985, 1986 Infocom, Inc. All rights reserved.\n")
            (utils/tell "ZORK is a registered trademark of Infocom, Inc.\n")
            (utils/tell "Release 1 / Serial number 1\n")
            (utils/tell "Clojure port by Nathan Douglas\n"))
        (utils/tell game-state "Failed to start transcript.")))))

(defn v-unscript
  "Stop transcription to file.

   ZIL: V-UNSCRIPT in gverbs.zil
     <ROUTINE V-UNSCRIPT ()
       <TELL \"Here ends a transcript of interaction with\" CR>
       <V-VERSION>
       <PUT 0 8 <BAND <GET 0 8> -2>>
       <RTRUE>>

   Closes the transcript file and stops writing output to it."
  [game-state]
  (if (utils/script-enabled?)
    (let [gs (-> game-state
                 (utils/tell "Here ends a transcript of interaction with")
                 (utils/crlf)
                 ;; Call v-version inline
                 (utils/tell "ZORK I: The Great Underground Empire\n")
                 (utils/tell "Infocom interactive fiction - a fantasy story\n")
                 (utils/tell "Copyright (c) 1981, 1982, 1983, 1984, 1985, 1986 Infocom, Inc. All rights reserved.\n")
                 (utils/tell "ZORK is a registered trademark of Infocom, Inc.\n")
                 (utils/tell "Release 1 / Serial number 1\n")
                 (utils/tell "Clojure port by Nathan Douglas\n"))]
      (utils/stop-script!)
      gs)
    (utils/tell game-state "Transcription is not on.")))

;;; ---------------------------------------------------------------------------
;;; VERIFY COMMAND
;;; ---------------------------------------------------------------------------
;;; ZIL: V-VERIFY in gverbs.zil

(defn v-verify
  "Verify the game file integrity.

   ZIL: V-VERIFY in gverbs.zil
     <ROUTINE V-VERIFY ()
       <TELL \"Verifying disk...\" CR>
       <COND (<VERIFY>
              <TELL \"The disk is correct.\" CR>)
             (T
              <TELL CR \"** Disk Failure **\" CR>)>>

   In the original ZIL, this verified the game disk for corruption.
   In our Clojure implementation, we always return success since there's
   no disk to verify."
  [game-state]
  (-> game-state
      (utils/tell "Verifying disk...")
      (utils/tell "\nThe disk is correct.")))
