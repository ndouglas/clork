(ns clork.parser.output
  "Parser error messages and printing utilities.

   This module handles:
   - UNKNOWN-WORD - \"I don't know the word X\"
   - CANT-USE - \"You used the word X in a way I don't understand\"
   - WORD-PRINT - Print a word from the input buffer
   - THING-PRINT - Print a noun clause
   - BUFFER-PRINT - Print tokens from a range
   - PREP-PRINT - Print a preposition
   - WHICH-PRINT - \"Which X do you mean?\"

   ZIL Reference: gparser.zil
   - Lines 655-686: UNKNOWN-WORD, CANT-USE routines
   - Lines 658-663: WORD-PRINT routine
   - Lines 810-849: THING-PRINT, BUFFER-PRINT routines
   - Lines 851-858: PREP-PRINT routine
   - Lines 1146-1166: WHICH-PRINT routine"
  (:require [clork.utils :as utils]
            [clork.game-state :as game-state]
            [clork.parser.state :as parser-state]
            [clork.parser.lexer :as lexer]))

;;;; ============================================================================
;;;; PARSER OUTPUT - Error Messages and Printing
;;;; ============================================================================
;;;;
;;;; Design Note:
;;;;   All player-facing parser messages are collected here for easy
;;;;   localization and consistency. Error messages should be helpful
;;;;   and match the original Infocom style.
;;;;
;;;; ============================================================================

;;; ---------------------------------------------------------------------------
;;; ERROR MESSAGES
;;; ---------------------------------------------------------------------------

(defn unknown-word
  "Print error for unrecognized word.

   ZIL: UNKNOWN-WORD routine, lines 665-675

   Prints: I don't know the word \"frobnitz\".

   Also sets up OOPS recovery by storing the word position."
  [game-state ptr]
  (let [word (lexer/lexv-word game-state ptr)
        gs (-> game-state
               ;; Store position for OOPS command
               (assoc-in [:parser :oops-ptr] ptr)
               ;; Clear flags
               (assoc-in [:parser :quote-flag] false)
               (assoc-in [:parser :oflag] false))]

    ;; Special case: if we're in SAY mode, just say nothing happens
    (if (= (parser-state/get-prsa gs) :say)
      (utils/tell gs "Nothing happens.\n")
      (utils/tell gs (str "I don't know the word \"" word "\".\n")))))

(defn cant-use
  "Print error for word used in wrong context.

   ZIL: CANT-USE routine, lines 677-686

   Prints: You used the word \"lamp\" in a way that I don't understand.

   This is for words that ARE in the vocabulary but don't make sense
   in the current context."
  [game-state ptr]
  (let [word (lexer/lexv-word game-state ptr)
        gs (-> game-state
               (assoc-in [:parser :quote-flag] false)
               (assoc-in [:parser :oflag] false))]

    (if (= (parser-state/get-prsa gs) :say)
      (utils/tell gs "Nothing happens.\n")
      (utils/tell gs (str "You used the word \"" word
                          "\" in a way that I don't understand.\n")))))

;;; ---------------------------------------------------------------------------
;;; PRINTING UTILITIES
;;; ---------------------------------------------------------------------------

(defn word-print
  "Print a word from the input buffer.

   ZIL: WORD-PRINT routine, lines 658-663

   In ZIL, this reads character by character from the input buffer.
   In Clojure, we can just print the token's word directly."
  [game-state token]
  (if (map? token)
    (print (:word token))
    (print (str token))))

(defn buffer-print
  "Print tokens from a range in the lexv.

   ZIL: BUFFER-PRINT routine, lines 819-849

   Prints a sequence of tokens with spaces between them.
   Handles special cases:
   - Commas print without preceding space
   - 'me' prints the player's description
   - Numbers print as numbers
   - Pronouns may be resolved to their referent"
  [game-state begin-ptr end-ptr include-the?]
  (loop [ptr begin-ptr
         first? true
         printed-noun? false]
    (when (< ptr end-ptr)
      (let [word (lexer/lexv-word game-state ptr)]
        (cond
          ;; Comma - print without space
          (lexer/special-word? word :comma)
          (do
            (print ", ")
            (recur (inc ptr) true printed-noun?))

          ;; Period - skip
          (lexer/special-word? word :period)
          (recur (inc ptr) first? printed-noun?)

          ;; ME - print player description
          (lexer/special-word? word :me)
          (do
            (print (game-state/thing-name game-state (:player game-state)))
            (recur (inc ptr) false true))

          ;; Number
          (= word :intnum)
          (do
            (print (get-in game-state [:parser :number]))
            (recur (inc ptr) false true))

          ;; Regular word
          :else
          (do
            ;; Print space unless first word
            (when (not first?)
              (print " "))
            ;; Print "the" if first word and requested
            (when (and first? (not printed-noun?) include-the?)
              (print "the "))
            ;; Handle IT pronoun - resolve to referenced object if available
            (if (and (lexer/special-word? word :it)
                     (get-in game-state [:parser :it-object]))
              (print (game-state/thing-name game-state (get-in game-state [:parser :it-object])))
              (print word))
            (recur (inc ptr) false false)))))))

(defn thing-print
  "Print a noun clause (direct or indirect object).

   ZIL: THING-PRINT routine, lines 810-817

   Arguments:
     game-state - current state
     prso? - true for direct object (PRSO), false for indirect (PRSI)
     include-the? - whether to prefix with 'the'"
  [game-state prso? include-the?]
  (let [begin-key (if prso? :nc1 :nc2)
        end-key (if prso? :nc1l :nc2l)
        begin-ptr (get-in game-state [:parser :itbl (begin-key parser-state/itbl-indices)])
        end-ptr (get-in game-state [:parser :itbl (end-key parser-state/itbl-indices)])]
    (buffer-print game-state begin-ptr end-ptr include-the?)))

(defn prep-print
  "Print a preposition word.

   ZIL: PREP-PRINT routine, lines 851-858

   Looks up the preposition number and prints its word form."
  [game-state prep]
  (when (some? prep)
    (print " ")
    ;; prep is now a keyword like :around, :in
    (print (name prep))))

;;; ---------------------------------------------------------------------------
;;; DISAMBIGUATION
;;; ---------------------------------------------------------------------------

(defn which-print
  "Print disambiguation prompt for ambiguous objects.

   ZIL: WHICH-PRINT routine, lines 1146-1166

   Prints: Which lamp do you mean, the brass lantern or the broken lamp?

   Arguments:
     game-state - current state
     start-idx - starting index in match table
     count - number of ambiguous objects
     match-table - the table with ambiguous matches (:prso or :prsi)"
  [game-state start-idx count match-table-key]
  (let [match-table (get-in game-state [:parser match-table-key] [])
        nam (get-in game-state [:parser :nam])
        adj (get-in game-state [:parser :adjn])
        oflag? (get-in game-state [:parser :oflag])
        merged? (get-in game-state [:parser :merged])
        and-flag? (get-in game-state [:parser :and-flag])]

    ;; Print "Which X do you mean, "
    (print "Which ")
    (cond
      ;; In orphan/merged mode, print the word literally
      (or oflag? merged? and-flag?)
      (print (or nam adj "one"))

      ;; Otherwise, print the noun clause
      :else
      (thing-print game-state (= match-table-key :prso) false))

    (print " do you mean, ")

    ;; List the options
    (loop [remaining count
           idx start-idx]
      (when (pos? remaining)
        (let [obj-id (nth match-table idx nil)]
          (when obj-id
            (print "the ")
            (print (game-state/thing-name game-state obj-id))
            (cond
              (= remaining 2)
              (do
                (when (> count 2) (print ","))
                (print " or "))
              (> remaining 2)
              (print ", ")))
          (recur (dec remaining) (inc idx)))))

    (println "?")))

;;; ---------------------------------------------------------------------------
;;; STANDARD PARSER MESSAGES
;;; ---------------------------------------------------------------------------

(def parser-messages
  "Standard parser error messages, matching Infocom style."
  {:beg-pardon       "I beg your pardon?"
   :no-verb          "There was no verb in that sentence!"
   :missing-noun     "There seems to be a noun missing in that sentence!"
   :bad-syntax       "That sentence isn't one I recognize."
   :too-many-nouns   "There were too many nouns in that sentence."
   :oops-nothing     "There was no word to replace!"
   :oops-clumsy      "I can't help your clumsiness."
   :oops-quote       "Sorry, you can't correct mistakes in quoted text."
   :oops-first       "Warning: only the first word after OOPS is used."
   :again-no-cmd     "Beg pardon?"
   :again-fragment   "It's difficult to repeat fragments."
   :again-mistake    "That would just repeat a mistake."
   :again-confused   "I couldn't understand that sentence."
   :too-dark         "It's too dark to see!"
   :dont-have        "You don't have that!"
   :not-here         "You can't see any such thing."
   :need-object      "You can't see any such thing."
   :cant-orphan      "\"I don't understand! What are you referring to?\""})

(defn parser-say
  "Print a standard parser message."
  [game-state msg-key]
  (utils/tell game-state (str (get parser-messages msg-key
                                   "Something went wrong.") "\n")))

;;; ---------------------------------------------------------------------------
;;; PROMPT GENERATION
;;; ---------------------------------------------------------------------------

(defn orphan-prompt
  "Generate a prompt for the missing object in orphan mode.

   Returns a string like 'What do you want to take?'
   or 'What do you want to put in the case?'"
  [game-state drive1 drive2]
  (let [verb-word (or (get-in game-state [:parser :vtbl 0])
                      (get-in game-state [:parser :ovtbl 0])
                      "do")]
    (str "What do you want to "
         ;; Handle special case where verb-word is 0 (TELL command)
         (if (zero? verb-word) "tell" verb-word)
         ;; Add object if this is for second object
         (when drive2
           (str " " (thing-print game-state true true)))
         ;; Add preposition
         (when-let [prep (if drive1
                           (:prep1 drive1)
                           (when drive2 (:prep2 drive2)))]
           (str " " (name prep)))
         "?")))
