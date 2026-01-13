(ns clork.parser.clause
  "Noun phrase parsing for the parser.

   This module handles:
   - CLAUSE routine - The main noun phrase parser
   - Handling of adjectives, nouns, articles
   - Object lists with AND/COMMA
   - BUT/EXCEPT exclusions

   ZIL Reference: gparser.zil
   - Lines 440-510: CLAUSE routine"
  (:require [clork.parser.state :as parser-state]
            [clork.parser.lexer :as lexer]))

;;;; ============================================================================
;;;; PARSER CLAUSE - Noun Phrase Parsing
;;;; ============================================================================
;;;;
;;;; What is a Noun Clause?
;;;;   A noun clause is the part of a sentence that refers to an object.
;;;;   Examples:
;;;;     "the brass lantern"     -> adjective(brass) + noun(lantern)
;;;;     "rusty knife"           -> adjective(rusty) + noun(knife)
;;;;     "all"                   -> special (take everything)
;;;;     "sword and shield"      -> two objects joined by AND
;;;;     "all but the lamp"      -> everything except one object
;;;;
;;;; How CLAUSE Works:
;;;;   Starting at PTR, scan words until we hit:
;;;;   - A sentence break (period, comma, THEN)
;;;;   - A preposition that starts a new clause
;;;;   - End of input
;;;;
;;;;   Along the way, collect:
;;;;   - The starting position (stored in P-NC1 or P-NC2)
;;;;   - The ending position (stored in P-NC1L or P-NC2L)
;;;;   - Handle AND/BUT to chain multiple objects
;;;;
;;;; ============================================================================

;;; ---------------------------------------------------------------------------
;;; CLAUSE PARSING
;;; ---------------------------------------------------------------------------

(defn clause
  "Parse a noun clause starting at PTR.

   ZIL: CLAUSE routine, lines 440-510
     <ROUTINE CLAUSE (PTR VAL WRD \"AUX\" OFF NUM (ANDFLG <>) (FIRST?? T) NW (LW 0))
       ...>

   Arguments:
     game-state - current game state
     ptr - index into lexv where clause starts
     val - preposition value (if clause was preceded by a preposition)
     wrd - the preposition word (if any)

   Returns:
     Updated game-state with clause info stored in :parser :itbl,
     and new ptr position. Returns {:ptr -1} if end of input reached.

   The clause boundaries are stored in ITBL:
     P-NC1/P-NC1L for the first (direct object) clause
     P-NC2/P-NC2L for the second (indirect object) clause

   Example parsing 'the brass lantern':
     - Skip 'the' (article)
     - Note 'brass' as adjective
     - Note 'lantern' as noun
     - Store start/end positions in ITBL"

  [game-state ptr val wrd]

  ;; Calculate offset for storing results:
  ;; First clause (P-NCN=1) uses NC1/NC1L
  ;; Second clause (P-NCN=2) uses NC2/NC2L
  (let [ncn (parser-state/get-ncn game-state)
        off (* (dec ncn) 2)  ; 0 for first clause, 2 for second

        ;; If we have a preposition, store it
        ;; val is the preposition keyword (like :around, :in) or nil
        gs-with-prep
        (if (some? val)
          (-> game-state
              ;; Store prep value at PREP1 or PREP2
              (parser-state/set-itbl (+ (:prep1 parser-state/itbl-indices) off) val)
              ;; Store prep word at PREP1N or PREP2N
              (parser-state/set-itbl (+ (:prep1n parser-state/itbl-indices) off) wrd)
              ;; Advance past the preposition - use assoc-in since :ptr might be nil
              (assoc-in [:parser :ptr] (inc ptr)))
          ;; No prep, restore P-LEN since we didn't consume a word
          (parser-state/inc-len game-state))

        ;; Check if we're at end of input
        p-len (parser-state/get-len gs-with-prep)]

    (if (zero? p-len)
      ;; Empty clause (e.g., "take" with no object)
      {:game-state (parser-state/dec-ncn gs-with-prep)
       :ptr -1}

      ;; Start parsing the clause
      ;; Store the starting position
      ;; If we had a preposition, start AFTER it (inc ptr), otherwise at ptr
      (let [clause-start (if (some? val) (inc ptr) ptr)
            nc-slot (+ (:nc1 parser-state/itbl-indices) off)
            gs-with-start (parser-state/set-itbl gs-with-prep nc-slot clause-start)]

        ;; Skip leading articles (the, a, an)
        ;; ZIL: <COND (<EQUAL? <GET ,P-LEXV .PTR> ,W?THE ,W?A ,W?AN> ...)>
        (loop [gs gs-with-start
               current-ptr clause-start
               and-flag false   ; Have we seen AND/COMMA?
               first? true      ; Is this the first word?
               last-word nil]   ; Previous word

          (let [remaining (parser-state/get-len gs)]
            (if (neg? (dec remaining))
              ;; End of input - store end position and return
              (let [end-slot (+ (:nc1l parser-state/itbl-indices) off)
                    gs-final (-> gs
                                 (parser-state/set-itbl end-slot current-ptr)
                                 (parser-state/set-len remaining))]
                {:game-state gs-final
                 :ptr -1})

              ;; Get current word
              (let [current-word (lexer/lexv-word gs current-ptr)
                    next-word (when (pos? remaining)
                                (lexer/lexv-word gs (inc current-ptr)))

                    ;; Check word type
                    gs-decremented (parser-state/dec-len gs)

                    ;; Handle the word
                    result
                    (cond
                      ;; AND or COMMA - marks object list
                      (or (lexer/special-word? current-word :and)
                          (lexer/special-word? current-word :comma))
                      {:action :continue
                       :and-flag true}

                      ;; ALL/ONE - special quantifiers
                      (or (lexer/special-word? current-word :all)
                          (lexer/special-word? current-word :one))
                      (if (lexer/special-word? next-word :of)
                        ;; "all of the" - skip the "of"
                        {:action :skip-next}
                        {:action :continue})

                      ;; BUT/EXCEPT - exclusion marker in "all but X" patterns
                      ;; These are handled by snarfem, so we just continue here
                      (or (lexer/special-word? current-word :but)
                          (lexer/special-word? current-word :except))
                      {:action :continue}

                      ;; THEN or PERIOD - end of clause
                      (or (lexer/special-word? current-word :then)
                          (lexer/special-word? current-word :period))
                      {:action :end-clause}

                      ;; Preposition - might end clause if not first word
                      (lexer/wt? current-word :preposition)
                      (if (and (not first?)
                               (parser-state/get-itbl gs :verb))
                        {:action :end-clause-backup}  ; Back up and let main loop handle
                        {:action :continue})

                      ;; Object word - potential end of clause
                      (lexer/wt? current-word :object)
                      (cond
                        ;; "X of Y" - continue parsing
                        (and (pos? remaining)
                             (lexer/special-word? next-word :of)
                             (not (lexer/special-word? current-word :all))
                             (not (lexer/special-word? current-word :one)))
                        {:action :continue}

                        ;; Adjective that's also a noun, followed by another noun
                        (and (lexer/wt? current-word :adjective)
                             next-word
                             (lexer/wt? next-word :object))
                        {:action :continue}

                        ;; End of object reference (no AND, not followed by BUT)
                        (and (not and-flag)
                             (not (lexer/special-word? next-word :but))
                             (not (lexer/special-word? next-word :except))
                             (not (lexer/special-word? next-word :and))
                             (not (lexer/special-word? next-word :comma)))
                        {:action :end-clause-here}

                        :else
                        {:action :continue
                         :and-flag false})  ; Clear AND flag after object

                      ;; Adjective or buzz-word - continue
                      (or (lexer/wt? current-word :adjective)
                          (lexer/wt? current-word :buzz-word))
                      {:action :continue}

                      ;; IT pronoun - treat like an object word
                      ;; The actual resolution happens in snarfem
                      (lexer/special-word? current-word :it)
                      (cond
                        ;; End of object reference (no AND, not followed by BUT)
                        (and (not and-flag)
                             (not (lexer/special-word? next-word :but))
                             (not (lexer/special-word? next-word :except))
                             (not (lexer/special-word? next-word :and))
                             (not (lexer/special-word? next-word :comma)))
                        {:action :end-clause-here}

                        :else
                        {:action :continue
                         :and-flag false})

                      ;; Unknown word in context
                      :else
                      {:action :error
                       :word current-word})]

                ;; Process the result
                (case (:action result)
                  :continue
                  (recur gs-decremented
                         (inc current-ptr)
                         (or (:and-flag result) and-flag)
                         false
                         current-word)

                  :skip-next
                  (recur (parser-state/dec-len gs-decremented)
                         (+ current-ptr 2)
                         and-flag
                         false
                         current-word)

                  :end-clause
                  (let [end-slot (+ (:nc1l parser-state/itbl-indices) off)]
                    {:game-state (-> gs-decremented
                                     (parser-state/set-itbl end-slot current-ptr)
                                     (parser-state/inc-len))  ; Restore for main loop
                     :ptr (dec current-ptr)})

                  :end-clause-backup
                  (let [end-slot (+ (:nc1l parser-state/itbl-indices) off)]
                    {:game-state (-> gs-decremented
                                     (parser-state/set-itbl end-slot current-ptr)
                                     (parser-state/inc-len))
                     :ptr (dec current-ptr)})

                  :end-clause-here
                  (let [end-slot (+ (:nc1l parser-state/itbl-indices) off)]
                    {:game-state (parser-state/set-itbl gs-decremented end-slot (inc current-ptr))
                     :ptr current-ptr})

                  :error
                  {:game-state gs-decremented
                   :error :cant-use
                   :word (:word result)
                   :ptr current-ptr})))))))))

;;; ---------------------------------------------------------------------------
;;; ARTICLE HANDLING
;;; ---------------------------------------------------------------------------

(defn skip-articles
  "Skip past leading articles (the, a, an) in input.

   Returns the new pointer position after any articles."
  [game-state ptr]
  (loop [current-ptr ptr]
    (let [word (lexer/lexv-word game-state current-ptr)]
      (if (or (lexer/special-word? word :the)
              (lexer/special-word? word :a)
              (lexer/special-word? word :an))
        (recur (inc current-ptr))
        current-ptr))))

;;; ---------------------------------------------------------------------------
;;; CLAUSE BOUNDARY DETECTION
;;; ---------------------------------------------------------------------------

(defn clause-terminator?
  "Check if a word terminates a noun clause.

   Words that end clauses:
   - Sentence breaks: period, comma (when not in list), THEN
   - Prepositions (start a new clause)
   - End of input"
  [word]
  (or (nil? word)
      (lexer/special-word? word :period)
      (lexer/special-word? word :then)
      (lexer/wt? word :preposition)))

(defn in-object-list?
  "Check if we're in the middle of an object list (X and Y and Z).

   After seeing AND or COMMA, we expect more objects, so a following
   preposition might be part of 'put X and Y in Z' rather than ending
   the first clause."
  [parser-state]
  (get parser-state :and-flag false))
