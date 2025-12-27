(ns clork.parser
  "Main parser module - orchestrates the parsing pipeline."
  (:require [clork.utils :as utils]
            [clork.game-state :as game-state]
            [clork.verb-defs :as verb-defs]
            [clork.parser.state :as parser-state]
            [clork.parser.input :as input]
            [clork.parser.lexer :as lexer]
            [clork.parser.clause :as clause]
            [clork.parser.syntax :as syntax]
            [clork.parser.objects :as objects]
            [clork.parser.orphan :as orphan]
            [clork.parser.validation :as validation]
            [clork.parser.output :as output]))

;;;; ============================================================================
;;;; PARSER - Main Entry Point and Orchestration
;;;; ============================================================================
;;;;
;;;; This is the main parser module that orchestrates the parsing pipeline.
;;;; The actual implementation is split across these submodules:
;;;;
;;;;   parser/state.clj      - Constants, state structure, initialization (separate ns)
;;;;   parser/input.clj      - Reading input, lexv management
;;;;   parser/lexer.clj      - Tokenization, word type checking (WT?)
;;;;   parser/clause.clj     - Noun phrase parsing (CLAUSE routine)
;;;;   parser/syntax.clj     - Syntax checking, GWIM
;;;;   parser/objects.clj    - Object resolution (SNARF-OBJECTS, GET-OBJECT)
;;;;   parser/orphan.clj     - Incomplete command handling
;;;;   parser/validation.clj - TAKE-CHECK, MANY-CHECK, ACCESSIBLE?
;;;;   parser/output.clj     - Error messages and prompts
;;;;
;;;; ZIL Reference: gparser.zil
;;;;   The main PARSER routine is lines 109-406. This file implements
;;;;   the top-level flow; submodules implement the helper routines.
;;;;
;;;; Parsing Pipeline:
;;;;   1. Initialize parser state (parser-init)
;;;;   2. Read input or restore from buffer (parser-read-command)
;;;;   3. Tokenize input into lexv (tokenize, lexv-from-input)
;;;;   4. Handle OOPS/AGAIN special commands
;;;;   5. Main parsing loop - extract verb, preps, noun clauses
;;;;   6. Handle directions as immediate WALK commands
;;;;   7. Merge with orphan state if applicable (orphan-merge)
;;;;   8. Check syntax against verb patterns (syntax-check)
;;;;   9. Resolve objects from noun clauses (snarf-objects)
;;;;   10. Validate object counts (many-check)
;;;;   11. Auto-take if needed (take-check)
;;;;   12. Return parsed command for execution
;;;;
;;;; ============================================================================

;; Re-export parser.state functions for convenience
(def sibreaks parser-state/sibreaks)
(def p-itbllen parser-state/p-itbllen)
(def itbl-indices parser-state/itbl-indices)
(def syntax-offsets parser-state/syntax-offsets)
(def p-synlen parser-state/p-synlen)
(def p-sonums parser-state/p-sonums)
(def initial-parser-state parser-state/initial-parser-state)
(def parser-init-tbl parser-state/parser-init-tbl)
(def parser-init parser-state/parser-init)
(def parser-success parser-state/parser-success)
(def parser-error parser-state/parser-error)
(def parser-result? parser-state/parser-result?)

;; Re-export state accessor functions
(def get-itbl parser-state/get-itbl)
(def set-itbl parser-state/set-itbl)
(def get-otbl parser-state/get-otbl)
(def set-otbl parser-state/set-otbl)
(def clear-itbl parser-state/clear-itbl)
(def get-ncn parser-state/get-ncn)
(def set-ncn parser-state/set-ncn)
(def inc-ncn parser-state/inc-ncn)
(def dec-ncn parser-state/dec-ncn)
(def get-len parser-state/get-len)
(def set-len parser-state/set-len)
(def inc-len parser-state/inc-len)
(def dec-len parser-state/dec-len)
(def get-parser-error parser-state/get-parser-error)
(def set-parser-error parser-state/set-parser-error)
(def get-prsa parser-state/get-prsa)
(def set-prsa parser-state/set-prsa)
(def get-prso parser-state/get-prso)
(def set-prso parser-state/set-prso)
(def get-prsi parser-state/get-prsi)
(def set-prsi parser-state/set-prsi)

;; Aliases for external module functions used throughout parser
(def tell utils/tell)
(def crlf utils/crlf)
(def crlf-if utils/crlf-if)
(def thing-name game-state/thing-name)
(def get-thing game-state/get-thing)
(def get-contents game-state/get-contents)
(def flag? game-state/flag?)
(def set-flag game-state/set-flag)
(def unset-flag game-state/unset-flag)
(def set-thing-flag? game-state/set-thing-flag?)
(def set-here-flag? game-state/set-here-flag?)
(def get-thing-loc-id game-state/get-thing-loc-id)
(def get-thing-location game-state/get-thing-location)
(def get-here game-state/get-here)
(def get-winner game-state/get-winner)
(def verbose? game-state/verbose?)
(def search-bits game-state/search-bits)
(def getflags game-state/getflags)
(def meta-location game-state/meta-location)
(def ^:dynamic *verb-vocabulary* verb-defs/*verb-vocabulary*)
(def ^:dynamic *verb-syntaxes* verb-defs/*verb-syntaxes*)

;; Re-export input functions
(def parser-set-winner-to-player input/parser-set-winner-to-player)
(def parser-read-command input/parser-read-command)
(def parser-read-command-input input/parser-read-command-input)
(def parser-restore-reserve input/parser-restore-reserve)
(def parser-restore-cont input/parser-restore-cont)
(def parser-set-here-to-winner-loc input/parser-set-here-to-winner-loc)
(def stuff input/stuff)
(def inbuf-stuff input/inbuf-stuff)

;; Re-export lexer functions
(def tokenize lexer/tokenize)
(def lexv-from-input lexer/lexv-from-input)
(def lexv-count lexer/lexv-count)
(def lexv-word lexer/lexv-word)
(def lexv-get lexer/lexv-get)
(def wt? lexer/wt?)
(def special-word? lexer/special-word?)
(def number?-token lexer/number?-token)
(def parse-number lexer/parse-number)
(def parts-of-speech lexer/parts-of-speech)

;; Re-export clause functions
(def clause clause/clause)
(def skip-articles clause/skip-articles)
(def clause-terminator? clause/clause-terminator?)
(def in-object-list? clause/in-object-list?)

;; Re-export syntax functions
(def syntax-check syntax/syntax-check)
(def syntax-found syntax/syntax-found)
(def gwim syntax/gwim)
(def make-syntax syntax/make-syntax)
(def verb-syntaxes syntax/verb-syntaxes)
(def syntax-matches? syntax/syntax-matches?)
(def find-matching-syntax syntax/find-matching-syntax)
(def try-gwim syntax/try-gwim)
(def get-verb-syntaxes syntax/get-verb-syntaxes)

;; Re-export objects functions
(def snarf-objects objects/snarf-objects)
(def get-object objects/get-object)
(def search-list objects/search-list)
(def obj-found objects/obj-found)
(def match-table-count objects/match-table-count)
(def this-it? objects/this-it?)
(def do-sl objects/do-sl)
(def global-check objects/global-check)
(def snarfem objects/snarfem)
(def but-merge objects/but-merge)

;; Re-export orphan functions
(def orphan orphan/orphan)
(def orphan-merge orphan/orphan-merge)

;; Re-export validation functions
(def take-check validation/take-check)
(def many-check validation/many-check)
(def accessible? validation/accessible?)
(def lit? validation/lit?)
(def meta-loc validation/meta-loc)
(def held? validation/held?)
(def itake-check validation/itake-check)
(def itake validation/itake)
(def room-has-global? validation/room-has-global?)
(def room? validation/room?)

;; Re-export output functions
(def unknown-word output/unknown-word)
(def cant-use output/cant-use)
(def parser-say output/parser-say)
(def which-print output/which-print)
(def thing-print output/thing-print)
(def buffer-print output/buffer-print)

;;; ---------------------------------------------------------------------------
;;; FORWARD DECLARATIONS (LOCAL)
;;; ---------------------------------------------------------------------------

(declare handle-oops handle-again parse-command parse-tokens)

;;; ---------------------------------------------------------------------------
;;; MAIN PARSER ENTRY POINT
;;; ---------------------------------------------------------------------------

(defn parser
  "The main input parser.

   ZIL: PARSER routine, gparser.zil lines 109-406

   This is THE parser - the function that turns player input into
   structured commands. It's called from the main game loop for each
   turn.

   The parser is complex because it handles:
   - Normal commands: 'take lamp', 'go north'
   - Multi-object: 'take all', 'drop sword and shield'
   - Multi-command: 'go north then take lamp'
   - Incomplete commands: 'take' → 'What do you want to take?'
   - Error correction: 'OOPS lantern' to fix typos
   - Repetition: 'AGAIN' or 'G' to repeat last command
   - Pronouns: 'take it', 'examine them'
   - Disambiguation: 'Which lamp do you mean?'

   Arguments:
     game-state - current game state

   Returns:
     Updated game-state with parsed command in:
       :parser :prsa - the action (verb)
       :parser :prso - direct object(s)
       :parser :prsi - indirect object(s)

     Or game-state with :parser :error set if parsing failed."
  [game-state]

  ;; === Phase 1: Initialize ===
  (let [gs (-> game-state
               (parser-init)
               (input/parser-set-winner-to-player)
               (input/parser-read-command))]

    ;; === Phase 2: Tokenize Input ===
    (let [gs (if-let [input (:input gs)]
               (assoc-in gs [:parser :lexv] (lexer/lexv-from-input input))
               gs)
          token-count (lexer/lexv-count gs)]

      ;; Empty input?
      (if (zero? token-count)
        (do
          (output/parser-say gs :beg-pardon)
          (set-parser-error gs {:type :empty-input}))

        ;; === Phase 3: Handle Special Commands (OOPS, AGAIN) ===
        (let [first-word (lexer/lexv-word gs 0)
              gs (set-len gs token-count)]

          (cond
            ;; OOPS - Error correction
            (lexer/special-word? first-word :oops)
            (handle-oops gs)

            ;; AGAIN / G - Repeat last command
            (or (lexer/special-word? first-word :again)
                (lexer/special-word? first-word :g))
            (handle-again gs)

            ;; Normal parsing
            :else
            (parse-command gs)))))))

;;; ---------------------------------------------------------------------------
;;; SPECIAL COMMAND HANDLERS
;;; ---------------------------------------------------------------------------

(defn handle-oops
  "Handle the OOPS error correction command.

   OOPS replaces the last unknown word with a new word.
   'OOPS lantern' fixes a typo in the previous command."
  [game-state]
  ;; TODO: Implement OOPS handling
  ;; For now, just report not implemented
  (output/parser-say game-state :oops-nothing)
  (set-parser-error game-state {:type :oops-failed}))

(defn handle-again
  "Handle the AGAIN/G repeat command.

   Repeats the last successful command."
  [game-state]
  (let [oflag? (get-in game-state [:parser :oflag])
        won? (get-in game-state [:parser :won])]
    (cond
      ;; No previous command
      (nil? (get-in game-state [:parser :again-lexv]))
      (do
        (output/parser-say game-state :again-no-cmd)
        (set-parser-error game-state {:type :no-again}))

      ;; Can't repeat fragments
      oflag?
      (do
        (output/parser-say game-state :again-fragment)
        (set-parser-error game-state {:type :again-fragment}))

      ;; Last command failed
      (not won?)
      (do
        (output/parser-say game-state :again-mistake)
        (set-parser-error game-state {:type :again-mistake}))

      ;; OK to repeat
      :else
      (let [;; Restore previous lexv
            gs (-> game-state
                   (assoc-in [:parser :lexv]
                             (get-in game-state [:parser :again-lexv]))
                   ;; Restore saved state
                   (assoc :winner (get-in game-state [:parser :owinner]))
                   (assoc-in [:parser :merged]
                             (get-in game-state [:parser :omerged])))
            ;; Copy OTBL to ITBL
            gs (reduce
                (fn [s i]
                  (set-itbl s i (get-otbl s i)))
                gs
                (range (inc p-itbllen)))]
        ;; Now parse with restored state
        (parse-command gs)))))

;;; ---------------------------------------------------------------------------
;;; MAIN PARSING LOGIC
;;; ---------------------------------------------------------------------------

(defn parse-command
  "Parse a normal command (not OOPS or AGAIN).

   This is the main parsing loop that extracts verbs, prepositions,
   and noun clauses from the tokenized input."
  [game-state]

  ;; Save lexv for AGAIN
  (let [gs (-> game-state
               (assoc-in [:parser :again-lexv]
                         (get-in game-state [:parser :lexv]))
               (assoc-in [:parser :dir] nil)
               (set-ncn 0)
               (assoc-in [:parser :getflags] 0))]

    ;; === Main Parsing Loop ===
    ;; Scan through tokens identifying verbs, preps, nouns
    (let [result (parse-tokens gs)]

      (if (:error result)
        ;; Parsing failed - return game-state with error set
        (set-parser-error (:game-state result) (:error result))

        ;; === Post-Parse Processing ===
        (let [gs (:game-state result)]

          ;; Direction shortcut?
          (if-let [dir (get-in gs [:parser :dir])]
            ;; Direct walk command
            (-> gs
                (set-prsa :walk)
                (set-prso [dir])
                (assoc-in [:parser :oflag] false)
                (assoc-in [:parser :walk-dir] dir)
                (assoc-in [:parser :again-dir] dir))

            ;; Normal command processing
            (let [;; Try orphan merge if in orphan mode
                  gs (if (get-in gs [:parser :oflag])
                       (or (orphan/orphan-merge gs) gs)
                       gs)

                  ;; Clear walk-dir
                  gs (-> gs
                         (assoc-in [:parser :walk-dir] nil)
                         (assoc-in [:parser :again-dir] nil))]

              ;; === Validation Pipeline ===
              (let [syntax-result (syntax/syntax-check gs)]
                (if (not (:success syntax-result))
                  ;; Syntax check failed
                  (do
                    (when-let [msg (get-in syntax-result [:error :message])]
                      (println msg))
                    (set-parser-error (:game-state syntax-result)
                                      (:error syntax-result)))

                  ;; Continue with object resolution
                  (let [gs (:game-state syntax-result)
                        snarf-result (objects/snarf-objects gs)]

                    (if (and (parser-result? snarf-result) (not (:success snarf-result)))
                      ;; Object resolution failed
                      (do
                        (when-let [msg (get-in snarf-result [:error :message])]
                          (println msg))
                        (set-parser-error (:game-state snarf-result)
                                          (:error snarf-result)))

                      ;; Continue with validation
                      (let [gs (if (parser-result? snarf-result)
                                 (:game-state snarf-result)
                                 snarf-result)
                            many-result (validation/many-check gs)]

                        (if (not (:success many-result))
                          ;; Too many objects
                          (do
                            (when-let [msg (get-in many-result [:error :message])]
                              (println msg))
                            (set-parser-error (:game-state many-result)
                                              (:error many-result)))

                          ;; Final: take check
                          (let [gs (:game-state many-result)
                                take-result (validation/take-check gs)]

                            (if (not (:success take-result))
                              ;; Take check failed
                              (do
                                (when-let [msg (get-in take-result [:error :message])]
                                  (println msg))
                                (set-parser-error (:game-state take-result)
                                                  (:error take-result)))

                              ;; SUCCESS! Mark as won and return
                              (-> (:game-state take-result)
                                  (assoc-in [:parser :won] true)
                                  (set-parser-error nil)))))))))))))))))

(defn parse-tokens
  "Parse tokens from lexv, extracting verbs, preps, and noun clauses.

   ZIL: The main REPEAT loop in PARSER, lines 246-364

   Returns {:game-state gs} on success, or {:error ...} on failure."
  [game-state]
  (loop [gs game-state
         ptr 0
         verb nil
         of-flag false
         last-word nil]

    (let [remaining (get-len gs)]
      (if (neg? (dec remaining))
        ;; End of input
        {:game-state (assoc-in gs [:parser :quote-flag] false)}

        ;; Get current word
        (let [word (lexer/lexv-word gs ptr)
              ;; Try to parse as number if not in vocab
              word (or word
                       (when-let [num-result (lexer/number?-token gs (lexer/lexv-get gs ptr))]
                         (:word num-result)))
              next-word (when (pos? remaining)
                          (lexer/lexv-word gs (inc ptr)))
              gs (dec-len gs)]

          (cond
            ;; No word (unknown)
            (nil? word)
            (do
              (output/unknown-word gs ptr)
              {:error {:type :unknown-word :ptr ptr}
               :game-state gs})

            ;; Sentence terminator
            (or (lexer/special-word? word :then)
                (lexer/special-word? word :period))
            (let [gs (if (pos? (get-len gs))
                       (assoc-in gs [:parser :cont] (inc ptr))
                       gs)]
              {:game-state gs})

            ;; Quote toggle (for SAY command)
            (lexer/special-word? word :quote)
            (let [gs (update-in gs [:parser :quote-flag] not)
                  gs (if (pos? (get-len gs))
                       (assoc-in gs [:parser :cont] (inc ptr))
                       gs)]
              {:game-state gs})

            ;; Direction word (potential shortcut)
            (and (lexer/wt? word :direction true)
                 (or (nil? verb) (= verb :walk))
                 (or (zero? (get-len gs))  ; Last word
                     (lexer/special-word? next-word :then)
                     (lexer/special-word? next-word :period)))
            (let [dir (lexer/wt? word :direction true)]
              {:game-state (assoc-in gs [:parser :dir] dir)})

            ;; Verb word
            (and (lexer/wt? word :verb true)
                 (nil? verb))
            (let [verb-val (lexer/wt? word :verb true)
                  gs (-> gs
                         (set-itbl :verb verb-val)
                         (set-itbl :verbn word)
                         (assoc-in [:parser :vtbl 0] word))]
              (recur gs (inc ptr) verb-val of-flag word))

            ;; Preposition, adjective, object, or "all"/"one"
            (or (lexer/wt? word :preposition)
                (lexer/special-word? word :all)
                (lexer/special-word? word :one)
                (lexer/wt? word :adjective)
                (lexer/wt? word :object))
            (let [prep-val (lexer/wt? word :preposition true)]
              (cond
                ;; "X of Y" pattern
                (and (pos? (get-len gs))
                     (lexer/special-word? next-word :of)
                     (nil? prep-val)
                     (not (lexer/special-word? word :all))
                     (not (lexer/special-word? word :one)))
                (recur gs (inc ptr) verb true word)

                ;; Preposition at end of sentence
                (and prep-val
                     (or (zero? (get-len gs))
                         (lexer/special-word? next-word :then)
                         (lexer/special-word? next-word :period)))
                (let [ncn (get-ncn gs)
                      gs (-> gs
                             (assoc-in [:parser :end-on-prep] true)
                             (cond-> (< ncn 2)
                               (-> (set-itbl :prep1 prep-val)
                                   (set-itbl :prep1n word))))]
                  (recur gs (inc ptr) verb of-flag word))

                ;; Too many noun clauses
                (= (get-ncn gs) 2)
                (do
                  (output/parser-say gs :too-many-nouns)
                  {:error {:type :too-many-nouns}
                   :game-state gs})

                ;; Start a noun clause
                :else
                (let [gs (-> gs
                             (inc-ncn)
                             (assoc-in [:parser :act] verb))
                      clause-result (clause/clause gs ptr prep-val word)]
                  (if (:error clause-result)
                    clause-result
                    (let [new-ptr (:ptr clause-result)
                          gs (:game-state clause-result)]
                      (if (neg? new-ptr)
                        {:game-state gs}
                        (recur gs (inc new-ptr) verb of-flag word)))))))

            ;; OF word
            (lexer/special-word? word :of)
            (if (or (not of-flag)
                    (lexer/special-word? next-word :period)
                    (lexer/special-word? next-word :then))
              (do
                (output/cant-use gs ptr)
                {:error {:type :bad-of} :game-state gs})
              (recur gs (inc ptr) verb false word))

            ;; Buzz word (the, a, an) - skip
            (lexer/wt? word :buzz-word)
            (recur gs (inc ptr) verb of-flag word)

            ;; Unknown usage
            :else
            (do
              (output/cant-use gs ptr)
              {:error {:type :cant-use :ptr ptr}
               :game-state gs})))))))
