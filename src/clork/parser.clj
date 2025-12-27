(in-ns 'clork.core)

;;;; ============================================================================
;;;; PARSER - Main Entry Point and Orchestration
;;;; ============================================================================
;;;;
;;;; This is the main parser module that orchestrates the parsing pipeline.
;;;; The actual implementation is split across these submodules:
;;;;
;;;;   parser/state.clj      - Constants, state structure, initialization
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

;;; ---------------------------------------------------------------------------
;;; FORWARD DECLARATIONS
;;; ---------------------------------------------------------------------------
;;; Only declare functions defined WITHIN the parser submodules that have
;;; ordering issues. Do NOT redeclare functions from other modules (flags.clj,
;;; utils.clj, game_state.clj) - that would shadow the real definitions!

(declare lit?)              ; validation.clj - called from input.clj

;;; ---------------------------------------------------------------------------
;;; LOAD SUBMODULES
;;; ---------------------------------------------------------------------------
;;; Order matters! Later modules may depend on earlier ones.

(load "parser/state")       ; Constants, initial-parser-state, parser-init
(load "parser/input")       ; parser-read-command, parser-restore-*
(load "parser/lexer")       ; tokenize, wt?, parse-number
(load "parser/clause")      ; clause parsing
(load "parser/syntax")      ; syntax-check, gwim
(load "parser/objects")     ; snarf-objects, get-object, search-list
(load "parser/orphan")      ; orphan-merge, orphan
(load "parser/validation")  ; take-check, many-check, accessible?
(load "parser/output")      ; error messages, prompts

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
               (parser-set-winner-to-player)
               (parser-read-command))]

    ;; === Phase 2: Tokenize Input ===
    (let [gs (if-let [input (:input gs)]
               (assoc-in gs [:parser :lexv] (lexv-from-input input))
               gs)
          token-count (lexv-count gs)]

      ;; Empty input?
      (if (zero? token-count)
        (do
          (parser-say gs :beg-pardon)
          (assoc-in gs [:parser :error] {:type :empty-input}))

        ;; === Phase 3: Handle Special Commands (OOPS, AGAIN) ===
        (let [first-word (lexv-word gs 0)
              gs (assoc-in gs [:parser :len] token-count)]

          (cond
            ;; OOPS - Error correction
            (special-word? first-word :oops)
            (handle-oops gs)

            ;; AGAIN / G - Repeat last command
            (or (special-word? first-word :again)
                (special-word? first-word :g))
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
  (parser-say game-state :oops-nothing)
  (assoc-in game-state [:parser :error] {:type :oops-failed}))

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
        (parser-say game-state :again-no-cmd)
        (assoc-in game-state [:parser :error] {:type :no-again}))

      ;; Can't repeat fragments
      oflag?
      (do
        (parser-say game-state :again-fragment)
        (assoc-in game-state [:parser :error] {:type :again-fragment}))

      ;; Last command failed
      (not won?)
      (do
        (parser-say game-state :again-mistake)
        (assoc-in game-state [:parser :error] {:type :again-mistake}))

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
                  (assoc-in s [:parser :itbl i]
                            (get-in s [:parser :otbl i])))
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
               (assoc-in [:parser :ncn] 0)
               (assoc-in [:parser :getflags] 0))]

    ;; === Main Parsing Loop ===
    ;; Scan through tokens identifying verbs, preps, nouns
    (let [result (parse-tokens gs)]

      (if (:error result)
        ;; Parsing failed - return game-state with error set
        (assoc-in (:game-state result) [:parser :error] (:error result))

        ;; === Post-Parse Processing ===
        (let [gs (:game-state result)]

          ;; Direction shortcut?
          (if-let [dir (get-in gs [:parser :dir])]
            ;; Direct walk command
            (-> gs
                (assoc-in [:parser :prsa] :walk)
                (assoc-in [:parser :prso] [dir])
                (assoc-in [:parser :oflag] false)
                (assoc-in [:parser :walk-dir] dir)
                (assoc-in [:parser :again-dir] dir))

            ;; Normal command processing
            (let [;; Try orphan merge if in orphan mode
                  gs (if (get-in gs [:parser :oflag])
                       (or (orphan-merge gs) gs)
                       gs)

                  ;; Clear walk-dir
                  gs (-> gs
                         (assoc-in [:parser :walk-dir] nil)
                         (assoc-in [:parser :again-dir] nil))]

              ;; === Validation Pipeline ===
              (let [syntax-result (syntax-check gs)]
                (if (not (:success syntax-result))
                  ;; Syntax check failed
                  (do
                    (when-let [msg (get-in syntax-result [:error :message])]
                      (println msg))
                    (assoc-in (:game-state syntax-result)
                              [:parser :error]
                              (:error syntax-result)))

                  ;; Continue with object resolution
                  (let [gs (:game-state syntax-result)
                        snarf-result (snarf-objects gs)]

                    (if (and (parser-result? snarf-result) (not (:success snarf-result)))
                      ;; Object resolution failed
                      (do
                        (when-let [msg (get-in snarf-result [:error :message])]
                          (println msg))
                        (assoc-in (:game-state snarf-result)
                                  [:parser :error]
                                  (:error snarf-result)))

                      ;; Continue with validation
                      (let [gs (if (parser-result? snarf-result)
                                 (:game-state snarf-result)
                                 snarf-result)
                            many-result (many-check gs)]

                        (if (not (:success many-result))
                          ;; Too many objects
                          (do
                            (when-let [msg (get-in many-result [:error :message])]
                              (println msg))
                            (assoc-in (:game-state many-result)
                                      [:parser :error]
                                      (:error many-result)))

                          ;; Final: take check
                          (let [gs (:game-state many-result)
                                take-result (take-check gs)]

                            (if (not (:success take-result))
                              ;; Take check failed
                              (do
                                (when-let [msg (get-in take-result [:error :message])]
                                  (println msg))
                                (assoc-in (:game-state take-result)
                                          [:parser :error]
                                          (:error take-result)))

                              ;; SUCCESS! Mark as won and return
                              (-> (:game-state take-result)
                                  (assoc-in [:parser :won] true)
                                  (assoc-in [:parser :error] nil)))))))))))))))))

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

    (let [remaining (get-in gs [:parser :len] 0)]
      (if (neg? (dec remaining))
        ;; End of input
        {:game-state (assoc-in gs [:parser :quote-flag] false)}

        ;; Get current word
        (let [word (lexv-word gs ptr)
              ;; Try to parse as number if not in vocab
              word (or word
                       (when-let [num-result (number?-token gs (lexv-get gs ptr))]
                         (:word num-result)))
              next-word (when (pos? remaining)
                          (lexv-word gs (inc ptr)))
              gs (update-in gs [:parser :len] dec)]

          (cond
            ;; No word (unknown)
            (nil? word)
            (do
              (unknown-word gs ptr)
              {:error {:type :unknown-word :ptr ptr}
               :game-state gs})

            ;; Sentence terminator
            (or (special-word? word :then)
                (special-word? word :period))
            (let [gs (if (pos? (get-in gs [:parser :len] 0))
                       (assoc-in gs [:parser :cont] (inc ptr))
                       gs)]
              {:game-state gs})

            ;; Quote toggle (for SAY command)
            (special-word? word :quote)
            (let [gs (update-in gs [:parser :quote-flag] not)
                  gs (if (pos? (get-in gs [:parser :len] 0))
                       (assoc-in gs [:parser :cont] (inc ptr))
                       gs)]
              {:game-state gs})

            ;; Direction word (potential shortcut)
            (and (wt? word :direction true)
                 (or (nil? verb) (= verb :walk))
                 (or (= (get-in gs [:parser :len]) 0)  ; Last word
                     (special-word? next-word :then)
                     (special-word? next-word :period)))
            (let [dir (wt? word :direction true)]
              {:game-state (assoc-in gs [:parser :dir] dir)})

            ;; Verb word
            (and (wt? word :verb true)
                 (nil? verb))
            (let [verb-val (wt? word :verb true)
                  gs (-> gs
                         (assoc-in [:parser :itbl (:verb itbl-indices)] verb-val)
                         (assoc-in [:parser :itbl (:verbn itbl-indices)] word)
                         (assoc-in [:parser :vtbl 0] word))]
              (recur gs (inc ptr) verb-val of-flag word))

            ;; Preposition, adjective, object, or "all"/"one"
            (or (wt? word :preposition)
                (special-word? word :all)
                (special-word? word :one)
                (wt? word :adjective)
                (wt? word :object))
            (let [prep-val (wt? word :preposition true)]
              (cond
                ;; "X of Y" pattern
                (and (pos? (get-in gs [:parser :len] 0))
                     (special-word? next-word :of)
                     (nil? prep-val)
                     (not (special-word? word :all))
                     (not (special-word? word :one)))
                (recur gs (inc ptr) verb true word)

                ;; Preposition at end of sentence
                (and prep-val
                     (or (zero? (get-in gs [:parser :len] 0))
                         (special-word? next-word :then)
                         (special-word? next-word :period)))
                (let [ncn (get-in gs [:parser :ncn] 0)
                      gs (-> gs
                             (assoc-in [:parser :end-on-prep] true)
                             (cond-> (< ncn 2)
                               (-> (assoc-in [:parser :itbl (:prep1 itbl-indices)] prep-val)
                                   (assoc-in [:parser :itbl (:prep1n itbl-indices)] word))))]
                  (recur gs (inc ptr) verb of-flag word))

                ;; Too many noun clauses
                (= (get-in gs [:parser :ncn] 0) 2)
                (do
                  (parser-say gs :too-many-nouns)
                  {:error {:type :too-many-nouns}
                   :game-state gs})

                ;; Start a noun clause
                :else
                (let [gs (-> gs
                             (update-in [:parser :ncn] inc)
                             (assoc-in [:parser :act] verb))
                      clause-result (clause gs ptr prep-val word)]
                  (if (:error clause-result)
                    clause-result
                    (let [new-ptr (:ptr clause-result)
                          gs (:game-state clause-result)]
                      (if (neg? new-ptr)
                        {:game-state gs}
                        (recur gs (inc new-ptr) verb of-flag word)))))))

            ;; OF word
            (special-word? word :of)
            (if (or (not of-flag)
                    (special-word? next-word :period)
                    (special-word? next-word :then))
              (do
                (cant-use gs ptr)
                {:error {:type :bad-of} :game-state gs})
              (recur gs (inc ptr) verb false word))

            ;; Buzz word (the, a, an) - skip
            (wt? word :buzz-word)
            (recur gs (inc ptr) verb of-flag word)

            ;; Unknown usage
            :else
            (do
              (cant-use gs ptr)
              {:error {:type :cant-use :ptr ptr}
               :game-state gs})))))))
