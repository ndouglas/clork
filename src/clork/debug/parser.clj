(ns clork.debug.parser
  "Parser debugging commands.

   Commands:
   - $parser result  - Show last parse result (prsa, prso, prsi)
   - $parser it      - Show what IT refers to
   - $parser state   - Show full parser state
   - $parser resolve - Test object resolution for a word
   - $parser lexv    - Show tokenized input
   - $parser itbl    - Show instruction table state
   - $parser vocab   - Look up word in vocabularies
   - $parser syntax  - Show syntax rules for a verb
   - $parser trace   - Parse a command with tracing enabled"
  (:require [clork.utils :as utils]
            [clork.verb-defs :as verb-defs]
            [clork.parser.state :as parser-state]
            [clork.parser.objects :as parser-objects]
            [clork.game-state :as gs]
            [clork.debug.trace :as trace]
            [clojure.string :as str]))

;;; ---------------------------------------------------------------------------
;;; HELPERS
;;; ---------------------------------------------------------------------------

(defn- format-keyword [kw]
  (if (keyword? kw) (name kw) (str kw)))

(defn- tell-line [game-state label value]
  (utils/tell game-state (str "  " label ": " value "\n")))

;;; ---------------------------------------------------------------------------
;;; $parser result
;;; ---------------------------------------------------------------------------

(defn cmd-parser-result
  "Show the last parse result (PRSA, PRSO, PRSI)."
  [game-state _args]
  (let [prsa (parser-state/get-prsa game-state)
        prso (parser-state/get-prso game-state)
        prso-all (parser-state/get-prso-all game-state)
        prsi (parser-state/get-prsi game-state)
        prsi-all (parser-state/get-prsi-all game-state)
        error (parser-state/get-parser-error game-state)]
    (-> game-state
        (utils/tell "Last Parse Result:\n")
        (tell-line "PRSA (action)" (or prsa "nil"))
        (tell-line "PRSO (direct obj)" (if prso
                                         (str (format-keyword prso)
                                              (when (> (count prso-all) 1)
                                                (str " [+" (dec (count prso-all)) " more]")))
                                         "nil"))
        (tell-line "PRSI (indirect obj)" (if prsi
                                           (str (format-keyword prsi)
                                                (when (> (count prsi-all) 1)
                                                  (str " [+" (dec (count prsi-all)) " more]")))
                                           "nil"))
        (tell-line "Error" (if error (str error) "none")))))

;;; ---------------------------------------------------------------------------
;;; $parser it
;;; ---------------------------------------------------------------------------

(defn cmd-parser-it
  "Show what IT currently refers to."
  [game-state _args]
  (let [it-id (:it game-state)
        it-obj (when it-id
                 (or (get-in game-state [:objects it-id])
                     (get-in game-state [:rooms it-id])))]
    (-> game-state
        (utils/tell "Pronoun Reference (IT):\n")
        (tell-line "IT" (if it-id
                          (str (format-keyword it-id)
                               (when it-obj
                                 (str " (" (:desc it-obj "?") ")")))
                          "nil"))
        ((fn [gs]
           (if it-obj
             (tell-line gs "Location" (or (:in it-obj) "n/a"))
             gs))))))

;;; ---------------------------------------------------------------------------
;;; $parser state
;;; ---------------------------------------------------------------------------

(defn cmd-parser-state
  "Show full parser state (all internal variables)."
  [game-state _args]
  (let [parser (get game-state :parser {})
        prso-raw (get parser :prso)
        prsi-raw (get parser :prsi)]
    (-> game-state
        (utils/tell "Full Parser State:\n")
        (utils/tell "  --- Parse Result ---\n")
        (tell-line "PRSA" (or (get parser :prsa) "nil"))
        (tell-line "PRSO (raw)" (pr-str prso-raw))
        (tell-line "PRSI (raw)" (pr-str prsi-raw))
        (tell-line "Error" (pr-str (get parser :error)))
        (utils/tell "  --- Object Search ---\n")
        (tell-line "NAM (noun)" (pr-str (get parser :nam)))
        (tell-line "ADJ (adjective)" (pr-str (get parser :adj)))
        (tell-line "GWIMBIT" (pr-str (get parser :gwimbit)))
        (tell-line "SLOCBITS" (pr-str (get parser :slocbits)))
        (tell-line "GETFLAGS" (pr-str (get parser :getflags)))
        (utils/tell "  --- Syntax ---\n")
        (tell-line "Verb" (pr-str (parser-state/get-itbl game-state :verb)))
        (tell-line "Prep1" (pr-str (parser-state/get-itbl game-state :prep1)))
        (tell-line "Prep2" (pr-str (parser-state/get-itbl game-state :prep2)))
        (tell-line "NCN" (parser-state/get-ncn game-state))
        (tell-line "Syntax" (pr-str (get parser :syntax)))
        (utils/tell "  --- Lexer ---\n")
        (tell-line "LEN" (parser-state/get-len game-state))
        (tell-line "Token count" (count (get-in parser [:lexv :tokens] []))))))

;;; ---------------------------------------------------------------------------
;;; $parser resolve <word> [adj]
;;; ---------------------------------------------------------------------------

(defn- check-this-it
  "Debug helper: check this-it? conditions for an object."
  [game-state obj-id nam adj gwimbit]
  (let [obj (gs/get-thing game-state obj-id)
        synonyms (set (map str/lower-case (or (:synonym obj) [])))
        adjectives (let [a (:adjective obj)]
                     (set (map str/lower-case
                               (cond
                                 (nil? a) []
                                 (string? a) [a]
                                 (sequential? a) a
                                 :else []))))
        invisible? (gs/set-thing-flag? game-state obj-id :invisible)
        nam-match? (or (nil? nam)
                       (contains? synonyms (str/lower-case (str nam))))
        adj-match? (or (nil? adj)
                       (contains? adjectives (str/lower-case (str adj))))
        gwim-match? (or (nil? gwimbit)
                        (and (number? gwimbit) (zero? gwimbit))
                        (gs/set-thing-flag? game-state obj-id gwimbit))]
    {:obj-id obj-id
     :desc (:desc obj)
     :synonyms synonyms
     :adjectives adjectives
     :invisible? invisible?
     :nam-match? nam-match?
     :adj-match? adj-match?
     :gwim-match? gwim-match?
     :pass? (and (not invisible?) nam-match? adj-match? gwim-match?)}))

(defn cmd-parser-resolve
  "Test object resolution for a word without executing a command.
   Shows exactly why objects match or don't match.

   Usage: $parser resolve <word> [adjective]"
  [game-state args]
  (if (empty? args)
    (utils/tell game-state "Usage: $parser resolve <word> [adjective]\n")
    (let [word (str/lower-case (first args))
          adj (when (second args) (str/lower-case (second args)))
          here-id (:here game-state)
          player-id (:player game-state)
          gwimbit (get-in game-state [:parser :gwimbit] 0)

          ;; Check lighting
          lit? (or (gs/set-thing-flag? game-state here-id :lit)
                   (gs/set-thing-flag? game-state here-id :on))

          ;; Check vocabulary
          vocab-entry (get verb-defs/*verb-vocabulary* word)
          is-object-word? (and vocab-entry
                               (contains? (:parts-of-speech vocab-entry) :object))

          ;; Get objects to check
          room-contents (gs/get-contents game-state here-id)
          player-contents (gs/get-contents game-state player-id)
          all-objects (distinct (concat room-contents player-contents))]

      (-> game-state
          (utils/tell (str "Object Resolution Test: \"" word "\""
                           (when adj (str " (adj: " adj ")")) "\n"))
          (utils/tell "  --- Environment ---\n")
          (tell-line "Room" (str here-id " (lit: " lit? ")"))
          (tell-line "GWIMBIT" (pr-str gwimbit))
          (tell-line "In vocabulary?" (if is-object-word? "yes" "NO - word not registered!"))
          (utils/tell "  --- Candidates ---\n")
          ((fn [gs]
             (if (empty? all-objects)
               (utils/tell gs "  (no objects in scope)\n")
               (reduce
                (fn [gs obj-id]
                  (let [check (check-this-it gs obj-id word adj gwimbit)
                        status (if (:pass? check) "✓ MATCH" "✗ no match")]
                    (-> gs
                        (utils/tell (str "  " (format-keyword obj-id)
                                         " (" (:desc check) "): " status "\n"))
                        ((fn [g]
                           (if (:pass? check)
                             g
                             ;; Show why it failed
                             (-> g
                                 ((fn [g2]
                                    (if (:invisible? check)
                                      (utils/tell g2 "      - invisible: true ✗\n")
                                      g2)))
                                 ((fn [g2]
                                    (if (not (:nam-match? check))
                                      (utils/tell g2 (str "      - name mismatch: \"" word
                                                          "\" not in " (:synonyms check) " ✗\n"))
                                      g2)))
                                 ((fn [g2]
                                    (if (not (:adj-match? check))
                                      (utils/tell g2 (str "      - adj mismatch: \"" adj
                                                          "\" not in " (:adjectives check) " ✗\n"))
                                      g2)))
                                 ((fn [g2]
                                    (if (not (:gwim-match? check))
                                      (utils/tell g2 (str "      - gwimbit mismatch: "
                                                          gwimbit " not a flag on object ✗\n"))
                                      g2))))))))))
                gs
                all-objects))))
          ((fn [gs]
             (if (not lit?)
               (utils/tell gs "  WARNING: Room is not lit - searches will fail!\n")
               gs)))))))

;;; ---------------------------------------------------------------------------
;;; $parser lexv
;;; ---------------------------------------------------------------------------

(defn cmd-parser-lexv
  "Show tokenized input (lexv table)."
  [game-state _args]
  (let [lexv (get-in game-state [:parser :lexv])
        input (:input game-state)]
    (-> game-state
        (utils/tell "Tokenized Input (lexv):\n")
        (tell-line "Raw input" (pr-str input))
        (tell-line "Token count" (count lexv))
        ((fn [gs]
           (if (empty? lexv)
             (utils/tell gs "  (no tokens)\n")
             (reduce-kv (fn [gs idx token]
                          (utils/tell gs (str "  [" idx "] " (pr-str token) "\n")))
                        gs
                        (vec lexv))))))))

;;; ---------------------------------------------------------------------------
;;; $parser itbl
;;; ---------------------------------------------------------------------------

(defn cmd-parser-itbl
  "Show instruction table state."
  [game-state _args]
  (let [;; Get the entire itbl map directly from game state
        itbl (get-in game-state [:parser :itbl] {})
        len (parser-state/get-len game-state)
        ncn (parser-state/get-ncn game-state)]
    (-> game-state
        (utils/tell "Instruction Table (ITBL):\n")
        (tell-line "LEN (tokens remaining)" len)
        (tell-line "NCN (noun clause number)" ncn)
        ((fn [gs]
           (if (empty? itbl)
             (utils/tell gs "  (itbl is empty)\n")
             (-> gs
                 (tell-line "Verb" (get itbl :verb))
                 (tell-line "Prep1" (get itbl :prep1))
                 (tell-line "Prep2" (get itbl :prep2))
                 (tell-line "NC1" (get itbl :nc1))
                 (tell-line "NC1L" (get itbl :nc1l))
                 (tell-line "NC2" (get itbl :nc2))
                 (tell-line "NC2L" (get itbl :nc2l))
                 ((fn [gs2]
                    (reduce-kv (fn [gs3 k v]
                                 (if (#{:verb :prep1 :prep2 :nc1 :nc1l :nc2 :nc2l} k)
                                   gs3
                                   (tell-line gs3 (format-keyword k) (str v))))
                               gs2
                               itbl))))))))))

;;; ---------------------------------------------------------------------------
;;; $parser vocab <word>
;;; ---------------------------------------------------------------------------

(defn cmd-parser-vocab
  "Look up a word in the vocabularies."
  [game-state args]
  (if (empty? args)
    (utils/tell game-state "Usage: $parser vocab <word>\n")
    (let [word (str/lower-case (first args))
          vocab verb-defs/*verb-vocabulary*
          match (get vocab word)]
      (-> game-state
          (utils/tell (str "Vocabulary lookup: \"" word "\"\n"))
          ((fn [gs]
             (if match
               (tell-line gs "Maps to action" match)
               (utils/tell gs "  Not found in vocabulary.\n"))))))))

;;; ---------------------------------------------------------------------------
;;; $parser syntax <verb>
;;; ---------------------------------------------------------------------------

(defn cmd-parser-syntax
  "Show syntax rules for a verb."
  [game-state args]
  (if (empty? args)
    (-> game-state
        (utils/tell "Usage: $parser syntax <verb>\n")
        (utils/tell "Available verbs: ")
        (utils/tell (str/join ", " (sort (map format-keyword (keys verb-defs/*verb-syntaxes*)))))
        (utils/tell "\n"))
    (let [verb-word (str/lower-case (first args))
          ;; First check if it's a word that maps to an action via vocabulary
          vocab-entry (get verb-defs/*verb-vocabulary* verb-word)
          action (or (when vocab-entry (:verb-value vocab-entry))
                     (keyword verb-word))
          syntax (get verb-defs/*verb-syntaxes* action)]
      (if syntax
        ;; syntax is a vector of syntax entries (verbs can have multiple patterns)
        (reduce-kv
         (fn [gs idx entry]
           (-> gs
               (utils/tell (str "  Pattern " idx ":\n"))
               (tell-line "Num objects" (:num-objects entry))
               ((fn [g]
                  (if-let [prep1 (:prep1 entry)]
                    (tell-line g "Prep1" prep1)
                    g)))
               ((fn [g]
                  (if-let [prep2 (:prep2 entry)]
                    (tell-line g "Prep2" prep2)
                    g)))
               ((fn [g]
                  (if-let [loc1 (:loc1 entry)]
                    (tell-line g "Loc1 (search bits)" loc1)
                    g)))
               ((fn [g]
                  (if-let [loc2 (:loc2 entry)]
                    (tell-line g "Loc2 (search bits)" loc2)
                    g)))))
         (utils/tell game-state (str "Syntax for " action " (" (count syntax) " pattern(s)):\n"))
         (vec syntax))
        (utils/tell game-state (str "Unknown verb: " verb-word "\n"))))))

;;; ---------------------------------------------------------------------------
;;; $parser verbs (bonus: list all registered verbs)
;;; ---------------------------------------------------------------------------

(defn cmd-parser-verbs
  "List all registered verbs."
  [game-state _args]
  (let [verbs (sort (keys verb-defs/*verb-syntaxes*))]
    (-> game-state
        (utils/tell (str "Registered verbs (" (count verbs) "):\n"))
        (utils/tell "  ")
        (utils/tell (str/join ", " (map format-keyword verbs)))
        (utils/tell "\n"))))

;;; ---------------------------------------------------------------------------
;;; $parser trace <command>
;;; ---------------------------------------------------------------------------

(defn cmd-parser-trace
  "Parse a command with tracing enabled (without executing).

   Temporarily enables parser tracing, parses the given command,
   then disables tracing. Shows the full parser pipeline execution.

   Usage: $parser trace take all"
  [game-state args]
  (if (empty? args)
    (utils/tell game-state "Usage: $parser trace <command>\nExample: $parser trace take all\n")
    (let [;; Enable parser tracing temporarily
          gs-with-trace (trace/enable-trace game-state :parser)
          ;; Set up the input to parse
          input (str/join " " args)
          gs-with-input (assoc gs-with-trace :input input)
          ;; Show what we're parsing
          gs-header (utils/tell gs-with-input (str "=== Parsing: \"" input "\" ===\n"))]
      ;; We need to require the parser dynamically to avoid circular dependency
      (try
        (require 'clork.parser)
        (let [parser-from-input (ns-resolve 'clork.parser 'parser-from-input)
              gs-parsed (parser-from-input gs-header)
              ;; Disable tracing
              gs-final (trace/disable-trace gs-parsed :parser)]
          ;; Show final result summary
          (-> gs-final
              (utils/tell "=== Parse Result ===\n")
              (tell-line "PRSA" (or (parser-state/get-prsa gs-parsed) "nil"))
              (tell-line "PRSO" (pr-str (get-in gs-parsed [:parser :prso])))
              (tell-line "PRSI" (pr-str (get-in gs-parsed [:parser :prsi])))
              (tell-line "Error" (pr-str (get-in gs-parsed [:parser :error])))))
        (catch Exception e
          (-> game-state
              (trace/disable-trace :parser)
              (utils/tell (str "Parse error: " (.getMessage e) "\n"))))))))

;;; ---------------------------------------------------------------------------
;;; MAIN PARSER DISPATCHER
;;; ---------------------------------------------------------------------------

(def subcommands
  {:result  {:handler cmd-parser-result  :help "Show last parse result (PRSA, PRSO, PRSI)"}
   :it      {:handler cmd-parser-it      :help "Show what IT refers to"}
   :state   {:handler cmd-parser-state   :help "Show full parser state (all internal variables)"}
   :resolve {:handler cmd-parser-resolve :help "Test object resolution for a word"}
   :lexv    {:handler cmd-parser-lexv    :help "Show tokenized input"}
   :itbl    {:handler cmd-parser-itbl    :help "Show instruction table state"}
   :vocab   {:handler cmd-parser-vocab   :help "Look up word in vocabularies"}
   :syntax  {:handler cmd-parser-syntax  :help "Show syntax rules for a verb"}
   :verbs   {:handler cmd-parser-verbs   :help "List all registered verbs"}
   :trace   {:handler cmd-parser-trace   :help "Parse a command with tracing (e.g., $parser trace take all)"}})

(defn cmd-parser
  "Main $parser command dispatcher."
  [game-state args]
  (if (empty? args)
    (reduce (fn [gs [name info]]
              (utils/tell gs (str "  $parser " (format-keyword name) " - " (:help info) "\n")))
            (utils/tell game-state "$parser subcommands:\n")
            (sort-by first subcommands))
    (let [subcmd (keyword (first args))
          sub-args (rest args)
          sub-info (get subcommands subcmd)]
      (if sub-info
        ((:handler sub-info) game-state sub-args)
        (utils/tell game-state (str "Unknown subcommand: " (first args) "\nType $parser for list.\n"))))))
