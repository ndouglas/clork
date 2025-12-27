(ns clork.debug.parser
  "Parser debugging commands.

   Commands:
   - $parser result - Show last parse result (prsa, prso, prsi)
   - $parser lexv   - Show tokenized input
   - $parser itbl   - Show instruction table state
   - $parser vocab  - Look up word in vocabularies
   - $parser syntax - Show syntax rules for a verb"
  (:require [clork.utils :as utils]
            [clork.verb-defs :as verb-defs]
            [clork.parser.state :as parser-state]
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
        prsi (parser-state/get-prsi game-state)
        error (parser-state/get-parser-error game-state)]
    (-> game-state
        (utils/tell "Last Parse Result:\n")
        (tell-line "PRSA (action)" (or prsa "nil"))
        (tell-line "PRSO (direct obj)" (if (seq prso)
                                         (str/join ", " (map format-keyword prso))
                                         "nil"))
        (tell-line "PRSI (indirect obj)" (if (seq prsi)
                                           (str/join ", " (map format-keyword prsi))
                                           "nil"))
        (tell-line "Error" (if error (str error) "none")))))

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
  (let [itbl (parser-state/get-itbl game-state)
        len (parser-state/get-len game-state)
        ncn (parser-state/get-ncn game-state)]
    (-> game-state
        (utils/tell "Instruction Table (ITBL):\n")
        (tell-line "LEN (tokens remaining)" len)
        (tell-line "NCN (noun clause number)" ncn)
        ((fn [gs]
           (if (nil? itbl)
             (utils/tell gs "  (itbl is nil)\n")
             (-> gs
                 (tell-line "Verb" (get itbl :verb))
                 (tell-line "Prep1" (get itbl :prep1))
                 (tell-line "Prep2" (get itbl :prep2))
                 (tell-line "NC1" (get itbl :nc1))
                 (tell-line "NC2" (get itbl :nc2))
                 ((fn [gs2]
                    (reduce-kv (fn [gs3 k v]
                                 (if (#{:verb :prep1 :prep2 :nc1 :nc2} k)
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
;;; MAIN PARSER DISPATCHER
;;; ---------------------------------------------------------------------------

(def subcommands
  {:result {:handler cmd-parser-result :help "Show last parse result (PRSA, PRSO, PRSI)"}
   :lexv   {:handler cmd-parser-lexv   :help "Show tokenized input"}
   :itbl   {:handler cmd-parser-itbl   :help "Show instruction table state"}
   :vocab  {:handler cmd-parser-vocab  :help "Look up word in vocabularies"}
   :syntax {:handler cmd-parser-syntax :help "Show syntax rules for a verb"}
   :verbs  {:handler cmd-parser-verbs  :help "List all registered verbs"}})

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
