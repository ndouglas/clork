(in-ns 'clork.core)

;;;; ============================================================================
;;;; PARSER LEXER - Tokenization and Word Type Checking
;;;; ============================================================================
;;;;
;;;; This file contains:
;;;;   - Tokenizing input strings into word lists
;;;;   - Word type checking (WT? - is this word a verb? noun? adjective?)
;;;;   - Number parsing (NUMBER? - parse "3:30" or "42")
;;;;   - Parts of speech classification
;;;;
;;;; ZIL Reference: gparser.zil
;;;;   - Lines 71-81: Lexv constants (P-LEXWORDS, P-LEXELEN, P-PSOFF, etc.)
;;;;   - Lines 430-436: WT? routine (word type checking)
;;;;   - Lines 512-534: NUMBER? routine (parse numbers and times)
;;;;
;;;; Key Concepts:
;;;;
;;;;   LEXV Structure (ZIL):
;;;;     The Z-machine's lexer produces a table of (word, length, position)
;;;;     entries. Each word is looked up in the vocabulary to get its
;;;;     parts of speech and semantic values.
;;;;
;;;;   Parts of Speech (PS?*):
;;;;     Words can have multiple parts of speech. "light" can be a verb
;;;;     (light the lamp), adjective (light sword), or noun (see the light).
;;;;     The WT? function checks if a word has a specific part of speech.
;;;;
;;;;   For Clojure:
;;;;     We simplify this: tokenize to a list of strings, then look up
;;;;     each word in a vocabulary map to get its properties.
;;;;
;;;; ============================================================================

;;; ---------------------------------------------------------------------------
;;; PARTS OF SPEECH
;;; ---------------------------------------------------------------------------
;;; These correspond to ZIL's PS?* constants used with WT?
;;;
;;; In ZIL, parts of speech are stored as bits in a byte, allowing words
;;; to have multiple parts of speech. The P1?* constants specify which
;;; value to return for that part of speech.
;;;
;;; ZIL: Lines 78-81
;;;   <CONSTANT P-PSOFF 4>     ; Offset to parts of speech byte
;;;   <CONSTANT P-P1OFF 5>     ; Offset to first part of speech value
;;;   <CONSTANT P-P1BITS 3>    ; Mask for primary part of speech

(def parts-of-speech
  "Bit flags for parts of speech.

   A word's PS byte is checked with BTST against these values.
   Example: (bit-test ps-byte (:verb parts-of-speech))"
  {:direction   1    ; PS?DIRECTION - north, south, up, etc.
   :verb        2    ; PS?VERB - take, drop, look, etc.
   :preposition 4    ; PS?PREPOSITION - in, on, with, etc.
   :adjective   8    ; PS?ADJECTIVE - brass, small, rusty, etc.
   :object     16    ; PS?OBJECT - lamp, sword, door, etc.
   :buzz-word  32})  ; PS?BUZZ-WORD - the, a, an, etc. (ignored)

;;; ---------------------------------------------------------------------------
;;; VOCABULARY STRUCTURE
;;; ---------------------------------------------------------------------------
;;; Each word in the vocabulary maps to its properties:
;;;   - :parts-of-speech - set of #{:verb :noun :adjective ...}
;;;   - :verb-value - action number if this is a verb
;;;   - :prep-value - preposition number if this is a preposition
;;;   - :dir-value - direction if this is a direction word
;;;   - :adj-value - adjective ID if this is an adjective
;;;   - :synonyms - other words this is equivalent to

;; The vocabulary is built from verb-definitions in verb_defs.clj
;; Future: will also include object vocabulary, direction vocabulary, etc.

(def vocabulary
  "The game's vocabulary. Maps lowercase word strings to property maps.

   This is derived from *verb-vocabulary* (from verb_defs.clj).
   Future versions will merge with object and direction vocabularies.

   Example entry:
   \"lamp\" {:parts-of-speech #{:object :adjective}
            :object-value :brass-lantern
            :adj-value :lamp-adj}"
  *verb-vocabulary*)

;;; ---------------------------------------------------------------------------
;;; SPECIAL WORDS
;;; ---------------------------------------------------------------------------
;;; These are referenced by name throughout the parser.
;;; ZIL: W?WORD constants like W?OOPS, W?AGAIN, W?THE, etc.

(def special-words
  "Words with special meaning to the parser."
  {:oops    "oops"      ; Error correction
   :again   "again"     ; Repeat last command
   :g       "g"         ; Abbreviation for again
   :the     "the"       ; Article (ignored)
   :a       "a"         ; Article (ignored)
   :an      "an"        ; Article (ignored)
   :of      "of"        ; Possessive connector
   :all     "all"       ; Take all, drop all
   :one     "one"       ; Pick one randomly
   :and     "and"       ; Object list connector
   :comma   ","         ; Also object list connector
   :but     "but"       ; Exclusion
   :except  "except"    ; Exclusion
   :then    "then"      ; Command chaining
   :period  "."         ; Sentence terminator
   :quote   "\""        ; For SAY commands
   :it      "it"        ; Pronoun reference
   :intnum  :intnum})   ; Placeholder for parsed numbers

(defn special-word?
  "Check if a word is a special parser word."
  [word key]
  (= (clojure.string/lower-case (str word))
     (get special-words key)))

;;; ---------------------------------------------------------------------------
;;; TOKENIZATION
;;; ---------------------------------------------------------------------------
;;; Convert input string to a sequence of tokens.

(defn tokenize
  "Split input string into a sequence of word tokens.

   Handles:
   - Whitespace separation
   - Punctuation as separate tokens (period, comma, quote)
   - Case normalization (all lowercase)

   Returns a vector of token maps:
   [{:word \"take\" :start 0 :length 4}
    {:word \"the\" :start 5 :length 3}
    ...]

   This replaces the Z-machine's READ instruction which populates P-LEXV."
  [input]
  (when input
    (let [;; Normalize: lowercase, handle punctuation
          normalized (-> input
                         clojure.string/trim
                         clojure.string/lower-case)
          ;; Add spaces around punctuation so they become separate tokens
          spaced (-> normalized
                     (clojure.string/replace #"([.,\"])" " $1 ")
                     (clojure.string/replace #"\s+" " ")
                     clojure.string/trim)]
      (when (seq spaced)
        (loop [remaining spaced
               position 0
               tokens []]
          (if (empty? remaining)
            tokens
            (let [trimmed (clojure.string/triml remaining)
                  skip-count (- (count remaining) (count trimmed))
                  new-pos (+ position skip-count)]
              (if (empty? trimmed)
                tokens
                (let [;; Find end of current word
                      end-idx (or (clojure.string/index-of trimmed " ")
                                  (count trimmed))
                      word (subs trimmed 0 end-idx)
                      token {:word word
                             :start new-pos
                             :length (count word)}]
                  (recur (subs trimmed end-idx)
                         (+ new-pos (count word))
                         (conj tokens token)))))))))))

(defn lexv-from-input
  "Create a lexv table from raw input string.

   The lexv is our Clojure equivalent of ZIL's P-LEXV table.
   It's a map containing:
   - :tokens - vector of token maps from tokenize
   - :count - number of tokens
   - :raw - original input string

   ZIL: The READ instruction populates P-LEXV with this structure."
  [input]
  (let [tokens (tokenize input)]
    {:tokens (vec tokens)
     :count (count tokens)
     :raw input}))

;;; ---------------------------------------------------------------------------
;;; WORD TYPE CHECKING (WT?)
;;; ---------------------------------------------------------------------------
;;; The core function for determining a word's part of speech.

(defn word-type?
  "Check if a word has a specific part of speech.

   ZIL: WT? routine, lines 430-436
     <ROUTINE WT? (PTR BIT \"OPTIONAL\" (B1 5) \"AUX\" (OFFS ,P-P1OFF) TYP)
       <COND (<BTST <SET TYP <GETB .PTR ,P-PSOFF>> .BIT>
         <COND (<G? .B1 4> <RTRUE>)
           (T
             <SET TYP <BAND .TYP ,P-P1BITS>>
             <COND (<NOT <EQUAL? .TYP .B1>> <SET OFFS <+ .OFFS 1>>)>
             <GETB .PTR .OFFS>)>)>>

   Arguments:
     word - the word string to check
     part-of-speech - keyword like :verb, :noun, :adjective
     return-value? - if true, return the value for that POS; else return boolean

   Returns:
     If return-value? is false: true/false
     If return-value? is true: the semantic value (verb number, etc.) or nil"
  ([word part-of-speech]
   (word-type? word part-of-speech false))
  ([word part-of-speech return-value?]
   (when-let [vocab-entry (get vocabulary (clojure.string/lower-case (str word)))]
     (let [has-pos? (contains? (:parts-of-speech vocab-entry) part-of-speech)]
       (if return-value?
         ;; Return the value for this part of speech
         (when has-pos?
           (case part-of-speech
             :verb (:verb-value vocab-entry)
             :direction (:dir-value vocab-entry)
             :preposition (:prep-value vocab-entry)
             :adjective (:adj-value vocab-entry)
             :object (:object-value vocab-entry)
             true))
         ;; Just return boolean
         has-pos?)))))

(defn wt?
  "Alias for word-type? matching ZIL naming.

   Usage:
     (wt? word :verb)           ; Is this a verb?
     (wt? word :verb true)      ; Get verb action number"
  ([word part-of-speech]
   (word-type? word part-of-speech false))
  ([word part-of-speech return-value?]
   (word-type? word part-of-speech return-value?)))

;;; ---------------------------------------------------------------------------
;;; NUMBER PARSING
;;; ---------------------------------------------------------------------------

(defn parse-number
  "Parse a number or time from a token.

   ZIL: NUMBER? routine, lines 512-534
   Handles:
   - Plain numbers: \"42\" -> 42
   - Times: \"3:30\" -> 210 (minutes since midnight, adjusted)

   Time handling quirks (from ZIL):
   - Hours < 8 are assumed PM (add 12)
   - Hours > 23 are invalid
   - Returns nil if not a valid number

   Returns the parsed number, or nil if not a number."
  [token-word]
  (when (string? token-word)
    (cond
      ;; Time format: H:MM or HH:MM
      (re-matches #"\d{1,2}:\d{2}" token-word)
      (let [[hours-str mins-str] (clojure.string/split token-word #":")
            hours (parse-long hours-str)
            mins (parse-long mins-str)]
        (when (and hours mins (<= hours 23) (<= mins 59))
          (let [adjusted-hours (if (< hours 8) (+ hours 12) hours)]
            (+ (* adjusted-hours 60) mins))))

      ;; Plain number
      (re-matches #"\d+" token-word)
      (let [n (parse-long token-word)]
        (when (<= n 10000) ; ZIL limit
          n))

      :else nil)))

(defn number?-token
  "Check if a token is a number and update parser state if so.

   ZIL: NUMBER? routine
   If the token parses as a number, this:
   - Stores the value in :parser :number
   - Returns :intnum (the special 'number' word)
   - Otherwise returns nil

   This allows numbers to be treated as a special word type in parsing."
  [game-state token]
  (when-let [num (parse-number (:word token))]
    {:game-state (assoc-in game-state [:parser :number] num)
     :word :intnum}))

;;; ---------------------------------------------------------------------------
;;; LEXV ACCESS HELPERS
;;; ---------------------------------------------------------------------------

(defn lexv-get
  "Get a token from the lexv at the given index.

   ZIL: <GET ,P-LEXV .PTR>"
  [game-state ptr]
  (get-in game-state [:parser :lexv :tokens ptr]))

(defn lexv-word
  "Get just the word string from a lexv entry."
  [game-state ptr]
  (:word (lexv-get game-state ptr)))

(defn lexv-count
  "Get the number of tokens in the lexv.

   ZIL: <GETB ,P-LEXV ,P-LEXWORDS>"
  [game-state]
  (get-in game-state [:parser :lexv :count] 0))
