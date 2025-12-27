(ns clork.parser.state
  "Parser state structure, constants, and initialization.
   This is a separate namespace to avoid circular dependencies.")

;;;; ============================================================================
;;;; PARSER STATE - Constants, Tables, and Initialization
;;;; ============================================================================
;;;;
;;;; This file contains:
;;;;   - Parser constants (table indices, offsets, bit flags)
;;;;   - Parser state structure (initial-parser-state)
;;;;   - Low-level state initialization (parser-init, parser-init-tbl)
;;;;
;;;; ZIL Reference: gparser.zil lines 1-130, specifically:
;;;;   - Global variables: P-TABLE, P-SYNTAX, P-ITBL, P-OTBL, P-VTBL, etc.
;;;;   - Constants: P-VERB, P-PREP1, P-NC1, P-ITBLLEN, etc.
;;;;   - The initialization portion of the PARSER routine
;;;;
;;;; Data Flow:
;;;;   This is the foundation layer. All other parser modules depend on the
;;;;   state structure defined here. The parser state lives in game-state
;;;;   under the :parser key.
;;;;
;;;; ============================================================================

;;; ---------------------------------------------------------------------------
;;; CONSTANTS - Table Indices and Offsets
;;; ---------------------------------------------------------------------------

;; Sentence break characters that terminate clauses
;; ZIL: <SETG SIBREAKS ".,\"">
(def sibreaks #{"." "," "\""})

;; Length of the instruction table (ITBL)
;; ZIL: <CONSTANT P-ITBLLEN 9>
(def p-itbllen 9)

;;; ---------------------------------------------------------------------------
;;; ITBL (Instruction Table) Layout
;;; ---------------------------------------------------------------------------
;;; The ITBL is a 10-word table that stores the parsed structure of a command.
;;; After parsing "PUT THE LAMP IN THE CASE", ITBL would contain:
;;;   [0] P-VERB   = verb number (e.g., ACT?PUT)
;;;   [1] P-VERBN  = pointer to verb word in lexv
;;;   [2] P-PREP1  = first preposition number (e.g., PR?IN)
;;;   [3] P-PREP1N = pointer to prep word
;;;   [4] P-PREP2  = second preposition number (for 3-object commands)
;;;   [5] P-PREP2N = pointer to second prep word
;;;   [6] P-NC1    = start of first noun clause ("THE LAMP")
;;;   [7] P-NC1L   = end of first noun clause
;;;   [8] P-NC2    = start of second noun clause ("THE CASE")
;;;   [9] P-NC2L   = end of second noun clause
;;;
;;; ZIL Constants:
;;;   <CONSTANT P-VERB 0>
;;;   <CONSTANT P-VERBN 1>
;;;   <CONSTANT P-PREP1 2>
;;;   <CONSTANT P-PREP1N 3>
;;;   <CONSTANT P-PREP2 4>
;;;   <CONSTANT P-PREP2N 5>
;;;   <CONSTANT P-NC1 6>
;;;   <CONSTANT P-NC1L 7>
;;;   <CONSTANT P-NC2 8>
;;;   <CONSTANT P-NC2L 9>

(def itbl-indices
  {:verb   0   ; Verb action number
   :verbn  1   ; Pointer to verb word
   :prep1  2   ; First preposition number
   :prep1n 3   ; Pointer to first prep word
   :prep2  4   ; Second preposition number
   :prep2n 5   ; Pointer to second prep word
   :nc1    6   ; Start of direct object noun clause
   :nc1l   7   ; End of direct object noun clause
   :nc2    8   ; Start of indirect object noun clause
   :nc2l   9}) ; End of indirect object noun clause

;;; ---------------------------------------------------------------------------
;;; SYNTAX TABLE Layout
;;; ---------------------------------------------------------------------------
;;; Each verb has a syntax table defining valid sentence structures.
;;; ZIL: <CONSTANT P-SYNLEN 8> - each syntax entry is 8 bytes
;;;
;;; Byte offsets within a syntax entry:
;;;   [0] P-SBITS   = flags (number of objects, etc.)
;;;   [1] P-SPREP1  = required preposition for object 1
;;;   [2] P-SPREP2  = required preposition for object 2
;;;   [3] P-SFWIM1  = FIND flags for GWIM on object 1
;;;   [4] P-SFWIM2  = FIND flags for GWIM on object 2
;;;   [5] P-SLOC1   = location bits for object 1
;;;   [6] P-SLOC2   = location bits for object 2
;;;   [7] P-SACTION = action routine to call
;;;
;;; P-SBITS flags (P-SONUMS mask = 3):
;;;   Bits 0-1: Number of objects required (0, 1, or 2)

(def syntax-offsets
  {:sbits   0
   :sprep1  1
   :sprep2  2
   :sfwim1  3
   :sfwim2  4
   :sloc1   5
   :sloc2   6
   :saction 7})

(def p-synlen 8)    ; Length of each syntax table entry
(def p-sonums 3)    ; Mask for number-of-objects bits

;;; ---------------------------------------------------------------------------
;;; LOCATION BITS - Where to search for objects
;;; ---------------------------------------------------------------------------
;;; NOTE: search-bits and getflags are now defined in game_state.clj
;;; so they're available to verb_defs.clj before parser loads.

;;; ---------------------------------------------------------------------------
;;; PARSER STATE STRUCTURE
;;; ---------------------------------------------------------------------------
;;; This map lives in game-state under :parser. It tracks all parser state
;;; across calls, enabling features like AGAIN, OOPS, and orphan resolution.

(defn initial-parser-state
  "Create the initial parser state map.

   This corresponds to all the parser globals in ZIL (P-* variables).
   The state is immutable and threaded through parser functions."
  []
  {
   ;; === Core Parse Results ===
   ;; These are the final outputs: what action with what objects

   ;; <GLOBAL PRSA <>> - Parsed Result: Action (verb)
   :prsa nil
   ;; <GLOBAL PRSO <>> - Parsed Result: Direct Object
   :prso nil
   ;; <GLOBAL PRSI <>> - Parsed Result: Indirect Object
   :prsi nil

   ;; === Tables ===

   ;; <GLOBAL P-TABLE 0> - Current match table being filled
   :table nil
   ;; <GLOBAL P-SYNTAX 0> - Matched syntax entry
   :syntax nil
   ;; <GLOBAL P-ONEOBJ 0> - Word for "one" object reference
   :oneobj nil

   ;; <GLOBAL P-ITBL ...> - Input/Instruction table (current parse)
   ;; See ITBL Layout documentation above
   :itbl (vec (repeat 10 0))

   ;; <GLOBAL P-OTBL ...> - Output table (previous parse, for AGAIN/orphans)
   :otbl (vec (repeat 10 0))

   ;; <GLOBAL P-VTBL ...> - Verb table scratch space
   :vtbl [0 0 0 0]

   ;; <GLOBAL P-OVTBL ...> - Old verb table
   :ovtbl [0 0 0 0]

   ;; <GLOBAL P-BUTS ...> - "but/except" exclusion list
   :buts nil

   ;; === Clause Copy Table ===
   ;; Used by CLAUSE-COPY to track source/dest pointers
   ;; <GLOBAL P-CCTBL <TABLE 0 0 0 0>>
   :cctbl {:sbptr nil   ; CC-SBPTR: source begin pointer
           :septr nil   ; CC-SEPTR: source end pointer
           :dbptr nil   ; CC-DBPTR: dest begin pointer
           :deptr nil}  ; CC-DEPTR: dest end pointer

   ;; === Lexical Tables ===
   ;; These store tokenized input for processing and replay

   ;; <GLOBAL P-LEXV ...> - Main lexical value table (current input)
   :lexv nil
   ;; <GLOBAL AGAIN-LEXV ...> - Saved for AGAIN command
   :again-lexv nil
   ;; <GLOBAL RESERVE-LEXV ...> - Overflow for multi-command input
   :reserve-lexv nil
   ;; <GLOBAL RESERVE-PTR <>> - Pointer into reserve-lexv
   :reserve-ptr nil

   ;; <GLOBAL P-CONT <>> - Continuation pointer for "then" chains
   :cont nil

   ;; === Parsing State ===

   ;; <GLOBAL P-LEN 0> - Remaining words to parse
   :len 0
   ;; <GLOBAL P-DIR 0> - Direction if this is a movement command
   :dir nil
   ;; <GLOBAL P-NCN 0> - Number of noun clauses found (0, 1, or 2)
   :ncn 0

   ;; Current adjective/noun being matched
   ;; <GLOBAL P-NAM <>> - Current noun word
   :nam nil
   ;; <GLOBAL P-ADJ <>> - Current adjective value
   :adj nil
   ;; <GLOBAL P-ADJN <>> - Current adjective word
   :adjn nil

   ;; <GLOBAL P-GETFLAGS 0> - Flags for GET-OBJECT (all/one/inhibit)
   :getflags 0

   ;; <GLOBAL P-SLOCBITS 0> - Current search location bits
   :slocbits 0

   ;; <GLOBAL P-GWIMBIT 0> - Required flag for GWIM matching
   :gwimbit 0

   ;; === Flags ===

   ;; <GLOBAL QUOTE-FLAG <>> - Inside quoted text (for SAY command)
   :quote-flag false
   ;; <GLOBAL P-END-ON-PREP <>> - Sentence ended on a preposition
   :end-on-prep false
   ;; <GLOBAL P-OFLAG <>> - Orphaning in progress
   :oflag false
   ;; <GLOBAL P-MERGED <>> - Previous parse was merged with current
   :merged false
   ;; <GLOBAL P-AND <>> - Found AND/COMMA in object list
   :and-flag false
   ;; <GLOBAL P-ADVERB <>> - Adverb found
   :adverb nil

   ;; === Orphan State ===
   ;; When a command is incomplete ("take" with no object), we enter
   ;; orphan mode and prompt for clarification.

   ;; <GLOBAL P-ACLAUSE <>> - Orphaned clause location
   :aclause nil
   ;; <GLOBAL P-ANAM <>> - Orphaned noun
   :anam nil
   ;; <GLOBAL P-AADJ <>> - Orphaned adjective
   :aadj nil

   ;; === Not-Here Tracking ===
   ;; When an object isn't accessible, these track what was asked for
   ;; <GLOBAL P-XNAM <>>
   :xnam nil
   ;; <GLOBAL P-XADJ <>>
   :xadj nil
   ;; <GLOBAL P-XADJN <>>
   :xadjn nil

   ;; === Direction and Walk State ===
   ;; <GLOBAL P-WALK-DIR <>> - Direction for WALK command
   :walk-dir nil
   ;; <GLOBAL AGAIN-DIR <>> - Saved direction for AGAIN
   :again-dir nil

   ;; === Numbers ===
   ;; <GLOBAL P-NUMBER 0> - Parsed number value
   :number 0
   ;; <GLOBAL P-DIRECTION 0> - Direction number
   :direction 0

   ;; === Saved State for Restoration ===
   ;; <SET OWINNER ,WINNER> - Saved winner
   :owinner nil
   ;; <SET OMERGED ,P-MERGED> - Saved merged flag
   :omerged nil

   ;; === Verb Tracking ===
   ;; Internal: current verb being processed
   :verb nil
   ;; <GLOBAL P-ACT <>> - Action for clause parsing
   :act nil

   ;; === IT-Object Tracking ===
   ;; <GLOBAL P-IT-OBJECT <>> - Last referenced object (for "it")
   :it-object nil})

;;; ---------------------------------------------------------------------------
;;; STATE INITIALIZATION
;;; ---------------------------------------------------------------------------

(defn parser-init-tbl
  "Copy the input table to the output table and blank the input table.

   This preserves the previous parse for AGAIN and orphan merging.

   ZIL: Lines 111-116 of PARSER routine
     <REPEAT ()
       <COND (<G? <SET CNT <+ .CNT 1>> ,P-ITBLLEN> <RETURN>)
         (T
           <COND (<NOT ,P-OFLAG>
             <PUT ,P-OTBL .CNT <GET ,P-ITBL .CNT>>)>
           <PUT ,P-ITBL .CNT 0>)>>

   Note: The conditional inside the loop (checking P-OFLAG each iteration)
   seems odd, but we preserve the original logic for accuracy."
  [game-state]
  (let [oflag (get-in game-state [:parser :oflag])
        itbl (get-in game-state [:parser :itbl])]
    (-> game-state
        ;; Copy itbl -> otbl only if not in orphan mode
        (cond-> (not oflag)
          (assoc-in [:parser :otbl] itbl))
        ;; Clear itbl
        (assoc-in [:parser :itbl] (vec (repeat 10 0))))))

(defn clear-itbl
  "Reset the instruction table to all zeros."
  [game-state]
  (assoc-in game-state [:parser :itbl] (vec (repeat 10 0))))

(defn parser-init
  "Initialize parser state for a new parse.

   Called at the start of each PARSER invocation to reset transient state
   while preserving information needed for AGAIN and orphan resolution.

   ZIL: Lines 109-124 of PARSER routine"
  [game-state]
  ;; Note: We can't use the accessors defined later in this file here,
  ;; so we use direct assoc-in for prso/prsi. The accessors are available
  ;; to all other modules that require this namespace.
  (-> game-state
      ;; Copy tables (preserves previous parse)
      (parser-init-tbl)
      ;; Save current winner/merged for potential restoration
      ;; <SET OWINNER ,WINNER>
      (assoc-in [:parser :owinner] (:winner game-state))
      ;; <SET OMERGED ,P-MERGED>
      (assoc-in [:parser :omerged] (get-in game-state [:parser :merged]))
      ;; Reset transient flags
      ;; <SETG P-ADVERB <>>
      (assoc-in [:parser :adverb] nil)
      ;; <SETG P-MERGED <>>
      (assoc-in [:parser :merged] false)
      ;; <SETG P-END-ON-PREP <>>
      (assoc-in [:parser :end-on-prep] false)
      ;; Clear match tables
      ;; <PUT ,P-PRSO ,P-MATCHLEN 0>
      (assoc-in [:parser :prso] nil)
      ;; <PUT ,P-PRSI ,P-MATCHLEN 0>
      (assoc-in [:parser :prsi] nil)
      ;; <PUT ,P-BUTS ,P-MATCHLEN 0>
      (assoc-in [:parser :buts] nil)))

;;; ---------------------------------------------------------------------------
;;; PARSER RESULT HELPERS
;;; ---------------------------------------------------------------------------
;;; Standard result structure for parser pipeline functions.
;;; All parser functions should return {:success bool :game-state gs :error map?}
;;;
;;; Using these helpers ensures consistency and prevents bugs like the
;;; (map? result) check that fails because game-states are also maps.

(defn parser-success
  "Return a successful parser result.

   Usage: (parser-success game-state)"
  [game-state]
  {:success true
   :game-state game-state
   :error nil})

(defn parser-error
  "Return a failed parser result.

   Usage: (parser-error game-state :error-type \"message\")"
  ([game-state error-type]
   (parser-error game-state error-type nil))
  ([game-state error-type message]
   {:success false
    :game-state game-state
    :error {:type error-type :message message}}))

(defn parser-result?
  "Check if a value is a parser result (has :success key).

   Use this instead of (map? x) to distinguish results from game-states."
  [x]
  (and (map? x) (contains? x :success)))

;;; ---------------------------------------------------------------------------
;;; PARSER STATE ACCESSORS
;;; ---------------------------------------------------------------------------
;;; These functions centralize access to commonly-used nested state paths,
;;; making it easier to refactor the state structure and improving readability.

;; --- Generic Accessors ---
;; Base functions that specific accessors are built on.

(defn get-parser-field
  "Get a value from parser state by field keyword."
  ([game-state field]
   (get-in game-state [:parser field]))
  ([game-state field default]
   (get-in game-state [:parser field] default)))

(defn set-parser-field
  "Set a value in parser state by field keyword."
  [game-state field value]
  (assoc-in game-state [:parser field] value))

(defn update-parser-field
  "Update a value in parser state with a function."
  [game-state field f]
  (update-in game-state [:parser field] f))

(defn get-table-val
  "Get a value from a parser table (itbl, otbl, etc.) by key.
   Key can be a keyword (:verb, :nc1, etc.) or an integer index."
  [game-state table-name key]
  (let [idx (if (keyword? key) (key itbl-indices) key)]
    (get-in game-state [:parser table-name idx])))

(defn set-table-val
  "Set a value in a parser table (itbl, otbl, etc.) by key.
   Key can be a keyword (:verb, :nc1, etc.) or an integer index."
  [game-state table-name key val]
  (let [idx (if (keyword? key) (key itbl-indices) key)]
    (assoc-in game-state [:parser table-name idx] val)))

;; --- ITBL/OTBL (Instruction Table) Accessors ---
;; The ITBL stores parsed command structure: verb, prepositions, noun clause pointers.
;; OTBL stores the previous parse for AGAIN and orphan resolution.

(defn get-itbl
  "Get a value from the instruction table by key."
  [game-state key]
  (get-table-val game-state :itbl key))

(defn set-itbl
  "Set a value in the instruction table by key."
  [game-state key val]
  (set-table-val game-state :itbl key val))

(defn get-otbl
  "Get a value from the old instruction table by key."
  [game-state key]
  (get-table-val game-state :otbl key))

(defn set-otbl
  "Set a value in the old instruction table by key."
  [game-state key val]
  (set-table-val game-state :otbl key val))

;; --- Core Parser State Accessors ---

(defn get-ncn  "Get the noun clause number (0, 1, or 2)." [game-state]
  (get-parser-field game-state :ncn 0))

(defn set-ncn  "Set the noun clause number." [game-state ncn]
  (set-parser-field game-state :ncn ncn))

(defn inc-ncn  "Increment the noun clause number." [game-state]
  (update-parser-field game-state :ncn inc))

(defn dec-ncn  "Decrement the noun clause number." [game-state]
  (update-parser-field game-state :ncn dec))

(defn get-len  "Get the remaining token count." [game-state]
  (get-parser-field game-state :len 0))

(defn set-len  "Set the remaining token count." [game-state len]
  (set-parser-field game-state :len len))

(defn inc-len  "Increment the token count (used when un-consuming a token)." [game-state]
  (update-parser-field game-state :len inc))

(defn dec-len  "Decrement the token count (used when consuming a token)." [game-state]
  (update-parser-field game-state :len dec))

(defn get-parser-error  "Get the parser error map, or nil if no error." [game-state]
  (get-parser-field game-state :error))

(defn set-parser-error  "Set the parser error. Pass nil to clear." [game-state error]
  (set-parser-field game-state :error error))

;; --- Parsed Results Accessors (prsa/prso/prsi) ---

(defn get-prsa  "Get the parsed action (verb)." [game-state]
  (get-parser-field game-state :prsa))

(defn set-prsa  "Set the parsed action (verb)." [game-state action]
  (set-parser-field game-state :prsa action))

(defn get-prso  "Get the parsed direct object(s)." [game-state]
  (get-parser-field game-state :prso))

(defn set-prso  "Set the parsed direct object(s)." [game-state objs]
  (set-parser-field game-state :prso objs))

(defn get-prsi  "Get the parsed indirect object(s)." [game-state]
  (get-parser-field game-state :prsi))

(defn set-prsi  "Set the parsed indirect object(s)." [game-state objs]
  (set-parser-field game-state :prsi objs))
