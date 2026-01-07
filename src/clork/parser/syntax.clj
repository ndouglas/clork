(ns clork.parser.syntax
  "Syntax checking and GWIM (Get What I Mean) for the parser.

   This module handles:
   - SYNTAX-CHECK - Validate command against verb's syntax patterns
   - SYNTAX-FOUND - Store matched syntax
   - GWIM - \"Get What I Mean\" - infer missing objects
   - Verb syntax table structure

   ZIL Reference: gparser.zil
   - Lines 693-705: Syntax constants
   - Lines 707-775: SYNTAX-CHECK routine
   - Lines 895-897: SYNTAX-FOUND routine
   - Lines 901-926: GWIM routine"
  (:require [clork.utils :as utils]
            [clork.game-state :as game-state]
            [clork.verb-defs :as verb-defs]
            [clork.parser.state :as parser-state]
            [clork.parser.objects :as objects]
            [clork.parser.orphan :as orphan]))

;;;; ============================================================================
;;;; PARSER SYNTAX - Syntax Checking and GWIM
;;;; ============================================================================
;;;;
;;;; What is Syntax Checking?
;;;;   Each verb has one or more valid syntax patterns. For example:
;;;;     TAKE: [object]              "take lamp"
;;;;     PUT:  [object] IN [object]  "put lamp in case"
;;;;     LOOK: (no objects)          "look"
;;;;
;;;;   SYNTAX-CHECK finds a pattern matching the parsed input.
;;;;   If no pattern matches, it may use GWIM to infer missing objects,
;;;;   or enter orphan mode to ask for clarification.
;;;;
;;;; What is GWIM?
;;;;   "Get What I Mean" tries to figure out what object the player meant
;;;;   when they didn't specify one. For example:
;;;;     "turn on" (no object specified)
;;;;   GWIM looks for an object with the LIGHTABLE flag that's accessible.
;;;;   If exactly one matches, it uses that object.
;;;;
;;;; ============================================================================

;;; ---------------------------------------------------------------------------
;;; FORWARD DECLARATIONS
;;; ---------------------------------------------------------------------------

(declare syntax-found try-gwim)

;;; ---------------------------------------------------------------------------
;;; VERB SYNTAX TABLES
;;; ---------------------------------------------------------------------------
;;; Each verb maps to a list of valid syntax patterns.
;;; A syntax pattern specifies:
;;;   - Required prepositions
;;;   - Number of objects
;;;   - GWIM hints (what flags to look for)
;;;   - Location constraints (must be held, in room, etc.)
;;;   - The action routine to call
;;;
;;; Structure (from ZIL constants):
;;;   P-SBITS   [0] - Flags: bits 0-1 = number of objects (0, 1, or 2)
;;;   P-SPREP1  [1] - Required preposition before object 1 (or 0)
;;;   P-SPREP2  [2] - Required preposition before object 2 (or 0)
;;;   P-SFWIM1  [3] - GWIM flags for object 1 (what property to search for)
;;;   P-SFWIM2  [4] - GWIM flags for object 2
;;;   P-SLOC1   [5] - Location bits for object 1 (search-bits)
;;;   P-SLOC2   [6] - Location bits for object 2
;;;   P-SACTION [7] - Action routine to invoke

(defn make-syntax
  "Create a syntax pattern entry.

   Arguments:
     num-objects - 0, 1, or 2 objects required
     prep1 - preposition before first object (nil if none)
     prep2 - preposition before second object (nil if none)
     gwim1 - GWIM flag for first object (what flag the object should have)
     gwim2 - GWIM flag for second object
     loc1 - location bits for first object (where to search)
     loc2 - location bits for second object
     action - the action keyword to invoke

   Example:
     (make-syntax 1 nil nil :lightable nil :held nil :light)
     ; TURN ON requires 1 object, no prep, object must be lightable and held"
  [num-objects prep1 prep2 gwim1 gwim2 loc1 loc2 action]
  {:num-objects num-objects
   :prep1 prep1
   :prep2 prep2
   :gwim1 gwim1
   :gwim2 gwim2
   :loc1 loc1
   :loc2 loc2
   :action action})

;; verb-syntaxes is built from verb-definitions in verb_defs.clj

(def verb-syntaxes
  "Map of verb keywords to their valid syntax patterns.

   This is derived from *verb-syntaxes* (from verb_defs.clj).

   Example entries:
   :take [{:num-objects 1, :prep1 nil, ...}]
   :put  [{:num-objects 2, :prep1 nil, :prep2 :in, ...}
          {:num-objects 2, :prep1 nil, :prep2 :on, ...}]"
  verb-defs/*verb-syntaxes*)

;;; ---------------------------------------------------------------------------
;;; SYNTAX CHECKING
;;; ---------------------------------------------------------------------------

(defn syntax-matches?
  "Check if a syntax pattern matches the parsed input.

   Compares:
   - Number of objects (from P-NCN) against pattern requirement
   - Prepositions in ITBL against pattern requirements

   For orphan mode support: when ncn=0, we don't require prep1 to match
   because the user will provide the prep with the object in response.
   Similarly for prep2 when ncn < num-objects.

   Returns true if the pattern could match this input."
  [game-state syntax]
  (let [ncn (parser-state/get-ncn game-state)
        prep1 (parser-state/get-itbl game-state :prep1)
        prep2 (parser-state/get-itbl game-state :prep2)
        ;; Normalize prep values - 0 means nil (no prep)
        prep1 (when (and prep1 (not= prep1 0)) prep1)
        prep2 (when (and prep2 (not= prep2 0)) prep2)
        num-objects (:num-objects syntax)]
    (and
     ;; Number of noun clauses must match or be handleable (GWIM/orphan)
     (<= ncn num-objects)
     ;; Preposition 1 matching:
     ;; - If ncn=0, we're in orphan mode - prep1 will come with the noun, so don't check
     ;; - Otherwise, prep1 must match exactly
     (or (zero? ncn)
         (= prep1 (:prep1 syntax)))
     ;; Preposition 2 matching:
     ;; - If ncn < 2, prep2 not yet parsed, so don't check
     ;; - Otherwise, prep2 must match exactly
     (or (< ncn 2)
         (= prep2 (:prep2 syntax))))))

(defn find-matching-syntax
  "Find all syntax patterns that could match the current parse.

   Returns a list of matching patterns. May include patterns that need
   GWIM to fill in missing objects."
  [game-state verb]
  (when-let [syntaxes (get verb-syntaxes verb)]
    (filter #(syntax-matches? game-state %) syntaxes)))

(defn syntax-check
  "Validate the parsed command against verb syntax patterns.

   ZIL: SYNTAX-CHECK routine, lines 707-775

   This function:
   1. Gets the verb from ITBL
   2. Looks up valid syntax patterns for that verb
   3. Finds patterns matching the parsed prepositions and object count
   4. First tries GWIM for patterns that need more objects (if they have GWIM hints)
   5. Falls back to exact match if GWIM fails
   6. If still no match: enters orphan mode

   Returns: parser result (use parser-success/parser-error helpers)"
  [game-state]
  (let [verb (parser-state/get-itbl game-state :verb)]
    (if (nil? verb)
      ;; No verb found
      (parser-state/parser-error game-state :no-verb "There was no verb in that sentence!")

      ;; Find matching syntaxes
      (let [matches (find-matching-syntax game-state verb)
            ncn (parser-state/get-ncn game-state)
            merged? (get-in game-state [:parser :merged])
            ;; Separate exact matches from those needing GWIM/orphan
            exact-matches (filter #(= (:num-objects %) ncn) matches)
            ;; Patterns that need more objects than provided (for GWIM or orphan mode)
            higher-matches (filter #(> (:num-objects %) ncn) matches)
            ;; Of those, which have GWIM hints for the missing object?
            gwim-capable (filter #(cond
                                    (zero? ncn) (:gwim1 %)
                                    (= ncn 1) (:gwim2 %)
                                    :else false)
                                 higher-matches)]

        (cond
          ;; In merged mode (post-orphan), try GWIM first for higher-object syntaxes
          ;; This handles "dig in sand" finding the shovel automatically
          (and merged? (seq gwim-capable))
          (let [gwim-result (try-gwim game-state gwim-capable)]
            (if (:success gwim-result)
              gwim-result
              ;; GWIM failed - fall back to exact match
              (if (seq exact-matches)
                (syntax-found game-state (first exact-matches))
                gwim-result)))

          ;; Exact match - use it (normal mode or no GWIM patterns)
          (seq exact-matches)
          (syntax-found game-state (first exact-matches))

          ;; Need GWIM/orphan for missing object(s) (no exact match available)
          (seq higher-matches)
          (try-gwim game-state higher-matches)

          ;; No matching syntax at all
          :else
          (parser-state/parser-error game-state :bad-syntax
                                     "That sentence isn't one I recognize."))))))

(defn syntax-found
  "Store the matched syntax pattern.

   ZIL: SYNTAX-FOUND routine, lines 895-897
     <ROUTINE SYNTAX-FOUND (SYN)
       <SETG P-SYNTAX .SYN>
       <SETG PRSA <GETB .SYN ,P-SACTION>>>

   When syntax doesn't have an :action field, uses the verb keyword as default."
  [game-state syntax]
  (let [;; Use syntax's :action if present, otherwise fall back to verb keyword
        verb (parser-state/get-itbl game-state :verb)
        action (or (:action syntax) verb)]
    (parser-state/parser-success (-> game-state
                                     (assoc-in [:parser :syntax] syntax)
                                     (parser-state/set-prsa action)))))

;;; ---------------------------------------------------------------------------
;;; GWIM - Get What I Mean
;;; ---------------------------------------------------------------------------

(defn gwim
  "Try to infer a missing object based on context.

   ZIL: GWIM routine, lines 901-926

   Arguments:
     game-state - current state
     gwim-flag - what flag the object should have (e.g., :lightable)
     loc-bits - where to search (e.g., :held, :in-room)
     prep - preposition context (for printing)

   GWIM searches accessible objects for ones matching the required flag.
   If exactly one matches, it's used automatically with a message like
   '(the brass lantern)'.

   If zero or multiple match, GWIM fails and we need to ask the player.

   Returns:
     {:found true, :object obj, :game-state gs} - exactly one match
     {:found false, :game-state gs} - zero or multiple matches"
  [game-state gwim-flag loc-bits prep]
  ;; Special case: RMUNGBIT means we want ROOMS (for actions that affect rooms)
  (if (= gwim-flag :room)
    {:found true
     :object :rooms
     :game-state game-state}

    ;; Normal case: search for matching objects
    ;; ZIL: <SETG P-GWIMBIT .GBIT> <SETG P-SLOCBITS .LBIT> <GET-OBJECT ,P-MERGE <>>
    (let [;; Thread state properly - set gwimbit and clear nam/adj for flag-only search
          gs (-> game-state
                 (assoc-in [:parser :gwimbit] gwim-flag)
                 (assoc-in [:parser :slocbits] loc-bits)
                 (assoc-in [:parser :nam] nil)    ; Clear noun - we want flag match only
                 (assoc-in [:parser :adj] nil))   ; Clear adjective

          ;; Actually call get-object to search for objects with this flag
          result (objects/get-object gs [] false)  ; false = don't print errors
          matches (when (:success result) (:matches result))
          gs-after (:game-state result)]

      (cond
        ;; Exactly one match - use it with feedback message
        (= (count matches) 1)
        (let [obj (first matches)
              ;; Print inference message: "(the brass lantern)"
              ;; Also clear gwimbit so snarf-objects doesn't filter by it
              ;; ZIL adds a blank line after the inference message
              gs-with-msg (-> gs-after
                              (assoc-in [:parser :gwimbit] nil)  ; Clear after success
                              (utils/tell "(")
                              (cond-> (and prep (not (get-in gs-after [:parser :end-on-prep])))
                                (utils/tell (str (name prep) " ")))
                              (utils/tell (str "the " (game-state/thing-name gs-after obj)))
                              (utils/tell ")\n\n"))]  ; Double newline for blank line
          {:found true
           :object obj
           :game-state gs-with-msg})

        ;; Zero or multiple matches - fail
        :else
        {:found false
         :game-state (-> gs-after
                         (assoc-in [:parser :gwimbit] nil))}))))

(defn try-gwim
  "Try to use GWIM to fill in missing objects for syntax patterns.

   Iterates through patterns that need GWIM (where num-objects > ncn)
   and tries to infer the missing object. Uses the first pattern
   where GWIM succeeds.

   If GWIM fails for all patterns, enters orphan mode to ask the player
   for the missing object."
  [game-state patterns]
  (let [ncn (parser-state/get-ncn game-state)]
    (loop [remaining patterns]
      (if (empty? remaining)
        ;; No pattern worked with GWIM - enter orphan mode to ask player
        ;; ZIL: SYNTAX-CHECK enters orphan mode when GWIM fails
        (let [pattern (first patterns)  ; Use first pattern for orphan prompt
              missing-first? (and (zero? ncn)
                                  (>= (:num-objects pattern) 1))
              drive1 (when missing-first? pattern)
              drive2 (when (not missing-first?) pattern)
              gs (orphan/orphan game-state drive1 drive2)
              ;; for-second-object? is the opposite of missing-first?
              prompt (orphan/prompt-for-object gs pattern (not missing-first?))]
          ;; Set oflag and return orphan error with prompt
          {:success false
           :game-state (assoc-in gs [:parser :oflag] true)
           :error {:type :orphan
                   :message prompt}})
        (let [pattern (first remaining)
              ;; Determine which object is missing
              missing-first? (and (zero? ncn)
                                  (>= (:num-objects pattern) 1))
              missing-second? (and (= ncn 1)
                                   (= (:num-objects pattern) 2))
              ;; Try GWIM for the missing object
              gwim-result
              (cond
                missing-first?
                (gwim game-state
                      (:gwim1 pattern)
                      (:loc1 pattern)
                      (:prep1 pattern))

                missing-second?
                (gwim game-state
                      (:gwim2 pattern)
                      (:loc2 pattern)
                      (:prep2 pattern))

                :else
                {:found false :game-state game-state})]

          (if (:found gwim-result)
            ;; GWIM succeeded - store the object and use this syntax
            (let [gs (:game-state gwim-result)
                  obj (:object gwim-result)
                  gs-with-obj
                  (if missing-first?
                    (parser-state/set-prso gs [obj])
                    (parser-state/set-prsi gs [obj]))]
              (syntax-found gs-with-obj pattern))

            ;; GWIM failed - try next pattern
            (recur (rest remaining))))))))

;;; ---------------------------------------------------------------------------
;;; VERB TABLE LOOKUP
;;; ---------------------------------------------------------------------------

(defn get-verb-syntaxes
  "Look up the syntax patterns for a verb.

   ZIL: <SET SYN <GET ,VERBS <- 255 .VERB>>>"
  [verb]
  (get verb-syntaxes verb))
