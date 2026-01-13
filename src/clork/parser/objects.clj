(ns clork.parser.objects
  "Object matching and resolution for the parser.

   This module handles:
   - SNARF-OBJECTS - Extract objects from parsed noun clauses
   - SNARFEM - Process a single noun clause into object list
   - GET-OBJECT - Find objects matching adjective/noun criteria
   - SEARCH-LIST - Search a container for matching objects
   - THIS-IT? - Test if an object matches the criteria
   - OBJ-FOUND - Add an object to the match table
   - GLOBAL-CHECK - Search global/pseudo objects
   - BUT-MERGE - Handle \"all but X\" exclusions

   ZIL Reference: gparser.zil
   - Lines 928-943: SNARF-OBJECTS routine
   - Lines 978-1030: SNARFEM routine
   - Lines 1040-1140: GET-OBJECT routine
   - Lines 1216-1237: SEARCH-LIST routine
   - Lines 1357-1370: THIS-IT? routine
   - Lines 1239-1242: OBJ-FOUND routine
   - Lines 1169-1200: GLOBAL-CHECK routine
   - Lines 945-958: BUT-MERGE routine"
  (:require [clork.game-state :as game-state]
            [clork.parser.state :as parser-state]
            [clork.parser.lexer :as lexer]
            [clork.random :as random]
            [clork.debug.trace :as trace]))

(defn- bit-set?
  "Check if a bit mask is set in a value.

   Unlike bit-test which takes a bit position, this takes a bit mask (value).
   Example: (bit-set? 52 4) checks if bit value 4 is set in 52."
  [value mask]
  (pos? (bit-and (or value 0) mask)))

;;;; ============================================================================
;;;; PARSER OBJECTS - Object Matching and Resolution
;;;; ============================================================================
;;;;
;;;; How Object Resolution Works:
;;;;   1. SNARF-OBJECTS is called after syntax check passes
;;;;   2. For each noun clause (NC1, NC2), SNARFEM processes the words
;;;;   3. SNARFEM extracts adjective/noun and calls GET-OBJECT
;;;;   4. GET-OBJECT searches accessible objects using SEARCH-LIST
;;;;   5. THIS-IT? tests each object against the adjective/noun criteria
;;;;   6. Results are accumulated in P-PRSO/P-PRSI match tables
;;;;
;;;; Match Tables:
;;;;   P-PRSO and P-PRSI are tables that accumulate matching objects.
;;;;   Element 0 is the count (P-MATCHLEN), elements 1+ are objects.
;;;;   This allows commands like "take all swords" to match multiple objects.
;;;;
;;;; ============================================================================

;;; ---------------------------------------------------------------------------
;;; FORWARD DECLARATIONS
;;; ---------------------------------------------------------------------------

(declare global-check do-sl but-merge)

;;; ---------------------------------------------------------------------------
;;; MATCH TABLE OPERATIONS
;;; ---------------------------------------------------------------------------

(defn obj-found
  "Add an object to a match table.

   ZIL: OBJ-FOUND routine, lines 1239-1242
     <ROUTINE OBJ-FOUND (OBJ TBL \"AUX\" PTR)
       <SET PTR <GET .TBL ,P-MATCHLEN>>
       <PUT .TBL <+ .PTR 1> .OBJ>
       <PUT .TBL ,P-MATCHLEN <+ .PTR 1>>>

   In Clojure, we use a vector where the first element is the count."
  [match-table obj]
  (conj (or match-table []) obj))

(defn match-table-count
  "Get the number of objects in a match table."
  [match-table]
  (count (or match-table [])))

;;; ---------------------------------------------------------------------------
;;; THIS-IT? - Object Matching
;;; ---------------------------------------------------------------------------

(defn this-it?
  "Test if an object matches the current search criteria.

   ZIL: THIS-IT? routine, lines 1357-1370
     <ROUTINE THIS-IT? (OBJ TBL \"AUX\" SYNS)
       <COND (<FSET? .OBJ ,INVISIBLE> <RFALSE>)
         (<AND ,P-NAM
           <NOT <ZMEMQ ,P-NAM <SET SYNS <GETPT .OBJ ,P?SYNONYM>> ...>>>
           <RFALSE>)
         (<AND ,P-ADJ
           <OR <NOT <SET SYNS <GETPT .OBJ ,P?ADJECTIVE>>>
               <NOT <ZMEMQB ,P-ADJ .SYNS ...>>>>
           <RFALSE>)
         (<AND <NOT <ZERO? ,P-GWIMBIT>> <NOT <FSET? .OBJ ,P-GWIMBIT>>>
           <RFALSE>)>
       <RTRUE>>

   Criteria checked:
   1. Object must not be invisible
   2. If P-NAM set, object's synonyms must include that noun
   3. If P-ADJ set, object's adjectives must include that adjective
   4. If P-GWIMBIT set, object must have that flag

   Arguments:
     game-state - current state
     obj-id - object to test
     match-table - which table we're filling (for context)

   Returns: true if object matches all criteria"
  [game-state obj-id]
  (let [obj (game-state/get-thing game-state obj-id)
        nam (get-in game-state [:parser :nam])
        adj (get-in game-state [:parser :adj])
        gwimbit (get-in game-state [:parser :gwimbit])
        ;; Object synonyms are stored as :synonym (vector of strings)
        synonyms (set (map clojure.string/lower-case (or (:synonym obj) [])))
        ;; Object adjectives can be string or vector
        adjectives (let [a (:adjective obj)]
                     (set (map clojure.string/lower-case
                               (cond
                                 (nil? a) []
                                 (string? a) [a]
                                 (sequential? a) a
                                 :else []))))]

    (and
     ;; Not invisible
     (not (game-state/set-thing-flag? game-state obj-id :invisible))

     ;; Name matches (if specified)
     (or (nil? nam)
         (contains? synonyms (clojure.string/lower-case (str nam))))

     ;; Adjective matches (if specified)
     (or (nil? adj)
         (contains? adjectives (clojure.string/lower-case (str adj))))

     ;; GWIM flag matches (if specified)
     ;; gwimbit of 0 or nil means "don't filter by flag"
     (or (nil? gwimbit)
         (and (number? gwimbit) (zero? gwimbit))
         (game-state/set-thing-flag? game-state obj-id gwimbit)))))

;;; ---------------------------------------------------------------------------
;;; SEARCH-LIST - Container Search
;;; ---------------------------------------------------------------------------

(defn search-list
  "Search a container for objects matching the current criteria.

   ZIL: SEARCH-LIST routine, lines 1216-1237

   Arguments:
     game-state - current state
     container - object/room to search
     match-table - table to add matches to
     level - search depth (:top, :bottom, :all)

   Level controls recursion:
     :top - only search direct contents, don't recurse
     :bottom - only search nested contents
     :all - search everything

   Recursion happens into open containers, surfaces, and searchable objects.

   Returns: updated match-table with found objects"
  [game-state container match-table level]
  (let [contents (game-state/get-contents game-state container)]
    (reduce
     (fn [table obj-id]
       (let [obj (game-state/get-thing game-state obj-id)
             ;; Check if this object matches (unless :bottom level)
             table-with-obj
             (if (and (not= level :bottom)
                      (this-it? game-state obj-id))
               (obj-found table obj-id)
               table)

             ;; Check if we should recurse into this object
             ;; ZIL: Lines 1223-1228 of SEARCH-LIST
             ;; Recurse if:
             ;;   1. ((level != TOP) OR SEARCHBIT OR SURFACEBIT)
             ;;   2. AND has contents
             ;;   3. AND (OPEN OR TRANSPARENT) - but surfaces are inherently accessible
             ;; Note: SEARCHBIT/SURFACEBIT bypass level check but still need OPEN/TRANS
             has-contents? (seq (game-state/get-contents game-state obj-id))
             is-surface? (game-state/set-thing-flag? game-state obj-id :surface)
             is-searchable? (game-state/set-thing-flag? game-state obj-id :search)
             is-open-or-trans? (or (game-state/set-thing-flag? game-state obj-id :open)
                                   (game-state/set-thing-flag? game-state obj-id :transparent))
             ;; Level check bypass: surface/search flags OR level is not :top
             level-allows? (or (not= level :top) is-surface? is-searchable?)
             should-recurse?
             (and has-contents?
                  level-allows?
                  ;; Surfaces are inherently accessible, searchable containers need open/trans
                  (or is-surface? is-open-or-trans?))

             ;; Determine recursion level
             recurse-level
             (cond
               is-surface? :all
               is-searchable? :all
               :else :top)]

         ;; Recurse if appropriate
         (if (and should-recurse? (not= level :top))
           (search-list game-state obj-id table-with-obj recurse-level)
           table-with-obj)))
     match-table
     contents)))

;;; ---------------------------------------------------------------------------
;;; GET-OBJECT - Main Object Search
;;; ---------------------------------------------------------------------------

(defn do-sl
  "Perform a search-list with location bits.

   ZIL: DO-SL routine, lines 1202-1210

   Interprets the location bits to determine search level and calls
   search-list appropriately."
  [game-state container match-table bit1 bit2]
  (let [slocbits (get-in game-state [:parser :slocbits] 0)
        both-set? (and (bit-set? slocbits bit1) (bit-set? slocbits bit2))]
    (cond
      ;; Both bits set - search all levels
      both-set?
      (search-list game-state container match-table :all)

      ;; Only bit1 - top level only
      (bit-set? slocbits bit1)
      (search-list game-state container match-table :top)

      ;; Only bit2 - bottom level only
      (bit-set? slocbits bit2)
      (search-list game-state container match-table :bottom)

      ;; Neither - no search
      :else
      match-table)))

(defn get-object
  "Find objects matching the current adjective/noun criteria.

   ZIL: GET-OBJECT routine, lines 1040-1140

   This is the main object resolution function. It:
   1. Checks for inhibit flag (skip search)
   2. Handles adjective-as-noun case
   3. Searches player inventory (if LIT)
   4. Searches room contents (if LIT)
   5. Searches global objects if no local matches
   6. Handles 'all' and 'one' flags
   7. Handles ambiguity (multiple matches)

   Arguments:
     game-state - current state
     match-table - table to accumulate matches
     verbose? - whether to print error messages

   Returns:
     {:success true, :matches [...], :game-state gs}
     {:success false, :game-state gs, :error ...}"
  ([game-state match-table]
   (get-object game-state match-table true))
  ([game-state match-table verbose?]
   (let [gflags (get-in game-state [:parser :getflags] 0)
         nam (get-in game-state [:parser :nam])
         adj (get-in game-state [:parser :adj])
         ;; Check if current room is lit
         ;; Either the global :lit flag (set by goto based on room + light sources)
         ;; OR the room's native :lit/:on flag
         here (:here game-state)
         lit? (or (:lit game-state)
                  (game-state/set-thing-flag? game-state here :lit)
                  (game-state/set-thing-flag? game-state here :on))
         player (:player game-state)
         ;; Trace the search parameters
         gs-traced (trace/trace-parser-get-object
                    game-state here lit?
                    (game-state/get-contents game-state here))]

     (cond
       ;; Inhibit flag set - skip search
       (bit-set? gflags (:inhibit game-state/getflags))
       {:success true :matches match-table :game-state game-state}

       ;; Adjective with no noun - check if adjective is also a noun
       (and (nil? nam) adj)
       (if (lexer/wt? (get-in game-state [:parser :adjn]) :object)
         ;; Use adjective as noun
         (recur (-> game-state
                    (assoc-in [:parser :nam]
                              (get-in game-state [:parser :adjn]))
                    (assoc-in [:parser :adj] nil))
                match-table
                verbose?)
         ;; Can't find noun
         (if verbose?
           {:success false
            :game-state game-state
            :error {:type :missing-noun
                    :message "There seems to be a noun missing in that sentence!"}}
           {:success false :game-state game-state}))

       ;; No noun and no adjective and not "all" mode
       (and (nil? nam) (nil? adj)
            (not (bit-set? gflags (:all game-state/getflags)))
            (nil? (get-in game-state [:parser :gwimbit])))
       (if verbose?
         {:success false
          :game-state game-state
          :error {:type :missing-noun
                  :message "There seems to be a noun missing in that sentence!"}}
         {:success false :game-state game-state})

       ;; Normal search
       :else
       (let [;; Set slocbits for full search if not in "all" mode
             gs (if (or (not (bit-set? gflags (:all game-state/getflags)))
                        (zero? (get-in game-state [:parser :slocbits] 0)))
                  (assoc-in game-state [:parser :slocbits] -1)
                  game-state)

             ;; Track incoming match count to separate new from previous
             ;; This is important for AND clauses where we accumulate matches
             incoming-count (match-table-count match-table)

             ;; GWIMBIT allows searching in darkness (FIND clause in ZIL syntax)
             ;; e.g., "turn on lamp" has (FIND LIGHTBIT) which sets gwimbit
             gwimbit (get-in gs [:parser :gwimbit])
             search-in-dark? (some? gwimbit)

             ;; Search player inventory (if lit OR gwimbit set)
             ;; ZIL: Player always knows what they're carrying, and FIND clause
             ;; allows finding objects with specific flags even in darkness
             matches-after-player
             (if (or lit? search-in-dark?)
               (do-sl gs player match-table
                      (:held game-state/search-bits) (:carried game-state/search-bits))
               match-table)

             ;; Search room contents (only if lit - can't see room contents in dark)
             matches-after-room
             (if lit?
               (do-sl gs here matches-after-player
                      (:on-ground game-state/search-bits) (:in-room game-state/search-bits))
               matches-after-player)

             ;; Trace what we found
             _ (when (trace/trace-enabled? game-state :parser)
                 (trace/trace-parser-matches game-state matches-after-player "player inventory")
                 (trace/trace-parser-matches game-state matches-after-room "room"))

             total-count (match-table-count matches-after-room)
             ;; Only count NEW matches for ambiguity check
             new-match-count (- total-count incoming-count)]

         (cond
           ;; "all" mode - just return what we found
           (bit-set? gflags (:all game-state/getflags))
           {:success true :matches matches-after-room :game-state gs}

           ;; "one" mode - pick randomly if multiple
           (and (bit-set? gflags (:one game-state/getflags))
                (pos? new-match-count))
           (let [;; Pick from only the NEW matches
                 new-matches (vec (drop incoming-count matches-after-room))
                 picked (random/rand-nth* new-matches)]
             {:success true :matches (conj (vec (take incoming-count matches-after-room)) picked) :game-state gs})

           ;; Multiple NEW matches - ambiguous
           (> new-match-count 1)
           {:success false
            :game-state gs
            :error {:type :ambiguous
                    :matches (vec (drop incoming-count matches-after-room))}}

           ;; No NEW matches - try globals
           (zero? new-match-count)
           (let [global-matches (global-check gs match-table)]
             (if (> (match-table-count global-matches) incoming-count)
               {:success true :matches global-matches :game-state gs}
               {:success false
                :game-state gs
                :error {:type :not-here
                        :message "You can't see any such thing."}}))

           ;; Exactly one NEW match - success
           :else
           {:success true :matches matches-after-room :game-state gs}))))))

;;; ---------------------------------------------------------------------------
;;; GLOBAL-CHECK - Global Object Search
;;; ---------------------------------------------------------------------------

(defn- this-it-global?
  "Like this-it? but skips the invisible check.

   For global objects explicitly listed in a room's :globals set,
   we skip the invisible check since the room declares them visible.
   This handles cases like the grating being visible from grating-room
   even though it starts invisible (hidden under leaves from above)."
  [game-state obj-id]
  (let [obj (game-state/get-thing game-state obj-id)
        nam (get-in game-state [:parser :nam])
        adj (get-in game-state [:parser :adj])
        gwimbit (get-in game-state [:parser :gwimbit])
        synonyms (set (map clojure.string/lower-case (or (:synonym obj) [])))
        adjectives (let [a (:adjective obj)]
                     (set (map clojure.string/lower-case
                               (cond
                                 (nil? a) []
                                 (string? a) [a]
                                 (sequential? a) a
                                 :else []))))]
    (and
     ;; NOTE: No invisible check here - room's :globals overrides invisibility

     ;; Name matches (if specified)
     (or (nil? nam)
         (contains? synonyms (clojure.string/lower-case (str nam))))

     ;; Adjective matches (if specified)
     (or (nil? adj)
         (contains? adjectives (clojure.string/lower-case (str adj))))

     ;; GWIM flag matches (if specified)
     (or (nil? gwimbit)
         (and (number? gwimbit) (zero? gwimbit))
         (game-state/set-thing-flag? game-state obj-id gwimbit)))))

(defn global-check
  "Search global and pseudo objects.

   ZIL: GLOBAL-CHECK routine, lines 1169-1200

   Global objects are things like 'floor', 'ceiling', 'sky' that are
   conceptually present in many rooms. Pseudo objects are room-specific
   vocabulary that refers to scenery.

   This is called when local search finds nothing.

   We search for objects with :in :local-globals, but ONLY if they're
   listed in the current room's :globals set. Each room specifies which
   global objects are visible from that location.

   NOTE: We skip the invisible check for room globals. If a room lists
   an object in its :globals, that room considers it visible regardless
   of the object's :invisible flag. This handles cases like the grating
   being visible from below (grating-room) even though it's hidden under
   leaves from above (grating-clearing)."
  [game-state match-table]
  (let [;; Get the current room's globals set
        here (:here game-state)
        room (get-in game-state [:rooms here])
        room-globals (or (:globals room) #{})

        ;; Find all objects with :in :local-globals that are visible from here
        global-objects (->> (vals (:objects game-state))
                            (filter #(= (:in %) :local-globals))
                            ;; Only include if this room lists it in :globals
                            (filter #(contains? room-globals (:id %))))

        ;; Filter to objects that match the current search criteria
        ;; Use this-it-global? which skips invisible check
        matching-objects
        (filter (fn [obj]
                  (this-it-global? game-state (:id obj)))
                global-objects)]

    ;; Add matching objects to match-table
    (reduce (fn [tbl obj]
              (obj-found tbl (:id obj)))
            match-table
            matching-objects)))

;;; ---------------------------------------------------------------------------
;;; SNARF-OBJECTS - Process Noun Clauses
;;; ---------------------------------------------------------------------------

(defn snarfem
  "Process a single noun clause into an object list.

   ZIL: SNARFEM routine, lines 978-1030

   Walks through the tokens in a noun clause, extracting:
   - 'all' / 'one' flags
   - 'but' / 'except' exclusions
   - Adjectives
   - Nouns

   Then calls GET-OBJECT to resolve to actual objects.

   Arguments:
     game-state - current state
     ptr - start of clause in lexv
     end-ptr - end of clause
     match-table-key - :prso or :prsi

   Returns: {:success bool, :game-state gs, :matches [...] or :error {...}}"
  [game-state ptr end-ptr match-table-key]
  (let [initial-table (get-in game-state [:parser match-table-key] [])
        ;; ZIL: SNARFEM sets slocbits from syntax loc1/loc2 at the start
        ;; This controls where get-object searches when in "all" mode
        ;; Note: loc1/loc2 are already converted to numeric bits by verb_defs.clj
        syntax (get-in game-state [:parser :syntax])
        loc-bits (if (= match-table-key :prso)
                   (or (:loc1 syntax) 0)
                   (or (:loc2 syntax) 0))]
    (loop [gs (-> game-state
                  (assoc-in [:parser :and-flag] false)
                  (assoc-in [:parser :getflags] 0)
                  (assoc-in [:parser :nam] nil)
                  (assoc-in [:parser :adj] nil)
                  ;; Set slocbits from syntax loc1/loc2 for "all" mode
                  (assoc-in [:parser :slocbits] loc-bits))
           current-ptr ptr
           but-mode? false
           match-table initial-table]

      (if (>= current-ptr end-ptr)
        ;; End of clause - do final get-object
        (if but-mode?
          ;; In but-mode: resolve the excluded objects and remove them from match-table
          ;; ZIL: After "all but X", resolve X and use BUT-MERGE to remove from matches
          (let [buts-list (get-in gs [:parser :buts] [])
                but-result (get-object gs buts-list)]
            (if (:success but-result)
              (let [excluded-objects (:matches but-result)
                    final-matches (but-merge match-table excluded-objects)]
                {:success true
                 :game-state (assoc-in (:game-state but-result)
                                       [:parser match-table-key]
                                       final-matches)
                 :matches final-matches})
              but-result))
          ;; Normal mode: just resolve the objects
          (let [result (get-object gs match-table)]
            (if (:success result)
              {:success true
               :game-state (assoc-in (:game-state result)
                                     [:parser match-table-key]
                                     (:matches result))
               :matches (:matches result)}
              result)))

        ;; Process current word
        (let [word (lexer/lexv-word gs current-ptr)
              next-word (when (< (inc current-ptr) end-ptr)
                          (lexer/lexv-word gs (inc current-ptr)))]
          (cond
            ;; ALL - set flag
            (lexer/special-word? word :all)
            (let [new-gs (-> gs
                             (assoc-in [:parser :getflags] (:all game-state/getflags))
                             (trace/trace-parser-snarfem word "ALL")
                             (trace/trace-parser-getflags "ALL" (:all game-state/getflags)))
                  skip-of? (lexer/special-word? next-word :of)]
              (recur new-gs
                     (if skip-of? (+ current-ptr 2) (inc current-ptr))
                     but-mode?
                     match-table))

            ;; BUT / EXCEPT - switch to exclusion mode
            (or (lexer/special-word? word :but)
                (lexer/special-word? word :except))
            (let [result (get-object gs match-table)]
              (if (:success result)
                (recur (-> (:game-state result)
                           (assoc-in [:parser :buts] []))
                       (inc current-ptr)
                       true  ; Now in but-mode
                       (:matches result))
                result))

            ;; ONE / A - pick one flag
            (or (lexer/special-word? word :one)
                (lexer/special-word? word :a))
            (let [adj (get-in gs [:parser :adj])]
              (if (nil? adj)
                ;; Not in adjective context - set flag
                (let [new-gs (assoc-in gs [:parser :getflags] (:one game-state/getflags))
                      skip-of? (lexer/special-word? next-word :of)]
                  (recur new-gs
                         (if skip-of? (+ current-ptr 2) (inc current-ptr))
                         but-mode?
                         match-table))
                ;; After adjective - use ONEOBJ
                (let [new-gs (assoc-in gs [:parser :nam]
                                       (get-in gs [:parser :oneobj]))
                      result (get-object new-gs (if but-mode?
                                                  (get-in gs [:parser :buts] [])
                                                  match-table))]
                  (if (:success result)
                    (if (nil? next-word)
                      {:success true
                       :game-state (:game-state result)
                       :matches (:matches result)}
                      (recur (:game-state result)
                             (inc current-ptr)
                             but-mode?
                             (:matches result)))
                    result))))

            ;; AND / COMMA - process current object, continue for more
            (and (or (lexer/special-word? word :and)
                     (lexer/special-word? word :comma))
                 (not (and (lexer/special-word? next-word :and)
                           (lexer/special-word? next-word :comma))))
            (let [new-gs (assoc-in gs [:parser :and-flag] true)
                  result (get-object new-gs (if but-mode?
                                              (get-in gs [:parser :buts] [])
                                              match-table))]
              (if (:success result)
                (recur (:game-state result)
                       (inc current-ptr)
                       but-mode?
                       (:matches result))
                result))

            ;; Buzz word - skip
            (lexer/wt? word :buzz-word)
            (recur gs (inc current-ptr) but-mode? match-table)

            ;; OF - handled with flags
            (lexer/special-word? word :of)
            (let [new-flags (if (zero? (get-in gs [:parser :getflags] 0))
                              (:inhibit game-state/getflags)
                              (get-in gs [:parser :getflags]))]
              (recur (assoc-in gs [:parser :getflags] new-flags)
                     (inc current-ptr)
                     but-mode?
                     match-table))

            ;; IT pronoun - resolve to the object stored in game state :it
            (lexer/special-word? word :it)
            (let [it-obj (:it gs)]
              (if it-obj
                ;; Store the it-object for later use and add to match table
                (let [updated-gs (assoc-in gs [:parser :it-object] it-obj)
                      updated-table (obj-found match-table it-obj)]
                  ;; If we're at the end of the clause, return success
                  (if (>= (inc current-ptr) end-ptr)
                    {:success true
                     :game-state (assoc-in updated-gs [:parser match-table-key] updated-table)
                     :matches updated-table}
                    (recur updated-gs
                           (inc current-ptr)
                           but-mode?
                           updated-table)))
                ;; No IT referent - error
                {:success false
                 :game-state gs
                 :error {:type :no-it-referent
                         :message "I don't see what you're referring to."}}))

            ;; Adjective - store it
            (and (lexer/wt? word :adjective true)
                 (nil? (get-in gs [:parser :adj])))
            (recur (-> gs
                       (assoc-in [:parser :adj] (lexer/wt? word :adjective true))
                       (assoc-in [:parser :adjn] word))
                   (inc current-ptr)
                   but-mode?
                   match-table)

            ;; Object word - store as noun
            (lexer/wt? word :object true)
            (recur (-> gs
                       (assoc-in [:parser :nam] word)
                       (assoc-in [:parser :oneobj] word))
                   (inc current-ptr)
                   but-mode?
                   match-table)

            ;; Unknown - skip
            :else
            (recur gs (inc current-ptr) but-mode? match-table)))))))

(defn snarf-objects
  "Extract objects from all parsed noun clauses.

   ZIL: SNARF-OBJECTS routine, lines 928-943

   Processes NC2 first (indirect object), then NC1 (direct object).
   This order matters for proper 'but' handling.

   Returns: {:success bool, :game-state gs} or {:success false, :error ...}"
  [game-state]
  (let [nc1 (parser-state/get-itbl game-state :nc1)
        nc1l (parser-state/get-itbl game-state :nc1l)
        nc2 (parser-state/get-itbl game-state :nc2)
        nc2l (parser-state/get-itbl game-state :nc2l)
        ncn (parser-state/get-ncn game-state)]
    ;; Clear buts table
    (let [gs (assoc-in game-state [:parser :buts] [])

          ;; Process indirect object first (NC2)
          ;; Use ncn to determine if NC2 exists (ncn >= 2)
          ;; Also skip if PRSI is already set (e.g., by GWIM)
          gs-after-nc2
          (if (and (>= ncn 2)
                   (nil? (parser-state/get-prsi gs)))
            (let [result (snarfem gs nc2 nc2l :prsi)]
              (if (:success result)
                (:game-state result)
                (reduced result)))  ; Early exit on error
            gs)]

      (if (reduced? gs-after-nc2)  ; Check if it's an error result
        @gs-after-nc2  ; Unwrap the Reduced to get the error
        ;; Process direct object (NC1)
        ;; Use ncn to determine if NC1 exists (ncn >= 1)
        ;; Also skip if PRSO is already set (e.g., by GWIM)
        (if (and (>= ncn 1)
                 (nil? (parser-state/get-prso gs-after-nc2)))
          (snarfem gs-after-nc2 nc1 nc1l :prso)
          {:success true :game-state gs-after-nc2})))))

;;; ---------------------------------------------------------------------------
;;; BUT-MERGE - Exclusion Handling
;;; ---------------------------------------------------------------------------

(defn but-merge
  "Remove excluded objects from a match table.

   ZIL: BUT-MERGE routine, lines 945-958

   After processing 'all but X', removes X from the match list."
  [match-table buts-table]
  (let [but-set (set buts-table)]
    (vec (remove but-set match-table))))
