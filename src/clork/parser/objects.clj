(in-ns 'clork.core)

;;;; ============================================================================
;;;; PARSER OBJECTS - Object Matching and Resolution
;;;; ============================================================================
;;;;
;;;; This file contains:
;;;;   - SNARF-OBJECTS - Extract objects from parsed noun clauses
;;;;   - SNARFEM - Process a single noun clause into object list
;;;;   - GET-OBJECT - Find objects matching adjective/noun criteria
;;;;   - SEARCH-LIST - Search a container for matching objects
;;;;   - THIS-IT? - Test if an object matches the criteria
;;;;   - OBJ-FOUND - Add an object to the match table
;;;;   - GLOBAL-CHECK - Search global/pseudo objects
;;;;   - BUT-MERGE - Handle "all but X" exclusions
;;;;
;;;; ZIL Reference: gparser.zil
;;;;   - Lines 928-943: SNARF-OBJECTS routine
;;;;   - Lines 978-1030: SNARFEM routine
;;;;   - Lines 1040-1140: GET-OBJECT routine
;;;;   - Lines 1216-1237: SEARCH-LIST routine
;;;;   - Lines 1357-1370: THIS-IT? routine
;;;;   - Lines 1239-1242: OBJ-FOUND routine
;;;;   - Lines 1169-1200: GLOBAL-CHECK routine
;;;;   - Lines 945-958: BUT-MERGE routine
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

(declare global-check do-sl)

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
  (let [obj (get-thing game-state obj-id)
        nam (get-in game-state [:parser :nam])
        adj (get-in game-state [:parser :adj])
        gwimbit (get-in game-state [:parser :gwimbit])]

    (and
     ;; Not invisible
     (not (set-thing-flag? game-state obj-id :invisible))

     ;; Name matches (if specified)
     (or (nil? nam)
         (contains? (set (:synonyms obj)) nam))

     ;; Adjective matches (if specified)
     (or (nil? adj)
         (contains? (set (:adjectives obj)) adj))

     ;; GWIM flag matches (if specified)
     (or (nil? gwimbit)
         (set-thing-flag? game-state obj-id gwimbit)))))

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
  (let [contents (get-contents game-state container)]
    (reduce
     (fn [table obj-id]
       (let [obj (get-thing game-state obj-id)
             ;; Check if this object matches (unless :bottom level)
             table-with-obj
             (if (and (not= level :bottom)
                      (this-it? game-state obj-id))
               (obj-found table obj-id)
               table)

             ;; Check if we should recurse into this object
             should-recurse?
             (and
              ;; Has contents
              (seq (get-contents game-state obj-id))
              ;; And is accessible (open, transparent, surface, or searchable)
              (or (set-thing-flag? game-state obj-id :open)
                  (set-thing-flag? game-state obj-id :transparent)
                  (set-thing-flag? game-state obj-id :surface)
                  (set-thing-flag? game-state obj-id :search)))

             ;; Determine recursion level
             recurse-level
             (cond
               (set-thing-flag? game-state obj-id :surface) :all
               (set-thing-flag? game-state obj-id :search) :all
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
        both-set? (and (bit-test slocbits bit1) (bit-test slocbits bit2))]
    (cond
      ;; Both bits set - search all levels
      both-set?
      (search-list game-state container match-table :all)

      ;; Only bit1 - top level only
      (bit-test slocbits bit1)
      (search-list game-state container match-table :top)

      ;; Only bit2 - bottom level only
      (bit-test slocbits bit2)
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
   (let [getflags (get-in game-state [:parser :getflags] 0)
         nam (get-in game-state [:parser :nam])
         adj (get-in game-state [:parser :adj])
         lit? (:lit game-state)
         player (:player game-state)
         here (:here game-state)]

     (cond
       ;; Inhibit flag set - skip search
       (bit-test getflags (:inhibit getflags))
       {:success true :matches match-table :game-state game-state}

       ;; Adjective with no noun - check if adjective is also a noun
       (and (nil? nam) adj)
       (if (wt? (get-in game-state [:parser :adjn]) :object)
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
            (not (bit-test getflags (:all getflags)))
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
             gs (if (or (not (bit-test getflags (:all getflags)))
                        (zero? (get-in game-state [:parser :slocbits] 0)))
                  (assoc-in game-state [:parser :slocbits] -1)
                  game-state)

             ;; Search player inventory (if lit)
             matches-after-player
             (if lit?
               (do-sl gs player match-table
                      (:held search-bits) (:carried search-bits))
               match-table)

             ;; Search room contents (if lit)
             matches-after-room
             (if lit?
               (do-sl gs here matches-after-player
                      (:on-ground search-bits) (:in-room search-bits))
               matches-after-player)

             match-count (match-table-count matches-after-room)]

         (cond
           ;; "all" mode - just return what we found
           (bit-test getflags (:all getflags))
           {:success true :matches matches-after-room :game-state gs}

           ;; "one" mode - pick randomly if multiple
           (and (bit-test getflags (:one getflags))
                (pos? match-count))
           (let [picked (rand-nth matches-after-room)]
             {:success true :matches [picked] :game-state gs})

           ;; Multiple matches - ambiguous
           (> match-count 1)
           {:success false
            :game-state gs
            :error {:type :ambiguous
                    :matches matches-after-room}}

           ;; No matches - try globals
           (zero? match-count)
           (let [global-matches (global-check gs match-table)]
             (if (pos? (match-table-count global-matches))
               {:success true :matches global-matches :game-state gs}
               {:success false
                :game-state gs
                :error {:type :not-here}}))

           ;; Exactly one match - success
           :else
           {:success true :matches matches-after-room :game-state gs}))))))

;;; ---------------------------------------------------------------------------
;;; GLOBAL-CHECK - Global Object Search
;;; ---------------------------------------------------------------------------

(defn global-check
  "Search global and pseudo objects.

   ZIL: GLOBAL-CHECK routine, lines 1169-1200

   Global objects are things like 'floor', 'ceiling', 'sky' that are
   conceptually present in many rooms. Pseudo objects are room-specific
   vocabulary that refers to scenery.

   This is called when local search finds nothing."
  [game-state match-table]
  ;; TODO: Implement when we have global/pseudo object definitions
  ;; For now, just return the table unchanged
  match-table)

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
  (let [initial-table (get-in game-state [:parser match-table-key] [])]
    (loop [gs (-> game-state
                  (assoc-in [:parser :and-flag] false)
                  (assoc-in [:parser :getflags] 0)
                  (assoc-in [:parser :nam] nil)
                  (assoc-in [:parser :adj] nil))
           current-ptr ptr
           but-mode? false
           match-table initial-table]

      (if (>= current-ptr end-ptr)
        ;; End of clause - do final get-object
        (let [result (get-object gs (if but-mode?
                                      (get-in gs [:parser :buts] [])
                                      match-table))]
          (if (:success result)
            {:success true
             :game-state (assoc-in (:game-state result)
                                   [:parser match-table-key]
                                   (:matches result))
             :matches (:matches result)}
            result))

        ;; Process current word
        (let [word (lexv-word gs current-ptr)
              next-word (when (< (inc current-ptr) end-ptr)
                          (lexv-word gs (inc current-ptr)))]

          (cond
            ;; ALL - set flag
            (special-word? word :all)
            (let [new-gs (assoc-in gs [:parser :getflags] (:all getflags))
                  skip-of? (special-word? next-word :of)]
              (recur new-gs
                     (if skip-of? (+ current-ptr 2) (inc current-ptr))
                     but-mode?
                     match-table))

            ;; BUT / EXCEPT - switch to exclusion mode
            (or (special-word? word :but)
                (special-word? word :except))
            (let [result (get-object gs match-table)]
              (if (:success result)
                (recur (-> (:game-state result)
                           (assoc-in [:parser :buts] []))
                       (inc current-ptr)
                       true  ; Now in but-mode
                       (:matches result))
                result))

            ;; ONE / A - pick one flag
            (or (special-word? word :one)
                (special-word? word :a))
            (let [adj (get-in gs [:parser :adj])]
              (if (nil? adj)
                ;; Not in adjective context - set flag
                (let [new-gs (assoc-in gs [:parser :getflags] (:one getflags))
                      skip-of? (special-word? next-word :of)]
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
            (and (or (special-word? word :and)
                     (special-word? word :comma))
                 (not (and (special-word? next-word :and)
                           (special-word? next-word :comma))))
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
            (wt? word :buzz-word)
            (recur gs (inc current-ptr) but-mode? match-table)

            ;; OF - handled with flags
            (special-word? word :of)
            (let [new-flags (if (zero? (get-in gs [:parser :getflags] 0))
                              (:inhibit getflags)
                              (get-in gs [:parser :getflags]))]
              (recur (assoc-in gs [:parser :getflags] new-flags)
                     (inc current-ptr)
                     but-mode?
                     match-table))

            ;; Adjective - store it
            (and (wt? word :adjective true)
                 (nil? (get-in gs [:parser :adj])))
            (recur (-> gs
                       (assoc-in [:parser :adj] (wt? word :adjective true))
                       (assoc-in [:parser :adjn] word))
                   (inc current-ptr)
                   but-mode?
                   match-table)

            ;; Object word - store as noun
            (wt? word :object true)
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
  (let [nc1 (get-in game-state [:parser :itbl (:nc1 itbl-indices)])
        nc1l (get-in game-state [:parser :itbl (:nc1l itbl-indices)])
        nc2 (get-in game-state [:parser :itbl (:nc2 itbl-indices)])
        nc2l (get-in game-state [:parser :itbl (:nc2l itbl-indices)])]

    ;; Clear buts table
    (let [gs (assoc-in game-state [:parser :buts] [])

          ;; Process indirect object first (NC2)
          gs-after-nc2
          (if (and nc2 (not (zero? nc2)))
            (let [result (snarfem gs nc2 nc2l :prsi)]
              (if (:success result)
                (:game-state result)
                (reduced result)))  ; Early exit on error
            gs)]

      (if (map? gs-after-nc2)  ; Check if it's an error result
        gs-after-nc2
        ;; Process direct object (NC1)
        (if (and nc1 (not (zero? nc1)))
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
