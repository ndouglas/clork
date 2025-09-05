(in-ns 'clork.core)

;;;; ============================================================================
;;;; PARSER ORPHAN - Incomplete Command Handling
;;;; ============================================================================
;;;;
;;;; This file contains:
;;;;   - ORPHAN-MERGE - Merge previous parse with current for completion
;;;;   - ORPHAN - Set up orphan state for next parse
;;;;   - ACLAUSE-WIN / NCLAUSE-WIN - Resolve orphaned clauses
;;;;   - CLAUSE-COPY - Copy clause data between tables
;;;;   - Prompting for missing information
;;;;
;;;; ZIL Reference: gparser.zil
;;;;   - Lines 541-630: ORPHAN-MERGE routine
;;;;   - Lines 632-653: ACLAUSE-WIN, NCLAUSE-WIN routines
;;;;   - Lines 782-808: ORPHAN routine
;;;;   - Lines 860-887: CLAUSE-COPY, CLAUSE-ADD routines
;;;;
;;;; What is Orphaning?
;;;;   When the player types an incomplete command like "take", the parser
;;;;   needs to ask "What do you want to take?" and remember the verb for
;;;;   when they respond with just "the lamp".
;;;;
;;;;   The ORPHAN system saves the partial parse (the verb, any prepositions)
;;;;   so that when the next input comes in, it can be merged with the saved
;;;;   state to create a complete command.
;;;;
;;;; Example Flow:
;;;;   > take
;;;;   "What do you want to take?"
;;;;   > the lamp
;;;;
;;;;   1. First parse: verb=TAKE, no objects → enters orphan mode
;;;;   2. P-OFLAG set, previous parse saved to P-OTBL
;;;;   3. Second parse: just "the lamp" → noun clause
;;;;   4. ORPHAN-MERGE combines: verb from OTBL + noun from new parse
;;;;   5. Result: complete TAKE LAMP command
;;;;
;;;; The Gnarly Part:
;;;;   This gets complex when dealing with:
;;;;   - Two-object verbs: "put" → "in what?" → "the lamp" → "in the case"
;;;;   - Ambiguous objects: "which sword?" → "the rusty one"
;;;;   - Mix of verb and object: "give bob" (verb+indirect, need direct)
;;;;
;;;; ============================================================================

;;; ---------------------------------------------------------------------------
;;; FORWARD DECLARATIONS
;;; ---------------------------------------------------------------------------

(declare clause-copy clause-copy-with-adj merge-to-nc1 merge-to-nc2
         merge-aclause finalize-merge aclause-win nclause-win)

;;; ---------------------------------------------------------------------------
;;; ORPHAN STATE SETUP
;;; ---------------------------------------------------------------------------

(defn orphan
  "Set up orphan state for an incomplete command.

   ZIL: ORPHAN routine, lines 782-808

   Called when we have a valid verb but can't complete parsing due to
   missing objects. Saves current state to OTBL for later merging.

   Arguments:
     game-state - current state
     drive1 - syntax pattern for first object (if missing)
     drive2 - syntax pattern for second object (if missing)

   This function:
   1. Saves P-VTBL to P-OVTBL (verb info)
   2. Copies P-ITBL to P-OTBL (parsed structure)
   3. Copies clause data if we have partial objects
   4. Records which object(s) need to be filled in"
  [game-state drive1 drive2]
  (let [gs (if (get-in game-state [:parser :merged])
             game-state
             ;; Clear the orphan clause match table
             (assoc-in game-state [:parser :oclause] []))

        ;; Save verb table
        gs (-> gs
               (assoc-in [:parser :ovtbl]
                         (get-in gs [:parser :vtbl])))]

    ;; Copy ITBL to OTBL
    (loop [cnt 0
           gs gs]
      (if (> cnt p-itbllen)
        ;; Done copying, now handle clause data
        (let [ncn (get-in gs [:parser :ncn] 0)

              ;; If we have 2 noun clauses, copy NC2
              gs (if (= ncn 2)
                   (clause-copy gs :itbl :otbl :nc2 :nc2l)
                   gs)

              ;; If we have at least 1 noun clause, copy NC1
              gs (if (>= ncn 1)
                   (clause-copy gs :itbl :otbl :nc1 :nc1l)
                   gs)

              ;; Record which object position needs filling
              gs (cond
                   drive1
                   (-> gs
                       (assoc-in [:parser :otbl (:prep1 itbl-indices)]
                                 (:prep1 drive1))
                       (assoc-in [:parser :otbl (:nc1 itbl-indices)] 1))

                   drive2
                   (-> gs
                       (assoc-in [:parser :otbl (:prep2 itbl-indices)]
                                 (:prep2 drive2))
                       (assoc-in [:parser :otbl (:nc2 itbl-indices)] 1))

                   :else gs)]
          gs)

        ;; Copy this slot
        (recur (inc cnt)
               (assoc-in gs [:parser :otbl cnt]
                         (get-in gs [:parser :itbl cnt])))))))

;;; ---------------------------------------------------------------------------
;;; ORPHAN-MERGE - Combine Previous and Current Parse
;;; ---------------------------------------------------------------------------

(defn orphan-merge
  "Merge an orphaned parse with new input.

   ZIL: ORPHAN-MERGE routine, lines 541-630

   This is the complex heart of orphan resolution. When P-OFLAG is set,
   this merges the saved OTBL with the new ITBL to create a complete parse.

   Cases handled:
   1. New input is just a noun → fills in missing object slot
   2. New input is same verb + noun → uses new noun for missing slot
   3. New input has adjective → applies to orphaned noun
   4. New input is different verb → abandons orphan, starts fresh

   Returns:
     game-state with merged parse, P-OFLAG cleared
     or nil if merge not possible (new command doesn't complete old)"
  [game-state]
  ;; Clear orphan flag
  (let [gs (assoc-in game-state [:parser :oflag] false)

        ;; Get the first word of new input to check if it's a verb or adjective
        first-word (lexv-word gs 0)
        verb-from-itbl (get-in gs [:parser :itbl (:verb itbl-indices)])
        verb-from-otbl (get-in gs [:parser :otbl (:verb itbl-indices)])

        ;; Check if first word is a verb matching the old verb
        ;; or is an adjective
        first-is-old-verb? (and (wt? first-word :verb true)
                                (= (wt? first-word :verb true) verb-from-otbl))
        first-is-adjective? (wt? first-word :adjective)]

    (cond
      ;; New input starts with same verb or an adjective - merge
      (or first-is-old-verb? first-is-adjective?)
      (let [;; Determine if we're merging as adjective
            adj? first-is-adjective?

            ;; Get current noun clause count
            ncn (get-in gs [:parser :ncn] 0)]

        (cond
          ;; New verb doesn't match old (and it's not an adjective)
          (and (not (nil? verb-from-itbl))
               (not adj?)
               (not= verb-from-itbl verb-from-otbl))
          nil  ; Can't merge

          ;; Two noun clauses already - can't merge more
          (= ncn 2)
          nil

          ;; NC1 in OTBL is marked as needing fill (value = 1)
          (= (get-in gs [:parser :otbl (:nc1 itbl-indices)]) 1)
          (merge-to-nc1 gs adj?)

          ;; NC2 in OTBL is marked as needing fill
          (= (get-in gs [:parser :otbl (:nc2 itbl-indices)]) 1)
          (merge-to-nc2 gs adj?)

          ;; Check for aclause (orphaned adjective clause)
          (get-in gs [:parser :aclause])
          (merge-aclause gs adj?)

          ;; No slot to fill
          :else nil))

      ;; New input starts with object word but no verb
      (and (wt? first-word :object true)
           (zero? (get-in gs [:parser :ncn] 0)))
      ;; Treat as noun clause to fill orphan
      (let [gs (-> gs
                   (assoc-in [:parser :itbl (:verb itbl-indices)] 0)
                   (assoc-in [:parser :itbl (:verbn itbl-indices)] 0)
                   (assoc-in [:parser :itbl (:nc1 itbl-indices)] 0)  ; Will be set by clause copy
                   (assoc-in [:parser :ncn] 1))]
        (merge-to-nc1 gs false))

      ;; Can't merge - different verb or incompatible structure
      :else nil)))

(defn merge-to-nc1
  "Merge new input into the NC1 (direct object) slot.

   Helper for orphan-merge when filling the first object slot."
  [game-state adj?]
  (let [;; Check preposition compatibility
        prep-from-itbl (get-in game-state [:parser :itbl (:prep1 itbl-indices)])
        prep-from-otbl (get-in game-state [:parser :otbl (:prep1 itbl-indices)])]

    (if (or (= prep-from-itbl prep-from-otbl)
            (nil? prep-from-itbl))
      ;; Compatible - do the merge
      (let [gs (if adj?
                 ;; Adjective merge: set NC1 from lexv start
                 (assoc-in game-state [:parser :otbl (:nc1 itbl-indices)]
                           0)  ; Start of lexv
                 ;; Normal merge: copy NC1 from ITBL
                 (assoc-in game-state [:parser :otbl (:nc1 itbl-indices)]
                           (get-in game-state [:parser :itbl (:nc1 itbl-indices)])))

            gs (assoc-in gs [:parser :otbl (:nc1l itbl-indices)]
                         (get-in gs [:parser :itbl (:nc1l itbl-indices)]))]

        ;; Copy OTBL back to ITBL and set merged flag
        (finalize-merge gs))

      ;; Incompatible prepositions
      nil)))

(defn merge-to-nc2
  "Merge new input into the NC2 (indirect object) slot.

   Helper for orphan-merge when filling the second object slot."
  [game-state adj?]
  (let [prep-from-itbl (get-in game-state [:parser :itbl (:prep1 itbl-indices)])
        prep-from-otbl (get-in game-state [:parser :otbl (:prep2 itbl-indices)])]

    (if (or (= prep-from-itbl prep-from-otbl)
            (nil? prep-from-itbl))
      ;; Compatible - merge NC1 from ITBL into NC2 slot of OTBL
      (let [gs (if adj?
                 (assoc-in game-state [:parser :itbl (:nc1 itbl-indices)] 0)
                 game-state)

            gs (-> gs
                   (assoc-in [:parser :otbl (:nc2 itbl-indices)]
                             (get-in gs [:parser :itbl (:nc1 itbl-indices)]))
                   (assoc-in [:parser :otbl (:nc2l itbl-indices)]
                             (get-in gs [:parser :itbl (:nc1l itbl-indices)]))
                   (assoc-in [:parser :ncn] 2))]

        (finalize-merge gs))
      nil)))

(defn merge-aclause
  "Merge new input with an orphaned adjective clause.

   ZIL: The ACLAUSE handling in ORPHAN-MERGE

   When the parser asked 'which sword?' and player said 'the rusty one',
   we need to match 'rusty' with the orphaned 'sword' reference."
  [game-state adj?]
  ;; This is complex - for now, delegate to aclause-win or nclause-win
  ;; TODO: Implement full aclause merging
  (if adj?
    (aclause-win game-state (get-in game-state [:parser :adj]))
    (nclause-win game-state)))

(defn finalize-merge
  "Complete the merge by copying OTBL to ITBL.

   ZIL: The final loop in ORPHAN-MERGE, lines 625-630"
  [game-state]
  ;; Copy verb table
  (let [gs (-> game-state
               (assoc-in [:parser :vtbl 0]
                         (get-in game-state [:parser :ovtbl 0]))
               (assoc-in [:parser :vtbl 2]
                         (get-in game-state [:parser :ovtbl 2]))
               (assoc-in [:parser :vtbl 3]
                         (get-in game-state [:parser :ovtbl 3]))
               (assoc-in [:parser :otbl (:verbn itbl-indices)]
                         (get-in game-state [:parser :vtbl]))
               (assoc-in [:parser :vtbl 2] 0))]

    ;; Copy all of OTBL to ITBL
    (loop [cnt 0
           gs gs]
      (if (> cnt p-itbllen)
        (assoc-in gs [:parser :merged] true)
        (recur (inc cnt)
               (assoc-in gs [:parser :itbl cnt]
                         (get-in gs [:parser :otbl cnt])))))))

;;; ---------------------------------------------------------------------------
;;; CLAUSE WIN FUNCTIONS
;;; ---------------------------------------------------------------------------

(defn aclause-win
  "Resolve an orphaned adjective clause.

   ZIL: ACLAUSE-WIN routine, lines 632-643

   When player responds to 'which X?' with 'the rusty one', this
   applies the adjective to the orphaned clause."
  [game-state adj]
  (let [gs (-> game-state
               (assoc-in [:parser :itbl (:verb itbl-indices)]
                         (get-in game-state [:parser :otbl (:verb itbl-indices)])))]
    ;; Set up clause copy pointers
    (let [aclause (get-in gs [:parser :aclause])
          gs (-> gs
                 (assoc-in [:parser :cctbl :sbptr] aclause)
                 (assoc-in [:parser :cctbl :septr] (inc aclause))
                 (assoc-in [:parser :cctbl :dbptr] aclause)
                 (assoc-in [:parser :cctbl :deptr] (inc aclause)))]
      ;; Do the clause copy with adjective insertion
      (let [gs (clause-copy-with-adj gs :otbl :otbl adj)]
        (-> gs
            ;; Update NCN if NC2 was filled
            (cond-> (not (zero? (get-in gs [:parser :otbl (:nc2 itbl-indices)])))
              (assoc-in [:parser :ncn] 2))
            ;; Clear aclause
            (assoc-in [:parser :aclause] nil))))))

(defn nclause-win
  "Resolve an orphaned noun clause.

   ZIL: NCLAUSE-WIN routine, lines 645-653

   When the new input provides a complete noun (not just adjective),
   this copies it to fill the orphaned slot."
  [game-state]
  (let [gs (-> game-state
               (assoc-in [:parser :cctbl :sbptr] (:nc1 itbl-indices))
               (assoc-in [:parser :cctbl :septr] (:nc1l itbl-indices))
               (assoc-in [:parser :cctbl :dbptr] (get-in game-state [:parser :aclause]))
               (assoc-in [:parser :cctbl :deptr]
                         (inc (get-in game-state [:parser :aclause]))))]
    (let [gs (clause-copy gs :itbl :otbl)]
      (-> gs
          (cond-> (not (zero? (get-in gs [:parser :otbl (:nc2 itbl-indices)])))
            (assoc-in [:parser :ncn] 2))
          (assoc-in [:parser :aclause] nil)))))

;;; ---------------------------------------------------------------------------
;;; CLAUSE COPY
;;; ---------------------------------------------------------------------------

(defn clause-copy
  "Copy clause data between tables.

   ZIL: CLAUSE-COPY routine, lines 860-879

   Copies noun clause boundaries and associated data from one table
   to another, using the pointers in P-CCTBL."
  [game-state src-table dest-table]
  ;; Get the pointers
  (let [sb (get-in game-state [:parser :cctbl :sbptr])
        se (get-in game-state [:parser :cctbl :septr])
        db (get-in game-state [:parser :cctbl :dbptr])
        de (get-in game-state [:parser :cctbl :deptr])

        ;; Get source values
        src-begin (get-in game-state [:parser src-table sb])
        src-end (get-in game-state [:parser src-table se])]

    ;; Store in destination
    (-> game-state
        (assoc-in [:parser dest-table db] src-begin)
        (assoc-in [:parser dest-table de] src-end))))

(defn clause-copy-with-adj
  "Copy clause data while inserting an adjective.

   Used when merging 'rusty' with orphaned 'sword' clause."
  [game-state src-table dest-table adj]
  ;; Basic clause copy plus adjective handling
  ;; TODO: Implement full adjective insertion
  (clause-copy game-state src-table dest-table))

;;; ---------------------------------------------------------------------------
;;; ORPHAN PROMPTING
;;; ---------------------------------------------------------------------------

(defn prompt-for-object
  "Generate a prompt for the missing object.

   Used when entering orphan mode to ask something like:
   'What do you want to take?'
   'What do you want to put in the case?'"
  [game-state syntax for-second-object?]
  (let [verb-word (get-in game-state [:parser :vtbl 0])
        prep (if for-second-object?
               (:prep2 syntax)
               (:prep1 syntax))]
    (str "What do you want to "
         (or verb-word "do")
         (when prep (str " " (name prep)))
         "?")))

(defn cant-orphan
  "Called when we can't enter orphan mode (e.g., NPC as winner).

   ZIL: CANT-ORPHAN routine, lines 777-779"
  [game-state]
  (-> game-state
      (tell "\"I don't understand! What are you referring to?\"\n")))
