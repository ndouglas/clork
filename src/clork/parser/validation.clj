(in-ns 'clork.core)

;;;; ============================================================================
;;;; PARSER VALIDATION - Pre-Action Checks
;;;; ============================================================================
;;;;
;;;; This file contains:
;;;;   - TAKE-CHECK / ITAKE-CHECK - Auto-take objects if needed
;;;;   - MANY-CHECK - Verify verb allows multiple objects
;;;;   - ACCESSIBLE? - Check if player can touch an object
;;;;   - META-LOC - Find the room containing an object
;;;;   - LIT? - Check if a location is lit
;;;;
;;;; ZIL Reference: gparser.zil
;;;;   - Lines 1244-1292: TAKE-CHECK, ITAKE-CHECK routines
;;;;   - Lines 1294-1313: MANY-CHECK routine
;;;;   - Lines 1372-1396: ACCESSIBLE? routine
;;;;   - Lines 1398-1408: META-LOC routine
;;;;   - Lines 1333-1355: LIT? routine
;;;;
;;;; When Are These Called?
;;;;   After SYNTAX-CHECK and SNARF-OBJECTS succeed, but before the action
;;;;   routine runs. These are the final validation steps.
;;;;
;;;; TAKE-CHECK:
;;;;   Some verbs require holding the object (e.g., DROP, THROW).
;;;;   If the player says "drop sword" but isn't holding it, TAKE-CHECK
;;;;   can auto-take it first if it's takeable.
;;;;
;;;; MANY-CHECK:
;;;;   Controls whether multiple objects are allowed in each slot.
;;;;   Most verbs allow multiple direct objects ("take all", "put all in case")
;;;;   but restrict indirect objects ("put sword in case", not "put sword in all").
;;;;
;;;; ============================================================================

;;; ---------------------------------------------------------------------------
;;; FORWARD DECLARATIONS
;;; ---------------------------------------------------------------------------

(declare room? room-has-global? itake)

;;; ---------------------------------------------------------------------------
;;; ACCESSIBILITY CHECKS
;;; ---------------------------------------------------------------------------

(defn meta-loc
  "Find the room containing an object (climbing the containment hierarchy).

   ZIL: META-LOC routine, lines 1398-1408
     <ROUTINE META-LOC (OBJ)
       <REPEAT ()
         <COND (<NOT .OBJ> <RFALSE>)
           (<IN? .OBJ ,GLOBAL-OBJECTS> <RETURN ,GLOBAL-OBJECTS>)>
         <COND (<IN? .OBJ ,ROOMS> <RETURN .OBJ>)
           (T <SET OBJ <LOC .OBJ>>)>>>

   Follows the :in chain upward until we find:
   - nil (object not in world)
   - A room (return the room)
   - GLOBAL-OBJECTS (return that)"
  [game-state obj-id]
  (loop [current obj-id]
    (cond
      ;; Not in world
      (nil? current)
      nil

      ;; Is a global object
      (= (get-thing-location game-state current) :global-objects)
      :global-objects

      ;; Is a room (or in ROOMS)
      (room? game-state current)
      current

      ;; Keep climbing
      :else
      (recur (get-thing-location game-state current)))))

(defn accessible?
  "Check if the player can physically touch an object.

   ZIL: ACCESSIBLE? routine, lines 1372-1396

   An object is accessible if:
   - It's not invisible
   - It exists (has a location)
   - It's a global object, OR
   - It's in a room-local global list for HERE, OR
   - It's in the same room as the player AND
     - It's directly in the room/player/player's container, OR
     - It's in an OPEN container that's accessible

   Arguments:
     game-state - current state
     obj-id - object to check

   Returns: true if player can touch the object"
  [game-state obj-id]
  (let [obj (get-thing game-state obj-id)
        loc (get-thing-location game-state obj-id)
        winner (:winner game-state)
        here (:here game-state)
        winner-loc (get-thing-location game-state winner)]

    (cond
      ;; Invisible objects are not accessible
      (set-thing-flag? game-state obj-id :invisible)
      false

      ;; No location means not in world
      (nil? loc)
      false

      ;; Global objects are always accessible
      (= loc :global-objects)
      true

      ;; Local-global objects (room scenery)
      (and (= loc :local-globals)
           (room-has-global? game-state here obj-id))
      true

      ;; Must be in same room as player
      (not= (meta-loc game-state obj-id) here)
      (if (= (meta-loc game-state obj-id) winner-loc)
        true  ; In same location as winner (e.g., both in vehicle)
        false)

      ;; Directly held by winner, in room, or in winner's location
      (or (= loc winner)
          (= loc here)
          (= loc winner-loc))
      true

      ;; In an open container that's accessible
      (and (set-thing-flag? game-state loc :open)
           (accessible? game-state loc))
      true

      ;; Otherwise not accessible
      :else
      false)))

;;; ---------------------------------------------------------------------------
;;; LIGHTING CHECK
;;; ---------------------------------------------------------------------------

(defn lit?
  "Check if a room is lit.

   ZIL: LIT? routine, lines 1333-1355

   A room is lit if:
   - ALWAYS-LIT is set (debug mode), OR
   - The room has the ON flag (inherently lit), OR
   - There's a light source in the room/player that's ON

   Arguments:
     game-state - current state
     room-id - room to check
     check-room-flag? - if true, also check room's ONBIT

   Returns: true if the room is lit"
  ([game-state room-id]
   (lit? game-state room-id true))
  ([game-state room-id check-room-flag?]
   (cond
     ;; No room specified - default to lit (fallback)
     (nil? room-id)
     true

     ;; Debug always-lit mode
     (and (:always-lit game-state)
          (= (:winner game-state) (:player game-state)))
     true

     ;; Room is inherently lit
     (and check-room-flag?
          (set-thing-flag? game-state room-id :on))
     true

     ;; Search for light source
     :else
     (let [;; Save and set GWIMBIT to search for lit objects
           gs (-> game-state
                  (assoc-in [:parser :gwimbit] :on)
                  (assoc-in [:parser :slocbits] -1))
           winner (:winner game-state)
           player (:player game-state)
           old-here (:here game-state)

           ;; Temporarily set HERE for the search
           gs (assoc gs :here room-id)

           ;; Search winner's inventory
           matches (do-sl gs winner [] 1 1)

           ;; Also search player if different from winner and in room
           matches (if (and (not= winner player)
                            (= (get-thing-location gs player) room-id))
                     (do-sl gs player matches 1 1)
                     matches)

           ;; Search the room
           matches (do-sl gs room-id matches 1 1)]

       (pos? (count matches))))))

;;; ---------------------------------------------------------------------------
;;; MANY-CHECK
;;; ---------------------------------------------------------------------------

(defn many-check
  "Verify that multiple objects are allowed for this verb.

   ZIL: MANY-CHECK routine, lines 1294-1313

   Some verbs don't make sense with multiple objects:
   - PUT X IN Y - can only put one thing at a time (in this parser)
   - GIVE X TO Y - can only give one thing

   If player said 'put all in case' but verb doesn't allow multiple
   direct objects, this returns an error.

   Returns:
     {:success true, :game-state gs} - OK
     {:success false, :game-state gs, :error ...} - too many objects"
  [game-state]
  (let [syntax (get-in game-state [:parser :syntax])
        prso (get-in game-state [:parser :prso] [])
        prsi (get-in game-state [:parser :prsi] [])
        loc1 (:loc1 syntax)
        loc2 (:loc2 syntax)

        ;; Check if SMANY bit is set
        allows-many-direct? (and loc1
                                 (bit-test loc1 (:many search-bits)))
        allows-many-indirect? (and loc2
                                   (bit-test loc2 (:many search-bits)))

        loss (cond
               ;; Multiple direct objects but not allowed
               (and (> (count prso) 1)
                    (not allows-many-direct?))
               :direct

               ;; Multiple indirect objects but not allowed
               (and (> (count prsi) 1)
                    (not allows-many-indirect?))
               :indirect

               :else nil)]

    (if loss
      {:success false
       :game-state game-state
       :error {:type :too-many-objects
               :which loss
               :message (str "You can't use multiple "
                             (if (= loss :indirect) "in" "")
                             "direct objects with \""
                             (get-in game-state [:parser :vtbl 0])
                             "\".")}}
      {:success true :game-state game-state})))

;;; ---------------------------------------------------------------------------
;;; TAKE-CHECK
;;; ---------------------------------------------------------------------------

(defn held?
  "Check if an object is held by the player (directly or in carried container).

   ZIL: Uses HELD? macro internally"
  [game-state obj-id]
  (let [player (:player game-state)]
    (loop [current obj-id]
      (let [loc (get-thing-location game-state current)]
        (cond
          (nil? loc) false
          (= loc player) true
          (set-thing-flag? game-state loc :container)
          (recur loc)
          :else false)))))

(defn itake-check
  "Check a single match table for take requirements.

   ZIL: ITAKE-CHECK routine, lines 1248-1292

   For each object in the table, if the syntax requires HAVE or TAKE:
   - HAVE: Must already be holding it
   - TAKE: Try to auto-take if not holding

   Arguments:
     game-state - current state
     match-table - :prso or :prsi
     ibits - location bits from syntax

   Returns:
     {:success true, :game-state gs} - all objects OK
     {:success false, :game-state gs, :error ...} - can't take/don't have"
  [game-state match-table ibits]
  (if (or (nil? ibits)
          (empty? (get-in game-state [:parser match-table] [])))
    {:success true :game-state game-state}

    (let [need-have? (bit-test ibits (:have search-bits))
          can-take? (bit-test ibits (:take search-bits))
          objects (get-in game-state [:parser match-table] [])]

      (reduce
       (fn [{:keys [success game-state] :as acc} obj-id]
         (if (not success)
           acc  ; Already failed, short-circuit

           (cond
             ;; Special handling for IT pronoun
             (= obj-id :it)
             (let [it-obj (get-in game-state [:parser :it-object])]
               (if (accessible? game-state it-obj)
                 {:success true
                  :game-state game-state}
                 {:success false
                  :game-state game-state
                  :error {:type :not-here
                          :message "I don't see what you're referring to."}}))

             ;; Already holding it - OK
             (held? game-state obj-id)
             {:success true :game-state game-state}

             ;; Special objects that don't need taking
             (or (= obj-id :hands)
                 (= obj-id :me))
             {:success true :game-state game-state}

             ;; Has TRYTAKEBIT - always "taken" succeeds
             (set-thing-flag? game-state obj-id :trytake)
             {:success true :game-state game-state}

             ;; NPC winner can't auto-take
             (not= (:winner game-state) (:player game-state))
             {:success true :game-state game-state}

             ;; Can auto-take - try it
             can-take?
             (let [take-result (itake game-state obj-id)]
               (if (= take-result true)
                 ;; Take failed silently (handled by itake)
                 {:success true :game-state game-state}
                 ;; Take succeeded, print "(Taken)"
                 (do
                   (println "(Taken)")
                   {:success true :game-state game-state})))

             ;; Must HAVE but don't - error
             need-have?
             (if (= obj-id :not-here-object)
               {:success false
                :game-state game-state
                :error {:type :dont-have
                        :message "You don't have that!"}}
               {:success false
                :game-state game-state
                :error {:type :dont-have
                        :object obj-id
                        :message (str "You don't have the "
                                      (thing-name game-state obj-id) ".")}})

             ;; Otherwise OK
             :else
             {:success true :game-state game-state})))

       {:success true :game-state game-state}
       objects))))

(defn take-check
  "Check both PRSO and PRSI for take requirements.

   ZIL: TAKE-CHECK routine, lines 1244-1246
     <ROUTINE TAKE-CHECK ()
       <AND <ITAKE-CHECK ,P-PRSO <GETB ,P-SYNTAX ,P-SLOC1>>
            <ITAKE-CHECK ,P-PRSI <GETB ,P-SYNTAX ,P-SLOC2>>>>

   Returns:
     {:success true, :game-state gs} - OK
     {:success false, ...} - take/have failed"
  [game-state]
  (let [syntax (get-in game-state [:parser :syntax])
        loc1 (:loc1 syntax)
        loc2 (:loc2 syntax)

        ;; Check direct objects
        result1 (itake-check game-state :prso loc1)]

    (if (not (:success result1))
      result1
      ;; Check indirect objects
      (itake-check (:game-state result1) :prsi loc2))))

;;; ---------------------------------------------------------------------------
;;; HELPER: ITAKE (simplified)
;;; ---------------------------------------------------------------------------

(defn itake
  "Attempt to take an object (simplified version).

   The full ITAKE is in the verb handlers. This is a simplified version
   for auto-take during parsing.

   Returns true if take failed (object can't be taken),
   false if take succeeded."
  [game-state obj-id]
  ;; TODO: Implement actual take logic
  ;; For now, just check if it's takeable
  (if (set-thing-flag? game-state obj-id :take)
    false  ; Would succeed
    true)) ; Would fail

;;; ---------------------------------------------------------------------------
;;; HELPERS
;;; ---------------------------------------------------------------------------

(defn room-has-global?
  "Check if a room has a specific local-global object.

   Local globals are scenery objects listed in a room's GLOBAL property."
  [game-state room-id obj-id]
  ;; TODO: Implement when rooms have global lists
  false)

(defn room?
  "Check if an object ID is a room."
  [game-state obj-id]
  (contains? (:rooms game-state) obj-id))
