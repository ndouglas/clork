(ns clork.game-state
  "Game state management and flag operations."
  (:require [clork.parser.state :as parser-state]))

;;; ---------------------------------------------------------------------------
;;; PARSER CONSTANTS
;;; ---------------------------------------------------------------------------
;;; These are defined here (rather than in parser/) so they're available
;;; to verb_defs.clj which is loaded before the parser.

(def search-bits
  "Location bits controlling where to search for objects.

   ZIL Constants from gparser.zil lines 1032-1038."
  {:held     128   ; SH - search player's inventory
   :carried   64   ; SC - search containers player is carrying
   :in-room   32   ; SIR - search room floor
   :on-ground 16   ; SOG - search containers in room
   :take       8   ; STAKE - auto-take if not held
   :many       4   ; SMANY - allow 'all', 'everything'
   :have       2}) ; SHAVE - must already be holding

(def getflags
  "Flags for GET-OBJECT parsing state.

   ZIL: P-ALL, P-ONE, P-INHIBIT constants."
  {:all     1   ; Player said 'all' or 'everything'
   :one     2   ; Player said 'one' or 'a' (pick randomly)
   :inhibit 4}) ; Inhibit object search (for 'of' constructs)

;;; ---------------------------------------------------------------------------
;;; GAME STATE
;;; ---------------------------------------------------------------------------

(defn initial-game-state
  "Return an initial game state."
  []
  {:rooms {}
   :objects {}
   :i-candles 40
   :i-lantern 200
    ;; <GLOBAL HERE 0>
   :here :west-of-house
   :it :mailbox
   :lit false
   :adventurer :adventurer
    ;; <GLOBAL WINNER 0>
   :winner :adventurer
   :player :adventurer
   :verbose false
   :super-brief false
   :won false
   :deaths 0
   ;; Scoring system - ZIL: SCORE, BASE-SCORE, SCORE-MAX, MOVES
   :score 0
   :base-score 0
   :score-max 350
   :moves 0
   ;; Load capacity - ZIL: LOAD-MAX, LOAD-ALLOWED (gglobals.zil lines 92-94)
   :load-max 100
   :load-allowed 100
   ;; Combat flags - ZIL: TROLL-FLAG, CYCLOPS-FLAG, MAGIC-FLAG, CYCLOWRATH
   :troll-flag false
   :cyclops-flag false  ; Cyclops asleep/defeated
   :magic-flag false    ; East wall opened (cyclops fled)
   :cyclowrath 0        ; Cyclops anger level
   ;; Dam/Reservoir flags - ZIL: LOW-TIDE, GATES-OPEN, GATE-FLAG, WATER-LEVEL
   :low-tide false      ; Water level low (reservoir empty) - starts false (reservoir full)
   :gates-open false    ; Sluice gates open
   :gate-flag false     ; Bolt can be turned (set by yellow button)
   :water-level 0       ; Flooding progress in maintenance room (0-8)
   ;; Loud Room puzzle flag - ZIL: LOUD-FLAG
   :loud-flag false     ; Echo puzzle solved (room is quiet)
   :parser (parser-state/initial-parser-state)
    ;; Undo system state
   :undo-stack []
   :redo-stack []
   :undo-limit 100
    ;; Trace system state
   :trace {:verbs false
           :parser false
           :actions false
           :daemons false}
    ;; Daemon system state
   :daemons {}
   :daemon-history []
    ;; Script mode tracking
   :parser-error-count 0})

;;; ---------------------------------------------------------------------------
;;; FLAG OPERATIONS
;;; ---------------------------------------------------------------------------
;;; Generic flag operations for objects, rooms, and other entities.
;;;
;;; *** IMPORTANT: TWO-LAYER FLAG SYSTEM ***
;;;
;;; Flags have two sources:
;;;   1. Static :flags set - defined in object/room definitions (e.g., #{:take :light})
;;;   2. Runtime overrides - set via set-flag/unset-flag as separate keys
;;;
;;; When checking flags, ALWAYS use set-thing-flag? (or flag?), which checks both.
;;;
;;; NEVER use (contains? (:flags obj) :xxx) - this only checks the static set
;;; and will miss runtime changes. This pattern causes bugs like objects showing
;;; wrong states (e.g., "(providing light)" when turned off).
;;;
;;; The flag lint test (flag_lint_test.clj) will catch these issues.
;;;
;;; Base functions:
;;;   set-flag, unset-flag, flag? - work with entity-type (:objects, :rooms)
;;;
;;; Polymorphic functions (auto-detect entity type):
;;;   set-thing-flag, unset-thing-flag, set-thing-flag?
;;;
;;; Current room convenience:
;;;   set-here-flag, unset-here-flag, set-here-flag?

(defn- resolve-entity-type
  "Resolve the entity type for a thing-id. Returns :objects, :rooms, or nil."
  [game-state thing-id]
  (cond
    (contains? (:objects game-state) thing-id) :objects
    (contains? (:rooms game-state) thing-id) :rooms
    (= (:player game-state) thing-id) :objects
    :else nil))

(defn set-flag
  "Sets a flag on an entity. entity-type is :objects or :rooms."
  [game-state entity-type entity-id flag]
  (assoc-in game-state [entity-type entity-id flag] true))

(defn unset-flag
  "Unsets a flag on an entity. entity-type is :objects or :rooms."
  [game-state entity-type entity-id flag]
  (assoc-in game-state [entity-type entity-id flag] false))

(defn flag?
  "Returns true if a flag is set on an entity. entity-type is :objects or :rooms.
   Checks both direct flag keys (e.g., [:rooms :id :lit]) and :flags sets.
   If the direct key is explicitly set (true/false), that takes precedence.
   Otherwise falls back to checking the :flags set."
  [game-state entity-type entity-id flag]
  (let [direct-flag (get-in game-state [entity-type entity-id flag])]
    (if (some? direct-flag)
      ;; Direct key is explicitly set - use its value
      direct-flag
      ;; Not explicitly set - check the :flags set
      (contains? (get-in game-state [entity-type entity-id :flags] #{}) flag))))

;; Polymorphic functions that auto-detect entity type

(defn- resolve-thing
  "Resolve entity-type and actual-id for a thing, or throw if not found.
   Returns a map with :entity-type and :actual-id keys."
  [game-state thing-id]
  (if-let [entity-type (resolve-entity-type game-state thing-id)]
    {:entity-type entity-type
     :actual-id (if (= (:player game-state) thing-id)
                  (:adventurer game-state)
                  thing-id)}
    (throw (Exception. (str "Thing " thing-id " not found!")))))

(defn set-thing-flag
  "Sets a flag on a room or object."
  [game-state thing-id flag]
  (let [{:keys [entity-type actual-id]} (resolve-thing game-state thing-id)]
    (set-flag game-state entity-type actual-id flag)))

(defn unset-thing-flag
  "Unsets a flag on room or object."
  [game-state thing-id flag]
  (let [{:keys [entity-type actual-id]} (resolve-thing game-state thing-id)]
    (unset-flag game-state entity-type actual-id flag)))

(defn set-thing-flag?
  "Indicates whether a flag is set on a room or object."
  [game-state thing-id flag]
  (let [{:keys [entity-type actual-id]} (resolve-thing game-state thing-id)]
    (flag? game-state entity-type actual-id flag)))

;; Current room convenience functions

(defn set-here-flag
  "Sets a flag on the current room."
  [game-state flag]
  (set-flag game-state :rooms (:here game-state) flag))

(defn unset-here-flag
  "Unsets a flag on the current room."
  [game-state flag]
  (unset-flag game-state :rooms (:here game-state) flag))

(defn set-here-flag?
  "Indicates whether a flag is set on the current room."
  [game-state flag]
  (flag? game-state :rooms (:here game-state) flag))

(defn get-thing
  "Get an object or room based on its ID."
  [game-state id]
  (or (get-in game-state [:rooms id])
      (get-in game-state [:objects id])))

(defn thing-name
  "Get the description/name of a thing (object or room)."
  [game-state id]
  (or (:desc (get-thing game-state id))
      (name id)))

(defn get-thing-loc-id
  "Return the ID of the object or room in which the specified id is located."
  [game-state thing-id]
  (:in (get-thing game-state thing-id)))

;; Alias for compatibility with parser code
(def get-thing-location get-thing-loc-id)

(defn get-contents
  "Return the IDs of all objects inside the given container.
   For player/actor inventory, sorts by acquisition sequence (LIFO - newest first).
   For rooms, sorts by :order field with inv-seq as tie-breaker (for LIFO among dropped objects).
   For object containers, sorts by inv-seq if present (LIFO), otherwise by :order.

   Note: When searching actor inventory, we normalize :player, :adventurer, and winner
   to find objects regardless of which specific ID was used when placing them.
   This handles the case where code might place objects :in :player or :in :adventurer."
  [game-state container-id]
  (let [winner (:winner game-state)
        player (:player game-state)
        is-actor? (or (= container-id :adventurer)
                      (= container-id :player)
                      (= container-id winner)
                      (= container-id player))
        ;; For actor inventory, search all equivalent actor IDs
        ;; Use hash-set to allow duplicates (when winner/player are :adventurer)
        actor-ids (when is-actor?
                    (hash-set :adventurer :player winner player))
        is-room? (contains? (:rooms game-state) container-id)
        contents (->> (:objects game-state)
                      (filter (fn [[_ obj]]
                                (if is-actor?
                                  ;; For actors, check if :in matches any equivalent ID
                                  (contains? actor-ids (:in obj))
                                  ;; For other containers, exact match
                                  (= (:in obj) container-id)))))]
    (cond
      ;; Player inventory: sort by inv-seq descending (LIFO - newest first)
      is-actor?
      (->> contents
           (sort-by (fn [[_ obj]] (- (or (:inv-seq obj) 0))))
           (map first))
      ;; Rooms: dropped objects (have inv-seq) come first in LIFO order,
      ;; then original objects (no inv-seq) in their :order
      ;; This matches ZIL behavior where dropped objects go to front of list
      is-room?
      (->> contents
           (sort-by (fn [[_ obj]]
                      (let [inv-seq (:inv-seq obj)
                            order (or (:order obj) 999)]
                        (if inv-seq
                          ;; Dropped: sort first (0), then by LIFO (higher inv-seq first)
                          [0 (- inv-seq)]
                          ;; Original: sort second (1), then by :order
                          [1 order]))))
           (map first))
      ;; Object containers: sort by inv-seq if present, otherwise by :order
      ;; This handles both initial placement (no inv-seq) and gameplay PUT (has inv-seq)
      :else
      (->> contents
           (sort-by (fn [[_ obj]]
                      (if-let [seq (:inv-seq obj)]
                        ;; Has inv-seq: use LIFO (negated for descending)
                        [(- seq) 0]
                        ;; No inv-seq: use :order for stable initial ordering
                        [0 (or (:order obj) 999)])))
           (map first)))))

;;; ---------------------------------------------------------------------------
;;; WEIGHT CALCULATION
;;; ---------------------------------------------------------------------------

(defn get-size
  "Get the size of an object.

   ZIL: <PROPDEF SIZE 5> in zork1.zil line 32
   Default SIZE is 5, not 0."
  [game-state obj-id]
  (let [obj (get-thing game-state obj-id)]
    (or (:size obj) 5)))

(defn weight
  "Calculate the total weight of an object including contents.

   ZIL: WEIGHT routine in gverbs.zil
     <ROUTINE WEIGHT (OBJ)
       <SET WT <GETP .OBJ ,P?SIZE>>
       <DO-SL .OBJ>  ; Recursively add contents
       <RETURN .WT>>

   Returns the sum of the object's :size and the weight of all contained objects."
  [game-state obj-id]
  (let [base-size (get-size game-state obj-id)
        contents (get-contents game-state obj-id)
        contents-weight (reduce + 0 (map #(weight game-state %) contents))]
    (+ base-size contents-weight)))

(defn verbose?
  "Indicate whether we should be operating in verbose mode."
  [game-state]
  (or (get game-state :verbose false) (not (set-here-flag? game-state :touch))))

(defn get-here
  "Return the current room's object."
  [game-state]
  (get-thing game-state (:here game-state)))

(defn get-winner
  "Return the WINNER object."
  [game-state]
  (get-thing game-state (:winner game-state)))

(defn get-winner-loc
  "Return the location of the winner."
  [game-state]
  (get-thing game-state (get-thing-loc-id game-state (:winner game-state))))

(defn get-winner-loc-id
  "Return the location ID of the winner."
  [game-state]
  (get-thing-loc-id game-state :winner))

(defn get-player
  "Return the PLAYER object."
  [game-state]
  (get-thing game-state (:player game-state)))

(defn add-room
  "Add a room to the game state"
  [game-state room]
  (assoc game-state :rooms
         (assoc (:rooms game-state) (:id room) room)))

(defn add-rooms
  "Add each of the list of rooms to the game state"
  [game-state rooms]
  (reduce add-room game-state rooms))

(defn add-object
  "Add an object to the game state. Optionally takes an order index.
   If the object already has an :order field, it is preserved."
  ([game-state object]
   (add-object game-state object nil))
  ([game-state object order]
   ;; Only set order if object doesn't already have one
   (let [obj-with-order (if (and order (not (contains? object :order)))
                          (assoc object :order order)
                          object)]
     (assoc game-state :objects
            (assoc (:objects game-state) (:id obj-with-order) obj-with-order)))))

(defn add-objects
  "Add each of the list of objects to the game state, preserving order.
   Objects receive an :order field matching their position in the list.
   This mirrors ZIL's FIRST?/NEXT? iteration order for room descriptions."
  [game-state objects]
  (reduce-kv (fn [gs idx obj] (add-object gs obj idx))
             game-state
             (vec objects)))

(defn game-state-copy
  "Set the value of one key in game-state to the value of another."
  [game-state source-key dest-key]
  (assoc-in game-state dest-key (get-in game-state source-key)))

;;
;; <ROUTINE META-LOC (OBJ)
;; 	 <REPEAT ()
;; 		 <COND (<NOT .OBJ>
;; 			<RFALSE>)
;; 		       (<IN? .OBJ ,GLOBAL-OBJECTS>
;; 			<RETURN ,GLOBAL-OBJECTS>)>
;; 		 <COND (<IN? .OBJ ,ROOMS>
;; 			<RETURN .OBJ>)
;; 		       (T
;; 			<SET OBJ <LOC .OBJ>>)>>>

(defn meta-location
  "Return the 'meta-location' of the thing."
  [game-state thing]
  (cond
    (nil? thing)
    nil
    (contains? (:objects game-state) thing)
    :objects
    (contains? (:rooms game-state) thing)
    :rooms
    true
    (meta-location game-state (:in thing))))

;;; ---------------------------------------------------------------------------
;;; STATE VALIDATION
;;; ---------------------------------------------------------------------------
;;; Functions to validate game state integrity. Useful for debugging.

(defn validate-state
  "Validate that game state has required structure and data.
   Returns {:valid? true} if valid, or {:valid? false :errors [...]} if invalid.

   Checks:
   - Required keys exist (:rooms, :objects, :here, :winner, :player)
   - :rooms and :objects are non-empty maps
   - :here refers to an existing room
   - :winner and :player refer to existing objects"
  [game-state]
  (let [errors (cond-> []
                 ;; Check game-state is not nil
                 (nil? game-state)
                 (conj "Game state is nil")

                 ;; Check required keys
                 (and (some? game-state) (not (contains? game-state :rooms)))
                 (conj "Missing :rooms key")

                 (and (some? game-state) (not (contains? game-state :objects)))
                 (conj "Missing :objects key")

                 (and (some? game-state) (not (contains? game-state :here)))
                 (conj "Missing :here key")

                 ;; Check :rooms is a non-empty map
                 (and (some? game-state)
                      (contains? game-state :rooms)
                      (or (not (map? (:rooms game-state)))
                          (empty? (:rooms game-state))))
                 (conj (str ":rooms is not a non-empty map (got: "
                            (type (:rooms game-state))
                            " with " (count (:rooms game-state)) " entries)"))

                 ;; Check :objects is a non-empty map
                 (and (some? game-state)
                      (contains? game-state :objects)
                      (or (not (map? (:objects game-state)))
                          (empty? (:objects game-state))))
                 (conj (str ":objects is not a non-empty map (got: "
                            (type (:objects game-state))
                            " with " (count (:objects game-state)) " entries)"))

                 ;; Check :here refers to existing room
                 (and (some? game-state)
                      (:here game-state)
                      (map? (:rooms game-state))
                      (not (contains? (:rooms game-state) (:here game-state))))
                 (conj (str ":here (" (:here game-state) ") not found in :rooms"))

                 ;; Check :winner refers to existing object
                 (and (some? game-state)
                      (:winner game-state)
                      (map? (:objects game-state))
                      (not (contains? (:objects game-state) (:winner game-state))))
                 (conj (str ":winner (" (:winner game-state) ") not found in :objects")))]
    (if (empty? errors)
      {:valid? true}
      {:valid? false :errors errors})))

(defn assert-valid-state
  "Assert that game state is valid. Throws exception with details if invalid.
   Returns game-state unchanged if valid (for use in threading)."
  [game-state]
  (let [{:keys [valid? errors]} (validate-state game-state)]
    (when-not valid?
      (throw (ex-info "Invalid game state" {:errors errors :state game-state})))
    game-state))

;;; ---------------------------------------------------------------------------
;;; ACTION RETURN CONVENTION
;;; ---------------------------------------------------------------------------
;;; Room and object actions can return game-state with :use-default-handling
;;; set to true to signal that the caller should apply default handling.
;;; This is safer than returning nil, which can corrupt state if propagated.

(defn use-default
  "Signal that the action didn't handle the request and default handling should be used.
   Use this instead of returning nil from action handlers."
  [game-state]
  (assoc game-state :use-default-handling true))

(defn use-default?
  "Check if an action result signals that default handling should be used."
  [game-state]
  (and (map? game-state)
       (:use-default-handling game-state)))

(defn clear-use-default
  "Clear the :use-default-handling flag from game-state."
  [game-state]
  (dissoc game-state :use-default-handling))