(ns clork.verbs-inventory
  "Inventory manipulation handlers: take, drop, read.

   ZIL Reference: V-TAKE, V-DROP, V-READ in gverbs.zil."
  (:require [clork.utils :as utils]
            [clork.game-state :as gs]
            [clork.parser.state :as parser-state]
            [clork.verbs-health :as verbs-health]
            [clork.debug.trace :as trace]
            [clork.random :as random]))

;;; ---------------------------------------------------------------------------
;;; TAKE COMMAND
;;; ---------------------------------------------------------------------------
;;; ZIL: V-TAKE and ITAKE in gverbs.zil, PRE-TAKE for preconditions

(def ^:private yuks
  "Humorous responses for trying to take non-takeable objects.
   ZIL: YUKS global in gverbs.zil"
  ["A valiant attempt."
   "You can't be serious."
   "An interesting idea..."
   "What a concept!"])

(defn takeable?
  "Returns true if the object has the :take flag."
  [obj]
  (contains? (or (:flags obj) #{}) :take))

(defn in-closed-container?
  "Returns true if the object is inside a closed container.
   Returns false for objects in special locations like :local-globals."
  [game-state obj-id]
  (let [loc-id (gs/get-thing-loc-id game-state obj-id)]
    ;; Only check container flags if loc-id is an actual object
    ;; (not nil, not :local-globals, not a room)
    (and loc-id
         (contains? (:objects game-state) loc-id)
         (gs/set-thing-flag? game-state loc-id :cont)
         (not (gs/set-thing-flag? game-state loc-id :open)))))

(defn move-to-inventory
  "Move an object to the winner's (player's) inventory.
   Tracks acquisition sequence for LIFO inventory ordering (ZIL behavior)."
  [game-state obj-id]
  (let [winner (:winner game-state)
        seq-num (inc (or (:inv-seq game-state) 0))]
    (-> game-state
        (assoc :inv-seq seq-num)
        (assoc-in [:objects obj-id :in] winner)
        (assoc-in [:objects obj-id :inv-seq] seq-num))))

(defn already-holding?
  "Returns true if the player is already holding the object."
  [game-state obj-id]
  (let [winner (:winner game-state)
        obj-loc (gs/get-thing-loc-id game-state obj-id)]
    (= obj-loc winner)))

(defn- trytake?
  "Returns true if the object has the :trytake flag (action should be tried first)."
  [obj]
  (contains? (or (:flags obj) #{}) :trytake))

(defn- add-flag
  "Add a flag to an object's flag set in game-state."
  [game-state obj-id flag]
  (let [current-flags (get-in game-state [:objects obj-id :flags] #{})]
    (assoc-in game-state [:objects obj-id :flags] (conj current-flags flag))))

(defn v-take
  "Take an object and add it to inventory.

   ZIL: V-TAKE in gverbs.zil (line 1398)
     <ROUTINE V-TAKE ()
       <COND (<EQUAL? <ITAKE> T>
              <COND (<FSET? ,PRSO ,WEARBIT>
                     <TELL \"You are now wearing the \" D ,PRSO \".\" CR>)
                    (T
                     <TELL \"Taken.\" CR>)>)>>

   PRE-TAKE checks (line 1369):
   - Already holding: \"You already have that!\"
   - In closed container: \"You can't reach something that's inside a closed container.\"

   ITAKE checks (line 1916):
   - Not takeable (no TAKEBIT): pick from YUKS
   - In closed container: fail silently
   - Too heavy: \"Your load is too heavy\"
   - Too many items: \"You're holding too many things already!\"
   - Success: move to inventory, set TOUCHBIT

   Objects with TRYTAKEBIT have their action handler called first,
   which can override the default take behavior (e.g., rug, trophy case)."
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        obj (gs/get-thing game-state prso)
        desc (:desc obj)
        action-fn (:action obj)]
    ;; For objects with :trytake flag, try their action handler first
    (if-let [result (when (and (trytake? obj) action-fn)
                      (let [gs-traced (trace/trace-action-call game-state prso :take)
                            action-result (action-fn gs-traced)]
                        (trace/trace-action-result game-state prso (some? action-result))
                        action-result))]
      result
      ;; Default take behavior
      (cond
        ;; Already holding it
        (already-holding? game-state prso)
        (utils/tell game-state "You already have that!")

        ;; In a closed container - can't reach it
        (in-closed-container? game-state prso)
        (utils/tell game-state "You can't reach something that's inside a closed container.")

        ;; Not takeable - respond with humor
        (not (takeable? obj))
        (utils/tell game-state (random/rand-nth* yuks))

        ;; Too heavy to carry - ZIL: ITAKE check (gverbs.zil lines 1947-1956)
        ;; Only check weight if object is NOT inside something winner is carrying
        ;; (if it's inside something they're carrying, weight is already counted)
        (let [obj-loc (gs/get-thing-loc-id game-state prso)
              winner (:winner game-state)
              load-allowed (or (:load-allowed game-state) 100)
              load-max (or (:load-max game-state) 100)
              obj-weight (gs/weight game-state prso)
              winner-weight (gs/weight game-state winner)]
          (and (not= obj-loc winner)  ; Not already inside winner's carried items
               (> (+ obj-weight winner-weight) load-allowed)))
        (let [load-allowed (or (:load-allowed game-state) 100)
              load-max (or (:load-max game-state) 100)]
          (if (< load-allowed load-max)
            ;; Player is injured (reduced capacity)
            (utils/tell game-state "Your load is too heavy, especially in light of your condition.")
            ;; Normal capacity
            (utils/tell game-state "Your load is too heavy.")))

        ;; Success - take the object
        :else
        (let [state (-> game-state
                        (move-to-inventory prso)
                        (add-flag prso :touch)
                        ;; ZIL: SCORE-OBJ called in ITAKE (gverbs.zil line 1977)
                        ;; Treasures are scored when first taken
                        (verbs-health/score-obj prso))]
          (utils/tell state "Taken."))))))

;;; ---------------------------------------------------------------------------
;;; READ COMMAND
;;; ---------------------------------------------------------------------------
;;; ZIL: V-READ in gverbs.zil

(defn- readable?
  "Returns true if the object has the :read flag."
  [obj]
  (contains? (or (:flags obj) #{}) :read))

(defn v-read
  "Read text on an object.

   ZIL: V-READ in gverbs.zil (line 1159)
     <ROUTINE V-READ ()
       <COND (<NOT <FSET? ,PRSO ,READBIT>>
              <TELL \"How does one read a \" D ,PRSO \"?\" CR>)
             (T
              <TELL <GETP ,PRSO ,P?TEXT> CR>)>>"
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        obj (gs/get-thing game-state prso)
        desc (:desc obj)]
    (cond
      ;; Not readable
      (not (readable? obj))
      (utils/tell game-state (str "How does one read a " desc "?"))

      ;; Readable - print the text
      :else
      (if-let [text (:text obj)]
        (utils/tell game-state text)
        (utils/tell game-state "There's nothing written on it.")))))

;;; ---------------------------------------------------------------------------
;;; DROP COMMAND
;;; ---------------------------------------------------------------------------
;;; ZIL: V-DROP and IDROP in gverbs.zil

(defn carrying?
  "Returns true if the player (winner) is carrying the object,
   either directly or in a container they're holding."
  [game-state obj-id]
  (let [winner (:winner game-state)
        obj-loc (gs/get-thing-loc-id game-state obj-id)]
    (or (= obj-loc winner)
        ;; Check if in a container the winner is holding
        (when obj-loc
          (= (gs/get-thing-loc-id game-state obj-loc) winner)))))

(defn- drop-to-room
  "Move an object to the current room.
   Sets inv-seq for proper LIFO ordering among dropped objects."
  [game-state obj-id]
  (let [here (:here game-state)
        seq-num (inc (or (:inv-seq game-state) 0))]
    (-> game-state
        (assoc :inv-seq seq-num)
        (assoc-in [:objects obj-id :in] here)
        (assoc-in [:objects obj-id :inv-seq] seq-num))))

(defn v-drop
  "Drop an object from inventory.

   ZIL: V-DROP in gverbs.zil (line 495)
     <ROUTINE V-DROP ()
       <COND (<IDROP>
              <TELL \"Dropped.\" CR>)>>

   PRE-DROP (line 490):
     <ROUTINE PRE-DROP ()
       <COND (<EQUAL? ,PRSO <LOC ,WINNER>>
              <PERFORM ,V?DISEMBARK ,PRSO>
              <RTRUE>)>>
   If PRSO is the player's vehicle, redirect to DISEMBARK.

   IDROP (line 1982):
   - Check if not carrying: \"You're not carrying the X.\"
   - Check if in closed container: \"The X is closed.\"
   - Move to current room

   First checks the object's action handler (e.g., rope in dome room)."
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        winner (:winner game-state)
        player-loc (gs/get-thing-loc-id game-state winner)]
    ;; PRE-DROP: If PRSO is the player's vehicle, redirect to disembark
    (if (= prso player-loc)
      ;; Player is inside PRSO - this is a disembark request
      (let [here (:here game-state)
            here-room (gs/get-thing game-state here)
            has-rland? (contains? (or (:flags here-room) #{}) :rland)]
        (cond
          ;; Room has RLAND - can safely disembark
          has-rland?
          (-> game-state
              (assoc-in [:objects winner :in] here)
              (utils/tell "You are on your own feet again."))
          ;; Can't disembark here (would be fatal - in water, etc.)
          :else
          (utils/tell game-state "You realize that getting out here would be fatal.")))
      ;; Normal drop logic
      (let [obj (gs/get-thing game-state prso)
            action-fn (:action obj)
            desc (:desc obj)]
        ;; First try the object's action handler
        (if-let [result (when action-fn (action-fn game-state))]
          result
          ;; Default behavior
          (cond
            ;; Not carrying it
            (not (carrying? game-state prso))
            (utils/tell game-state (str "You're not carrying the " desc "."))

            ;; Success - drop the object
            :else
            (let [state (drop-to-room game-state prso)]
              (utils/tell state "Dropped."))))))))
