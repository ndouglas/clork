(ns clork.verbs-put
  "Put verb handlers: put in, put on, put under, put behind.

   ZIL Reference: V-PUT, V-PUT-ON, V-PUT-UNDER, V-PUT-BEHIND in gverbs.zil."
  (:require [clork.utils :as utils]
            [clork.game-state :as gs]
            [clork.parser.state :as parser-state]
            [clork.verbs-health :as verbs-health]
            [clork.verbs-inventory :as verbs-inv]))

;;; ---------------------------------------------------------------------------
;;; PUT HELPERS
;;; ---------------------------------------------------------------------------

(defn- openable?
  "Returns true if the object has the :door flag (can be opened/closed).

   ZIL: OPENABLE? checks DOORBIT."
  [game-state obj-id]
  (gs/set-thing-flag? game-state obj-id :door))

(defn- can-put-in?
  "Returns true if the container can accept objects.

   ZIL: <OR <FSET? ,PRSI ,OPENBIT> <OPENABLE? ,PRSI> <FSET? ,PRSI ,VEHBIT>>"
  [game-state container-id]
  (or (gs/set-thing-flag? game-state container-id :open)
      (openable? game-state container-id)
      (gs/set-thing-flag? game-state container-id :vehicle)))

(defn- get-capacity
  "Get the capacity of a container. Returns 0 if not specified."
  [game-state obj-id]
  (let [obj (gs/get-thing game-state obj-id)]
    (or (:capacity obj) 0)))

(defn- get-size
  "Get the size of an object. Returns 0 if not specified."
  [game-state obj-id]
  (let [obj (gs/get-thing game-state obj-id)]
    (or (:size obj) 0)))

(defn- weight
  "Calculate the total weight of an object including contents.

   ZIL: WEIGHT routine."
  [game-state obj-id]
  (let [base-size (get-size game-state obj-id)
        contents (gs/get-contents game-state obj-id)
        contents-weight (reduce + 0 (map #(weight game-state %) contents))]
    (+ base-size contents-weight)))

(defn- room-for?
  "Check if there's room in the container for the object.

   ZIL: <G? <- <+ <WEIGHT ,PRSI> <WEIGHT ,PRSO>> <GETP ,PRSI ,P?SIZE>>
            <GETP ,PRSI ,P?CAPACITY>>"
  [game-state obj-id container-id]
  (let [capacity (get-capacity game-state container-id)
        container-size (get-size game-state container-id)
        container-weight (weight game-state container-id)
        obj-weight (weight game-state obj-id)
        ;; ZIL checks: (container_weight + obj_weight - container_size) > capacity
        ;; Which means: used space + obj_weight > capacity
        used-space (- container-weight container-size)
        would-use (+ used-space obj-weight)]
    ;; If capacity is 0 (unset), assume unlimited
    (or (zero? capacity)
        (<= would-use capacity))))

;;; ---------------------------------------------------------------------------
;;; V-PUT
;;; ---------------------------------------------------------------------------
;;; ZIL: <ROUTINE V-PUT ()> in gverbs.zil (lines 1101-1130)

(defn- score-put
  "Score an object being put in a container.
   Scores :value for any container, and additionally :tvalue for the trophy case."
  [game-state obj-id container-id]
  (let [gs (verbs-health/score-obj game-state obj-id)]
    (if (= container-id :trophy-case)
      (verbs-health/score-tvalue gs obj-id)
      gs)))

(defn v-put
  "Put an object inside a container.

   ZIL: V-PUT in gverbs.zil (lines 1101-1130)
   1. Check if container can accept objects (open, openable, or vehicle)
   2. Check if container is open
   3. Check if putting object in itself
   4. Check if already in container
   5. Check capacity
   6. Check if holding the object (try to take if not)
   7. Move object and score"
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        prsi (parser-state/get-prsi game-state)
        obj (gs/get-thing game-state prso)
        container (gs/get-thing game-state prsi)
        obj-desc (:desc obj)
        container-desc (:desc container)]
    (cond
      ;; ZIL: <COND (<OR <FSET? ,PRSI ,OPENBIT> <OPENABLE? ,PRSI> <FSET? ,PRSI ,VEHBIT>>)
      ;;            (T <TELL "You can't do that." CR> <RTRUE>)>
      (not (can-put-in? game-state prsi))
      (utils/tell game-state "You can't do that.")

      ;; ZIL: <COND (<NOT <FSET? ,PRSI ,OPENBIT>>
      ;;             <TELL "The " D ,PRSI " isn't open." CR>
      ;;             <THIS-IS-IT ,PRSI>)
      (not (gs/set-thing-flag? game-state prsi :open))
      (-> game-state
          (utils/tell (str "The " container-desc " isn't open."))
          (assoc :it prsi))

      ;; ZIL: (<EQUAL? ,PRSI ,PRSO> <TELL "How can you do that?" CR>)
      (= prsi prso)
      (utils/tell game-state "How can you do that?")

      ;; ZIL: (<IN? ,PRSO ,PRSI>
      ;;       <TELL "The " D ,PRSO " is already in the " D ,PRSI "." CR>)
      (= (:in obj) prsi)
      (utils/tell game-state (str "The " obj-desc " is already in the " container-desc "."))

      ;; ZIL: (<G? ... <GETP ,PRSI ,P?CAPACITY>> <TELL "There's no room." CR>)
      (not (room-for? game-state prso prsi))
      (utils/tell game-state "There's no room.")

      ;; ZIL: (<AND <NOT <HELD? ,PRSO>> <FSET? ,PRSO ,TRYTAKEBIT>>
      ;;       <TELL "You don't have the " D ,PRSO "." CR> <RTRUE>)
      (and (not (verbs-inv/already-holding? game-state prso))
           (gs/set-thing-flag? game-state prso :trytake))
      (utils/tell game-state (str "You don't have the " obj-desc "."))

      ;; ZIL: (<AND <NOT <HELD? ,PRSO>> <NOT <ITAKE>>> <RTRUE>)
      ;; Try to take the object if not holding it
      (not (verbs-inv/already-holding? game-state prso))
      (let [;; Try implicit take - simplified version
            take-result (verbs-inv/v-take (assoc-in game-state [:parser :prso] [prso]))]
        (if (verbs-inv/already-holding? take-result prso)
          ;; Successfully took it, now put it in
          (let [seq-num (inc (or (:inv-seq take-result) 0))]
            (-> take-result
                (assoc :inv-seq seq-num)
                (assoc-in [:objects prso :in] prsi)
                (assoc-in [:objects prso :inv-seq] seq-num)
                (gs/set-thing-flag prso :touch)
                (score-put prso prsi)
                (utils/tell "Done.")))
          ;; Couldn't take it
          take-result))

      ;; ZIL: (T <MOVE ,PRSO ,PRSI> <FSET ,PRSO ,TOUCHBIT>
      ;;         <SCORE-OBJ ,PRSO> <TELL "Done." CR>)
      :else
      (let [seq-num (inc (or (:inv-seq game-state) 0))]
        (-> game-state
            (assoc :inv-seq seq-num)
            (assoc-in [:objects prso :in] prsi)
            (assoc-in [:objects prso :inv-seq] seq-num)
            (gs/set-thing-flag prso :touch)
            (score-put prso prsi)
            (utils/tell "Done."))))))

;;; ---------------------------------------------------------------------------
;;; V-PUT-ON
;;; ---------------------------------------------------------------------------
;;; ZIL: <ROUTINE V-PUT-ON ()> in gverbs.zil (lines 1135-1142)

(defn v-put-on
  "Put an object on a surface.

   ZIL: V-PUT-ON in gverbs.zil (lines 1135-1142)
   - If PRSI is GROUND, drop the object
   - If PRSI has SURFACEBIT, call v-put
   - Otherwise, no good surface"
  [game-state]
  (let [prsi (parser-state/get-prsi game-state)
        container (gs/get-thing game-state prsi)
        container-desc (:desc container)]
    (cond
      ;; ZIL: (<EQUAL? ,PRSI ,GROUND> <PERFORM ,V?DROP ,PRSO> <RTRUE>)
      (= prsi :ground)
      (verbs-inv/v-drop game-state)

      ;; ZIL: (<FSET? ,PRSI ,SURFACEBIT> <V-PUT>)
      (gs/set-thing-flag? game-state prsi :surface)
      (v-put game-state)

      ;; ZIL: (T <TELL "There's no good surface on the " D ,PRSI "." CR>)
      :else
      (utils/tell game-state (str "There's no good surface on the " container-desc ".")))))

;;; ---------------------------------------------------------------------------
;;; V-PUT-BEHIND
;;; ---------------------------------------------------------------------------
;;; ZIL: <ROUTINE V-PUT-BEHIND ()> in gverbs.zil (line 1132-1133)

(defn v-put-behind
  "Put an object behind something.

   ZIL: V-PUT-BEHIND in gverbs.zil (line 1132-1133)
   Always fails with 'That hiding place is too obvious.'"
  [game-state]
  (utils/tell game-state "That hiding place is too obvious."))

;;; ---------------------------------------------------------------------------
;;; V-PUT-UNDER
;;; ---------------------------------------------------------------------------
;;; ZIL: <ROUTINE V-PUT-UNDER ()> in gverbs.zil (lines 1144-1145)

(defn v-put-under
  "Put an object under something.

   ZIL: V-PUT-UNDER in gverbs.zil (lines 1144-1145)
   Always fails with 'You can't do that.'"
  [game-state]
  (utils/tell game-state "You can't do that."))
