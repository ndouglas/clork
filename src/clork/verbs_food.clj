(ns clork.verbs-food
  "Food and drink verb handlers.

   ZIL Reference:
   - V-EAT in gverbs.zil (lines 499-532)
   - V-DRINK in gverbs.zil (lines 484-485)
   - HIT-SPOT helper in gverbs.zil (lines 534-540)

   EAT behavior:
   - If object has FOODBIT: eat it, remove from game
   - If object has DRINKBIT: complex water handling
   - Otherwise: 'I don't think that would agree with you'

   DRINK behavior:
   - Just calls EAT (which checks DRINKBIT)"
  (:require [clork.utils :as utils]
            [clork.game-state :as gs]))

;;; ---------------------------------------------------------------------------
;;; HELPER FUNCTIONS
;;; ---------------------------------------------------------------------------

(defn- remove-object
  "Remove an object from the game world.
   ZIL: REMOVE-CAREFULLY - sets object's location to nil."
  [game-state obj-id]
  (assoc-in game-state [:objects obj-id :in] nil))

(defn- hit-spot
  "The water drinking success message.
   ZIL: HIT-SPOT routine (gverbs.zil lines 534-540)"
  [game-state prso]
  ;; If water is not global, remove it from game
  ;; For now, just remove regular water
  (-> game-state
      (cond-> (= prso :water) (remove-object :water))
      (utils/tell "Thank you very much. I was rather thirsty (from all this talking, probably).")
      (utils/crlf)))

(defn- player-holding?
  "Check if the player is holding the object directly or in a container they're holding.
   ZIL: <IN? ,PRSO ,WINNER> or <IN? <LOC ,PRSO> ,WINNER>"
  [game-state obj-id]
  (let [winner (:winner game-state)
        obj-loc (gs/get-thing-loc-id game-state obj-id)]
    (or (= obj-loc winner)
        (= (gs/get-thing-loc-id game-state obj-loc) winner))))

(defn- accessible?
  "Check if an object is accessible to the player.
   An object is accessible if player is holding it or it's in a held container."
  [game-state obj-id]
  (when obj-id
    (let [obj-loc (gs/get-thing-loc-id game-state obj-id)
          winner (:winner game-state)
          here (:here game-state)]
      (or (= obj-loc winner)
          (= obj-loc here)
          ;; Check if the container is accessible
          (when obj-loc
            (let [container-loc (gs/get-thing-loc-id game-state obj-loc)]
              (or (= container-loc winner)
                  (= container-loc here))))))))

;;; ---------------------------------------------------------------------------
;;; V-EAT
;;; ---------------------------------------------------------------------------
;;; ZIL: V-EAT (gverbs.zil lines 499-532)

(defn v-eat
  "Handle EAT and DRINK verbs.

   ZIL: V-EAT checks FOODBIT and DRINKBIT to determine behavior.
   - FOODBIT: Can be eaten, removes object from game
   - DRINKBIT: Complex water handling with container checks"
  [game-state]
  (let [prso (first (get-in game-state [:parser :prso]))
        verb (get-in game-state [:parser :prsa])
        is-food? (gs/set-thing-flag? game-state prso :food)
        is-drink? (gs/set-thing-flag? game-state prso :drink)]
    (cond
      ;; FOODBIT set - can be eaten
      is-food?
      (cond
        ;; Not holding it
        (not (player-holding? game-state prso))
        (-> game-state
            (utils/tell "You're not holding that.")
            (utils/crlf))

        ;; Trying to DRINK food
        (= verb :drink)
        (-> game-state
            (utils/tell "How can you drink that?")
            (utils/crlf))

        ;; Success - eat the food
        :else
        (-> game-state
            (remove-object prso)
            (utils/tell "Thank you very much. It really hit the spot.")
            (utils/crlf)))

      ;; DRINKBIT set - can be drunk
      is-drink?
      (let [container-id (gs/get-thing-loc-id game-state prso)]
        (cond
          ;; Water in an accessible container that player is NOT holding
          (and (accessible? game-state container-id)
               (not (player-holding? game-state container-id)))
          (-> game-state
              (utils/tell "You have to be holding the ")
              (utils/tell (gs/thing-name game-state container-id))
              (utils/tell " first.")
              (utils/crlf))

          ;; Container exists but not open
          (and container-id
               (gs/get-thing game-state container-id)
               (not (gs/set-thing-flag? game-state container-id :open)))
          (-> game-state
              (utils/tell "You'll have to open the ")
              (utils/tell (gs/thing-name game-state container-id))
              (utils/tell " first.")
              (utils/crlf))

          ;; Container not accessible at all
          (and container-id
               (not (accessible? game-state container-id)))
          (-> game-state
              (utils/tell "There isn't any water here.")
              (utils/crlf))

          ;; Success - drink it
          :else
          (hit-spot game-state prso)))

      ;; Neither food nor drink
      :else
      (-> game-state
          (utils/tell "I don't think that the ")
          (utils/tell (gs/thing-name game-state prso))
          (utils/tell " would agree with you.")
          (utils/crlf)))))

;;; ---------------------------------------------------------------------------
;;; V-DRINK
;;; ---------------------------------------------------------------------------
;;; ZIL: V-DRINK (gverbs.zil lines 484-485) - just calls V-EAT

(defn v-drink
  "Handle DRINK verb.
   ZIL: V-DRINK just calls V-EAT."
  [game-state]
  (v-eat game-state))

;;; ---------------------------------------------------------------------------
;;; V-DRINK-FROM
;;; ---------------------------------------------------------------------------
;;; ZIL: V-DRINK-FROM (gverbs.zil lines 487-488)

(defn v-drink-from
  "Handle DRINK FROM verb.
   ZIL: V-DRINK-FROM - 'How peculiar!'"
  [game-state]
  (-> game-state
      (utils/tell "How peculiar!")
      (utils/crlf)))

;;; ---------------------------------------------------------------------------
;;; V-GIVE
;;; ---------------------------------------------------------------------------
;;; ZIL: V-GIVE (gverbs.zil lines 729-733)

(defn v-give
  "Handle GIVE verb.

   ZIL: V-GIVE - Default handler for giving objects to NPCs.
   - First tries the PRSI (recipient) object's action handler
   - If PRSI is not an actor: 'You can't give a X to a Y!'
   - Otherwise: 'The Y refuses it politely.'

   Specific NPCs (cyclops, troll, thief) override this in their action handlers.

   Note: In ZIL, PERFORM calls PRSI action before verb handler (gmain.zil:252).
   Since our perform doesn't do this systematically, we handle it here."
  [game-state]
  (let [prso (first (get-in game-state [:parser :prso]))
        prsi (first (get-in game-state [:parser :prsi]))
        prsi-obj (gs/get-thing game-state prsi)
        action-fn (:action prsi-obj)]
    ;; First try the PRSI (recipient) object's action handler
    ;; ZIL: PERFORM calls PRSI action before verb handler
    (if-let [result (when action-fn (action-fn game-state))]
      result
      ;; PRSI didn't handle it - use default response
      (let [prso-name (gs/thing-name game-state prso)
            prsi-name (gs/thing-name game-state prsi)
            is-actor? (gs/set-thing-flag? game-state prsi :actor)]
        (if (not is-actor?)
          ;; Can't give to non-actors
          (-> game-state
              (utils/tell "You can't give a ")
              (utils/tell prso-name)
              (utils/tell " to a ")
              (utils/tell prsi-name)
              (utils/tell "!")
              (utils/crlf))
          ;; Actor refuses
          (-> game-state
              (utils/tell "The ")
              (utils/tell prsi-name)
              (utils/tell " refuses it politely.")
              (utils/crlf)))))))

;;; ---------------------------------------------------------------------------
;;; V-LOCK / V-UNLOCK
;;; ---------------------------------------------------------------------------
;;; ZIL: V-LOCK (gverbs.zil lines 871-872)
;;; ZIL: V-UNLOCK (gverbs.zil lines 1524-1525) - just calls V-LOCK

(defn v-lock
  "Handle LOCK verb.

   ZIL: V-LOCK - Default handler just says it doesn't work.
   Specific lockable objects (grating, etc.) override in their action handlers.

   First tries the object's action handler. If the object handles the verb,
   returns that result. Otherwise uses the default 'doesn't work' message."
  [game-state]
  (let [prso (first (get-in game-state [:parser :prso]))
        obj (gs/get-thing game-state prso)
        action-fn (:action obj)]
    ;; First try the object's action handler
    (if-let [result (when action-fn (action-fn game-state))]
      result
      ;; Object didn't handle it - default response
      (-> game-state
          (utils/tell "It doesn't seem to work.")
          (utils/crlf)))))

(defn v-unlock
  "Handle UNLOCK verb.

   ZIL: V-UNLOCK - Just calls V-LOCK (which tries object action first)."
  [game-state]
  (v-lock game-state))
