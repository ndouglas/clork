(ns clork.verbs-combat
  "Combat verb handlers - ATTACK, KILL, etc.

   ZIL Reference: gverbs.zil V-ATTACK, 1actions.zil HERO-BLOW"
  (:require [clork.utils :as utils]
            [clork.game-state :as gs]
            [clork.parser.state :as parser-state]
            [clork.verbs-health :as health]
            [clork.combat :as combat]
            [clork.random :as random]))

;;; ---------------------------------------------------------------------------
;;; VILLAIN REGISTRY
;;; ---------------------------------------------------------------------------
;;; Uses combat/villains-registry as the single source of truth.
;;; ZIL: VILLAINS global table (1actions.zil line 3814)

(defn get-villain-entry
  "Get the villain entry for an object, or nil if not a villain.
   Uses combat/villains-registry as the single source of truth."
  [villain-id]
  (get combat/villains-registry villain-id))

;; Forward declarations
(declare villain-result call-villain-action)

;;; ---------------------------------------------------------------------------
;;; HERO-BLOW - Player attacks villain
;;; ---------------------------------------------------------------------------
;;; ZIL: HERO-BLOW routine (1actions.zil line 3489)

(defn hero-blow
  "Player attacks a villain.

   ZIL: HERO-BLOW (1actions.zil line 3489-3561)

   Flow:
   1. Find villain in VILLAINS table
   2. Set FIGHTBIT on target (they're now in combat)
   3. If player staggered, attack fails
   4. Calculate attack/defense strengths
   5. Check if villain is unarmed/unconscious (auto-kill)
   6. Otherwise, select table and roll result
   7. Apply result to villain"
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        prsi (parser-state/get-prsi game-state)
        villain-entry (get-villain-entry prso)
        winner (:winner game-state)]

    (if-not villain-entry
      ;; Not a registered villain - shouldn't happen if parser did its job
      (utils/tell game-state (str "You can't fight the " (combat/get-thing-name game-state prso) "."))

      ;; Set fight flag on villain (they're now in combat)
      (let [gs (gs/set-thing-flag game-state prso :fight)]

        ;; Check if player is staggered
        (if (gs/set-thing-flag? gs winner :staggered)
          (-> gs
              (gs/unset-thing-flag winner :staggered)
              (utils/tell "You are still recovering from that last blow, so your attack is ineffective."))

          ;; Normal attack
          (let [att (max 1 (health/fight-strength gs))
                def (combat/villain-strength gs villain-entry prsi)
                villain-weapon (combat/find-weapon gs prso)
                defender-name (combat/get-thing-name gs prso)
                weapon-name (combat/get-thing-name gs prsi)]

            (cond
              ;; Villain has 0 defense (somehow already dead?)
              (zero? def)
              (utils/tell gs (str "Attacking the " defender-name " is pointless."))

              ;; Villain is unconscious or unarmed - instant kill
              (or (nil? villain-weapon) (neg? def))
              (let [status (if (neg? def) "unconscious" "unarmed")]
                (-> gs
                    (utils/tell (str "The " status " " defender-name " cannot defend himself: He dies."))
                    (villain-result prso 0 combat/killed)))

              ;; Normal combat - roll for result
              :else
              (let [[table _] (combat/select-result-table def att)
                    result (combat/roll-combat-result table)
                    ;; Apply STAGGER -> LOSE-WEAPON with 25% probability
                    result (combat/apply-out-modifier result (some? villain-weapon) false)
                    ;; Get message
                    message (combat/select-combat-message combat/hero-melee result defender-name weapon-name)
                    ;; Calculate new defense based on result
                    new-def (cond
                              (or (= result combat/missed) (= result combat/hesitate))
                              def

                              (= result combat/unconscious)
                              (- def) ; Negate to indicate unconscious

                              (or (= result combat/killed) (= result combat/sitting-duck))
                              0

                              (= result combat/light-wound)
                              (max 0 (- def 1))

                              (= result combat/serious-wound)
                              (max 0 (- def 2))

                              :else def)]
                (-> gs
                    (utils/tell message)
                    ;; Set staggered flag if result was stagger
                    (cond-> (= result combat/stagger)
                      (gs/set-thing-flag prso :staggered))
                    ;; Handle weapon loss
                    (cond-> (= result combat/lose-weapon)
                      (-> (gs/unset-thing-flag villain-weapon :ndesc)
                          (combat/drop-weapon prso villain-weapon)
                          (utils/this-is-it villain-weapon)))
                    ;; Apply damage
                    (villain-result prso new-def result))))))))))

;;; ---------------------------------------------------------------------------
;;; VILLAIN-RESULT - Apply combat result to villain
;;; ---------------------------------------------------------------------------
;;; ZIL: VILLAIN-RESULT (1actions.zil line 3579)

(defn villain-result
  "Apply combat result to a villain.

   ZIL: VILLAIN-RESULT (1actions.zil line 3579-3593)

   Updates villain's strength and handles death/unconsciousness.
   When killed, removes villain from play and calls their F-DEAD action.
   When unconscious, calls their F-UNCONSCIOUS action."
  [game-state villain-id new-strength result]
  (let [gs (assoc-in game-state [:objects villain-id :strength] new-strength)]
    (cond
      ;; Villain killed (strength = 0)
      (zero? new-strength)
      (-> gs
          (gs/unset-thing-flag villain-id :fight)
          (utils/tell (str "\nAlmost as soon as the " (combat/get-thing-name gs villain-id)
                           " breathes his last breath, a cloud of sinister black fog "
                           "envelops him, and when the fog lifts, the carcass has disappeared."))
          ;; Move villain to limbo
          (assoc-in [:objects villain-id :in] :limbo)
          ;; Call villain's F-DEAD action (will be handled by troll action handler)
          (call-villain-action villain-id :f-dead))

      ;; Villain unconscious (negative strength)
      (= result combat/unconscious)
      (-> gs
          (call-villain-action villain-id :f-unconscious))

      ;; Otherwise just return state with updated strength
      :else gs)))

;;; ---------------------------------------------------------------------------
;;; VILLAIN ACTION DISPATCH
;;; ---------------------------------------------------------------------------

(defn call-villain-action
  "Call a villain's action handler with a specific mode.

   mode is one of: :f-dead, :f-unconscious, :f-conscious, :f-busy?, :f-first?"
  [game-state villain-id mode]
  (let [action-fn (get-in game-state [:objects villain-id :action])]
    (if action-fn
      (action-fn game-state mode)
      game-state)))

;;; ---------------------------------------------------------------------------
;;; V-ATTACK - Main attack verb handler
;;; ---------------------------------------------------------------------------
;;; ZIL: V-ATTACK (gverbs.zil line 192)

(defn v-attack
  "Attack a creature with a weapon.

   ZIL: V-ATTACK (gverbs.zil line 192-209)

   Validates:
   1. Target has ACTORBIT (can be fought)
   2. Player has specified a weapon (or has one in hand)
   3. Player is holding the weapon
   4. The weapon has WEAPONBIT

   If all checks pass, calls HERO-BLOW to execute the attack."
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        prsi (parser-state/get-prsi game-state)
        winner (:winner game-state)]
    (cond
      ;; Not an actor - can't fight it
      (not (gs/set-thing-flag? game-state prso :actor))
      (utils/tell game-state (str "I've known strange people, but fighting a "
                                  (combat/get-thing-name game-state prso) "?"))

      ;; No weapon specified or bare hands
      (or (nil? prsi) (= prsi :hands))
      (utils/tell game-state (str "Trying to attack a "
                                  (combat/get-thing-name game-state prso)
                                  " with your bare hands is suicidal."))

      ;; Not holding the weapon
      (not (= (gs/get-thing-loc-id game-state prsi) winner))
      (utils/tell game-state (str "You aren't even holding the "
                                  (combat/get-thing-name game-state prsi) "."))

      ;; Weapon doesn't have WEAPONBIT
      (not (gs/set-thing-flag? game-state prsi :weapon))
      (utils/tell game-state (str "Trying to attack the "
                                  (combat/get-thing-name game-state prso)
                                  " with a " (combat/get-thing-name game-state prsi)
                                  " is suicidal."))

      ;; All checks pass - attack!
      :else
      (hero-blow game-state))))

;;; ---------------------------------------------------------------------------
;;; V-STAB - Stab without weapon
;;; ---------------------------------------------------------------------------

(defn v-stab
  "Stab something (without specifying weapon).

   ZIL: V-STAB (1actions.zil)
   If player has a weapon, uses it. Otherwise, bare hands message."
  [game-state]
  (let [prso (parser-state/get-prso game-state)
        winner (:winner game-state)
        weapon (combat/find-weapon game-state winner)]
    (if weapon
      ;; Set prsi to the weapon and call v-attack
      (-> game-state
          (parser-state/set-prsi weapon)
          v-attack)
      ;; No weapon
      (utils/tell game-state (str "Trying to attack a "
                                  (combat/get-thing-name game-state prso)
                                  " with your bare hands is suicidal.")))))
