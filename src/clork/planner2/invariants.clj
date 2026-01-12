(ns clork.planner2.invariants
  "State invariant checking for the speedrun planner.

   Provides pre-condition verification before executing goals:
   - Item possession checks
   - Light availability in dark areas
   - Enemy presence/status verification
   - Object location validation
   - Flag state verification

   Used to catch state inconsistencies before they cause execution failures."
  (:require [clork.planner2.observe :as obs]
            [clork.game-state :as gs]
            [clork.combat :as combat-engine]
            [clork.verbs-health :as health]))

;;; ---------------------------------------------------------------------------
;;; INVARIANT RESULT TYPES
;;; ---------------------------------------------------------------------------

(defrecord InvariantResult
  [passed?       ; Boolean - all invariants passed
   invariants    ; Vector of {:name :passed? :message :severity}
   critical-failures ; Vector of critical failures
   warnings])    ; Vector of warnings

(defn make-result
  "Create an invariant check result."
  [invariants]
  (let [failures (filter #(not (:passed? %)) invariants)
        critical (filter #(= :critical (:severity %)) failures)
        warnings (filter #(= :warning (:severity %)) failures)]
    (->InvariantResult
     (empty? critical)
     invariants
     (vec critical)
     (vec warnings))))

;;; ---------------------------------------------------------------------------
;;; INDIVIDUAL INVARIANT CHECKS
;;; ---------------------------------------------------------------------------

(defn check-has-item
  "Check that player has a specific item."
  [game-state item-id & {:keys [severity] :or {severity :critical}}]
  (let [has? (obs/has-item? game-state item-id)]
    {:name (str "has-" (name item-id))
     :passed? has?
     :severity severity
     :message (if has?
                (str "Player has " (name item-id))
                (str "Player missing " (name item-id)))}))

(defn check-has-light
  "Check that player has light source active."
  [game-state & {:keys [severity] :or {severity :critical}}]
  (let [has-light? (obs/lantern-on? game-state)
        room-lit? (obs/lit? game-state)
        ok? (or has-light? room-lit?)]
    {:name "has-light"
     :passed? ok?
     :severity severity
     :message (cond
                has-light? "Lantern is on"
                room-lit? "Room is naturally lit"
                :else "No light source available")}))

(defn check-at-room
  "Check that player is at a specific room."
  [game-state room-id & {:keys [severity] :or {severity :critical}}]
  (let [here (:here game-state)
        at? (= here room-id)]
    {:name (str "at-" (name room-id))
     :passed? at?
     :severity severity
     :message (if at?
                (str "At " (name room-id))
                (str "Expected " (name room-id) ", actually at " (name here)))}))

(defn check-enemy-present
  "Check that an enemy is present and alive."
  [game-state enemy-id & {:keys [severity] :or {severity :critical}}]
  (let [here (:here game-state)
        enemy-loc (get-in game-state [:objects enemy-id :in])
        enemy-strength (get-in game-state [:objects enemy-id :strength] 0)
        present? (= enemy-loc here)
        alive? (or (nil? enemy-strength) (pos? enemy-strength))]
    {:name (str "enemy-" (name enemy-id) "-present")
     :passed? (and present? alive?)
     :severity severity
     :message (cond
                (not present?) (str (name enemy-id) " not in current room")
                (not alive?) (str (name enemy-id) " is already dead")
                :else (str (name enemy-id) " present and alive"))}))

(defn check-enemy-dead
  "Check that an enemy is dead (for post-combat verification)."
  [game-state enemy-id & {:keys [severity] :or {severity :critical}}]
  (let [enemy-loc (get-in game-state [:objects enemy-id :in])
        enemy-strength (get-in game-state [:objects enemy-id :strength] 0)
        dead? (or (= enemy-loc :limbo) (and (some? enemy-strength) (zero? enemy-strength)))]
    {:name (str "enemy-" (name enemy-id) "-dead")
     :passed? dead?
     :severity severity
     :message (if dead?
                (str (name enemy-id) " is dead")
                (str (name enemy-id) " is still alive"))}))

(defn check-flag-set
  "Check that a flag is set."
  [game-state entity-type entity-id flag & {:keys [severity] :or {severity :critical}}]
  (let [set? (gs/set-thing-flag? game-state entity-id flag)]
    {:name (str (name flag) "-set")
     :passed? set?
     :severity severity
     :message (if set?
                (str (name flag) " is set on " (name entity-id))
                (str (name flag) " not set on " (name entity-id)))}))

(defn check-flag-clear
  "Check that a flag is clear."
  [game-state entity-type entity-id flag & {:keys [severity] :or {severity :critical}}]
  (let [clear? (not (gs/set-thing-flag? game-state entity-id flag))]
    {:name (str (name flag) "-clear")
     :passed? clear?
     :severity severity
     :message (if clear?
                (str (name flag) " is clear on " (name entity-id))
                (str (name flag) " is set on " (name entity-id) " (expected clear)"))}))

(defn check-object-at
  "Check that an object is at a specific location."
  [game-state obj-id location & {:keys [severity] :or {severity :critical}}]
  (let [actual-loc (get-in game-state [:objects obj-id :in])
        at? (= actual-loc location)]
    {:name (str (name obj-id) "-at-" (name location))
     :passed? at?
     :severity severity
     :message (if at?
                (str (name obj-id) " is at " (name location))
                (str (name obj-id) " expected at " (name location)
                     ", actually at " (if actual-loc (name actual-loc) "nil")))}))

(defn check-player-alive
  "Check that player is alive."
  [game-state & {:keys [severity] :or {severity :critical}}]
  (let [alive? (obs/player-alive? game-state)]
    {:name "player-alive"
     :passed? alive?
     :severity severity
     :message (if alive? "Player is alive" "Player is DEAD")}))

(defn check-combat-ready
  "Check that player is ready for combat (has weapon and light)."
  [game-state enemy-id & {:keys [severity] :or {severity :critical}}]
  (let [winner (:winner game-state)
        weapon (combat-engine/find-weapon game-state winner)
        has-light? (or (obs/lit? game-state) (obs/lantern-on? game-state))
        ready? (and weapon has-light?)]
    {:name "combat-ready"
     :passed? ready?
     :severity severity
     :message (cond
                (not weapon) "No weapon equipped"
                (not has-light?) "No light for combat"
                :else "Ready for combat")}))

(defn check-player-strength
  "Check that player has minimum strength for combat."
  [game-state min-strength & {:keys [severity] :or {severity :warning}}]
  (let [strength (health/fight-strength game-state false)
        ok? (>= strength min-strength)]
    {:name (str "strength>=" min-strength)
     :passed? ok?
     :severity severity
     :message (str "Player strength: " strength
                   (if ok? " (sufficient)" (str " (need " min-strength ")")))}))

;;; ---------------------------------------------------------------------------
;;; GOAL-SPECIFIC INVARIANT SETS
;;; ---------------------------------------------------------------------------

(defn invariants-for-navigation
  "Invariants needed before navigating to a room."
  [game-state target-room]
  [(check-player-alive game-state)
   (check-has-light game-state :severity :warning)])

(defn invariants-for-combat
  "Invariants needed before combat."
  [game-state enemy-id enemy-room]
  [(check-player-alive game-state)
   (check-at-room game-state enemy-room)
   (check-combat-ready game-state enemy-id)
   (check-enemy-present game-state enemy-id)
   (check-player-strength game-state 3 :severity :warning)])

(defn invariants-for-take
  "Invariants needed before taking an object."
  [game-state obj-id obj-room]
  [(check-player-alive game-state)
   (check-at-room game-state obj-room)
   (check-has-light game-state)])

(defn invariants-for-deposit
  "Invariants needed before depositing a treasure."
  [game-state item-id]
  [(check-player-alive game-state)
   (check-at-room game-state :living-room)
   (check-has-item game-state item-id)
   (check-flag-set game-state :objects :trophy-case :open)])

;;; ---------------------------------------------------------------------------
;;; MAIN VERIFICATION FUNCTIONS
;;; ---------------------------------------------------------------------------

(defn verify-invariants
  "Run a set of invariants and return a result.

   Parameters:
   - game-state: Current game state
   - invariants: Vector of invariant check results

   Returns InvariantResult."
  [game-state invariants]
  (make-result invariants))

(defn verify-goal-preconditions
  "Verify preconditions for a goal.

   Parameters:
   - game-state: Current game state
   - goal: Goal map with :type and other keys

   Returns InvariantResult."
  [game-state goal]
  (let [invariants
        (case (:type goal)
          :at-room (invariants-for-navigation game-state (:room goal))
          :kill-enemy (invariants-for-combat game-state
                                             (:enemy goal)
                                             (or (:room goal) (:here game-state)))
          :have-item (let [item (:item goal)
                           item-loc (get-in game-state [:objects item :in])
                           winner (:winner game-state)
                           already-has? (or (= item-loc :adventurer)
                                            (= item-loc winner)
                                            (= item-loc :player))]
                       (if already-has?
                         ;; Already have item, just check alive
                         [(check-player-alive game-state)]
                         ;; Need to take it
                         (if (contains? (:rooms game-state) item-loc)
                           (invariants-for-take game-state item item-loc)
                           [(check-player-alive game-state)])))
          :item-deposited (invariants-for-deposit game-state (:item goal))
          :lantern-on [(check-player-alive game-state)
                       (check-has-item game-state :brass-lantern)]
          ;; Default: just check player is alive
          [(check-player-alive game-state)])]
    (verify-invariants game-state invariants)))

(defn check-state-consistency
  "Run a comprehensive state consistency check.
   Returns InvariantResult with all findings."
  [game-state]
  (let [winner (:winner game-state)
        here (:here game-state)
        invariants
        [(check-player-alive game-state)
         ;; Check dark room safety
         (let [room-dark? (not (obs/lit? game-state))
               has-light? (obs/lantern-on? game-state)]
           {:name "dark-room-safety"
            :passed? (or (not room-dark?) has-light?)
            :severity :warning
            :message (cond
                       (not room-dark?) "Room is lit"
                       has-light? "Have active light in dark room"
                       :else "In dark room without light!")})
         ;; Check weapon status
         (let [weapon (combat-engine/find-weapon game-state winner)]
           {:name "has-weapon"
            :passed? (some? weapon)
            :severity :warning
            :message (if weapon
                       (str "Armed with " (name weapon))
                       "No weapon equipped")})
         ;; Check lantern fuel (if carrying)
         (let [has-lantern? (obs/has-item? game-state :brass-lantern)
               lantern-power (get-in game-state [:objects :brass-lantern :power] 0)]
           {:name "lantern-fuel"
            :passed? (or (not has-lantern?) (> lantern-power 50))
            :severity :warning
            :message (if has-lantern?
                       (str "Lantern power: " lantern-power)
                       "No lantern")})]]
    (verify-invariants game-state invariants)))

;;; ---------------------------------------------------------------------------
;;; FORMATTED OUTPUT
;;; ---------------------------------------------------------------------------

(defn format-result
  "Format an InvariantResult for display."
  [result]
  (let [lines [(str "=== Invariant Check ===")
               (str "Overall: " (if (:passed? result) "PASSED" "FAILED"))
               ""
               "Checks:"]]
    (str (clojure.string/join
          "\n"
          (concat
           lines
           (for [inv (:invariants result)]
             (str "  " (if (:passed? inv) "[OK]" "[FAIL]")
                  " " (:name inv)
                  " - " (:message inv)
                  (when (and (not (:passed? inv))
                             (= :warning (:severity inv)))
                    " (warning)")))
           (when (seq (:critical-failures result))
             ["" "Critical Failures:"])
           (for [fail (:critical-failures result)]
             (str "  ! " (:name fail) ": " (:message fail)))))
         "\n")))

(defn print-result
  "Print an invariant result to stdout."
  [result]
  (println (format-result result)))
