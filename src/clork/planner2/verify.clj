(ns clork.planner2.verify
  "Post-action verification for the speedrun planner.

   Provides functions to verify that executed actions had their expected effects:
   - Navigation: player is at expected room
   - Take: player has expected item
   - Open: container is open
   - Combat: enemy is dead
   - Turn on: lantern is lit

   Also detects unexpected side effects (theft, damage, etc.)."
  (:require [clork.planner2.observe :as obs]
            [clork.planner2.thief :as thief]
            [clork.game-state :as gs]))

;;; ---------------------------------------------------------------------------
;;; VERIFICATION RESULT
;;; ---------------------------------------------------------------------------

(defrecord VerifyResult
  [success?       ; Boolean - action had expected effect
   action         ; The action that was verified
   expected       ; What we expected
   actual         ; What actually happened
   side-effects]) ; Vector of detected side effects

(defn make-result
  "Create a verification result."
  [success? action expected actual side-effects]
  (->VerifyResult success? action expected actual (vec side-effects)))

(defn success
  "Create a successful verification result."
  [action expected]
  (make-result true action expected expected []))

(defn failure
  "Create a failed verification result."
  [action expected actual]
  (make-result false action expected actual []))

(defn with-side-effects
  "Add side effects to a result."
  [result side-effects]
  (update result :side-effects into side-effects))

;;; ---------------------------------------------------------------------------
;;; INDIVIDUAL ACTION VERIFICATION
;;; ---------------------------------------------------------------------------

(defn verify-move
  "Verify that a move action succeeded.
   Returns VerifyResult."
  [pre-state post-state expected-room]
  (let [actual-room (:here post-state)
        moved? (= actual-room expected-room)
        theft (thief/detect-theft pre-state post-state)]
    (cond-> (if moved?
              (success {:type :move :to expected-room} expected-room)
              (failure {:type :move :to expected-room} expected-room actual-room))
      theft (with-side-effects [{:type :theft :items (:stolen theft)}]))))

(defn verify-take
  "Verify that a take action succeeded.
   Returns VerifyResult."
  [pre-state post-state item-id]
  (let [winner (:winner post-state)
        has-item? (obs/has-item? post-state item-id)
        item-loc (get-in post-state [:objects item-id :in])
        ;; Check if thief stole something while we were taking
        theft (thief/detect-theft pre-state post-state)]
    (cond-> (if has-item?
              (success {:type :take :item item-id} item-id)
              (failure {:type :take :item item-id}
                       {:expected :player-inventory}
                       {:actual-location item-loc}))
      theft (with-side-effects [{:type :theft :items (:stolen theft)}]))))

(defn verify-drop
  "Verify that a drop action succeeded.
   Returns VerifyResult."
  [pre-state post-state item-id]
  (let [here (:here post-state)
        item-loc (get-in post-state [:objects item-id :in])
        dropped? (= item-loc here)]
    (if dropped?
      (success {:type :drop :item item-id} item-id)
      (failure {:type :drop :item item-id}
               {:expected here}
               {:actual item-loc}))))

(defn verify-open
  "Verify that an open action succeeded.
   Returns VerifyResult."
  [pre-state post-state container-id]
  (let [open? (gs/set-thing-flag? post-state container-id :open)]
    (if open?
      (success {:type :open :container container-id} :open)
      (failure {:type :open :container container-id}
               {:expected :open}
               {:actual :closed}))))

(defn verify-close
  "Verify that a close action succeeded.
   Returns VerifyResult."
  [pre-state post-state container-id]
  (let [closed? (not (gs/set-thing-flag? post-state container-id :open))]
    (if closed?
      (success {:type :close :container container-id} :closed)
      (failure {:type :close :container container-id}
               {:expected :closed}
               {:actual :open}))))

(defn verify-turn-on
  "Verify that a turn-on action succeeded.
   Returns VerifyResult."
  [pre-state post-state item-id]
  (let [on? (gs/set-thing-flag? post-state item-id :on)]
    (if on?
      (success {:type :turn-on :item item-id} :on)
      (failure {:type :turn-on :item item-id}
               {:expected :on}
               {:actual :off}))))

(defn verify-turn-off
  "Verify that a turn-off action succeeded.
   Returns VerifyResult."
  [pre-state post-state item-id]
  (let [off? (not (gs/set-thing-flag? post-state item-id :on))]
    (if off?
      (success {:type :turn-off :item item-id} :off)
      (failure {:type :turn-off :item item-id}
               {:expected :off}
               {:actual :on}))))

(defn verify-combat
  "Verify that combat resulted in enemy death.
   Returns VerifyResult."
  [pre-state post-state enemy-id]
  (let [enemy-loc (get-in post-state [:objects enemy-id :in])
        enemy-strength (get-in post-state [:objects enemy-id :strength] 0)
        enemy-dead? (or (= enemy-loc :limbo)
                        (and (some? enemy-strength)
                             (<= enemy-strength 0)))
        player-alive? (obs/player-alive? post-state)]
    (cond
      (not player-alive?)
      (failure {:type :combat :enemy enemy-id}
               {:expected :player-wins}
               {:actual :player-died})

      enemy-dead?
      (success {:type :combat :enemy enemy-id} :enemy-dead)

      :else
      (failure {:type :combat :enemy enemy-id}
               {:expected :enemy-dead}
               {:actual {:enemy-location enemy-loc
                         :enemy-strength enemy-strength}}))))

(defn verify-put-in
  "Verify that an item was put in a container.
   Returns VerifyResult."
  [pre-state post-state item-id container-id]
  (let [item-loc (get-in post-state [:objects item-id :in])
        in-container? (= item-loc container-id)]
    (if in-container?
      (success {:type :put :item item-id :container container-id} container-id)
      (failure {:type :put :item item-id :container container-id}
               {:expected container-id}
               {:actual item-loc}))))

;;; ---------------------------------------------------------------------------
;;; SIDE EFFECT DETECTION
;;; ---------------------------------------------------------------------------

(defn detect-side-effects
  "Detect unexpected changes between pre and post states.
   Returns vector of side effect maps."
  [pre-state post-state]
  (let [side-effects []
        ;; Check for theft
        theft (thief/detect-theft pre-state post-state)]
    (cond-> side-effects
      theft (conj {:type :theft
                   :items (:stolen theft)
                   :thief-location (:thief-location theft)})

      ;; Check if player took damage
      (let [pre-wounds (get-in pre-state [:objects (:winner pre-state) :strength] 0)
            post-wounds (get-in post-state [:objects (:winner post-state) :strength] 0)]
        (> post-wounds pre-wounds))
      (conj {:type :player-damaged
             :wounds-taken (- (get-in post-state [:objects (:winner post-state) :strength] 0)
                              (get-in pre-state [:objects (:winner pre-state) :strength] 0))})

      ;; Check if player died
      (and (obs/player-alive? pre-state)
           (not (obs/player-alive? post-state)))
      (conj {:type :player-died})

      ;; Check if lantern fuel decreased significantly
      (let [pre-fuel (get-in pre-state [:objects :brass-lantern :power] 0)
            post-fuel (get-in post-state [:objects :brass-lantern :power] 0)]
        (and (pos? pre-fuel)
             (< post-fuel (* pre-fuel 0.9))))  ; More than 10% drop
      (conj {:type :lantern-low
             :remaining (get-in post-state [:objects :brass-lantern :power] 0)}))))

;;; ---------------------------------------------------------------------------
;;; GENERIC ACTION VERIFICATION
;;; ---------------------------------------------------------------------------

(defn verify-action
  "Verify an action based on its type.

   action-spec is a map with:
   - :type - action type (:move, :take, :open, :combat, etc.)
   - Additional keys depending on type

   Returns VerifyResult."
  [pre-state post-state action-spec]
  (case (:type action-spec)
    :move (verify-move pre-state post-state (:to action-spec))
    :take (verify-take pre-state post-state (:item action-spec))
    :drop (verify-drop pre-state post-state (:item action-spec))
    :open (verify-open pre-state post-state (:container action-spec))
    :close (verify-close pre-state post-state (:container action-spec))
    :turn-on (verify-turn-on pre-state post-state (:item action-spec))
    :turn-off (verify-turn-off pre-state post-state (:item action-spec))
    :combat (verify-combat pre-state post-state (:enemy action-spec))
    :kill (verify-combat pre-state post-state (:enemy action-spec))
    :put (verify-put-in pre-state post-state (:item action-spec) (:container action-spec))
    ;; Default: can't verify, assume success
    (success action-spec :unknown)))

;;; ---------------------------------------------------------------------------
;;; FORMAT OUTPUT
;;; ---------------------------------------------------------------------------

(defn format-result
  "Format a VerifyResult for display."
  [result]
  (str (if (:success? result) "[OK] " "[FAIL] ")
       (pr-str (:action result))
       (when-not (:success? result)
         (str "\n  Expected: " (pr-str (:expected result))
              "\n  Actual: " (pr-str (:actual result))))
       (when (seq (:side-effects result))
         (str "\n  Side effects: " (pr-str (:side-effects result))))))

(defn print-result
  "Print a verification result."
  [result]
  (println (format-result result)))

;;; ---------------------------------------------------------------------------
;;; BATCH VERIFICATION
;;; ---------------------------------------------------------------------------

(defn verify-sequence
  "Verify a sequence of actions with their expected outcomes.

   Takes a vector of {:action action-spec :pre pre-state :post post-state}.
   Returns vector of VerifyResults."
  [action-records]
  (mapv (fn [{:keys [action pre post]}]
          (verify-action pre post action))
        action-records))

(defn all-succeeded?
  "Check if all verification results succeeded."
  [results]
  (every? :success? results))

(defn failures-only
  "Filter to only failed verifications."
  [results]
  (remove :success? results))

(defn summarize-results
  "Create a summary of verification results."
  [results]
  (let [total (count results)
        successes (count (filter :success? results))
        failures (count (remove :success? results))
        side-effects (mapcat :side-effects results)]
    {:total total
     :successes successes
     :failures failures
     :success-rate (if (pos? total) (double (/ successes total)) 0.0)
     :side-effects (vec (distinct side-effects))}))

(defn print-summary
  "Print verification summary."
  [results]
  (let [summary (summarize-results results)]
    (println "=== Verification Summary ===")
    (println "Total actions:" (:total summary))
    (println "Successes:" (:successes summary))
    (println "Failures:" (:failures summary))
    (println (format "Success rate: %.1f%%" (* 100 (:success-rate summary))))
    (when (seq (:side-effects summary))
      (println "Side effects detected:")
      (doseq [se (:side-effects summary)]
        (println "  -" (pr-str se))))))
