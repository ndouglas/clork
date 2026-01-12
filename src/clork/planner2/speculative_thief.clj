(ns clork.planner2.speculative-thief
  "Speculative execution for thief encounters.

   The thief has complex RNG-dependent behavior:
   - Movement: Cycles through valid rooms each turn
   - Appearance: 30% to appear dramatically when in same room
   - Theft: 75% per valuable item, 10% per junk item
   - Combat: Same RNG-based system as troll
   - Retreat: If losing, may flee (except in treasure room)

   This module enables:
   1. Predicting thief location N turns ahead
   2. Predicting if/when theft will occur
   3. Finding action sequences that avoid thief or guarantee victory
   4. Timing thief encounters for optimal RNG state"
  (:require [clork.random :as rng]
            [clork.planner2.thief :as thief]
            [clork.planner2.combat :as combat]
            [clork.planner2.speculative :as spec]
            [clork.game-state :as gs]
            [clork.thief :as thief-impl]))

;;; ---------------------------------------------------------------------------
;;; THIEF LOCATION PREDICTION
;;; ---------------------------------------------------------------------------

(defn predict-thief-location
  "Predict where the thief will be after N turns.
   Uses the thief's room-cycling algorithm.

   Returns {:location room-id :turns-to-reach N}."
  [game-state turns-ahead]
  (if (thief/thief-dead? game-state)
    {:location nil :dead true}
    (let [current-loc (thief/thief-location game-state)]
      (loop [loc current-loc
             turns 0]
        (if (>= turns turns-ahead)
          {:location loc :turns-from-now turns}
          (let [next-loc (thief-impl/find-next-room game-state loc)]
            (if next-loc
              (recur next-loc (inc turns))
              {:location loc :turns-from-now turns :stuck true})))))))

(defn turns-until-thief-at
  "Calculate how many turns until thief reaches a specific room.
   Returns nil if thief won't reach room within max-turns."
  [game-state target-room max-turns]
  (if (thief/thief-dead? game-state)
    nil
    (let [current-loc (thief/thief-location game-state)]
      (loop [loc current-loc
             turns 0]
        (cond
          (= loc target-room) turns
          (>= turns max-turns) nil
          :else
          (let [next-loc (thief-impl/find-next-room game-state loc)]
            (if next-loc
              (recur next-loc (inc turns))
              nil)))))))

(defn thief-path
  "Get the sequence of rooms the thief will visit over N turns.
   Returns vector of room-ids."
  [game-state turns-ahead]
  (if (thief/thief-dead? game-state)
    []
    (let [current-loc (thief/thief-location game-state)]
      (loop [loc current-loc
             turns 0
             path [current-loc]]
        (if (>= turns turns-ahead)
          path
          (let [next-loc (thief-impl/find-next-room game-state loc)]
            (if next-loc
              (recur next-loc (inc turns) (conj path next-loc))
              path)))))))

;;; ---------------------------------------------------------------------------
;;; THIEF ENCOUNTER PREDICTION
;;; ---------------------------------------------------------------------------

(defrecord ThiefEncounterResult
  [encounter-type   ; :none, :appears, :steals, :combat, :retreats
   thief-location   ; Where thief is
   items-stolen     ; Items that would be stolen (if any)
   combat-outcome   ; :win/:death/:timeout (if combat)
   rng-consumed])   ; How many RNG calls were made

(defn simulate-thief-turn
  "Simulate a single thief daemon turn.
   Returns [new-game-state encounter-result]."
  [game-state]
  (let [rng-before (rng/save-state)
        ;; Run the thief daemon
        new-gs (thief-impl/i-thief game-state)
        rng-after (rng/save-state)
        rng-consumed (- (:call-count rng-after) (:call-count rng-before))
        ;; Detect what happened
        thief-was-here? (:thief-here game-state)
        thief-now-here? (:thief-here new-gs)
        thief-visible? (not (gs/set-thing-flag? new-gs :thief :invisible))
        fighting? (gs/set-thing-flag? new-gs :thief :fight)
        ;; Check for theft
        theft (thief/detect-theft game-state new-gs)
        encounter-type (cond
                         theft :steals
                         (and (not thief-was-here?) thief-now-here? thief-visible?) :appears
                         fighting? :combat
                         :else :none)]
    [new-gs
     (->ThiefEncounterResult
      encounter-type
      (thief/thief-location new-gs)
      (:stolen theft)
      nil
      rng-consumed)]))

(defn speculative-thief-turn
  "Simulate thief turn without affecting real RNG state.
   Returns ThiefEncounterResult."
  [game-state]
  (let [initial-state (rng/save-state)
        [_ result] (simulate-thief-turn game-state)]
    (rng/restore-state! initial-state)
    result))

;;; ---------------------------------------------------------------------------
;;; THIEF ENCOUNTER AVOIDANCE
;;; ---------------------------------------------------------------------------

(defn will-thief-appear?
  "Check if thief will appear dramatically this turn.
   Uses speculative execution."
  [game-state]
  (let [result (speculative-thief-turn game-state)]
    (= :appears (:encounter-type result))))

(defn will-thief-steal?
  "Check if thief will steal from player this turn.
   Uses speculative execution."
  [game-state]
  (let [result (speculative-thief-turn game-state)]
    (seq (:items-stolen result))))

(defn find-safe-burn-sequence
  "Find an action sequence where thief doesn't appear/steal.

   Returns {:safe? bool, :burn-actions [...], :outcome ThiefEncounterResult}
   or nil if no safe sequence found within max-burns."
  [game-state execute-fn & {:keys [max-burns] :or {max-burns 10}}]
  (let [initial-rng (rng/save-state)]
    ;; First check: is current state safe?
    (let [result (speculative-thief-turn game-state)]
      (if (= :none (:encounter-type result))
        {:safe? true :burn-actions [] :outcome result}

        ;; Try burn sequences
        (loop [sequences (spec/simple-burn-sequences max-burns)]
          (if (empty? sequences)
            nil  ; No safe sequence found

            (let [burn-seq (first sequences)
                  ;; Execute burn sequence speculatively
                  [final-gs _] (rng/with-speculative []
                                 (reduce (fn [gs cmd]
                                           (execute-fn gs cmd))
                                         game-state
                                         burn-seq))
                  ;; Check thief behavior
                  result (speculative-thief-turn final-gs)]

              (if (= :none (:encounter-type result))
                {:safe? true :burn-actions burn-seq :outcome result}
                (recur (rest sequences))))))))))

;;; ---------------------------------------------------------------------------
;;; THIEF COMBAT PLANNING
;;; ---------------------------------------------------------------------------

(defn speculative-thief-combat
  "Simulate combat with the thief.
   Returns {:outcome :win/:death/:timeout, :turns N}."
  [game-state]
  (spec/speculative-combat game-state :thief))

(defn find-winning-thief-plan
  "Find an action sequence that guarantees victory against the thief.

   Like find-winning-combat-plan but for thief specifically.
   Considers that thief may retreat if losing (except in treasure room)."
  [game-state execute-fn & {:keys [max-burn-actions] :or {max-burn-actions 10}}]
  (spec/find-winning-combat-plan game-state :thief execute-fn
                                  :max-burn-actions max-burn-actions))

(defn analyze-thief-timing
  "Analyze thief combat outcomes across different RNG states.
   Returns statistics on win rate, best timing, etc."
  [game-state execute-fn num-samples]
  (spec/analyze-combat-timing game-state :thief execute-fn num-samples))

;;; ---------------------------------------------------------------------------
;;; COMPREHENSIVE THIEF PREDICTION
;;; ---------------------------------------------------------------------------

(defrecord ThiefPrediction
  [turns-ahead       ; How many turns we're predicting
   thief-locations   ; Vector of predicted locations
   encounter-turns   ; Turns where we'd encounter thief on our route
   theft-risk        ; :none, :low, :medium, :high
   recommended-action ; :proceed, :wait, :avoid, :engage
   safe-burn-seq     ; Action sequence to avoid encounter (if any)
   combat-plan])     ; Winning combat plan (if engagement recommended)

(defn predict-thief-interactions
  "Comprehensive prediction of thief behavior for a planned route.

   Parameters:
   - game-state: Current game state
   - planned-route: Vector of room-ids we plan to visit
   - execute-fn: Function (gs cmd) -> gs for executing commands

   Returns ThiefPrediction with recommendations."
  [game-state planned-route execute-fn]
  (let [route-len (count planned-route)
        ;; Get thief path
        thief-path-vec (thief-path game-state route-len)
        ;; Find where paths intersect
        route-set (set planned-route)
        thief-set (set thief-path-vec)
        intersection (clojure.set/intersection route-set thief-set)
        ;; Check for direct encounters (same room at same turn)
        encounter-turns (for [turn (range route-len)
                              :let [our-room (nth planned-route turn nil)
                                    thief-room (nth thief-path-vec turn nil)]
                              :when (and our-room thief-room (= our-room thief-room))]
                          turn)
        ;; Assess risk
        has-treasures? (some (fn [item-id]
                               (pos? (get-in game-state [:objects item-id :tvalue] 0)))
                             (gs/get-contents game-state (:winner game-state)))
        theft-risk (cond
                     (thief/thief-dead? game-state) :none
                     (and (seq encounter-turns) has-treasures?) :high
                     (seq encounter-turns) :medium
                     (seq intersection) :low
                     :else :none)
        ;; Determine recommendation
        avoid-info (thief/should-avoid-thief? game-state)
        recommended-action (cond
                             (thief/thief-dead? game-state) :proceed
                             (= theft-risk :none) :proceed
                             (:avoid? avoid-info) :avoid
                             :else :engage)
        ;; Find safe burn sequence if avoidance recommended
        safe-burn (when (= recommended-action :avoid)
                    (find-safe-burn-sequence game-state execute-fn :max-burns 5))
        ;; Find combat plan if engagement recommended
        combat-plan (when (= recommended-action :engage)
                      (find-winning-thief-plan game-state execute-fn :max-burn-actions 5))]

    (->ThiefPrediction
     route-len
     thief-path-vec
     (vec encounter-turns)
     theft-risk
     (if (and (= recommended-action :avoid) (nil? safe-burn))
       :wait  ; Can't find safe burn, recommend waiting
       recommended-action)
     (:burn-actions safe-burn)
     combat-plan)))

;;; ---------------------------------------------------------------------------
;;; THIEF-AWARE ROUTING
;;; ---------------------------------------------------------------------------

(defn route-avoids-thief?
  "Check if a route avoids the thief for the duration of travel."
  [game-state route]
  (let [route-len (count route)
        thief-path-vec (thief-path game-state route-len)]
    (not-any? (fn [turn]
                (let [our-room (nth route turn nil)
                      thief-room (nth thief-path-vec turn nil)]
                  (and our-room thief-room (= our-room thief-room))))
              (range route-len))))

(defn optimal-departure-time
  "Find the optimal number of turns to wait before starting a route
   to avoid thief encounters.

   Returns {:wait-turns N, :safe? bool} or nil if no safe time found."
  [game-state route max-wait]
  (loop [wait 0]
    (if (> wait max-wait)
      nil
      (let [;; Simulate thief moving for 'wait' turns
            future-thief-loc (predict-thief-location game-state wait)
            ;; Create hypothetical state with thief at predicted location
            future-gs (assoc-in game-state [:objects :thief :in]
                                (:location future-thief-loc))
            ;; Check if route is safe from this state
            safe? (route-avoids-thief? future-gs route)]
        (if safe?
          {:wait-turns wait :safe? true}
          (recur (inc wait)))))))

;;; ---------------------------------------------------------------------------
;;; FORMAT OUTPUT
;;; ---------------------------------------------------------------------------

(defn format-thief-prediction
  "Format a ThiefPrediction for display."
  [pred]
  (str "=== Thief Prediction ===\n"
       "Looking ahead: " (:turns-ahead pred) " turns\n"
       "Thief path: " (vec (take 5 (:thief-locations pred)))
       (when (> (count (:thief-locations pred)) 5) "...") "\n"
       "Encounter turns: " (if (seq (:encounter-turns pred))
                             (vec (:encounter-turns pred))
                             "none") "\n"
       "Theft risk: " (name (:theft-risk pred)) "\n"
       "Recommended: " (name (:recommended-action pred)) "\n"
       (when (:safe-burn-seq pred)
         (str "Safe burn sequence: " (pr-str (:safe-burn-seq pred)) "\n"))
       (when (:combat-plan pred)
         (str "Combat plan available: "
              (count (:pre-actions (:combat-plan pred))) " pre-actions\n"))))

(defn print-thief-prediction
  "Print a thief prediction."
  [pred]
  (println (format-thief-prediction pred)))

