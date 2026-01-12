(ns clork.planner2.schedule-test
  "Tests for the timed effect scheduler."
  (:require [clojure.test :refer :all]
            [clork.planner2.schedule :as schedule]
            [clork.core :as core]))

;;; ---------------------------------------------------------------------------
;;; TIMER STATE TESTS
;;; ---------------------------------------------------------------------------

(deftest make-timer-state-test
  (testing "Initial timer state"
    (let [state (schedule/make-timer-state)]
      (is (map? state))
      (is (= {} (:active-timers state)))
      (is (= {} (:consumables state)))
      (is (= 0 (:current-turn state))))))

(deftest start-timer-test
  (testing "Starting a timer"
    (let [state (-> (schedule/make-timer-state)
                    (schedule/start-timer :low-tide 8 :reservoir-drains))]
      (is (contains? (:active-timers state) :low-tide))
      (let [timer (get-in state [:active-timers :low-tide])]
        (is (= 0 (:started-turn timer)))
        (is (= 8 (:duration timer)))
        (is (= :reservoir-drains (:effect timer)))))))

(deftest advance-turn-test
  (testing "Advancing turn counter"
    (let [state (-> (schedule/make-timer-state)
                    schedule/advance-turn
                    schedule/advance-turn)]
      (is (= 2 (:current-turn state))))))

(deftest timer-remaining-test
  (testing "Timer remaining calculation"
    (let [state (-> (schedule/make-timer-state)
                    (schedule/start-timer :test 8 :effect)
                    schedule/advance-turn
                    schedule/advance-turn
                    schedule/advance-turn)]
      ;; Started at 0, now at turn 3, duration 8 -> 5 remaining
      (is (= 5 (schedule/timer-remaining state :test)))))

  (testing "Timer at zero"
    (let [state (-> (schedule/make-timer-state)
                    (schedule/start-timer :test 2 :effect)
                    schedule/advance-turn
                    schedule/advance-turn)]
      (is (= 0 (schedule/timer-remaining state :test))))))

(deftest timer-complete-test
  (testing "Timer not complete"
    (let [state (-> (schedule/make-timer-state)
                    (schedule/start-timer :test 8 :effect)
                    schedule/advance-turn)]
      (is (not (schedule/timer-complete? state :test)))))

  (testing "Timer complete"
    (let [state (-> (schedule/make-timer-state)
                    (schedule/start-timer :test 2 :effect)
                    schedule/advance-turn
                    schedule/advance-turn)]
      (is (schedule/timer-complete? state :test)))))

(deftest active-timers-test
  (testing "Active timers list"
    (let [state (-> (schedule/make-timer-state)
                    (schedule/start-timer :timer1 10 :e1)
                    (schedule/start-timer :timer2 2 :e2)
                    schedule/advance-turn
                    schedule/advance-turn)]
      ;; timer1 still active (8 remaining), timer2 complete (0 remaining)
      (let [active (set (schedule/active-timers state))]
        (is (contains? active :timer1))
        (is (not (contains? active :timer2)))))))

;;; ---------------------------------------------------------------------------
;;; CONSUMABLE TESTS
;;; ---------------------------------------------------------------------------

(deftest set-consumable-test
  (testing "Setting consumable"
    (let [state (-> (schedule/make-timer-state)
                    (schedule/set-consumable :candles 40))]
      (is (= 40 (schedule/consumable-remaining state :candles))))))

(deftest use-consumable-test
  (testing "Using consumable decrements"
    (let [state (-> (schedule/make-timer-state)
                    (schedule/set-consumable :candles 40)
                    (schedule/use-consumable :candles)
                    (schedule/use-consumable :candles))]
      (is (= 38 (schedule/consumable-remaining state :candles))))))

(deftest consumable-low-test
  (testing "Consumable not low"
    (let [state (-> (schedule/make-timer-state)
                    (schedule/set-consumable :candles 40))]
      (is (not (schedule/consumable-low? state :candles 40)))))

  (testing "Consumable is low (< 20%)"
    (let [state (-> (schedule/make-timer-state)
                    (schedule/set-consumable :candles 5))]
      (is (schedule/consumable-low? state :candles 40)))))

;;; ---------------------------------------------------------------------------
;;; DAM SCHEDULING TESTS
;;; ---------------------------------------------------------------------------

(deftest schedule-dam-drain-test
  (testing "Dam drain schedule structure"
    (let [gs (core/init-game)
          plan {:treasures [:bag-of-coins :platinum-bar :sapphire-bracelet]}
          dam-schedule (schedule/schedule-dam-drain gs plan)]
      (is (map? dam-schedule))
      (is (contains? dam-schedule :trigger-action))
      (is (contains? dam-schedule :wait-time))
      (is (= 8 (:wait-time dam-schedule)))
      (is (seq (:work-during-wait dam-schedule))))))

;;; ---------------------------------------------------------------------------
;;; EXORCISM SCHEDULING TESTS
;;; ---------------------------------------------------------------------------

(deftest schedule-exorcism-test
  (testing "Exorcism schedule structure"
    (let [ex (schedule/schedule-exorcism)]
      (is (map? ex))
      (is (contains? ex :phase-1))
      (is (contains? ex :phase-2))
      (is (contains? ex :phase-3))
      (is (false? (:interruptible? ex)))
      ;; Phase 1 has 6-turn window
      (is (= 6 (get-in ex [:phase-1 :window])))
      ;; Phase 2 has 3-turn window
      (is (= 3 (get-in ex [:phase-2 :window]))))))

;;; ---------------------------------------------------------------------------
;;; SCHEDULE GENERATION TESTS
;;; ---------------------------------------------------------------------------

(deftest generate-schedule-test
  (testing "Generate schedule for simple treasures"
    (let [gs (core/init-game)
          schedule (schedule/generate-schedule gs [:egg])]
      (is (seq schedule))
      ;; Should end with deposit-all
      (is (= :deposit-all (:type (last schedule))))))

  (testing "Generate schedule includes dam for dam-dependent treasures"
    (let [gs (core/init-game)
          schedule (schedule/generate-schedule gs [:sapphire-bracelet])]
      (is (seq schedule))
      ;; Should have parallel-work entry for dam
      (is (some #(= :parallel-work (:type %)) schedule)))))

;;; ---------------------------------------------------------------------------
;;; SCHEDULE ANALYSIS TESTS
;;; ---------------------------------------------------------------------------

(deftest schedule-turns-test
  (testing "Empty schedule has zero turns"
    (is (= 0 (schedule/schedule-turns []))))

  (testing "Schedule with entries has turns"
    (let [schedule [{:type :move :to :kitchen}
                    {:type :collect :treasure :egg}
                    {:type :deposit-all}]]
      (is (pos? (schedule/schedule-turns schedule))))))

;;; ---------------------------------------------------------------------------
;;; WARNING TESTS
;;; ---------------------------------------------------------------------------

(deftest check-consumable-warnings-test
  (testing "No warnings when consumables are OK"
    (let [state (-> (schedule/make-timer-state)
                    (schedule/set-consumable :candles 40))]
      (is (empty? (schedule/check-consumable-warnings state)))))

  (testing "Warning when candles low"
    (let [state (-> (schedule/make-timer-state)
                    (schedule/set-consumable :candles 5))]
      (let [warnings (schedule/check-consumable-warnings state)]
        (is (some #(= :candles (:item %)) warnings))))))

(deftest check-timer-warnings-test
  (testing "No warnings when timer has time"
    (let [state (-> (schedule/make-timer-state)
                    (schedule/start-timer :test 10 :effect))]
      (is (empty? (schedule/check-timer-warnings state)))))

  (testing "Warning when timer almost expired"
    (let [state (-> (schedule/make-timer-state)
                    (schedule/start-timer :test 10 :effect)
                    ;; Advance to turn 8, leaving 2 turns
                    (assoc :current-turn 8))]
      (let [warnings (schedule/check-timer-warnings state)]
        (is (some #(= :test (:timer %)) warnings))))))
