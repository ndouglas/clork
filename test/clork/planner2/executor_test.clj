(ns clork.planner2.executor-test
  "Tests for the unified executor."
  (:require [clojure.test :refer :all]
            [clork.planner2.executor :as executor]
            [clork.planner2.schedule :as schedule]
            [clork.planner2.goals :as goals]
            [clork.core :as core]))

;;; ---------------------------------------------------------------------------
;;; EXECUTION STATE TESTS
;;; ---------------------------------------------------------------------------

(deftest make-execution-state-test
  (testing "Initial execution state"
    (let [gs (core/init-game)
          state (executor/make-execution-state gs [:egg])]
      (is (map? state))
      (is (= gs (:game-state state)))
      (is (= [:egg] (:treasures state)))
      (is (seq (:schedule state)))
      (is (= 0 (:schedule-index state)))
      (is (= :running (:status state)))
      (is (= 0 (:turn-count state)))
      (is (= 0 (:deaths state))))))

(deftest current-schedule-entry-test
  (testing "Get current schedule entry"
    (let [gs (core/init-game)
          state (executor/make-execution-state gs [:egg])]
      (is (some? (executor/current-schedule-entry state))))))

(deftest advance-schedule-test
  (testing "Advance schedule index"
    (let [gs (core/init-game)
          state (-> (executor/make-execution-state gs [:egg])
                    executor/advance-schedule)]
      (is (= 1 (:schedule-index state))))))

(deftest schedule-complete-test
  (testing "Schedule not complete at start"
    (let [gs (core/init-game)
          state (executor/make-execution-state gs [:egg])]
      (is (not (executor/schedule-complete? state)))))

  (testing "Schedule complete when index >= count"
    (let [gs (core/init-game)
          state (executor/make-execution-state gs [:egg])
          ;; Force index past end
          state (assoc state :schedule-index (count (:schedule state)))]
      (is (executor/schedule-complete? state)))))

;;; ---------------------------------------------------------------------------
;;; GOAL CONVERSION TESTS
;;; ---------------------------------------------------------------------------

(deftest schedule-entry->goal-test
  (testing "Move entry -> at-room goal"
    (let [entry {:type :move :to :kitchen}
          goal (executor/schedule-entry->goal entry)]
      (is (= :at-room (:type goal)))
      (is (= :kitchen (:room goal)))))

  (testing "Prep entry -> flag-set goal"
    (let [entry {:type :prep :id :troll-flag}
          goal (executor/schedule-entry->goal entry)]
      (is (= :flag-set (:type goal)))
      (is (= :troll-flag (:flag goal)))))

  (testing "Collect entry -> have-item goal"
    (let [entry {:type :collect :treasure :egg}
          goal (executor/schedule-entry->goal entry)]
      (is (= :have-item (:type goal)))
      (is (= :egg (:item goal)))))

  (testing "Deposit-all entry -> all-treasures-deposited goal"
    (let [entry {:type :deposit-all}
          goal (executor/schedule-entry->goal entry)]
      (is (= :all-treasures-deposited (:type goal))))))

(deftest entry-requires-combat-test
  (testing "Troll prep requires combat"
    (let [entry {:type :prep :id :troll-flag}]
      (is (executor/entry-requires-combat? entry))))

  (testing "Dome prep does not require combat"
    (let [entry {:type :prep :id :dome-flag}]
      (is (not (executor/entry-requires-combat? entry))))))

;;; ---------------------------------------------------------------------------
;;; DEATH DETECTION TESTS
;;; ---------------------------------------------------------------------------

(deftest detect-death-test
  (testing "No death in fresh game"
    (let [gs (core/init-game)
          state (executor/make-execution-state gs [:egg])]
      (is (not (executor/detect-death state))))))

;;; ---------------------------------------------------------------------------
;;; REPLAN TESTS
;;; ---------------------------------------------------------------------------

(deftest replan-from-current-test
  (testing "Replanning creates new schedule"
    (let [gs (core/init-game)
          state (executor/make-execution-state gs [:egg :painting])
          ;; Mark egg as deposited
          state (update state :deposited-treasures conj :egg)
          replanned (executor/replan-from-current state)]
      ;; Should have new schedule
      (is (seq (:schedule replanned)))
      ;; Schedule index reset
      (is (= 0 (:schedule-index replanned)))
      ;; Replan counter incremented
      (is (= 1 (:replans replanned)))
      ;; Status is running
      (is (= :running (:status replanned))))))

;;; ---------------------------------------------------------------------------
;;; RESULTS ANALYSIS TESTS
;;; ---------------------------------------------------------------------------

(deftest analyze-result-test
  (testing "Analyze a result"
    (let [result {:status :complete
                  :turn-count 50
                  :deaths 0
                  :replans 1
                  :treasures [:egg :painting]
                  :deposited-treasures #{:egg :painting}
                  :collected-treasures #{}}
          analysis (executor/analyze-result result)]
      (is (= :complete (:status analysis)))
      (is (= 50 (:turns analysis)))
      (is (= 0 (:deaths analysis)))
      (is (= 1 (:replans analysis)))
      (is (= 2 (:treasures-deposited analysis)))
      (is (= 1 (:completion-rate analysis))))))

;;; ---------------------------------------------------------------------------
;;; INTEGRATION TESTS (longer running)
;;; ---------------------------------------------------------------------------

;; Note: These are commented out by default as they take longer to run.
;; Uncomment to test full execution.

#_(deftest run-easy-treasures-smoke-test
    (testing "Run easy treasures (smoke test)"
      (let [gs (core/init-game)
            ;; Just test that it doesn't crash immediately
            result (executor/run-speedrun gs [:egg]
                                          :max-turns 50
                                          :max-deaths 1)]
        (is (map? result))
        (is (contains? #{:complete :timeout :failed :stuck} (:status result))))))
