(ns clork.planner2.timing-test
  "Tests for timing validation module."
  (:require [clojure.test :refer :all]
            [clork.planner2.timing :as timing]
            [clork.core :as core]
            [clork.game-state :as gs]))

;;; ---------------------------------------------------------------------------
;;; CONSTANTS TESTS
;;; ---------------------------------------------------------------------------

(deftest timing-constants-test
  (testing "Timing constants are reasonable"
    (is (= 200 timing/lantern-max-turns))
    (is (= 40 timing/candles-max-turns))
    (is (= 8 timing/dam-drain-turns))
    (is (= 6 timing/exorcism-bell-window))
    (is (= 3 timing/exorcism-candles-window))))

;;; ---------------------------------------------------------------------------
;;; RESOURCE STATE TESTS
;;; ---------------------------------------------------------------------------

(deftest make-resource-state-test
  (testing "Creates initial resource state"
    (let [state (timing/make-resource-state)]
      (is (= 200 (:lantern-remaining state)))
      (is (= 40 (:candles-remaining state)))
      (is (false? (:candles-lit? state)))
      (is (false? (:lantern-on? state)))
      (is (nil? (:dam-timer state)))
      (is (= :none (:exorcism-state state))))))

(deftest observe-resource-state-test
  (testing "Observes state from game"
    (let [gs (core/init-game)
          state (timing/observe-resource-state gs)]
      (is (some? (:lantern-remaining state)))
      (is (some? (:candles-remaining state)))
      (is (boolean? (:lantern-on? state))))))

;;; ---------------------------------------------------------------------------
;;; EXORCISM VALIDATION TESTS
;;; ---------------------------------------------------------------------------

(deftest validate-exorcism-sequence-test
  (testing "Valid exorcism timing"
    (let [result (timing/validate-exorcism-sequence 10 2 1)]
      (is (:valid? result))
      (is (empty? (:errors result)))))

  (testing "Invalid candles timing"
    (let [result (timing/validate-exorcism-sequence 10 10 1)]  ; 10 > 6 turn window
      (is (not (:valid? result)))
      (is (some #(= :candles (:phase %)) (:errors result)))))

  (testing "Invalid book timing"
    (let [result (timing/validate-exorcism-sequence 10 2 5)]  ; 5 > 3 turn window
      (is (not (:valid? result)))
      (is (some #(= :book (:phase %)) (:errors result)))))

  (testing "Tight margins generate warnings"
    (let [result (timing/validate-exorcism-sequence 10 5 2)]  ; Close to limits
      (is (:valid? result))
      (is (seq (:warnings result))))))

;;; ---------------------------------------------------------------------------
;;; LANTERN VALIDATION TESTS
;;; ---------------------------------------------------------------------------

(deftest validate-lantern-usage-test
  (testing "Valid lantern usage"
    (let [result (timing/validate-lantern-usage 100 200)]
      (is (:valid? result))
      (is (empty? (:errors result)))))

  (testing "Excessive lantern usage"
    (let [result (timing/validate-lantern-usage 250 200)]
      (is (not (:valid? result)))
      (is (some #(= :lantern-exhaustion (:type %)) (:errors result)))))

  (testing "Low margin warns"
    (let [result (timing/validate-lantern-usage 190 200)]  ; Only 10 turns margin
      (is (:valid? result))
      (is (seq (:warnings result))))))

;;; ---------------------------------------------------------------------------
;;; CANDLE VALIDATION TESTS
;;; ---------------------------------------------------------------------------

(deftest validate-candle-usage-test
  (testing "Valid candle usage"
    (let [result (timing/validate-candle-usage 15 40)]
      (is (:valid? result))))

  (testing "Excessive candle usage"
    (let [result (timing/validate-candle-usage 50 40)]
      (is (not (:valid? result)))
      (is (some #(= :candles-exhaustion (:type %)) (:errors result))))))

;;; ---------------------------------------------------------------------------
;;; DAM VALIDATION TESTS
;;; ---------------------------------------------------------------------------

(deftest validate-dam-timing-test
  (testing "Valid dam timing"
    (let [result (timing/validate-dam-timing 10 [])]
      (is (:valid? result))))

  (testing "Too early dam access"
    (let [result (timing/validate-dam-timing 5 [])]  ; Only 5 turns, need 8
      (is (not (:valid? result)))
      (is (some #(= :dam-not-drained (:type %)) (:errors result)))))

  (testing "Exact timing warns"
    (let [result (timing/validate-dam-timing 8 [])]  ; Exactly 8 turns
      (is (:valid? result))
      (is (seq (:warnings result))))))

;;; ---------------------------------------------------------------------------
;;; PLAN TIMING ANALYSIS TESTS
;;; ---------------------------------------------------------------------------

(deftest analyze-plan-timing-test
  (testing "Analyzes simple plan"
    (let [segments [{:type :surface :turns 20}
                    {:type :underground :turns 50}]
          state (timing/make-resource-state)
          analysis (timing/analyze-plan-timing segments state)]
      (is (= 70 (:total-turns analysis)))
      (is (= 50 (:underground-turns analysis)))
      (is (:lantern-valid? analysis))))

  (testing "Detects lantern exhaustion"
    (let [segments [{:type :underground :turns 250}]  ; Too many
          state (timing/make-resource-state)
          analysis (timing/analyze-plan-timing segments state)]
      (is (not (:lantern-valid? analysis)))
      (is (seq (:errors analysis))))))

;;; ---------------------------------------------------------------------------
;;; EXECUTION WINDOW TESTS
;;; ---------------------------------------------------------------------------

(deftest exorcism-execution-window-test
  (testing "Calculates exorcism window"
    (let [state (timing/make-resource-state)
          window (timing/exorcism-execution-window state)]
      (is (number? (:start window)))
      (is (number? (:end window)))
      (is (number? (:optimal window)))))

  (testing "Insufficient candles returns error"
    (let [state (assoc (timing/make-resource-state) :candles-remaining 10)
          window (timing/exorcism-execution-window state)]
      (is (:error window)))))

(deftest dam-execution-window-test
  (testing "Calculates dam window with enough parallel work"
    (let [window (timing/dam-execution-window 10)]  ; 10 > 8 turns
      (is (= 0 (:open-gates-at window)))
      (is (= 0 (:wait-if-needed window)))
      (is (string? (:note window)))))

  (testing "Calculates wait time when not enough parallel work"
    (let [window (timing/dam-execution-window 5)]  ; 5 < 8 turns
      (is (= 3 (:wait-if-needed window)))  ; Need 3 extra turns
      )))

(deftest lantern-budget-test
  (testing "Calculates lantern budget"
    (let [state (timing/make-resource-state)
          budget (timing/lantern-budget state 100)]
      (is (= 200 (:remaining budget)))
      (is (= 180 (:available-for-plan budget)))
      (is (= 80 (:surplus budget)))
      (is (:valid? budget))))

  (testing "Detects insufficient lantern"
    (let [state (timing/make-resource-state)
          budget (timing/lantern-budget state 250)]
      (is (not (:valid? budget)))
      (is (neg? (:surplus budget))))))

;;; ---------------------------------------------------------------------------
;;; FORMAT OUTPUT TESTS
;;; ---------------------------------------------------------------------------

(deftest format-timing-analysis-test
  (testing "Formats timing analysis"
    (let [analysis (timing/->TimingAnalysis
                    100 50 true true true true [] [])
          formatted (timing/format-timing-analysis analysis)]
      (is (string? formatted))
      (is (clojure.string/includes? formatted "Timing Analysis"))
      (is (clojure.string/includes? formatted "100")))))

(deftest format-lantern-budget-test
  (testing "Formats lantern budget"
    (let [budget {:remaining 200
                  :safety-margin 20
                  :available-for-plan 180
                  :required 100
                  :surplus 80
                  :valid? true
                  :recommendation "OK"}
          formatted (timing/format-lantern-budget budget)]
      (is (string? formatted))
      (is (clojure.string/includes? formatted "Lantern Budget")))))

