(ns clork.planner2.speculative-test
  "Tests for speculative combat execution."
  (:require [clojure.test :refer :all]
            [clork.planner2.speculative :as spec]
            [clork.random :as rng]
            [clork.core :as core]
            [clork.game-state :as gs]))

;;; ---------------------------------------------------------------------------
;;; FIXTURES
;;; ---------------------------------------------------------------------------

(defn fresh-game []
  (core/init-game))

(defn equipped-player []
  (-> (fresh-game)
      (assoc-in [:objects :sword :in] :adventurer)
      (assoc-in [:objects :brass-lantern :in] :adventurer)
      (gs/set-flag :objects :brass-lantern :on)))

(defn combat-ready-state []
  (-> (equipped-player)
      (assoc :here :troll-room)
      (assoc-in [:objects :troll :in] :troll-room)
      (assoc-in [:objects :troll :strength] 2)))

;;; ---------------------------------------------------------------------------
;;; RNG STATE TESTS
;;; ---------------------------------------------------------------------------

(deftest rng-state-save-restore-test
  (testing "RNG state can be saved and restored"
    (rng/init! 12345)
    (let [state1 (rng/save-state)
          v1 (rng/rand-int* 100)
          v2 (rng/rand-int* 100)
          v3 (rng/rand-int* 100)]
      ;; Restore and should get same sequence
      (rng/restore-state! state1)
      (is (= v1 (rng/rand-int* 100)))
      (is (= v2 (rng/rand-int* 100)))
      (is (= v3 (rng/rand-int* 100))))))

(deftest rng-call-count-test
  (testing "Call count is tracked"
    (rng/init! 12345)
    (is (= 0 (:call-count (rng/get-seed-info))))
    (rng/rand-int* 10)
    (is (= 1 (:call-count (rng/get-seed-info))))
    (rng/rand-int* 10)
    (rng/rand-int* 10)
    (is (= 3 (:call-count (rng/get-seed-info))))))

(deftest with-speculative-test
  (testing "Speculative execution restores RNG state"
    (rng/init! 12345)
    (let [initial-state (rng/save-state)
          [result final-state] (rng/with-speculative []
                                 (rng/rand-int* 100)
                                 (rng/rand-int* 100)
                                 :done)]
      (is (= :done result))
      ;; RNG should be restored
      (is (= (:call-count initial-state)
             (:call-count (rng/save-state)))))))

;;; ---------------------------------------------------------------------------
;;; SPECULATIVE COMBAT TESTS
;;; ---------------------------------------------------------------------------

(deftest speculative-combat-test
  (testing "Speculative combat doesn't change RNG state"
    (rng/init! 12345)
    (let [gs (combat-ready-state)
          initial-state (rng/save-state)
          result (spec/speculative-combat gs :troll)
          after-state (rng/save-state)]
      ;; RNG should be unchanged
      (is (= (:call-count initial-state) (:call-count after-state)))
      ;; Result should have outcome
      (is (#{:win :death :timeout} (:outcome result))))))

(deftest simulate-combat-determinism-test
  (testing "Same RNG state produces same combat outcome"
    (rng/init! 12345)
    (let [gs (combat-ready-state)
          result1 (spec/speculative-combat gs :troll)
          result2 (spec/speculative-combat gs :troll)]
      ;; Same seed + state = same outcome
      (is (= (:outcome result1) (:outcome result2)))
      (is (= (:turns result1) (:turns result2))))))

;;; ---------------------------------------------------------------------------
;;; BURN SEQUENCE TESTS
;;; ---------------------------------------------------------------------------

(deftest simple-burn-sequences-test
  (testing "Generates burn sequences"
    (let [seqs (spec/simple-burn-sequences 3)]
      (is (seq seqs))
      ;; Should include single waits
      (is (some #(= ["wait"] %) seqs))
      (is (some #(= ["wait" "wait"] %) seqs))
      ;; Should include alternating
      (is (some #(= ["wait" "look"] %) seqs)))))

;;; ---------------------------------------------------------------------------
;;; COMBAT PLAN TESTS
;;; ---------------------------------------------------------------------------

(deftest find-winning-combat-plan-basic-test
  (testing "Can search for winning plan"
    (rng/init! 12345)
    (let [gs (combat-ready-state)
          ;; Dummy execute-fn that just returns state
          execute-fn (fn [gs cmd] gs)
          plan (spec/find-winning-combat-plan gs :troll execute-fn
                                               :max-burn-actions 5)]
      ;; May or may not find a plan depending on RNG seed
      (is (or (nil? plan)
              (= :win (:expected-outcome plan)))))))

(deftest combat-plan-determinism-test
  (testing "Same seed produces same plan"
    (rng/init! 99999)
    (let [gs (combat-ready-state)
          execute-fn (fn [gs cmd] gs)
          plan1 (spec/find-winning-combat-plan gs :troll execute-fn
                                                :max-burn-actions 3)]
      (rng/init! 99999)
      (let [plan2 (spec/find-winning-combat-plan gs :troll execute-fn
                                                  :max-burn-actions 3)]
        ;; Same seed = same plan
        (is (= (:pre-actions plan1) (:pre-actions plan2)))
        (is (= (:expected-turns plan1) (:expected-turns plan2)))))))

;;; ---------------------------------------------------------------------------
;;; TIMING ANALYSIS TESTS
;;; ---------------------------------------------------------------------------

(deftest analyze-combat-timing-test
  (testing "Timing analysis produces statistics"
    (rng/init! 12345)
    (let [gs (combat-ready-state)
          execute-fn (fn [gs cmd] gs)
          analysis (spec/analyze-combat-timing gs :troll execute-fn 10)]
      (is (= 10 (:total-samples analysis)))
      (is (number? (:win-rate analysis)))
      (is (number? (:death-rate analysis)))
      ;; Total includes wins, deaths, timeouts, and possibly errors
      (is (<= (+ (:wins analysis) (:deaths analysis))
              (:total-samples analysis))))))

(deftest timing-analysis-preserves-rng-test
  (testing "Timing analysis doesn't change RNG state"
    (rng/init! 12345)
    (let [gs (combat-ready-state)
          execute-fn (fn [gs cmd] gs)
          initial-state (rng/save-state)
          _ (spec/analyze-combat-timing gs :troll execute-fn 20)
          after-state (rng/save-state)]
      (is (= (:call-count initial-state) (:call-count after-state))))))

;;; ---------------------------------------------------------------------------
;;; FORMAT OUTPUT TESTS
;;; ---------------------------------------------------------------------------

(deftest format-combat-plan-test
  (testing "Format with plan"
    (let [plan (spec/->CombatPlan ["wait" "wait"] :win 3 nil 1.0)
          formatted (spec/format-combat-plan plan :troll)]
      (is (clojure.string/includes? formatted "Combat Plan"))
      (is (clojure.string/includes? formatted "wait"))
      (is (clojure.string/includes? formatted "win"))))

  (testing "Format without plan"
    (let [formatted (spec/format-combat-plan nil :troll)]
      (is (clojure.string/includes? formatted "No winning plan")))))

(deftest format-timing-analysis-test
  (testing "Format timing analysis"
    (let [analysis {:total-samples 100
                    :win-rate 0.75
                    :death-rate 0.25
                    :first-win 3}
          formatted (spec/format-timing-analysis analysis :troll)]
      (is (clojure.string/includes? formatted "Timing Analysis"))
      (is (clojure.string/includes? formatted "75.0%")))))

