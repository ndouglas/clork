(ns clork.intel.goals-test
  (:require [clojure.test :refer :all]
            [clork.intel.goals :as goals]
            [clork.intel.affordances :as aff]
            [clork.game-state :as gs]
            [clork.debug.scenarios :as scenarios]))

;;; ---------------------------------------------------------------------------
;;; GOAL CHECKING TESTS
;;; ---------------------------------------------------------------------------

(deftest test-check-goal-game-flag-not-set
  (testing "check-goal returns false for unset game flag"
    (let [gs (scenarios/equipped-adventurer)
          result (goals/check-goal gs {:type :game-flag :flag :lld-flag})]
      (is (not (:satisfied result)))
      (is (= {:type :game-flag :flag :lld-flag} (:goal result))))))

(deftest test-check-goal-game-flag-set
  (testing "check-goal returns true for set game flag"
    (let [gs (-> (scenarios/equipped-adventurer)
                 (gs/set-game-flag :troll-flag))
          result (goals/check-goal gs {:type :game-flag :flag :troll-flag})]
      (is (:satisfied result)))))

(deftest test-check-goal-object-held
  (testing "check-goal returns true when object is held"
    (let [gs (scenarios/equipped-adventurer)  ; Has sword and lamp
          result (goals/check-goal gs {:type :object-held :object :sword})]
      (is (:satisfied result)))))

(deftest test-check-goal-object-not-held
  (testing "check-goal returns false when object is not held"
    (let [gs (scenarios/equipped-adventurer)
          result (goals/check-goal gs {:type :object-held :object :painting})]
      (is (not (:satisfied result))))))

(deftest test-check-goal-at-location
  (testing "check-goal returns true when at correct location"
    (let [gs (scenarios/equipped-adventurer :cellar)
          result (goals/check-goal gs {:type :at-location :room :cellar})]
      (is (:satisfied result)))))

(deftest test-check-goal-wrong-location
  (testing "check-goal returns false when at wrong location"
    (let [gs (scenarios/equipped-adventurer :cellar)
          result (goals/check-goal gs {:type :at-location :room :living-room})]
      (is (not (:satisfied result))))))

(deftest test-check-goal-object-flag
  (testing "check-goal returns true when object has flag"
    (let [gs (-> (scenarios/equipped-adventurer)
                 (gs/set-thing-flag :brass-lantern :on))
          result (goals/check-goal gs {:type :object-flag :object :brass-lantern :flag :on})]
      (is (:satisfied result)))))

(deftest test-check-goal-object-not-flag
  (testing "check-goal returns true when object lacks flag"
    (let [gs (scenarios/equipped-adventurer)
          ;; Lamp starts off in equipped-adventurer scenario, so :off flag shouldn't exist
          result (goals/check-goal gs {:type :object-not-flag :object :sword :flag :on})]
      (is (:satisfied result)))))

;;; ---------------------------------------------------------------------------
;;; EXPLAIN-GOAL TESTS
;;; ---------------------------------------------------------------------------

(deftest test-explain-satisfied-goal
  (testing "explain-goal shows satisfied status for met goals"
    (let [gs (-> (scenarios/equipped-adventurer)
                 (gs/set-game-flag :troll-flag))
          explanation (goals/explain-goal gs {:type :game-flag :flag :troll-flag})]
      (is (= :satisfied (:status explanation))))))

(deftest test-explain-unsatisfied-goal
  (testing "explain-goal shows blocked status for unmet goals"
    (let [gs (scenarios/equipped-adventurer)
          explanation (goals/explain-goal gs {:type :game-flag :flag :lld-flag})]
      (is (= :blocked (:status explanation)))
      (is (seq (:achievable-via explanation))))))

(deftest test-explain-lld-flag-finds-achiever
  (testing "explain-goal for lld-flag finds read-book-exorcism achiever"
    (let [gs (scenarios/equipped-adventurer)
          explanation (goals/explain-goal gs {:type :game-flag :flag :lld-flag})]
      (is (some #(= :read-book-exorcism (:affordance-id %))
                (:achievable-via explanation))))))

(deftest test-explain-shows-blocking-preconditions
  (testing "explain-goal shows which preconditions are blocking"
    (let [gs (scenarios/equipped-adventurer :west-of-house)  ; Not at entrance-to-hades
          explanation (goals/explain-goal gs {:type :game-flag :flag :lld-flag})
          achiever (first (filter #(= :read-book-exorcism (:affordance-id %))
                                  (:achievable-via explanation)))
          blocked-preconds (filter #(= :blocked (:status %))
                                   (:preconditions achiever))]
      ;; Should have blocked preconditions (location, xc flag, etc.)
      (is (seq blocked-preconds)))))

(deftest test-explain-no-achievers
  (testing "explain-goal reports no-achievers for impossible goals"
    ;; There's no affordance that sets a non-existent flag
    (let [gs (scenarios/equipped-adventurer)
          explanation (goals/explain-goal gs {:type :game-flag :flag :nonexistent-flag-xyz})]
      (is (= :no-achievers (:status explanation))))))

;;; ---------------------------------------------------------------------------
;;; WHY-NOT CONVENIENCE FUNCTIONS
;;; ---------------------------------------------------------------------------

(deftest test-why-not-flag
  (testing "why-not-flag? returns explanation for unset flag"
    (let [gs (scenarios/equipped-adventurer)
          explanation (goals/why-not-flag? gs :lld-flag)]
      (is (= {:type :game-flag :flag :lld-flag} (:goal explanation)))
      (is (not= :satisfied (:status explanation))))))

(deftest test-why-not-held
  (testing "why-not-held? returns explanation for unheld object"
    (let [gs (scenarios/equipped-adventurer)
          explanation (goals/why-not-held? gs :painting)]
      (is (= {:type :object-held :object :painting} (:goal explanation)))
      (is (not= :satisfied (:status explanation))))))

;;; ---------------------------------------------------------------------------
;;; ROOT CAUSE ANALYSIS
;;; ---------------------------------------------------------------------------

(deftest test-find-root-causes
  (testing "find-root-causes identifies deepest blockers"
    (let [gs (scenarios/equipped-adventurer :west-of-house)
          root-causes (goals/find-root-causes gs {:type :game-flag :flag :lld-flag})]
      ;; Should find some root causes
      (is (seq root-causes))
      ;; Each root cause should have a goal and reason
      (doseq [cause root-causes]
        (is (:goal cause))
        (is (:reason cause))))))

;;; ---------------------------------------------------------------------------
;;; CAUSAL CHAIN TESTS
;;; ---------------------------------------------------------------------------

(deftest test-exorcism-causal-chain
  (testing "lld-flag traces through xc -> xb causal chain"
    (let [gs (scenarios/equipped-adventurer :entrance-to-hades)
          explanation (goals/explain-goal gs {:type :game-flag :flag :lld-flag} 0 5)]
      ;; The explanation should eventually trace to xc requirement
      (let [read-book-achiever (first (filter #(= :read-book-exorcism (:affordance-id %))
                                              (:achievable-via explanation)))
            xc-blocked (some #(and (= :blocked (:status %))
                                   (= :xc (get-in % [:precond :flag])))
                             (:preconditions read-book-achiever))]
        (is xc-blocked "xc should be a blocking precondition")))))

(deftest test-explain-with-items
  (testing "explain-goal shows achievable status when preconditions can be met"
    (let [gs (-> (scenarios/equipped-adventurer :entrance-to-hades)
                 (gs/move-object :brass-bell :adventurer :test)
                 (gs/move-object :candles :adventurer :test)
                 (gs/move-object :black-book :adventurer :test)
                 (gs/set-game-flag :xb)
                 (gs/set-game-flag :xc))
          ;; Now lld-flag should be achievable (all preconditions met)
          explanation (goals/explain-goal gs {:type :game-flag :flag :lld-flag})]
      ;; The read-book-exorcism should be ready
      (is (some #(and (= :read-book-exorcism (:affordance-id %))
                      (= :ready (:status %)))
                (:achievable-via explanation))))))

;;; ---------------------------------------------------------------------------
;;; GOAL STATUS SUMMARY
;;; ---------------------------------------------------------------------------

(deftest test-goal-status-summary-satisfied
  (testing "goal-status-summary returns :satisfied for met goals"
    (let [gs (-> (scenarios/equipped-adventurer)
                 (gs/set-game-flag :troll-flag))
          status (goals/goal-status-summary gs {:type :game-flag :flag :troll-flag})]
      (is (= :satisfied status)))))

(deftest test-goal-status-summary-blocked
  (testing "goal-status-summary returns :blocked for unmet goals with blocked achievers"
    (let [gs (scenarios/equipped-adventurer :west-of-house)
          status (goals/goal-status-summary gs {:type :game-flag :flag :lld-flag})]
      (is (= :blocked status)))))

(deftest test-goal-status-summary-no-achievers
  (testing "goal-status-summary returns :no-achievers for impossible goals"
    (let [gs (scenarios/equipped-adventurer)
          status (goals/goal-status-summary gs {:type :game-flag :flag :impossible-flag-xyz})]
      (is (= :no-achievers status)))))

;;; ---------------------------------------------------------------------------
;;; PRECONDITION TO GOAL CONVERSION
;;; ---------------------------------------------------------------------------

(deftest test-precond-to-goal-conversion
  (testing "preconditions convert to appropriate goals"
    (is (= {:type :object-held :object :sword}
           (goals/precond->goal {:type :object-held :object :sword})))
    (is (= {:type :at-location :room :cellar}
           (goals/precond->goal {:type :at-location :room :cellar})))
    (is (= {:type :game-flag :flag :xb}
           (goals/precond->goal {:type :game-flag :flag :xb})))))

;;; ---------------------------------------------------------------------------
;;; FORMATTING TESTS
;;; ---------------------------------------------------------------------------

(deftest test-format-explanation
  (testing "format-explanation produces readable output"
    (let [gs (scenarios/equipped-adventurer)
          explanation (goals/explain-goal gs {:type :game-flag :flag :lld-flag})
          formatted (goals/format-explanation explanation)]
      (is (string? formatted))
      (is (clojure.string/includes? formatted "Goal:"))
      (is (clojure.string/includes? formatted "Status:")))))
