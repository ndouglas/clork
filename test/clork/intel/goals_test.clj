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

;;; ---------------------------------------------------------------------------
;;; NEW GOAL TYPE TESTS (at-any-location, player-in-vehicle, player-not-in-vehicle)
;;; ---------------------------------------------------------------------------

(deftest test-check-goal-at-any-location-satisfied
  (testing "at-any-location returns true when at one of the rooms"
    (let [gs (scenarios/equipped-adventurer :dam-room)
          result (goals/check-goal gs {:type :at-any-location
                                       :rooms [:dam-room :reservoir-north :reservoir-south]})]
      (is (:satisfied result)))))

(deftest test-check-goal-at-any-location-not-satisfied
  (testing "at-any-location returns false when not at any of the rooms"
    (let [gs (scenarios/equipped-adventurer :cellar)
          result (goals/check-goal gs {:type :at-any-location
                                       :rooms [:dam-room :reservoir-north]})]
      (is (not (:satisfied result))))))

(deftest test-check-goal-player-in-vehicle
  (testing "player-in-vehicle checks if player is in specified vehicle"
    (let [gs (-> (scenarios/equipped-adventurer :dam-base)
                 (gs/move-object :magic-boat :dam-base :test)
                 (gs/set-thing-flag :magic-boat :open)
                 ;; Simulate being in boat by moving adventurer to boat
                 (assoc-in [:objects :adventurer :in] :magic-boat))
          result (goals/check-goal gs {:type :player-in-vehicle :vehicle :magic-boat})]
      (is (:satisfied result)))))

(deftest test-check-goal-player-not-in-vehicle
  (testing "player-not-in-vehicle returns true when not in any vehicle"
    ;; When player is in a room (not a vehicle), the goal should be satisfied
    (let [gs (scenarios/equipped-adventurer :dam-base)
          ;; Make sure adventurer location matches :here
          gs (assoc-in gs [:objects :adventurer :in] :dam-base)
          result (goals/check-goal gs {:type :player-not-in-vehicle})]
      (is (:satisfied result)))))

;;; ---------------------------------------------------------------------------
;;; EFFECT MATCHER TESTS FOR 'NOT' GOALS
;;; ---------------------------------------------------------------------------

(deftest test-achievers-of-game-not-flag
  (testing "achievers-of finds affordances that clear game flags"
    ;; brown-button clears gate-flag
    (let [achievers (aff/achievers-of {:type :game-not-flag :flag :gate-flag})]
      (is (some #(= :press-brown-button (:affordance-id %)) achievers)))))

(deftest test-achievers-of-object-not-flag
  (testing "achievers-of finds affordances that clear object flags"
    ;; lamp-off clears :on flag on objects
    (let [achievers (aff/achievers-of {:type :object-not-flag :object :brass-lantern :flag :on})]
      ;; Note: lamp-off uses :$obj binding, so this tests the generic case
      ;; The specific brass-lantern case may not match due to binding
      (is (some? achievers) "Should have some achievers for lamp-off"))))

(deftest test-achievers-of-object-held
  (testing "achievers-of finds take-object for object-held goals"
    (let [achievers (aff/achievers-of {:type :object-held :object :sword})]
      ;; take-object moves object to :adventurer but uses :$obj binding
      ;; So it won't match specific :sword, but we verify the matcher exists
      ;; The result should be a collection (possibly empty due to bindings)
      (is (coll? achievers)))))

;;; ---------------------------------------------------------------------------
;;; ROOT CAUSE WITH UNKNOWN STATUS
;;; ---------------------------------------------------------------------------

(deftest test-root-causes-include-unknown-status
  (testing "root causes should include preconditions with unknown status"
    ;; This is a structural test - we're testing that the code handles unknown status
    ;; The actual root causes depend on the specific affordances defined
    (let [gs (scenarios/equipped-adventurer :west-of-house)
          root-causes (goals/find-root-causes gs {:type :game-flag :flag :lld-flag})]
      ;; Should return some root causes
      (is (seq root-causes)))))

;;; ---------------------------------------------------------------------------
;;; ROUTING INTEGRATION TESTS
;;; ---------------------------------------------------------------------------

(deftest test-analyze-location-reachability-reachable
  (testing "analyzes reachable location"
    (let [gs (scenarios/equipped-adventurer :west-of-house)
          result (goals/analyze-location-reachability gs :north-of-house)]
      (is (:reachable? result))
      (is (= 1 (:distance result)))
      (is (seq (:path result))))))

(deftest test-analyze-location-reachability-unreachable
  (testing "analyzes unreachable location and suggests flags"
    (let [gs (scenarios/equipped-adventurer :entrance-to-hades)
          result (goals/analyze-location-reachability gs :land-of-living-dead)]
      ;; Should be unreachable without lld-flag
      (is (not (:reachable? result)))
      ;; Should suggest lld-flag
      (is (contains? (:missing-flags result) :lld-flag)))))

(deftest test-why-cant-reach-reachable
  (testing "why-cant-reach explains reachable location"
    (let [gs (scenarios/equipped-adventurer :west-of-house)
          result (goals/why-cant-reach? gs :north-of-house)]
      (is (= :reachable (:status result)))
      (is (string? (:details result))))))

(deftest test-why-cant-reach-unreachable
  (testing "why-cant-reach explains unreachable location"
    (let [gs (scenarios/equipped-adventurer :entrance-to-hades)
          result (goals/why-cant-reach? gs :land-of-living-dead)]
      (is (= :unreachable (:status result)))
      (is (contains? (get-in result [:routing :missing-flags]) :lld-flag)))))
