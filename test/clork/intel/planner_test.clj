(ns clork.intel.planner-test
  "Tests for the speedrun planner module."
  (:require [clojure.test :refer :all]
            [clork.core :as core]
            [clork.game-state :as gs]
            [clork.intel.planner :as planner]
            [clork.intel.treasures :as treasures]
            [clork.intel.puzzles :as puzzles]
            [clork.intel.routing :as routing]))

;;; ---------------------------------------------------------------------------
;;; GOAL CREATION TESTS
;;; ---------------------------------------------------------------------------

(deftest test-goal-creation
  (testing "treasure goal creation"
    (let [goal (planner/treasure-goal :egg)]
      (is (= :collect-treasure (:type goal)))
      (is (= :egg (:object goal)))))

  (testing "puzzle goal creation"
    (let [goal (planner/puzzle-goal :exorcism)]
      (is (= :solve-puzzle (:type goal)))
      (is (= :exorcism (:puzzle goal)))))

  (testing "item goal creation"
    (let [goal (planner/item-goal :brass-lantern)]
      (is (= :acquire-item (:type goal)))
      (is (= :brass-lantern (:object goal)))))

  (testing "location goal creation"
    (let [goal (planner/location-goal :living-room)]
      (is (= :reach-location (:type goal)))
      (is (= :living-room (:room goal)))))

  (testing "deposit goal creation"
    (let [goal (planner/deposit-goal [:egg :portrait])]
      (is (= :deposit-treasures (:type goal)))
      (is (= [:egg :portrait] (:treasures goal))))))

;;; ---------------------------------------------------------------------------
;;; PUZZLE REQUIREMENT TESTS
;;; ---------------------------------------------------------------------------

(deftest test-compute-required-puzzles
  (testing "treasures with no puzzle requirements"
    (let [puzzles (planner/compute-required-puzzles [:egg :portrait])]
      ;; Egg and portrait have no puzzle requirements
      (is (set? puzzles))))

  (testing "treasure requiring puzzle"
    (let [puzzles (planner/compute-required-puzzles [:platinum-bar])]
      (is (contains? puzzles :loud-room-echo))))

  (testing "treasure requiring lld-flag"
    (let [puzzles (planner/compute-required-puzzles [:crystal-skull])]
      ;; Crystal skull needs lld-flag which needs exorcism
      (is (contains? puzzles :exorcism))))

  (testing "multiple treasures combine requirements"
    (let [puzzles (planner/compute-required-puzzles
                    [:platinum-bar :crystal-skull :pot-of-gold])]
      (is (contains? puzzles :loud-room-echo))
      (is (contains? puzzles :exorcism))
      (is (contains? puzzles :rainbow-solid)))))

;;; ---------------------------------------------------------------------------
;;; GOAL LOCATION TESTS
;;; ---------------------------------------------------------------------------

(deftest test-goal-location
  (testing "treasure goal location"
    (let [gs (core/init-game)
          goal (planner/treasure-goal :egg)
          loc (planner/goal-location gs goal)]
      (is (= :up-a-tree loc))))

  (testing "location goal location"
    (let [gs (core/init-game)
          goal (planner/location-goal :living-room)
          loc (planner/goal-location gs goal)]
      (is (= :living-room loc))))

  (testing "deposit goal location"
    (let [gs (core/init-game)
          goal (planner/deposit-goal [:egg])
          loc (planner/goal-location gs goal)]
      (is (= :living-room loc)))))

;;; ---------------------------------------------------------------------------
;;; TSP ORDERING TESTS
;;; ---------------------------------------------------------------------------

(deftest test-greedy-tsp-order
  (testing "orders goals by distance"
    (let [gs (core/init-game)
          flags (routing/extract-available-flags gs)
          ;; Create goals at different locations
          goals [(planner/treasure-goal :egg)      ; up-a-tree
                 (planner/treasure-goal :portrait) ; gallery
                 (planner/location-goal :kitchen)] ; kitchen
          ordered (planner/greedy-tsp-order gs flags goals :west-of-house)]
      ;; Should return all goals
      (is (= 3 (count ordered)))
      ;; All original goals should be present
      (is (= (set goals) (set ordered)))))

  (testing "empty goals returns empty"
    (let [gs (core/init-game)
          flags #{}
          ordered (planner/greedy-tsp-order gs flags [] :west-of-house)]
      (is (empty? ordered)))))

;;; ---------------------------------------------------------------------------
;;; ROUTE GENERATION TESTS
;;; ---------------------------------------------------------------------------

(deftest test-generate-route-to
  (testing "returns nil when already at target"
    (let [gs (core/init-game)
          flags (routing/extract-available-flags gs)
          route (planner/generate-route-to gs :west-of-house flags)]
      (is (nil? route))))

  (testing "generates route to reachable location"
    (let [gs (core/init-game)
          flags (routing/extract-available-flags gs)
          ;; north-of-house is directly reachable from west-of-house
          route (planner/generate-route-to gs :north-of-house flags)]
      ;; Should return a sequence of actions
      (is (seq route))
      ;; All actions should be walks
      (is (every? #(= :walk (:verb %)) route)))))

;;; ---------------------------------------------------------------------------
;;; GOAL ACTION GENERATION TESTS
;;; ---------------------------------------------------------------------------

(deftest test-generate-goal-actions
  (testing "location goal generates movement actions"
    (let [gs (core/init-game)
          flags (routing/extract-available-flags gs)
          ;; Use a reachable location
          goal (planner/location-goal :north-of-house)
          result (planner/generate-goal-actions gs goal flags)]
      (is (seq (:actions result)))
      (is (string? (:description result)))))

  (testing "treasure goal generates route + take"
    (let [gs (core/init-game)
          flags (routing/extract-available-flags gs)
          goal (planner/treasure-goal :egg)
          result (planner/generate-goal-actions gs goal flags)]
      (is (seq (:actions result)))
      ;; Last action should be take
      (let [last-action (last (:actions result))]
        (is (= :take (:verb last-action)))
        (is (= :egg (:direct-object last-action))))))

  (testing "deposit goal generates puts even without route"
    ;; Deposit goal generates put actions even if living-room is unreachable
    (let [gs (core/init-game)
          flags (routing/extract-available-flags gs)
          goal (planner/deposit-goal [:egg :portrait])
          result (planner/generate-goal-actions gs goal flags)]
      ;; Should have put actions for each treasure
      (let [put-actions (filter #(= :put (:verb %)) (:actions result))]
        (is (= 2 (count put-actions)))))))

;;; ---------------------------------------------------------------------------
;;; PLAN GENERATION TESTS
;;; ---------------------------------------------------------------------------

(deftest test-generate-plan
  (testing "generates plan from goals"
    (let [gs (core/init-game)
          ;; Use north-of-house which is reachable from start
          goals [(planner/location-goal :north-of-house)]
          plan (planner/generate-plan gs goals)]
      (is (map? plan))
      (is (= 1 (:goal-count plan)))
      (is (pos? (:action-count plan)))))

  (testing "empty goals generates empty plan"
    (let [gs (core/init-game)
          plan (planner/generate-plan gs [])]
      (is (= 0 (:goal-count plan)))
      (is (= 0 (:action-count plan))))))

;;; ---------------------------------------------------------------------------
;;; PLAN ANALYSIS TESTS
;;; ---------------------------------------------------------------------------

(deftest test-plan-stats
  (testing "calculates plan statistics"
    (let [gs (core/init-game)
          goals [(planner/location-goal :living-room)
                 (planner/treasure-goal :egg)]
          plan {:plan (planner/generate-plan gs goals)}
          stats (planner/plan-stats plan)]
      (is (number? (:total-moves stats)))
      (is (number? (:walk-moves stats)))
      (is (number? (:phases stats))))))

;;; ---------------------------------------------------------------------------
;;; TREASURE COLLECTION PLANNING TESTS
;;; ---------------------------------------------------------------------------

(deftest test-plan-treasure-collection
  (testing "plans collection of specific treasures"
    (let [gs (core/init-game)
          result (planner/plan-treasure-collection gs [:egg :painting])]
      (is (seq (:goals result)))
      (is (map? (:plan result)))))

  (testing "empty treasure list returns minimal plan"
    (let [gs (core/init-game)
          result (planner/plan-treasure-collection gs [])]
      ;; Should have deposit goal at minimum
      (is (seq (:goals result))))))

;;; ---------------------------------------------------------------------------
;;; SPEEDRUN PLANNING TESTS
;;; ---------------------------------------------------------------------------

;; Note: Full speedrun planning is computationally expensive.
;; These tests use smaller treasure sets to keep tests fast.

(deftest test-plan-treasure-collection-simple
  (testing "plans collection of easily reachable treasures"
    (let [gs (core/init-game)
          ;; Use just the egg which is reachable from start
          result (planner/plan-treasure-collection gs [:egg])]
      (is (seq (:goals result)))
      (is (map? (:plan result))))))

(deftest test-summarize-plan
  (testing "summarizes plan"
    (let [gs (core/init-game)
          result (planner/plan-treasure-collection gs [:egg])
          summary (planner/summarize-plan result)]
      (is (number? (:total-phases summary)))
      (is (number? (:total-actions summary))))))

;;; ---------------------------------------------------------------------------
;;; EXECUTION TESTS
;;; ---------------------------------------------------------------------------

(deftest test-execute-action
  (testing "executes valid action"
    (let [gs (core/init-game)
          action {:verb :walk :direct-object :north}
          result (planner/execute-action gs action)]
      (is (boolean? (:success result)))
      (is (map? (:game-state result))))))

(deftest test-execute-phase
  (testing "executes phase with multiple actions"
    (let [gs (core/init-game)
          phase {:phase "Test phase"
                 :actions [{:verb :walk :direct-object :north}]}
          result (planner/execute-phase gs phase)]
      (is (boolean? (:success result)))
      (is (number? (:actions-completed result))))))

;;; ---------------------------------------------------------------------------
;;; VALIDATION TESTS
;;; ---------------------------------------------------------------------------

(deftest test-validate-plan
  (testing "validates plan with valid rooms"
    (let [gs (core/init-game)
          plan {:plan {:actions [{:phase "test"
                                  :actions [{:verb :walk :direct-object :north}]}]}}
          result (planner/validate-plan gs plan)]
      ;; Direction walks don't need room validation
      (is (:valid result))))

  (testing "detects invalid room in plan"
    (let [gs (core/init-game)
          plan {:plan {:actions [{:phase "test"
                                  :actions [{:verb :walk :direct-object :nonexistent-room}]}]}}
          result (planner/validate-plan gs plan)]
      ;; Should detect non-existent room
      (is (seq (:issues result))))))
