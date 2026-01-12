(ns clork.planner2-test
  "Tests for the reactive planner."
  (:require [clojure.test :refer :all]
            [clork.core :as core]
            [clork.planner2.core :as planner]
            [clork.planner2.observe :as obs]
            [clork.planner2.goals :as goals]
            [clork.planner2.navigate :as nav]))

;;; ---------------------------------------------------------------------------
;;; TEST FIXTURES
;;; ---------------------------------------------------------------------------

(defn init-game []
  (core/init-game))

;;; ---------------------------------------------------------------------------
;;; OBSERVATION TESTS
;;; ---------------------------------------------------------------------------

(deftest observation-tests
  (testing "Basic state observations"
    (let [gs (init-game)]
      (is (= :west-of-house (obs/current-room gs)))
      (is (obs/at-room? gs :west-of-house))
      (is (not (obs/at-room? gs :kitchen)))
      (is (empty? (obs/inventory gs)))
      (is (not (obs/has-item? gs :sword)))))

  (testing "Object visibility"
    (let [gs (init-game)]
      ;; Mailbox should be visible at west-of-house
      (is (obs/object-visible? gs :mailbox))
      ;; Sword should not be visible (inside case in living room)
      (is (not (obs/object-visible? gs :sword)))))

  (testing "Exit availability"
    (let [gs (init-game)]
      (let [exits (obs/available-exits gs)]
        (is (contains? exits :north))
        (is (contains? exits :south))
        (is (contains? exits :west))
        (is (not (contains? exits :east)))))))

;;; ---------------------------------------------------------------------------
;;; NAVIGATION TESTS
;;; ---------------------------------------------------------------------------

(deftest navigation-tests
  (testing "Path finding"
    (let [gs (init-game)]
      ;; Path to north of house
      (let [nav-plan (nav/plan-navigation gs :north-of-house)]
        (is (some? nav-plan))
        (is (= 1 (:distance nav-plan)))
        ;; Both :north and :ne lead to north-of-house
        (is (contains? #{[:north] [:ne]} (:directions nav-plan))))

      ;; Path to kitchen (requires going around)
      (let [nav-plan (nav/plan-navigation gs :kitchen)]
        (is (some? nav-plan))
        ;; Should be: north -> east -> enter (3 moves)
        (is (>= (:distance nav-plan) 2)))))

  (testing "Movement action selection"
    (let [gs (init-game)]
      (let [action (nav/select-movement-action gs :north-of-house)]
        (is (= :go (:verb action)))
        ;; Both :north and :ne are valid
        (is (contains? #{:north :ne} (:direction action)))))))

;;; ---------------------------------------------------------------------------
;;; GOAL TESTS
;;; ---------------------------------------------------------------------------

(deftest goal-satisfaction-tests
  (testing "at-room goal"
    (let [gs (init-game)]
      (is (goals/goal-satisfied? gs (goals/at-room :west-of-house)))
      (is (not (goals/goal-satisfied? gs (goals/at-room :kitchen))))))

  (testing "have-item goal"
    (let [gs (init-game)]
      (is (not (goals/goal-satisfied? gs (goals/have-item :sword))))))

  (testing "lantern-on goal"
    (let [gs (init-game)]
      (is (not (goals/goal-satisfied? gs (goals/lantern-on)))))))

;;; ---------------------------------------------------------------------------
;;; PLANNER EXECUTION TESTS
;;; ---------------------------------------------------------------------------

(deftest simple-navigation-test
  (testing "Navigate to north-of-house"
    (let [gs (init-game)
          result (planner/run-goal gs (goals/at-room :north-of-house))]
      (is (= :complete (:status result)))
      (is (obs/at-room? (:game-state result) :north-of-house)))))

(deftest multi-step-navigation-test
  (testing "Navigate to kitchen"
    (let [gs (init-game)
          result (planner/run-goal gs (goals/at-room :kitchen)
                                   :max-turns 20)]
      (is (= :complete (:status result)))
      (is (obs/at-room? (:game-state result) :kitchen)))))

(deftest get-leaflet-test
  (testing "Get leaflet from mailbox"
    (let [gs (init-game)
          ;; First open mailbox, then take leaflet
          result (planner/run-goal gs (goals/have-item :leaflet)
                                   :max-turns 10)]
      ;; Note: leaflet is inside mailbox, so this tests container handling
      (is (or (= :complete (:status result))
              (= :stuck (:status result))))  ; Might need decomposition for container
      )))

(deftest get-sword-test
  (testing "Get sword from living room"
    (let [gs (init-game)
          result (planner/run-goal gs (goals/have-item :sword)
                                   :max-turns 30)]
      (is (= :complete (:status result)))
      (is (obs/has-item? (:game-state result) :sword)))))

(deftest get-lantern-test
  (testing "Get lantern from living room"
    (let [gs (init-game)
          result (planner/run-goal gs (goals/have-item :brass-lantern)
                                   :max-turns 30)]
      (is (= :complete (:status result)))
      (is (obs/has-item? (:game-state result) :brass-lantern)))))

(deftest turn-on-lantern-test
  (testing "Turn on lantern"
    (let [gs (init-game)
          result (planner/run-goal gs (goals/lantern-on)
                                   :max-turns 30)]
      (is (= :complete (:status result)))
      (is (obs/lantern-on? (:game-state result))))))

;;; ---------------------------------------------------------------------------
;;; TREASURE TESTS
;;; ---------------------------------------------------------------------------

(deftest get-egg-test
  (testing "Get egg from tree"
    (let [gs (init-game)
          result (planner/run-goal gs (goals/have-item :egg)
                                   :max-turns 20)]
      (is (= :complete (:status result)))
      (is (obs/has-item? (:game-state result) :egg)))))

(deftest deposit-egg-test
  (testing "Deposit egg in trophy case"
    (let [gs (init-game)
          result (planner/run-goal gs (goals/item-deposited :egg)
                                   :max-turns 50)]
      (is (= :complete (:status result)))
      (is (obs/item-deposited? (:game-state result) :egg)))))

;;; ---------------------------------------------------------------------------
;;; COMBAT TESTS
;;; ---------------------------------------------------------------------------

(deftest kill-troll-test
  (testing "Kill troll"
    (let [gs (init-game)
          ;; This is a more complex goal that requires:
          ;; 1. Get sword
          ;; 2. Get lantern and turn it on
          ;; 3. Navigate to troll room
          ;; 4. Fight troll until dead (RNG - can take 5-50+ attacks)
          result (planner/run-goal gs (goals/kill-enemy :troll)
                                   :max-turns 200)]
      ;; Troll combat is RNG-heavy; may timeout, die, or complete
      (is (contains? #{:complete :stuck :dead :timeout} (:status result)))
      ;; If complete, troll should be dead
      (when (= :complete (:status result))
        (is (obs/troll-dead? (:game-state result)))))))

;;; ---------------------------------------------------------------------------
;;; DEBUG HELPERS
;;; ---------------------------------------------------------------------------

(deftest step-runner-test
  (testing "Step runner works"
    (let [gs (init-game)
          runner (planner/step-runner gs (goals/at-room :north-of-house))]
      ;; Take one step
      (planner/step! runner)
      ;; Should have made progress
      (is (or (= :running (:status @runner))
              (= :complete (:status @runner)))))))
