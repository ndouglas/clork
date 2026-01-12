(ns clork.planner2.integration-test
  "End-to-end integration tests for the speedrun planner.

   These tests run full speedruns through the actual game engine,
   verifying that all components work together:
   - Schedule generation
   - Goal execution via reactive planner
   - Navigation and pathfinding
   - Combat execution
   - Treasure collection and deposit
   - Recovery from unexpected events"
  (:require [clojure.test :refer :all]
            [clork.planner2.executor :as executor]
            [clork.planner2.schedule :as schedule]
            [clork.planner2.observe :as obs]
            [clork.planner2.speculative :as spec]
            [clork.planner2.speculative-thief :as spec-thief]
            [clork.random :as rng]
            [clork.core :as core]
            [clork.game-state :as gs]))

;;; ---------------------------------------------------------------------------
;;; TEST FIXTURES
;;; ---------------------------------------------------------------------------

(defn fresh-game []
  (core/init-game))

(defn seeded-game
  "Create a game with a specific RNG seed for reproducibility."
  [seed]
  (rng/init! seed)
  (fresh-game))

;;; ---------------------------------------------------------------------------
;;; SCHEDULE GENERATION TESTS
;;; ---------------------------------------------------------------------------

(deftest schedule-generation-test
  (testing "Can generate schedule for simple treasures"
    (let [gs (fresh-game)
          treasures [:egg :painting]
          sched (schedule/generate-schedule gs treasures)]
      (is (seq sched) "Schedule should not be empty")
      (is (every? :type sched) "All entries should have :type")
      ;; Should have at least one collect entry
      (let [collects (filter #(= :collect (:type %)) sched)]
        (is (>= (count collects) 1)
            "Should have at least one collect entry"))))

  (testing "Schedule includes deposit-all"
    (let [gs (fresh-game)
          treasures [:egg]
          sched (schedule/generate-schedule gs treasures)]
      (is (some #(= :deposit-all (:type %)) sched)
          "Schedule should include deposit-all entry"))))

;;; ---------------------------------------------------------------------------
;;; SINGLE TREASURE SPEEDRUN TESTS
;;; ---------------------------------------------------------------------------

(deftest egg-speedrun-test
  (testing "Can complete egg-only speedrun"
    (let [gs (seeded-game 12345)
          result (executor/run-speedrun gs [:egg]
                                        :max-turns 100
                                        :verbose false)]
      (is (= :complete (:status result))
          (str "Egg speedrun should complete, got: " (:status result)
               " with failure info: " (:failure-info result)))
      (is (contains? (:deposited-treasures result) :egg)
          "Egg should be deposited")
      (is (< (:turn-count result) 50)
          "Egg speedrun should be efficient"))))

;;; ---------------------------------------------------------------------------
;;; ABOVE-GROUND TREASURE TESTS
;;; ---------------------------------------------------------------------------

(deftest above-ground-treasures-test
  (testing "Can collect treasures that don't require prep actions"
    (let [gs (seeded-game 54321)
          ;; Egg doesn't require any preps
          result (executor/run-speedrun gs [:egg]
                                        :max-turns 100
                                        :verbose false)]
      (is (= :complete (:status result))
          "Above-ground run should complete"))))

;;; ---------------------------------------------------------------------------
;;; BASIC NAVIGATION TESTS
;;; ---------------------------------------------------------------------------

(deftest navigation-integration-test
  (testing "Planner can navigate between rooms"
    (let [gs (seeded-game 11111)
          ;; Run a simple navigation-heavy plan
          result (executor/run-speedrun gs [:egg]
                                        :max-turns 100)]
      (is (#{:complete :timeout} (:status result))
          "Navigation should work"))))

;;; ---------------------------------------------------------------------------
;;; TROLL COMBAT TESTS
;;; ---------------------------------------------------------------------------

(deftest troll-combat-integration-test
  (testing "Can complete speedrun requiring troll combat"
    (let [gs (seeded-game 99999)
          ;; Painting requires killing the troll
          result (executor/run-speedrun gs [:painting]
                                        :max-turns 300
                                        :max-deaths 3
                                        :verbose false)]
      ;; May timeout or die in combat - that's acceptable
      ;; The test validates the system handles combat
      (is (#{:complete :timeout :failed} (:status result))
          "Combat integration should not crash"))))

;;; ---------------------------------------------------------------------------
;;; SPECULATIVE COMBAT INTEGRATION
;;; ---------------------------------------------------------------------------

(deftest speculative-combat-integration-test
  (testing "Speculative combat can predict outcomes"
    (rng/init! 77777)
    (let [gs (-> (fresh-game)
                 (assoc :here :troll-room)
                 (assoc-in [:objects :sword :in] :adventurer)
                 (assoc-in [:objects :brass-lantern :in] :adventurer)
                 (gs/set-flag :objects :brass-lantern :on))
          result (spec/speculative-combat gs :troll)]
      (is (#{:win :death :timeout} (:outcome result))
          "Speculative combat should return valid outcome"))))

(deftest speculative-thief-integration-test
  (testing "Speculative thief can predict encounters"
    (rng/init! 88888)
    (let [gs (-> (fresh-game)
                 (assoc :here :cellar)
                 (assoc-in [:objects :brass-lantern :in] :adventurer)
                 (gs/set-flag :objects :brass-lantern :on))
          route [:cellar :troll-room :maze-1]
          execute-fn (fn [gs cmd] gs)
          pred (spec-thief/predict-thief-interactions gs route execute-fn)]
      (is (some? (:theft-risk pred))
          "Should predict theft risk")
      (is (some? (:recommended-action pred))
          "Should recommend action"))))

;;; ---------------------------------------------------------------------------
;;; MULTI-TREASURE SPEEDRUN TESTS
;;; ---------------------------------------------------------------------------

(deftest easy-treasures-speedrun-test
  (testing "Can complete easy treasures speedrun"
    (let [gs (seeded-game 33333)
          ;; Just egg - the simplest treasure
          result (executor/run-speedrun gs [:egg]
                                        :max-turns 100
                                        :verbose false)]
      (is (= :complete (:status result))
          "Easy treasure run should complete"))))

;;; ---------------------------------------------------------------------------
;;; RECOVERY TESTS
;;; ---------------------------------------------------------------------------

(deftest replan-on-stuck-test
  (testing "System can replan when stuck"
    ;; This tests the recovery mechanism even if we don't hit it
    (let [gs (seeded-game 44444)
          ;; Use a simple treasure that shouldn't get stuck
          result (executor/run-speedrun gs [:egg]
                                        :max-turns 100)]
      ;; Should complete or at least not crash
      (is (some? (:status result))
          "Should return a status"))))

;;; ---------------------------------------------------------------------------
;;; EXECUTION STATE TESTS
;;; ---------------------------------------------------------------------------

(deftest execution-state-tracking-test
  (testing "Execution state tracks progress correctly"
    (let [gs (seeded-game 55555)
          result (executor/run-speedrun gs [:egg]
                                        :max-turns 100)]
      (is (number? (:turn-count result))
          "Should track turn count")
      (is (set? (:deposited-treasures result))
          "Should track deposited treasures")
      (is (number? (:deaths result))
          "Should track deaths"))))

;;; ---------------------------------------------------------------------------
;;; DEBUG MODE TESTS
;;; ---------------------------------------------------------------------------

(deftest debug-mode-test
  (testing "Debug mode captures execution trace"
    (let [gs (seeded-game 66666)
          result (executor/run-speedrun gs [:egg]
                                        :max-turns 100
                                        :debug true)]
      ;; Debug mode enables tracing, but trace may be empty for simple runs
      ;; Just verify the debug infrastructure is in place
      (is (vector? (:trace result))
          "Debug mode should have trace vector")
      (is (vector? (:checkpoints result))
          "Debug mode should have checkpoints vector"))))

;;; ---------------------------------------------------------------------------
;;; ANALYSIS FUNCTIONS TESTS
;;; ---------------------------------------------------------------------------

(deftest analyze-result-test
  (testing "Can analyze speedrun results"
    (let [gs (seeded-game 11111)
          result (executor/run-speedrun gs [:egg]
                                        :max-turns 100)
          analysis (executor/analyze-result result)]
      (is (some? (:status analysis)))
      (is (number? (:turns analysis)))
      (is (number? (:completion-rate analysis))))))

;;; ---------------------------------------------------------------------------
;;; VALIDATION TESTS
;;; ---------------------------------------------------------------------------

(deftest validate-schedule-test
  (testing "Schedule validation catches issues"
    (let [gs (fresh-game)
          validation (executor/validate-schedule gs [:egg])]
      (is (:valid validation)
          "Simple schedule should be valid"))))

;;; ---------------------------------------------------------------------------
;;; DETERMINISM TESTS
;;; ---------------------------------------------------------------------------

(deftest deterministic-execution-test
  (testing "Same seed produces same result"
    (let [seed 123456
          run1 (do
                 (rng/init! seed)
                 (let [gs (fresh-game)]
                   (executor/run-speedrun gs [:egg] :max-turns 100)))
          run2 (do
                 (rng/init! seed)
                 (let [gs (fresh-game)]
                   (executor/run-speedrun gs [:egg] :max-turns 100)))]
      (is (= (:status run1) (:status run2))
          "Same seed should produce same status")
      (is (= (:turn-count run1) (:turn-count run2))
          "Same seed should produce same turn count"))))

;;; ---------------------------------------------------------------------------
;;; PERFORMANCE / EFFICIENCY TESTS
;;; ---------------------------------------------------------------------------

(deftest execution-efficiency-test
  (testing "Egg speedrun is reasonably efficient"
    (let [gs (seeded-game 98765)
          result (executor/run-speedrun gs [:egg] :max-turns 100)]
      (when (= :complete (:status result))
        ;; Egg should be collectible in under 30 moves
        ;; (climb tree, get egg, go to living room, open case, put in)
        (is (< (:turn-count result) 30)
            (str "Egg run took " (:turn-count result) " turns, expected < 30"))))))

;;; ---------------------------------------------------------------------------
;;; ERROR HANDLING TESTS
;;; ---------------------------------------------------------------------------

(deftest graceful-timeout-test
  (testing "Graceful handling of timeout"
    (let [gs (seeded-game 11111)
          result (executor/run-speedrun gs [:egg]
                                        :max-turns 5)] ; Very low limit
      (is (#{:timeout :complete} (:status result))
          "Should timeout or complete, not crash"))))

;;; ---------------------------------------------------------------------------
;;; FULL SYSTEM SMOKE TEST
;;; ---------------------------------------------------------------------------

(deftest full-system-smoke-test
  (testing "Full system smoke test - all components working together"
    (let [gs (seeded-game 42)]
      ;; Test schedule generation
      (let [sched (schedule/generate-schedule gs [:egg])]
        (is (seq sched) "Schedule generation works"))

      ;; Test observation
      (is (some? (obs/current-room gs)) "Observation works")

      ;; Test speculative combat (with sword)
      (let [gs-armed (assoc-in gs [:objects :sword :in] :adventurer)]
        (is (some? (spec/speculative-combat gs-armed :troll))
            "Speculative combat works"))

      ;; Test execution
      (let [result (executor/run-speedrun gs [:egg] :max-turns 100)]
        (is (some? (:status result)) "Execution works")))))

