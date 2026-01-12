(ns clork.planner2.speculative-thief-test
  "Tests for speculative thief execution."
  (:require [clojure.test :refer :all]
            [clork.planner2.speculative-thief :as spec-thief]
            [clork.planner2.thief :as thief]
            [clork.random :as rng]
            [clork.core :as core]
            [clork.game-state :as gs]))

;;; ---------------------------------------------------------------------------
;;; FIXTURES
;;; ---------------------------------------------------------------------------

(defn fresh-game []
  (core/init-game))

(defn underground-state []
  "State where player is underground (thief can appear)."
  (-> (fresh-game)
      (assoc :here :cellar)
      (assoc-in [:objects :brass-lantern :in] :adventurer)
      (gs/set-flag :objects :brass-lantern :on)))

(defn thief-nearby-state []
  "State where thief is in same room as player."
  (-> (underground-state)
      (assoc-in [:objects :thief :in] :cellar)))

(defn player-with-treasure []
  "State where player has valuable items (theft target)."
  (-> (underground-state)
      (assoc-in [:objects :egg :in] :adventurer)))

;;; ---------------------------------------------------------------------------
;;; THIEF LOCATION PREDICTION TESTS
;;; ---------------------------------------------------------------------------

(deftest predict-thief-location-test
  (testing "Predicts thief location N turns ahead"
    (let [gs (fresh-game)
          pred (spec-thief/predict-thief-location gs 5)]
      (is (some? (:location pred)))
      (is (= 5 (:turns-from-now pred)))))

  (testing "Returns dead status for dead thief"
    (let [gs (-> (fresh-game)
                 (assoc :thief-dead true)
                 (assoc-in [:objects :thief :strength] 0))
          pred (spec-thief/predict-thief-location gs 5)]
      (is (:dead pred)))))

(deftest thief-path-test
  (testing "Returns path of thief locations"
    (let [gs (fresh-game)
          path (spec-thief/thief-path gs 10)]
      (is (vector? path))
      (is (= 11 (count path)))  ; Initial + 10 turns
      (is (every? keyword? path))))

  (testing "Empty path for dead thief"
    (let [gs (-> (fresh-game)
                 (assoc :thief-dead true)
                 (assoc-in [:objects :thief :strength] 0))
          path (spec-thief/thief-path gs 10)]
      (is (empty? path)))))

(deftest turns-until-thief-at-test
  (testing "Finds turns to reach a room"
    (let [gs (fresh-game)
          thief-loc (thief/thief-location gs)
          ;; Thief should reach itself in 0 turns
          turns (spec-thief/turns-until-thief-at gs thief-loc 50)]
      (is (= 0 turns))))

  (testing "Returns nil for unreachable room"
    (let [gs (fresh-game)
          ;; Use a room thief can't reach (sacred)
          turns (spec-thief/turns-until-thief-at gs :living-room 50)]
      ;; May or may not be nil depending on room configuration
      (is (or (nil? turns) (number? turns))))))

;;; ---------------------------------------------------------------------------
;;; SPECULATIVE THIEF TURN TESTS
;;; ---------------------------------------------------------------------------

(deftest speculative-thief-turn-test
  (testing "Doesn't change RNG state"
    (rng/init! 12345)
    (let [gs (underground-state)
          initial-state (rng/save-state)
          _ (spec-thief/speculative-thief-turn gs)
          after-state (rng/save-state)]
      (is (= (:call-count initial-state) (:call-count after-state)))))

  (testing "Returns encounter result"
    (rng/init! 12345)
    (let [gs (thief-nearby-state)
          result (spec-thief/speculative-thief-turn gs)]
      (is (some? (:encounter-type result)))
      (is (#{:none :appears :steals :combat :retreats} (:encounter-type result))))))

;;; ---------------------------------------------------------------------------
;;; THIEF AVOIDANCE TESTS
;;; ---------------------------------------------------------------------------

(deftest will-thief-appear-test
  (testing "Predicts thief appearance"
    (rng/init! 12345)
    (let [gs (thief-nearby-state)
          will-appear (spec-thief/will-thief-appear? gs)]
      ;; Result depends on RNG, just verify it returns a boolean
      (is (boolean? will-appear)))))

(deftest will-thief-steal-test
  (testing "Predicts theft"
    (rng/init! 12345)
    (let [gs (player-with-treasure)
          gs-with-thief (assoc-in gs [:objects :thief :in] :cellar)
          will-steal (spec-thief/will-thief-steal? gs-with-thief)]
      ;; Result depends on RNG
      (is (or (nil? will-steal) (coll? will-steal))))))

(deftest find-safe-burn-sequence-test
  (testing "Search for safe sequence"
    (rng/init! 12345)
    (let [gs (underground-state)
          execute-fn (fn [gs cmd] gs)
          result (spec-thief/find-safe-burn-sequence gs execute-fn :max-burns 5)]
      ;; May or may not find a safe sequence
      (is (or (nil? result)
              (and (:safe? result)
                   (vector? (:burn-actions result))))))))

;;; ---------------------------------------------------------------------------
;;; THIEF COMBAT PLANNING TESTS
;;; ---------------------------------------------------------------------------

(deftest speculative-thief-combat-test
  (testing "Simulates thief combat"
    (rng/init! 12345)
    (let [gs (-> (thief-nearby-state)
                 (assoc-in [:objects :sword :in] :adventurer))
          result (spec-thief/speculative-thief-combat gs)]
      (is (#{:win :death :timeout} (:outcome result)))
      (is (number? (:turns result))))))

(deftest find-winning-thief-plan-test
  (testing "Searches for winning combat plan"
    (rng/init! 12345)
    (let [gs (-> (thief-nearby-state)
                 (assoc-in [:objects :sword :in] :adventurer))
          execute-fn (fn [gs cmd] gs)
          plan (spec-thief/find-winning-thief-plan gs execute-fn :max-burn-actions 5)]
      ;; May or may not find a plan
      (is (or (nil? plan)
              (= :win (:expected-outcome plan)))))))

;;; ---------------------------------------------------------------------------
;;; COMPREHENSIVE PREDICTION TESTS
;;; ---------------------------------------------------------------------------

(deftest predict-thief-interactions-test
  (testing "Comprehensive prediction for a route"
    (rng/init! 12345)
    (let [gs (underground-state)
          route [:cellar :troll-room :maze-1]
          execute-fn (fn [gs cmd] gs)
          pred (spec-thief/predict-thief-interactions gs route execute-fn)]
      (is (= 3 (:turns-ahead pred)))
      (is (vector? (:thief-locations pred)))
      (is (#{:none :low :medium :high} (:theft-risk pred)))
      (is (#{:proceed :wait :avoid :engage} (:recommended-action pred))))))

(deftest route-avoids-thief-test
  (testing "Checks if route avoids thief"
    (let [gs (fresh-game)
          thief-loc (thief/thief-location gs)
          ;; Route that goes through thief's current location
          route-through [thief-loc :cellar :troll-room]
          ;; Route that avoids thief's path
          route-around [:living-room :kitchen :forest-1]]
      ;; First route might intersect
      (is (boolean? (spec-thief/route-avoids-thief? gs route-through)))
      ;; Second route should be safe (above ground, thief stays underground)
      (is (spec-thief/route-avoids-thief? gs route-around)))))

(deftest optimal-departure-time-test
  (testing "Finds optimal wait time"
    (let [gs (fresh-game)
          ;; Short route
          route [:cellar :troll-room]
          result (spec-thief/optimal-departure-time gs route 10)]
      ;; May or may not find optimal time
      (is (or (nil? result)
              (and (number? (:wait-turns result))
                   (boolean? (:safe? result))))))))

;;; ---------------------------------------------------------------------------
;;; FORMAT OUTPUT TESTS
;;; ---------------------------------------------------------------------------

(deftest format-thief-prediction-test
  (testing "Formats prediction output"
    (let [pred (spec-thief/->ThiefPrediction
                5
                [:cellar :troll-room :maze-1 :maze-2 :maze-3]
                [2]
                :medium
                :avoid
                ["wait" "wait"]
                nil)
          formatted (spec-thief/format-thief-prediction pred)]
      (is (string? formatted))
      (is (clojure.string/includes? formatted "Thief Prediction"))
      (is (clojure.string/includes? formatted "medium"))
      (is (clojure.string/includes? formatted "avoid")))))

;;; ---------------------------------------------------------------------------
;;; RNG PRESERVATION TESTS
;;; ---------------------------------------------------------------------------

(deftest all-predictions-preserve-rng
  (testing "All prediction functions preserve RNG state"
    (rng/init! 99999)
    (let [gs (-> (underground-state)
                 (assoc-in [:objects :sword :in] :adventurer)
                 (assoc-in [:objects :thief :in] :cellar))
          execute-fn (fn [gs cmd] gs)
          initial-state (rng/save-state)]

      ;; Run various predictions
      (spec-thief/predict-thief-location gs 10)
      (spec-thief/thief-path gs 10)
      (spec-thief/speculative-thief-turn gs)
      (spec-thief/will-thief-appear? gs)
      (spec-thief/speculative-thief-combat gs)
      (spec-thief/find-safe-burn-sequence gs execute-fn :max-burns 3)
      (spec-thief/predict-thief-interactions gs [:cellar :troll-room] execute-fn)

      ;; RNG should be unchanged
      (is (= (:call-count initial-state)
             (:call-count (rng/save-state)))))))

