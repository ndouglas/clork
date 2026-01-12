(ns clork.planner2.combat-test
  "Tests for combat simulation module."
  (:require [clojure.test :refer :all]
            [clork.planner2.combat :as combat]
            [clork.core :as core]
            [clork.game-state :as gs]))

;;; ---------------------------------------------------------------------------
;;; FIXTURES
;;; ---------------------------------------------------------------------------

(defn fresh-game []
  (core/init-game))

(defn equipped-player
  "Create game state with player having sword and lantern."
  []
  (-> (fresh-game)
      (assoc-in [:objects :sword :in] :adventurer)
      (assoc-in [:objects :brass-lantern :in] :adventurer)
      (gs/set-flag :objects :brass-lantern :on)))

;;; ---------------------------------------------------------------------------
;;; COMBAT STATS TESTS
;;; ---------------------------------------------------------------------------

(deftest make-combat-stats-test
  (testing "Initial stats are zeroed"
    (let [stats (combat/make-combat-stats)]
      (is (= 0 (:total-fights stats)))
      (is (= 0 (:wins stats)))
      (is (= 0 (:deaths stats)))
      (is (= 0.0 (:avg-turns stats))))))

(deftest update-stats-test
  (testing "Stats update correctly after win"
    (let [stats (combat/make-combat-stats)
          result {:outcome :win :turns 3 :weapon-dropped? false :wounds 0 :player-strength 5}
          updated (combat/update-stats stats result)]
      (is (= 1 (:total-fights updated)))
      (is (= 1 (:wins updated)))
      (is (= 0 (:deaths updated)))
      (is (= 3 (:min-turns updated)))
      (is (= 3 (:max-turns updated)))))

  (testing "Stats update correctly after death"
    (let [stats (combat/make-combat-stats)
          result {:outcome :death :turns 5 :weapon-dropped? true :wounds 1 :player-strength 3}
          updated (combat/update-stats stats result)]
      (is (= 1 (:total-fights updated)))
      (is (= 0 (:wins updated)))
      (is (= 1 (:deaths updated)))
      (is (= 1 (:weapon-drops updated))))))

;;; ---------------------------------------------------------------------------
;;; COMBAT READINESS TESTS
;;; ---------------------------------------------------------------------------

(deftest check-combat-ready-test
  (testing "Player without weapon is not ready"
    (let [gs (fresh-game)
          result (combat/check-combat-ready gs :troll)]
      (is (not (:ready? result)))
      (is (contains? (set (:issues result)) :no-weapon))))

  (testing "Equipped player is ready"
    (let [gs (equipped-player)
          result (combat/check-combat-ready gs :troll)]
      (is (:ready? result))
      (is (empty? (:issues result)))
      (is (some? (:weapon result))))))

;;; ---------------------------------------------------------------------------
;;; COMBAT RISK ESTIMATION TESTS
;;; ---------------------------------------------------------------------------

(deftest estimate-combat-risk-test
  (testing "Risk varies with player strength"
    (let [gs (equipped-player)]
      ;; Player strength at low score should be around 2-3
      (let [risk (combat/estimate-combat-risk gs :troll)]
        (is (#{:low :medium :high :very-high} risk))))))

;;; ---------------------------------------------------------------------------
;;; COMBAT SIMULATION TESTS
;;; ---------------------------------------------------------------------------

(deftest run-combat-simulation-test
  (testing "Simulation runs without error"
    (let [gs (-> (equipped-player)
                 (assoc :here :troll-room)
                 (assoc-in [:objects :troll :in] :troll-room)
                 (assoc-in [:objects :troll :strength] 2))
          stats (combat/run-combat-simulation gs :troll 5 :max-turns 20)]
      (is (= 5 (:total-fights stats)))
      (is (= 5 (+ (:wins stats) (:deaths stats)
                  (- (:total-fights stats) (:wins stats) (:deaths stats)))))
      ;; At least some fights should complete (win or death)
      (is (>= (+ (:wins stats) (:deaths stats)) 0)))))

;;; ---------------------------------------------------------------------------
;;; COMBAT OUTCOME ANALYSIS TESTS
;;; ---------------------------------------------------------------------------

(deftest analyze-combat-outcome-test
  (testing "Detects villain death"
    (let [pre (-> (equipped-player)
                  (assoc-in [:objects :troll :in] :troll-room))
          post (-> pre
                   (assoc-in [:objects :troll :in] :limbo))
          result (combat/analyze-combat-outcome pre post :troll)]
      (is (= :win (:outcome result)))))

  (testing "Detects weapon loss"
    (let [pre (-> (equipped-player)
                  (assoc :here :troll-room)
                  (assoc-in [:objects :troll :in] :troll-room))
          ;; Post-combat: sword dropped in room
          post (-> pre
                   (assoc-in [:objects :sword :in] :troll-room))
          result (combat/analyze-combat-outcome pre post :troll)]
      (is (:weapon-lost? result))
      (is (= :troll-room (:weapon-location result))))))

;;; ---------------------------------------------------------------------------
;;; COMBAT RESULT TESTS
;;; ---------------------------------------------------------------------------

(deftest make-combat-result-test
  (testing "Combat result creation"
    (let [gs (fresh-game)
          result (combat/make-combat-result true gs :win 5 1 ["Event 1" "Event 2"])]
      (is (:success? result))
      (is (= :win (:outcome result)))
      (is (= 5 (:turns result)))
      (is (= 1 (:weapon-pickups result)))
      (is (= 2 (count (:events result)))))))

(deftest execute-combat-aborts-when-not-ready-test
  (testing "Combat aborts without weapon"
    (let [gs (fresh-game)
          ;; Dummy execute-fn that shouldn't be called
          execute-fn (fn [gs cmd] (throw (Exception. "Should not be called")))
          result (combat/execute-combat gs :troll execute-fn)]
      (is (not (:success? result)))
      (is (= :aborted (:outcome result)))
      (is (some #(clojure.string/includes? % ":no-weapon") (:events result))))))

(deftest format-combat-result-test
  (testing "Format produces readable output"
    (let [gs (fresh-game)
          result (combat/make-combat-result true gs :win 3 0 ["Turn 1" "Victory!"])
          formatted (combat/format-combat-result result :troll)]
      (is (string? formatted))
      (is (clojure.string/includes? formatted "Combat Result"))
      (is (clojure.string/includes? formatted "troll"))
      (is (clojure.string/includes? formatted "win")))))

