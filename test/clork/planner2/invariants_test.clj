(ns clork.planner2.invariants-test
  "Tests for state invariant checking."
  (:require [clojure.test :refer :all]
            [clork.planner2.invariants :as inv]
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

;;; ---------------------------------------------------------------------------
;;; INDIVIDUAL CHECK TESTS
;;; ---------------------------------------------------------------------------

(deftest check-has-item-test
  (testing "Player without item fails check"
    (let [gs (fresh-game)
          result (inv/check-has-item gs :sword)]
      (is (not (:passed? result)))
      (is (= :critical (:severity result)))))

  (testing "Player with item passes check"
    (let [gs (equipped-player)
          result (inv/check-has-item gs :sword)]
      (is (:passed? result)))))

(deftest check-has-light-test
  (testing "Player in lit room passes"
    (let [gs (fresh-game)
          result (inv/check-has-light gs)]
      ;; West of house is lit
      (is (:passed? result))))

  (testing "Player with lantern on passes"
    (let [gs (-> (equipped-player)
                 (assoc :here :cellar))
          result (inv/check-has-light gs)]
      (is (:passed? result)))))

(deftest check-at-room-test
  (testing "Player at expected room passes"
    (let [gs (fresh-game)
          result (inv/check-at-room gs :west-of-house)]
      (is (:passed? result))))

  (testing "Player at wrong room fails"
    (let [gs (fresh-game)
          result (inv/check-at-room gs :kitchen)]
      (is (not (:passed? result))))))

(deftest check-player-alive-test
  (testing "Live player passes"
    (let [gs (fresh-game)
          result (inv/check-player-alive gs)]
      (is (:passed? result)))))

(deftest check-flag-set-test
  (testing "Set flag passes"
    (let [gs (-> (fresh-game)
                 (gs/set-flag :objects :trophy-case :open))
          result (inv/check-flag-set gs :objects :trophy-case :open)]
      (is (:passed? result))))

  (testing "Unset flag fails"
    (let [gs (fresh-game)
          result (inv/check-flag-set gs :objects :trophy-case :open)]
      (is (not (:passed? result))))))

(deftest check-object-at-test
  (testing "Object at expected location passes"
    (let [gs (equipped-player)
          result (inv/check-object-at gs :sword :adventurer)]
      (is (:passed? result))))

  (testing "Object at wrong location fails"
    (let [gs (fresh-game)
          result (inv/check-object-at gs :sword :adventurer)]
      (is (not (:passed? result))))))

;;; ---------------------------------------------------------------------------
;;; VERIFY INVARIANTS TEST
;;; ---------------------------------------------------------------------------

(deftest verify-invariants-test
  (testing "All passing invariants"
    (let [gs (equipped-player)
          invariants [(inv/check-player-alive gs)
                      (inv/check-has-item gs :sword)]
          result (inv/verify-invariants gs invariants)]
      (is (:passed? result))
      (is (empty? (:critical-failures result)))))

  (testing "Mixed results - critical failure"
    (let [gs (fresh-game)
          invariants [(inv/check-player-alive gs)
                      (inv/check-has-item gs :sword)]  ; Will fail
          result (inv/verify-invariants gs invariants)]
      (is (not (:passed? result)))
      (is (= 1 (count (:critical-failures result))))))

  (testing "Warning only does not fail overall"
    (let [gs (fresh-game)
          invariants [(inv/check-player-alive gs)
                      (inv/check-has-item gs :sword :severity :warning)]
          result (inv/verify-invariants gs invariants)]
      (is (:passed? result))
      (is (= 1 (count (:warnings result)))))))

;;; ---------------------------------------------------------------------------
;;; GOAL PRECONDITION TESTS
;;; ---------------------------------------------------------------------------

(deftest verify-goal-preconditions-test
  (testing "Navigation goal preconditions"
    (let [gs (fresh-game)
          goal {:type :at-room :room :kitchen}
          result (inv/verify-goal-preconditions gs goal)]
      ;; Player is alive, room is lit
      (is (:passed? result))))

  (testing "Combat goal preconditions - not ready"
    (let [gs (-> (fresh-game)
                 (assoc :here :troll-room))
          goal {:type :kill-enemy :enemy :troll :room :troll-room}
          result (inv/verify-goal-preconditions gs goal)]
      ;; No weapon, no light
      (is (not (:passed? result)))))

  (testing "Combat goal preconditions - ready"
    (let [gs (-> (equipped-player)
                 (assoc :here :troll-room)
                 (assoc-in [:objects :troll :in] :troll-room)
                 (assoc-in [:objects :troll :strength] 2))
          goal {:type :kill-enemy :enemy :troll :room :troll-room}
          result (inv/verify-goal-preconditions gs goal)]
      (is (:passed? result))))

  (testing "Take item goal preconditions"
    (let [gs (-> (equipped-player)
                 (assoc :here :living-room))
          goal {:type :have-item :item :sword}
          result (inv/verify-goal-preconditions gs goal)]
      (is (:passed? result))))

  (testing "Deposit goal preconditions - case closed"
    (let [gs (-> (equipped-player)
                 (assoc :here :living-room)
                 (assoc-in [:objects :egg :in] :adventurer))
          goal {:type :item-deposited :item :egg}
          result (inv/verify-goal-preconditions gs goal)]
      ;; Trophy case is closed
      (is (not (:passed? result)))))

  (testing "Deposit goal preconditions - all met"
    (let [gs (-> (equipped-player)
                 (assoc :here :living-room)
                 (assoc-in [:objects :egg :in] :adventurer)
                 (gs/set-flag :objects :trophy-case :open))
          goal {:type :item-deposited :item :egg}
          result (inv/verify-goal-preconditions gs goal)]
      (is (:passed? result)))))

;;; ---------------------------------------------------------------------------
;;; STATE CONSISTENCY TESTS
;;; ---------------------------------------------------------------------------

(deftest check-state-consistency-test
  (testing "Fresh game has some warnings"
    (let [gs (fresh-game)
          result (inv/check-state-consistency gs)]
      ;; Should pass but have warnings (no weapon)
      (is (:passed? result))
      (is (pos? (count (:warnings result))))))

  (testing "Equipped player passes most checks"
    (let [gs (equipped-player)
          result (inv/check-state-consistency gs)]
      (is (:passed? result)))))

;;; ---------------------------------------------------------------------------
;;; FORMAT OUTPUT TEST
;;; ---------------------------------------------------------------------------

(deftest format-result-test
  (testing "Format produces readable output"
    (let [gs (fresh-game)
          result (inv/check-state-consistency gs)
          formatted (inv/format-result result)]
      (is (string? formatted))
      (is (clojure.string/includes? formatted "Invariant Check"))
      (is (clojure.string/includes? formatted "Overall:")))))

