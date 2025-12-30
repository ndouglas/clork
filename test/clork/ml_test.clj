(ns clork.ml-test
  (:require [clojure.test :refer :all]
            [clork.ml :as ml]
            [clork.game-state :as gs]
            [clork.rooms :as rooms]
            [clork.objects :as objects]
            [clork.verb-defs :as verb-defs]))

(defn init-test-state
  "Create a fresh game state for testing."
  []
  (let [base (gs/initial-game-state)
        with-rooms (gs/add-rooms base rooms/all-rooms)
        with-objects (gs/add-objects with-rooms objects/all-objects)]
    ;; Register object vocabulary
    (verb-defs/register-object-vocabulary! (:objects with-objects))
    ;; Set initial room as lit
    (gs/set-here-flag with-objects :lit)))

(deftest test-get-visible-objects
  (testing "can get visible objects in starting room"
    (let [state (init-test-state)
          visible (ml/get-visible-objects state)]
      (is (vector? visible))
      (is (pos? (count visible)))
      ;; Should see the mailbox at west-of-house
      (is (some #(= :mailbox (:id %)) visible)))))

(deftest test-get-inventory
  (testing "inventory starts empty"
    (let [state (init-test-state)
          inv (ml/get-inventory state)]
      (is (vector? inv))
      (is (empty? inv)))))

(deftest test-get-available-exits
  (testing "can get exits from starting room"
    (let [state (init-test-state)
          exits (ml/get-available-exits state)]
      (is (map? exits))
      (is (pos? (count exits)))
      ;; West-of-house has north, south, west, ne exits
      (is (contains? exits :north))
      (is (contains? exits :south)))))

(deftest test-valid-actions
  (testing "valid-actions returns structured data"
    (let [state (init-test-state)
          actions (ml/valid-actions state)]
      (is (map? actions))
      (is (contains? actions :meta-verbs))
      (is (contains? actions :movement))
      (is (contains? actions :object-actions))
      (is (contains? actions :lit?))
      ;; Meta verbs should include :look
      (is (some #(= :look %) (:meta-verbs actions)))
      ;; Should have some movement options
      (is (pos? (count (get-in actions [:movement :directions]))))
      ;; Should have some object actions
      (is (pos? (count (:object-actions actions)))))))

(deftest test-action-list
  (testing "action-list returns flat list of executable actions"
    (let [state (init-test-state)
          actions (ml/action-list state)]
      (is (vector? actions))
      (is (pos? (count actions)))
      ;; Each action should have a :verb key
      (is (every? :verb actions))
      ;; Should include meta verbs
      (is (some #(= :look (:verb %)) actions))
      ;; Should include movement
      (is (some #(= :go (:verb %)) actions)))))

(deftest test-state-snapshot
  (testing "state-snapshot returns JSON-ready structure"
    (let [state (init-test-state)
          snapshot (ml/state-snapshot state)]
      (is (map? snapshot))
      (is (= 0 (:score snapshot)))
      (is (= 350 (:max-score snapshot)))
      (is (contains? snapshot :room))
      (is (contains? snapshot :valid-actions))
      (is (= :west-of-house (get-in snapshot [:room :id]))))))

(deftest test-object-flags
  (testing "can query object flags"
    (let [state (init-test-state)
          ;; Mailbox should have :cont (container) flag
          mailbox-flags (ml/object-flags state :mailbox)]
      (is (set? mailbox-flags))
      (is (contains? mailbox-flags :cont)))))

(deftest test-json-serialization
  (testing "can serialize snapshot to JSON"
    (let [state (init-test-state)
          snapshot (ml/state-snapshot state)
          json-str (ml/snapshot->json snapshot)]
      (is (string? json-str))
      (is (.contains json-str "score"))
      (is (.contains json-str "west-of-house")))))

(deftest test-execute-action-captures-output
  (testing "execute-action captures output from look"
    (let [state (init-test-state)
          result (ml/execute-action state {:verb :look})]
      (is (string? (:message result)))
      ;; Look should produce output describing the room
      (is (not (empty? (:message result))))
      (is (.contains (:message result) "House"))))

  (testing "execute-action captures output from inventory"
    (let [state (init-test-state)
          result (ml/execute-action state {:verb :inventory})]
      (is (string? (:message result)))
      ;; Inventory when empty should say something
      (is (not (empty? (:message result)))))))

(deftest test-action-parsing
  (testing "can parse action from JSON"
    (let [action (ml/action<-json "{\"verb\": \"look\"}")]
      (is (= :look (:verb action))))
    (let [action (ml/action<-json "{\"verb\": \"take\", \"direct-object\": \"lamp\"}")]
      (is (= :take (:verb action)))
      (is (= :lamp (:direct-object action))))))
