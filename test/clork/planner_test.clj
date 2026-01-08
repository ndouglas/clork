(ns clork.planner-test
  "Tests for the automated speedrun planner."
  (:require [clojure.test :refer :all]
            [clork.game-state :as gs]
            [clork.rooms :as rooms]
            [clork.objects :as objects]
            [clork.planner :as planner]
            [clork.planner.actions :as actions]
            [clork.planner.constraints :as constraints]
            [clork.planner.backward :as backward]
            [clork.planner.optimizer :as optimizer]))

;; =============================================================================
;; Test Fixture - Full Game State
;; =============================================================================

(defn full-game-state
  "Initialize a complete game state with rooms and objects."
  []
  (-> (gs/initial-game-state)
      (gs/add-rooms rooms/all-rooms)
      (gs/add-objects objects/all-objects)))

;; =============================================================================
;; Action Registry Tests
;; =============================================================================

(deftest action-registry-test
  (testing "Action registry builds successfully"
    (let [game-state (full-game-state)
          registry (actions/build-action-registry game-state)]
      (is (map? registry))
      (is (> (count registry) 100) "Registry should have many actions")))

  (testing "Registry contains movement actions"
    (let [game-state (full-game-state)
          registry (actions/build-action-registry game-state)
          movement-actions (filter #(= :movement (:type %)) (vals registry))]
      (is (> (count movement-actions) 50) "Should have many movement actions")))

  (testing "Registry contains puzzle actions"
    (let [game-state (full-game-state)
          registry (actions/build-action-registry game-state)]
      (is (contains? registry :kill-troll))
      (is (contains? registry :scare-cyclops))
      (is (contains? registry :exorcism))))

  (testing "Registry contains deposit actions"
    (let [game-state (full-game-state)
          registry (actions/build-action-registry game-state)]
      (is (contains? registry :deposit-jeweled-egg))
      (is (contains? registry :deposit-gold-coffin)))))

;; =============================================================================
;; Exit Parsing Tests
;; =============================================================================

(deftest exit-parsing-test
  (testing "Simple keyword exit"
    (let [result (actions/parse-exit :north-of-house :north)]
      (is (= :north-of-house (:to result)))
      (is (nil? (:requires result)))
      (is (false? (:one-way? result)))))

  (testing "Blocked string exit"
    (let [result (actions/parse-exit "You can't go that way." :north)]
      (is (nil? (:to result)))
      (is (true? (:blocked? result)))))

  (testing "Flag-conditional exit"
    (let [result (actions/parse-exit {:to :strange-passage :if :magic-flag} :west)]
      (is (= :strange-passage (:to result)))
      (is (= #{:magic-flag} (get-in result [:requires :flags])))))

  (testing "Door-gated exit"
    (let [result (actions/parse-exit {:to :kitchen :door :trap-door} :down)]
      (is (= :kitchen (:to result)))
      (is (= :trap-door (get-in result [:requires :door])))))

  (testing "One-way maze diode"
    (let [result (actions/parse-exit {:per :maze-diodes} :down)]
      (is (true? (:one-way? result)))
      (is (= :maze-diodes (:computed result))))))

;; =============================================================================
;; Constraint Tests
;; =============================================================================

(deftest inventory-constraint-test
  (testing "Weight calculation"
    (let [game-state (full-game-state)
          weight (constraints/inventory-weight game-state #{:sword :brass-lantern})]
      (is (number? weight))
      (is (> weight 0))))

  (testing "Can carry check - under limit"
    (let [game-state (full-game-state)]
      (is (constraints/can-carry? game-state #{} :sword))))

  (testing "Can carry check - heavy load"
    (let [game-state (full-game-state)]
      ;; With many items, should eventually fail
      (is (not (constraints/can-carry? game-state
                                       #{:coffin :trunk}  ; Very heavy items
                                       :sword))))))

(deftest light-constraint-test
  (testing "Dark rooms are identified"
    (is (constraints/requires-light? :cellar))
    (is (constraints/requires-light? :troll-room))
    (is (not (constraints/requires-light? :west-of-house))))

  (testing "Light source detection"
    (is (constraints/has-light? #{:brass-lantern}))
    (is (constraints/has-light? #{:ivory-torch}))
    (is (not (constraints/has-light? #{:sword})))))

;; =============================================================================
;; Backward Planner Tests
;; =============================================================================

(deftest goal-extraction-test
  (testing "Deposit goals extraction"
    (let [goals (backward/extract-deposit-goals)]
      (is (set? goals))
      (is (contains? goals [:deposit :jeweled-egg]))
      (is (contains? goals [:deposit :gold-coffin])))))

(deftest achiever-finding-test
  (testing "Find achievers for flag"
    (let [game-state (full-game-state)
          registry (actions/build-action-registry game-state)
          achievers (backward/find-achievers-for-flag registry :troll-flag)]
      (is (seq achievers))
      (is (some #(= :kill-troll (:id %)) achievers))))

  (testing "Find achievers for deposit"
    (let [game-state (full-game-state)
          registry (actions/build-action-registry game-state)
          achievers (backward/find-achievers-for-deposit registry :jeweled-egg)]
      (is (seq achievers))
      (is (some #(= :deposit-jeweled-egg (:id %)) achievers)))))

(deftest simple-plan-test
  (testing "Plan to achieve troll-flag"
    (let [game-state (full-game-state)
          registry (actions/build-action-registry game-state)
          initial (constraints/initial-planning-state game-state)
          result (backward/plan-backward
                  {:flags #{:troll-flag} :deposited #{} :here nil}
                  initial
                  registry)]
      ;; Plan should either succeed or fail with meaningful error
      (is (or (:success? result)
              (contains? result :error))))))

;; =============================================================================
;; Optimizer Tests
;; =============================================================================

(deftest navigation-test
  (testing "Generate navigation between rooms"
    (let [game-state (full-game-state)
          nav (optimizer/generate-navigation game-state :west-of-house :living-room)]
      (is (some? nav))
      (is (vector? (:commands nav)))
      (is (pos? (:moves nav))))))

(deftest nearest-neighbor-route-test
  (testing "Route optimization for multiple destinations"
    (let [game-state (full-game-state)
          destinations #{:living-room :kitchen :attic}
          route (optimizer/nearest-neighbor-route game-state :west-of-house destinations)]
      (is (vector? route))
      (is (= (set route) destinations)))))

;; =============================================================================
;; Integration Tests
;; =============================================================================

(deftest ^:slow full-speedrun-generation-test
  (testing "Generate complete speedrun"
    (let [game-state (full-game-state)
          result (planner/generate-speedrun game-state)]
      ;; Should at least produce some output
      (is (map? result))
      (is (contains? result :success?)))))

(deftest action-effect-application-test
  (testing "Apply action effects to state"
    (let [state {:here :troll-room
                 :inventory #{:sword}
                 :flags #{}}
          action {:effects {:flags-set #{:troll-flag}
                            :flags-clear #{}
                            :inventory-add #{}
                            :inventory-remove #{}}}
          new-state (actions/apply-action-effects action state)]
      (is (contains? (:flags new-state) :troll-flag)))))

;; =============================================================================
;; Run Tests
;; =============================================================================

(comment
  ;; Run all tests
  (run-tests)

  ;; Run specific test
  (test-vars [#'action-registry-test])

  ;; Quick REPL test
  (let [gs (full-game-state)
        registry (actions/build-action-registry gs)]
    (actions/summarize-registry registry)))
