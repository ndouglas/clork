(ns clork.intel.affordances-test
  (:require [clojure.test :refer :all]
            [clork.intel.affordances :as aff]
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
    (verb-defs/register-object-vocabulary! (:objects with-objects))
    (gs/set-here-flag with-objects :lit)))

;;; ---------------------------------------------------------------------------
;;; PRECONDITION CHECKER TESTS
;;; ---------------------------------------------------------------------------

(deftest test-check-object-visible-in-room
  (testing "object-visible returns true for objects in current room"
    (let [gs (init-test-state)
          ;; mailbox is in :west-of-house
          precond {:type :object-visible :object :mailbox}]
      (is (aff/check-precondition gs precond {})))))

(deftest test-check-object-visible-in-inventory
  (testing "object-visible returns true for objects in inventory"
    (let [gs (-> (init-test-state)
                 (gs/move-object :leaflet :adventurer :test))
          precond {:type :object-visible :object :leaflet}]
      (is (aff/check-precondition gs precond {})))))

(deftest test-check-object-visible-not-here
  (testing "object-visible returns false for objects elsewhere"
    (let [gs (init-test-state)
          ;; lamp is in :living-room, we're at :west-of-house
          precond {:type :object-visible :object :brass-lantern}]
      (is (not (aff/check-precondition gs precond {}))))))

(deftest test-check-object-held
  (testing "object-held returns true for objects in inventory"
    (let [gs (-> (init-test-state)
                 (gs/move-object :brass-lantern :adventurer :test))
          precond {:type :object-held :object :brass-lantern}]
      (is (aff/check-precondition gs precond {})))))

(deftest test-check-object-held-not-held
  (testing "object-held returns false for objects not in inventory"
    (let [gs (init-test-state)
          precond {:type :object-held :object :brass-lantern}]
      (is (not (aff/check-precondition gs precond {}))))))

(deftest test-check-object-flag
  (testing "object-flag returns true when object has flag"
    (let [gs (init-test-state)
          ;; brass-lantern has :light flag
          precond {:type :object-flag :object :brass-lantern :flag :light}]
      (is (aff/check-precondition gs precond {})))))

(deftest test-check-object-flag-missing
  (testing "object-flag returns false when object lacks flag"
    (let [gs (init-test-state)
          ;; brass-lantern doesn't have :on flag initially
          precond {:type :object-flag :object :brass-lantern :flag :on}]
      (is (not (aff/check-precondition gs precond {}))))))

(deftest test-check-object-not-flag
  (testing "object-not-flag returns true when object lacks flag"
    (let [gs (init-test-state)
          precond {:type :object-not-flag :object :brass-lantern :flag :on}]
      (is (aff/check-precondition gs precond {})))))

(deftest test-check-game-flag
  (testing "game-flag returns true when game flag is set"
    (let [gs (-> (init-test-state)
                 (gs/set-game-flag :troll-flag))
          precond {:type :game-flag :flag :troll-flag}]
      (is (aff/check-precondition gs precond {})))))

(deftest test-check-game-flag-not-set
  (testing "game-flag returns false when game flag is not set"
    (let [gs (init-test-state)
          precond {:type :game-flag :flag :troll-flag}]
      (is (not (aff/check-precondition gs precond {}))))))

(deftest test-check-game-not-flag
  (testing "game-not-flag returns true when game flag is not set"
    (let [gs (init-test-state)
          precond {:type :game-not-flag :flag :lld-flag}]
      (is (aff/check-precondition gs precond {})))))

(deftest test-check-at-location
  (testing "at-location returns true when player is at specified room"
    (let [gs (init-test-state)
          ;; Player starts at :west-of-house
          precond {:type :at-location :room :west-of-house}]
      (is (aff/check-precondition gs precond {})))))

(deftest test-check-at-location-wrong-room
  (testing "at-location returns false when player is elsewhere"
    (let [gs (init-test-state)
          precond {:type :at-location :room :living-room}]
      (is (not (aff/check-precondition gs precond {}))))))

(deftest test-check-exit-exists
  (testing "exit-exists returns true when room has valid exit"
    (let [gs (init-test-state)
          ;; west-of-house has north exit
          precond {:type :exit-exists :dir :north}]
      (is (aff/check-precondition gs precond {})))))

(deftest test-check-exit-exists-blocked
  (testing "exit-exists returns false for blocked exits (string destinations)"
    (let [gs (-> (init-test-state)
                 (assoc :here :forest-1))
          ;; forest-1 may have string blocked exits
          precond {:type :exit-exists :dir :up}]
      (is (not (aff/check-precondition gs precond {}))))))

;;; ---------------------------------------------------------------------------
;;; SATISFIED? TESTS
;;; ---------------------------------------------------------------------------

(deftest test-satisfied-all-preconds-met
  (testing "satisfied? returns true when all preconditions are met"
    (let [gs (-> (init-test-state)
                 (gs/move-object :brass-lantern :adventurer :test))
          ;; Create affordance with preconds we know are satisfied
          affordance {:preconds [{:type :object-held :object :brass-lantern}
                                 {:type :object-flag :object :brass-lantern :flag :light}]}]
      (is (aff/satisfied? gs affordance {})))))

(deftest test-satisfied-some-preconds-not-met
  (testing "satisfied? returns false when any precondition fails"
    (let [gs (init-test-state)  ;; lamp not in inventory
          affordance {:preconds [{:type :object-held :object :brass-lantern}
                                 {:type :object-flag :object :brass-lantern :flag :light}]}]
      (is (not (aff/satisfied? gs affordance {}))))))

;;; ---------------------------------------------------------------------------
;;; LEGAL-ACTIONS TESTS
;;; ---------------------------------------------------------------------------

(deftest test-legal-actions-returns-reasonable-count
  (testing "legal-actions returns a reasonable number of actions (5-50)"
    (let [gs (init-test-state)
          actions (aff/legal-actions gs)]
      ;; Should be more than 5 (movement + open mailbox)
      (is (>= (count actions) 5))
      ;; Should be less than 50 (not exploring entire action space)
      (is (<= (count actions) 50)))))

(deftest test-legal-actions-includes-movement
  (testing "legal-actions includes movement for valid exits"
    (let [gs (init-test-state)
          actions (aff/legal-actions gs)
          movement-actions (filter #(= :walk (:verb %)) actions)]
      ;; west-of-house has north, south, west, ne exits
      (is (>= (count movement-actions) 3)))))

(deftest test-legal-actions-includes-open-mailbox
  (testing "legal-actions includes opening mailbox at west-of-house"
    (let [gs (init-test-state)
          actions (aff/legal-actions gs)
          open-actions (filter #(and (= :open (:verb %))
                                     (= :mailbox (:direct-object %))) actions)]
      (is (= 1 (count open-actions))))))

(deftest test-legal-actions-excludes-open-when-already-open
  (testing "legal-actions excludes opening already-open containers"
    (let [gs (-> (init-test-state)
                 (gs/set-thing-flag :mailbox :open))
          actions (aff/legal-actions gs)
          open-mailbox (filter #(and (= :open (:verb %))
                                     (= :mailbox (:direct-object %))) actions)]
      (is (empty? open-mailbox)))))

(deftest test-legal-actions-includes-close-when-open
  (testing "legal-actions includes closing open containers"
    (let [gs (-> (init-test-state)
                 (gs/set-thing-flag :mailbox :open))
          actions (aff/legal-actions gs)
          close-mailbox (filter #(and (= :close (:verb %))
                                      (= :mailbox (:direct-object %))) actions)]
      (is (= 1 (count close-mailbox))))))

(deftest test-legal-actions-take-only-takeable
  (testing "legal-actions only includes take for takeable objects"
    (let [gs (init-test-state)
          actions (aff/legal-actions gs)
          take-actions (filter #(= :take (:verb %)) actions)]
      ;; All take actions should be for objects with :take flag
      (doseq [action take-actions]
        (is (gs/set-thing-flag? gs (:direct-object action) :take)
            (str "Should only take takeable objects: " (:direct-object action)))))))

(deftest test-legal-actions-drop-only-held
  (testing "legal-actions only includes drop for held objects"
    (let [gs (-> (init-test-state)
                 (gs/move-object :brass-lantern :adventurer :test)
                 (gs/move-object :sword :adventurer :test))
          actions (aff/legal-actions gs)
          drop-actions (filter #(= :drop (:verb %)) actions)]
      (is (= 2 (count drop-actions)))
      (is (some #(= :brass-lantern (:direct-object %)) drop-actions))
      (is (some #(= :sword (:direct-object %)) drop-actions)))))

(deftest test-legal-actions-lamp-on-for-light-sources
  (testing "legal-actions includes lamp-on for held light sources that are off"
    (let [gs (-> (init-test-state)
                 (gs/move-object :brass-lantern :adventurer :test))
          actions (aff/legal-actions gs)
          lamp-on-actions (filter #(= :lamp-on (:verb %)) actions)]
      (is (= 1 (count lamp-on-actions)))
      (is (= :brass-lantern (:direct-object (first lamp-on-actions)))))))

(deftest test-legal-actions-no-lamp-on-when-already-on
  (testing "legal-actions excludes lamp-on for already-on light sources"
    (let [gs (-> (init-test-state)
                 (gs/move-object :brass-lantern :adventurer :test)
                 (gs/set-thing-flag :brass-lantern :on))
          actions (aff/legal-actions gs)
          lamp-on-actions (filter #(and (= :lamp-on (:verb %))
                                        (= :brass-lantern (:direct-object %))) actions)]
      (is (empty? lamp-on-actions)))))

;;; ---------------------------------------------------------------------------
;;; SPECIAL ACTIONS TESTS
;;; ---------------------------------------------------------------------------

(deftest test-echo-action-at-loud-room
  (testing "echo action is available at loud-room when puzzle not solved"
    (let [gs (-> (init-test-state)
                 (assoc :here :loud-room))
          actions (aff/legal-actions gs)
          echo-actions (filter #(= :echo (:verb %)) actions)]
      (is (= 1 (count echo-actions))))))

(deftest test-no-echo-action-when-solved
  (testing "echo action is not available when loud-flag is set"
    (let [gs (-> (init-test-state)
                 (assoc :here :loud-room)
                 (gs/set-game-flag :loud-flag))
          actions (aff/legal-actions gs)
          echo-actions (filter #(= :echo (:verb %)) actions)]
      (is (empty? echo-actions)))))

(deftest test-exorcism-ring-bell-action
  (testing "ring-bell exorcism action available at entrance-to-hades with bell"
    (let [gs (-> (init-test-state)
                 (assoc :here :entrance-to-hades)
                 (gs/move-object :brass-bell :adventurer :test))
          actions (aff/legal-actions gs)
          ring-actions (filter #(and (= :ring (:verb %))
                                     (= :brass-bell (:direct-object %))) actions)]
      (is (= 1 (count ring-actions))))))

(deftest test-no-exorcism-when-complete
  (testing "exorcism actions not available when lld-flag is set"
    (let [gs (-> (init-test-state)
                 (assoc :here :entrance-to-hades)
                 (gs/move-object :brass-bell :adventurer :test)
                 (gs/set-game-flag :lld-flag))
          actions (aff/legal-actions gs)
          ring-actions (filter #(and (= :ring (:verb %))
                                     (= :brass-bell (:direct-object %))) actions)]
      (is (empty? ring-actions)))))

;;; ---------------------------------------------------------------------------
;;; REGISTRY QUERY TESTS
;;; ---------------------------------------------------------------------------

(deftest test-achievers-of-lld-flag
  (testing "achievers-of finds affordance that sets lld-flag"
    (let [achievers (aff/achievers-of {:type :game-flag :flag :lld-flag})]
      (is (= 1 (count achievers)))
      (is (= :read-book-exorcism (:affordance-id (first achievers)))))))

(deftest test-achievers-of-xb-flag
  (testing "achievers-of finds affordance that sets xb flag"
    (let [achievers (aff/achievers-of {:type :game-flag :flag :xb})]
      (is (= 1 (count achievers)))
      (is (= :ring-bell-exorcism (:affordance-id (first achievers)))))))

(deftest test-achievers-of-loud-flag
  (testing "achievers-of finds affordance that sets loud-flag"
    (let [achievers (aff/achievers-of {:type :game-flag :flag :loud-flag})]
      (is (= 1 (count achievers)))
      (is (= :echo-loud-room (:affordance-id (first achievers)))))))

(deftest test-effects-of-ring-bell
  (testing "effects-of returns declared effects for ring-bell-exorcism"
    (let [effects (aff/effects-of :ring-bell-exorcism)]
      (is (some #(= {:type :set-flag :flag :xb} %) effects))
      (is (some #(= {:type :move-object :object :brass-bell :to :limbo} %) effects)))))

(deftest test-preconditions-of-echo
  (testing "preconditions-of returns declared preconditions for echo-loud-room"
    (let [preconds (aff/preconditions-of :echo-loud-room)]
      (is (some #(= {:type :at-location :room :loud-room} %) preconds))
      (is (some #(= {:type :game-not-flag :flag :loud-flag} %) preconds)))))

(deftest test-get-affordance
  (testing "get-affordance retrieves affordance by ID"
    (let [aff (aff/get-affordance :take-object)]
      (is (some? aff))
      (is (= :take (:verb aff)))
      (is (= "Take a portable object" (:desc aff))))))

(deftest test-list-affordances
  (testing "list-affordances returns all affordance IDs"
    (let [ids (aff/list-affordances)]
      (is (some #{:take-object} ids))
      (is (some #{:drop-object} ids))
      (is (some #{:ring-bell-exorcism} ids))
      (is (some #{:echo-loud-room} ids)))))

;;; ---------------------------------------------------------------------------
;;; EXPLAIN LEGAL ACTIONS TEST
;;; ---------------------------------------------------------------------------

(deftest test-explain-legal-actions
  (testing "explain-legal-actions provides summary breakdown"
    (let [gs (init-test-state)
          explained (aff/explain-legal-actions gs)]
      (is (number? (:count explained)))
      (is (vector? (:actions explained)))
      (is (map? (:summary explained)))
      (is (number? (get-in explained [:summary :movement]))))))
