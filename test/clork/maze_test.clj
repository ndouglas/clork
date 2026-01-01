(ns clork.maze-test
  "Tests for maze rooms and navigation."
  (:require [clojure.test :refer [deftest testing is]]
            [clork.game-state :as gs]
            [clork.rooms :as rooms]
            [clork.objects :as objects]
            [clork.verbs-movement :as movement]
            [clork.parser.state :as parser-state]))

(defn full-game-state
  "Create a fully initialized game state with rooms and objects."
  []
  (-> (gs/initial-game-state)
      (gs/add-rooms rooms/all-rooms)
      (gs/add-objects objects/all-objects)))

(defn move-to
  "Move the player to a specific room for testing."
  [game-state room-id]
  (-> game-state
      (assoc :here room-id)
      (assoc-in [:objects :adventurer :in] room-id)))

(defn give-lit-lamp
  "Give the player a lit lamp so they can navigate dark areas."
  [game-state]
  (-> game-state
      (assoc-in [:objects :brass-lantern :in] :adventurer)
      (gs/set-thing-flag :brass-lantern :on)))

(defn walk
  "Simulate walking in a direction."
  [game-state direction]
  (-> game-state
      (assoc-in [:parser :prso] [direction])
      (movement/v-walk)))

;;; ---------------------------------------------------------------------------
;;; MAZE CONNECTIVITY TESTS
;;; ---------------------------------------------------------------------------

(deftest maze-basic-navigation-test
  (testing "can navigate through maze"
    (let [gs (-> (full-game-state)
                 (give-lit-lamp)
                 (move-to :maze-1))]
      ;; maze-1 north -> maze-1 (loops back)
      (is (= :maze-1 (:here (walk gs :north)))
          "maze-1 north should loop back to maze-1")

      ;; maze-1 south -> maze-2
      (is (= :maze-2 (:here (walk gs :south)))
          "maze-1 south should go to maze-2")

      ;; maze-1 west -> maze-4
      (is (= :maze-4 (:here (walk gs :west)))
          "maze-1 west should go to maze-4")

      ;; maze-1 east -> troll-room
      (is (= :troll-room (:here (walk gs :east)))
          "maze-1 east should go to troll-room"))))

(deftest maze-loop-exits-test
  (testing "some maze rooms have loop exits"
    ;; maze-6 west loops back to maze-6
    (let [gs (-> (full-game-state)
                 (give-lit-lamp)
                 (move-to :maze-6))]
      (is (= :maze-6 (:here (walk gs :west)))
          "maze-6 west should loop back to maze-6"))

    ;; maze-8 west loops back to maze-8
    (let [gs (-> (full-game-state)
                 (give-lit-lamp)
                 (move-to :maze-8))]
      (is (= :maze-8 (:here (walk gs :west)))
          "maze-8 west should loop back to maze-8"))

    ;; maze-9 nw loops back to maze-9
    (let [gs (-> (full-game-state)
                 (give-lit-lamp)
                 (move-to :maze-9))]
      (is (= :maze-9 (:here (walk gs :nw)))
          "maze-9 nw should loop back to maze-9"))

    ;; maze-14 nw loops back to maze-14
    (let [gs (-> (full-game-state)
                 (give-lit-lamp)
                 (move-to :maze-14))]
      (is (= :maze-14 (:here (walk gs :nw)))
          "maze-14 nw should loop back to maze-14"))))

(deftest dead-end-navigation-test
  (testing "dead ends have correct exits"
    ;; dead-end-1 south -> maze-4
    (let [gs (-> (full-game-state)
                 (give-lit-lamp)
                 (move-to :dead-end-1))]
      (is (= :maze-4 (:here (walk gs :south)))
          "dead-end-1 south should go to maze-4"))

    ;; dead-end-2 west -> maze-5
    (let [gs (-> (full-game-state)
                 (give-lit-lamp)
                 (move-to :dead-end-2))]
      (is (= :maze-5 (:here (walk gs :west)))
          "dead-end-2 west should go to maze-5"))

    ;; dead-end-3 north -> maze-8
    (let [gs (-> (full-game-state)
                 (give-lit-lamp)
                 (move-to :dead-end-3))]
      (is (= :maze-8 (:here (walk gs :north)))
          "dead-end-3 north should go to maze-8"))

    ;; dead-end-4 south -> maze-12
    (let [gs (-> (full-game-state)
                 (give-lit-lamp)
                 (move-to :dead-end-4))]
      (is (= :maze-12 (:here (walk gs :south)))
          "dead-end-4 south should go to maze-12"))))

;;; ---------------------------------------------------------------------------
;;; MAZE DIODES (ONE-WAY PASSAGES) TESTS
;;; ---------------------------------------------------------------------------

(deftest maze-diodes-test
  (testing "maze diodes are one-way passages"
    ;; maze-2 down -> maze-4 (one-way)
    (let [gs (-> (full-game-state)
                 (give-lit-lamp)
                 (move-to :maze-2))
          result (walk gs :down)]
      (is (= :maze-4 (:here result))
          "maze-2 down should go to maze-4 via diode"))

    ;; maze-7 down -> dead-end-1 (one-way)
    (let [gs (-> (full-game-state)
                 (give-lit-lamp)
                 (move-to :maze-7))
          result (walk gs :down)]
      (is (= :dead-end-1 (:here result))
          "maze-7 down should go to dead-end-1 via diode"))

    ;; maze-9 down -> maze-11 (one-way)
    (let [gs (-> (full-game-state)
                 (give-lit-lamp)
                 (move-to :maze-9))
          result (walk gs :down)]
      (is (= :maze-11 (:here result))
          "maze-9 down should go to maze-11 via diode"))

    ;; maze-12 down -> maze-5 (one-way)
    (let [gs (-> (full-game-state)
                 (give-lit-lamp)
                 (move-to :maze-12))
          result (walk gs :down)]
      (is (= :maze-5 (:here result))
          "maze-12 down should go to maze-5 via diode"))))

;;; ---------------------------------------------------------------------------
;;; GRATING ROOM TESTS
;;; ---------------------------------------------------------------------------

(deftest grating-room-connectivity-test
  (testing "grating room connects to maze"
    ;; grating-room sw -> maze-11
    (let [gs (-> (full-game-state)
                 (give-lit-lamp)
                 (move-to :grating-room))]
      (is (= :maze-11 (:here (walk gs :sw)))
          "grating-room sw should go to maze-11"))))

;;; ---------------------------------------------------------------------------
;;; MAZE OBJECTS TESTS
;;; ---------------------------------------------------------------------------

(deftest maze-5-objects-test
  (testing "maze-5 contains expected objects"
    (let [gs (full-game-state)]
      ;; Check skeleton is in maze-5
      (is (= :maze-5 (gs/get-thing-loc-id gs :skeleton))
          "skeleton should be in maze-5")

      ;; Check burned-out-lantern is in maze-5
      (is (= :maze-5 (gs/get-thing-loc-id gs :burned-out-lantern))
          "burned-out-lantern should be in maze-5")

      ;; Check bag-of-coins is in maze-5
      (is (= :maze-5 (gs/get-thing-loc-id gs :bag-of-coins))
          "bag-of-coins should be in maze-5")

      ;; Check rusty-knife is in maze-5
      (is (= :maze-5 (gs/get-thing-loc-id gs :rusty-knife))
          "rusty-knife should be in maze-5")

      ;; Check skeleton-key is in maze-5
      (is (= :maze-5 (gs/get-thing-loc-id gs :skeleton-key))
          "skeleton-key should be in maze-5"))))

(deftest bag-of-coins-value-test
  (testing "bag of coins has correct treasure values"
    (let [gs (full-game-state)
          bag (gs/get-thing gs :bag-of-coins)]
      (is (= 10 (:value bag))
          "bag of coins should have base value of 10")
      (is (= 5 (:tvalue bag))
          "bag of coins should have trophy case value of 5"))))

;;; ---------------------------------------------------------------------------
;;; PATH TO MAZE-5 (WITH SKELETON) TEST
;;; ---------------------------------------------------------------------------

(deftest path-to-skeleton-test
  (testing "path from troll room to skeleton exists"
    (let [gs (-> (full-game-state)
                 (give-lit-lamp)
                 (move-to :troll-room)
                 (assoc :troll-flag true))]  ; troll is defeated
      ;; troll-room west -> maze-1
      (let [gs1 (walk gs :west)]
        (is (= :maze-1 (:here gs1)))
        ;; maze-1 south -> maze-2
        (let [gs2 (walk gs1 :south)]
          (is (= :maze-2 (:here gs2)))
          ;; maze-2 east -> maze-3
          (let [gs3 (walk gs2 :east)]
            (is (= :maze-3 (:here gs3)))
            ;; maze-3 up -> maze-5
            (let [gs4 (walk gs3 :up)]
              (is (= :maze-5 (:here gs4))
                  "should be able to reach maze-5 via troll-room -> maze-1 -> maze-2 -> maze-3 -> maze-5"))))))))

;;; ---------------------------------------------------------------------------
;;; GRUE ATTACK TESTS
;;; ---------------------------------------------------------------------------

(deftest grue-attack-test
  (testing "walking in darkness triggers grue attack"
    ;; Player in maze without lamp should be eaten by grue when walking
    (let [gs (-> (full-game-state)
                 (move-to :maze-1))
          ;; No lamp, so it's dark
          result (walk gs :north)]
      ;; After grue attack, player should be resurrected in forest-1
      ;; (first death sends you to forest, not permanent death)
      (is (= :forest-1 (:here result))
          "grue attack should resurrect player in forest-1")
      (is (= 1 (:deaths result))
          "grue attack should increment death count")))

  (testing "walking with lit lamp does not trigger grue"
    ;; Player with lit lamp should be able to walk safely
    (let [gs (-> (full-game-state)
                 (give-lit-lamp)
                 (move-to :maze-1))
          result (walk gs :north)]
      (is (= :maze-1 (:here result))
          "with lamp, should walk normally (maze-1 north loops to maze-1)")
      (is (= 0 (:deaths result 0))
          "should not die with lamp"))))
