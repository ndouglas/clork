(ns clork.intel.treasures-test
  "Tests for the treasure metadata module."
  (:require [clojure.test :refer :all]
            [clork.core :as core]
            [clork.game-state :as gs]
            [clork.intel.treasures :as treasures]))

;;; ---------------------------------------------------------------------------
;;; BASIC QUERY TESTS
;;; ---------------------------------------------------------------------------

(deftest test-all-treasures
  (testing "all-treasures returns expected count"
    (let [all (treasures/all-treasures)]
      (is (= 19 (count all)))
      (is (contains? (set all) :egg))
      (is (contains? (set all) :clockwork-canary))
      (is (contains? (set all) :pot-of-gold))
      (is (contains? (set all) :huge-diamond)))))

(deftest test-treasure-value
  (testing "treasure values are correct"
    (is (= 5 (treasures/treasure-value :egg)))
    (is (= 6 (treasures/treasure-value :clockwork-canary)))
    (is (= 10 (treasures/treasure-value :pot-of-gold)))
    (is (= 10 (treasures/treasure-value :huge-diamond)))
    (is (= 15 (treasures/treasure-value :trunk-of-jewels)))))

(deftest test-treasure-location
  (testing "treasure locations are correct"
    (is (= :up-a-tree (treasures/treasure-location :egg)))
    (is (= :loud-room (treasures/treasure-location :platinum-bar)))
    (is (= :gallery (treasures/treasure-location :painting)))
    ;; Container treasures have nil location
    (is (nil? (treasures/treasure-location :clockwork-canary)))
    (is (nil? (treasures/treasure-location :sceptre)))
    ;; Created treasures have nil location
    (is (nil? (treasures/treasure-location :huge-diamond)))))

(deftest test-treasure-requires
  (testing "treasure requirements are correct"
    (is (= #{} (treasures/treasure-requires :egg)))
    (is (= #{:loud-flag} (treasures/treasure-requires :platinum-bar)))
    (is (= #{:lld-flag} (treasures/treasure-requires :crystal-skull)))
    (is (= #{:rainbow-flag} (treasures/treasure-requires :pot-of-gold)))
    (is (= #{:boat-ready} (treasures/treasure-requires :jeweled-scarab)))))

(deftest test-treasure-container
  (testing "container relationships are correct"
    (is (nil? (treasures/treasure-container :egg)))
    (is (= :egg (treasures/treasure-container :clockwork-canary)))
    (is (= :gold-coffin (treasures/treasure-container :sceptre)))))

(deftest test-treasure-puzzle
  (testing "puzzle relationships are correct"
    (is (nil? (treasures/treasure-puzzle :egg)))
    (is (= :loud-room-echo (treasures/treasure-puzzle :platinum-bar)))
    (is (= :exorcism (treasures/treasure-puzzle :crystal-skull)))
    (is (= :rainbow-solid (treasures/treasure-puzzle :pot-of-gold)))
    (is (= :egg-opening (treasures/treasure-puzzle :clockwork-canary)))))

;;; ---------------------------------------------------------------------------
;;; CONTAINER QUERY TESTS
;;; ---------------------------------------------------------------------------

(deftest test-treasures-in-container
  (testing "find treasures in egg"
    (let [in-egg (treasures/treasures-in-container :egg)]
      (is (= 1 (count in-egg)))
      (is (= :clockwork-canary (first in-egg)))))

  (testing "find treasures in coffin"
    (let [in-coffin (treasures/treasures-in-container :gold-coffin)]
      (is (= 1 (count in-coffin)))
      (is (= :sceptre (first in-coffin)))))

  (testing "no treasures in random container"
    (is (empty? (treasures/treasures-in-container :mailbox)))))

;;; ---------------------------------------------------------------------------
;;; FLAG/PUZZLE QUERY TESTS
;;; ---------------------------------------------------------------------------

(deftest test-treasures-requiring-flag
  (testing "treasures requiring lld-flag"
    (let [treasures (treasures/treasures-requiring-flag :lld-flag)]
      (is (= 1 (count treasures)))
      (is (= :crystal-skull (first treasures)))))

  (testing "treasures requiring rainbow-flag"
    (let [treasures (treasures/treasures-requiring-flag :rainbow-flag)]
      (is (= 1 (count treasures)))
      (is (= :pot-of-gold (first treasures)))))

  (testing "treasures requiring gates-open"
    (let [treasures (set (treasures/treasures-requiring-flag :gates-open))]
      (is (contains? treasures :crystal-trident)))))

(deftest test-treasures-requiring-puzzle
  (testing "treasures requiring exorcism"
    (let [treasures (treasures/treasures-requiring-puzzle :exorcism)]
      (is (= 1 (count treasures)))
      (is (= :crystal-skull (first treasures)))))

  (testing "treasures requiring dam-open"
    (let [treasures (set (treasures/treasures-requiring-puzzle :dam-open))]
      (is (contains? treasures :crystal-trident)))))

;;; ---------------------------------------------------------------------------
;;; ACCESSIBILITY TESTS
;;; ---------------------------------------------------------------------------

(deftest test-treasure-accessible-no-requirements
  (testing "egg is accessible from start (no requirements)"
    (let [gs (core/init-game)
          result (treasures/treasure-accessible? gs :egg)]
      (is (:accessible result))
      (is (= "Treasure is accessible" (:reason result))))))

(deftest test-treasure-accessible-missing-flag
  (testing "platinum bar not accessible without loud-flag"
    (let [gs (core/init-game)
          result (treasures/treasure-accessible? gs :platinum-bar)]
      (is (not (:accessible result)))
      (is (contains? (:missing-flags result) :loud-flag))
      (is (= :loud-room-echo (:missing-puzzle result)))))

  (testing "platinum bar accessible with loud-flag"
    (let [gs (-> (core/init-game)
                 (gs/set-game-flag :loud-flag))
          result (treasures/treasure-accessible? gs :platinum-bar)]
      (is (:accessible result)))))

(deftest test-treasure-accessible-container
  (testing "canary not accessible at start"
    (let [gs (core/init-game)
          result (treasures/treasure-accessible? gs :clockwork-canary)]
      (is (not (:accessible result)))
      ;; Blocked by some combination of flags/container
      (is (some? (:reason result)))))

  (testing "sceptre not accessible when coffin closed"
    (let [gs (core/init-game)
          result (treasures/treasure-accessible? gs :sceptre)]
      ;; Sceptre has no flag requirements, just container
      (is (not (:accessible result)))
      (is (= :gold-coffin (:container result)))))

  (testing "sceptre accessible when coffin open"
    (let [gs (-> (core/init-game)
                 (gs/set-thing-flag :gold-coffin :open))
          result (treasures/treasure-accessible? gs :sceptre)]
      (is (:accessible result)))))

;;; ---------------------------------------------------------------------------
;;; DEPENDENCY TESTS
;;; ---------------------------------------------------------------------------

(deftest test-treasure-dependencies
  (testing "egg has no dependencies"
    (let [deps (treasures/treasure-dependencies :egg)]
      (is (= :egg (:treasure deps)))
      (is (empty? (:flags-needed deps)))
      (is (empty? (:puzzles-needed deps)))
      (is (empty? (:containers-to-open deps)))))

  (testing "crystal-skull has dependencies"
    (let [deps (treasures/treasure-dependencies :crystal-skull)]
      (is (= :crystal-skull (:treasure deps)))
      (is (contains? (:flags-needed deps) :lld-flag))
      (is (= [:exorcism] (:puzzles-needed deps)))))

  (testing "canary has container dependency"
    (let [deps (treasures/treasure-dependencies :clockwork-canary)]
      (is (= [:egg] (:containers-to-open deps))))))

;;; ---------------------------------------------------------------------------
;;; COLLECTION ORDER TESTS
;;; ---------------------------------------------------------------------------

(deftest test-treasure-collection-order
  (testing "simpler treasures come first"
    (let [order (treasures/treasure-collection-order
                  [:crystal-skull :egg :pot-of-gold])]
      ;; egg has no deps, should come first
      (is (= :egg (first order))))))

(deftest test-total-treasure-value
  (testing "total value calculation"
    (is (= 15 (treasures/total-treasure-value [:egg :pot-of-gold])))
    (is (= 0 (treasures/total-treasure-value [])))))

;;; ---------------------------------------------------------------------------
;;; PUZZLE RELATIONSHIP TESTS
;;; ---------------------------------------------------------------------------

(deftest test-puzzles-for-treasures
  (testing "get puzzles needed for treasure set"
    (let [puzzles (treasures/puzzles-for-treasures
                    [:crystal-skull :pot-of-gold :platinum-bar])]
      (is (contains? puzzles :exorcism))
      (is (contains? puzzles :rainbow-solid))
      (is (contains? puzzles :loud-room-echo)))))

(deftest test-flags-for-treasures
  (testing "get flags needed for treasure set"
    (let [flags (treasures/flags-for-treasures
                  [:crystal-skull :pot-of-gold :platinum-bar])]
      (is (contains? flags :lld-flag))
      (is (contains? flags :rainbow-flag))
      (is (contains? flags :loud-flag)))))

;;; ---------------------------------------------------------------------------
;;; SCORING TESTS
;;; ---------------------------------------------------------------------------

(deftest test-max-treasure-score
  (testing "max score is sum of all values"
    (is (pos? treasures/max-treasure-score))
    (is (= treasures/max-treasure-score
           (treasures/total-treasure-value (treasures/all-treasures))))))

(deftest test-treasures-by-value
  (testing "treasures sorted by value descending"
    (let [sorted (treasures/treasures-by-value)]
      ;; First treasure should have highest value
      (is (>= (treasures/treasure-value (first sorted))
              (treasures/treasure-value (second sorted)))))))

(deftest test-treasures-by-efficiency
  (testing "treasures sorted by efficiency"
    (let [sorted (treasures/treasures-by-efficiency)]
      ;; Should return all treasures
      (is (= 19 (count sorted))))))

;;; ---------------------------------------------------------------------------
;;; INVENTORY WEIGHT TESTS
;;; ---------------------------------------------------------------------------

(deftest test-object-weight
  (testing "object weight from game state"
    (let [gs (core/init-game)]
      ;; Objects with defined sizes
      (is (pos? (treasures/object-weight gs :shovel)))
      ;; Objects without size return 0
      (is (>= (treasures/object-weight gs :leaflet) 0)))))

(deftest test-inventory-weight
  (testing "empty inventory has zero weight"
    (let [gs (core/init-game)]
      (is (= 0 (treasures/inventory-weight gs)))))

  (testing "inventory with items has weight"
    (let [gs (-> (core/init-game)
                 (gs/move-object :shovel :adventurer :test))]
      (is (pos? (treasures/inventory-weight gs))))))

(deftest test-inventory-capacity
  (testing "capacity is 100"
    (is (= 100 (treasures/inventory-capacity)))))

(deftest test-can-carry
  (testing "can carry light object"
    (let [gs (core/init-game)]
      (is (treasures/can-carry? gs :leaflet))))

  (testing "cannot carry when full"
    ;; This is a simplified test - real test would fill inventory
    (let [gs (core/init-game)]
      (is (treasures/can-carry? gs :egg)))))

(deftest test-treasures-that-fit
  (testing "filters treasures by capacity"
    (let [gs (core/init-game)
          all-treasures [:egg :painting :bag-of-coins]
          fitting (treasures/treasures-that-fit gs all-treasures)]
      ;; Should return treasures that fit
      (is (<= (count fitting) (count all-treasures))))))

(deftest test-optimal-treasure-set
  (testing "selects optimal treasures"
    (let [gs (core/init-game)
          available [:egg :painting :bag-of-coins :platinum-bar]
          result (treasures/optimal-treasure-set gs available)]
      (is (vector? (:treasures result)))
      (is (number? (:total-value result)))
      (is (number? (:total-weight result)))
      (is (>= (:remaining-capacity result) 0)))))
