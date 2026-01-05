(ns clork.treasures-test
  "Tests for treasure objects in Clork."
  (:require [clojure.test :refer :all]
            [clork.core :as core]
            [clork.game-state :as gs]))

;;; ---------------------------------------------------------------------------
;;; JADE FIGURINE
;;; ---------------------------------------------------------------------------

(deftest jade-figurine-exists-test
  (testing "Jade figurine exists with correct properties"
    (let [gs (core/init-game)
          jade (gs/get-thing gs :jade-figurine)]
      (is (some? jade) "Jade figurine should exist")
      (is (= :bat-room (:in jade)) "Jade figurine should be in bat-room")
      (is (= "jade figurine" (:desc jade)))
      (is (= 5 (:value jade)) "Jade figurine base value should be 5")
      (is (= 5 (:tvalue jade)) "Jade figurine trophy value should be 5")
      (is (= 10 (:size jade)) "Jade figurine size should be 10")
      (is (contains? (:flags jade) :take) "Jade figurine should be takeable"))))

;;; ---------------------------------------------------------------------------
;;; CRYSTAL TRIDENT
;;; ---------------------------------------------------------------------------

(deftest crystal-trident-exists-test
  (testing "Crystal trident exists with correct properties"
    (let [gs (core/init-game)
          trident (gs/get-thing gs :crystal-trident)]
      (is (some? trident) "Crystal trident should exist")
      (is (= :atlantis-room (:in trident)) "Crystal trident should be in atlantis-room")
      (is (= "crystal trident" (:desc trident)))
      (is (= 4 (:value trident)) "Crystal trident base value should be 4")
      (is (= 11 (:tvalue trident)) "Crystal trident trophy value should be 11")
      (is (= 20 (:size trident)) "Crystal trident size should be 20")
      (is (contains? (:flags trident) :take) "Crystal trident should be takeable"))))

(deftest crystal-trident-fdesc-test
  (testing "Crystal trident has first-time description"
    (let [gs (core/init-game)
          trident (gs/get-thing gs :crystal-trident)]
      (is (some? (:fdesc trident)) "Crystal trident should have FDESC")
      (is (re-find #"Poseidon" (:fdesc trident))
          "Crystal trident FDESC should mention Poseidon"))))

;;; ---------------------------------------------------------------------------
;;; SAPPHIRE BRACELET
;;; ---------------------------------------------------------------------------

(deftest sapphire-bracelet-exists-test
  (testing "Sapphire bracelet exists with correct properties"
    (let [gs (core/init-game)
          bracelet (gs/get-thing gs :sapphire-bracelet)]
      (is (some? bracelet) "Sapphire bracelet should exist")
      (is (= :gas-room (:in bracelet)) "Sapphire bracelet should be in gas-room")
      (is (= "sapphire-encrusted bracelet" (:desc bracelet)))
      (is (= 5 (:value bracelet)) "Sapphire bracelet base value should be 5")
      (is (= 5 (:tvalue bracelet)) "Sapphire bracelet trophy value should be 5")
      (is (= 10 (:size bracelet)) "Sapphire bracelet size should be 10")
      (is (contains? (:flags bracelet) :take) "Sapphire bracelet should be takeable"))))

;;; ---------------------------------------------------------------------------
;;; JEWELED SCARAB
;;; ---------------------------------------------------------------------------

(deftest jeweled-scarab-exists-test
  (testing "Jeweled scarab exists with correct properties"
    (let [gs (core/init-game)
          scarab (gs/get-thing gs :jeweled-scarab)]
      (is (some? scarab) "Jeweled scarab should exist")
      (is (= :sandy-cave (:in scarab)) "Jeweled scarab should be in sandy-cave")
      (is (= "beautiful jeweled scarab" (:desc scarab)))
      (is (= 5 (:value scarab)) "Jeweled scarab base value should be 5")
      (is (= 5 (:tvalue scarab)) "Jeweled scarab trophy value should be 5")
      (is (= 8 (:size scarab)) "Jeweled scarab size should be 8")
      (is (contains? (:flags scarab) :take) "Jeweled scarab should be takeable"))))

(deftest jeweled-scarab-invisible-test
  (testing "Jeweled scarab starts invisible (hidden in sand)"
    (let [gs (core/init-game)
          scarab (gs/get-thing gs :jeweled-scarab)]
      (is (contains? (:flags scarab) :invisible)
          "Jeweled scarab should have INVISIBLE flag (hidden until dug up)"))))

;;; ---------------------------------------------------------------------------
;;; SILVER CHALICE
;;; ---------------------------------------------------------------------------

(deftest silver-chalice-exists-test
  (testing "Silver chalice exists with correct properties"
    (let [gs (core/init-game)
          chalice (gs/get-thing gs :silver-chalice)]
      (is (some? chalice) "Silver chalice should exist")
      (is (= :treasure-room (:in chalice)) "Silver chalice should be in treasure-room")
      (is (= "chalice" (:desc chalice)))
      (is (= 10 (:value chalice)) "Silver chalice base value should be 10")
      (is (= 5 (:tvalue chalice)) "Silver chalice trophy value should be 5")
      (is (= 10 (:size chalice)) "Silver chalice size should be 10")
      (is (= 5 (:capacity chalice)) "Silver chalice capacity should be 5")
      (is (contains? (:flags chalice) :take) "Silver chalice should be takeable")
      (is (contains? (:flags chalice) :cont) "Silver chalice should be a container"))))

(deftest silver-chalice-has-action-test
  (testing "Silver chalice has action handler"
    (let [gs (core/init-game)
          chalice (gs/get-thing gs :silver-chalice)]
      (is (fn? (:action chalice)) "Silver chalice should have action handler"))))

;;; ---------------------------------------------------------------------------
;;; TREASURE SUMMARY
;;; ---------------------------------------------------------------------------

(deftest all-treasures-in-correct-rooms-test
  (testing "All implemented treasures are in their correct starting rooms"
    (let [gs (core/init-game)
          treasures {:jade-figurine :bat-room
                     :crystal-trident :atlantis-room
                     :sapphire-bracelet :gas-room
                     :jeweled-scarab :sandy-cave
                     :silver-chalice :treasure-room
                     :platinum-bar :loud-room
                     :bag-of-coins :maze-5
                     :painting :gallery
                     :egg :nest}]
      (doseq [[treasure-id expected-room] treasures]
        (let [treasure (gs/get-thing gs treasure-id)]
          (is (= expected-room (:in treasure))
              (str treasure-id " should be in " expected-room)))))))
