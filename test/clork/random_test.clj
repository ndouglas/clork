(ns clork.random-test
  (:require [clojure.test :refer [deftest testing is]]
            [clork.random :as random]))

(deftest seeded-random-test
  (testing "same seed produces same sequence"
    (random/init! 12345)
    (let [seq1 [(random/rand-int* 100)
                (random/rand-int* 100)
                (random/rand-int* 100)]]
      (random/init! 12345)
      (let [seq2 [(random/rand-int* 100)
                  (random/rand-int* 100)
                  (random/rand-int* 100)]]
        (is (= seq1 seq2) "Same seed should produce identical sequence"))))

  (testing "different seeds produce different sequences"
    (random/init! 12345)
    (let [seq1 [(random/rand-int* 100)
                (random/rand-int* 100)
                (random/rand-int* 100)]]
      (random/init! 54321)
      (let [seq2 [(random/rand-int* 100)
                  (random/rand-int* 100)
                  (random/rand-int* 100)]]
        (is (not= seq1 seq2) "Different seeds should produce different sequences")))))

(deftest rand-nth-test
  (testing "rand-nth* returns element from collection"
    (random/init! 42)
    (let [coll [:a :b :c :d :e]
          result (random/rand-nth* coll)]
      (is (contains? (set coll) result))))

  (testing "rand-nth* is reproducible with seed"
    (random/init! 999)
    (let [coll [:a :b :c :d :e]
          result1 (random/rand-nth* coll)]
      (random/init! 999)
      (let [result2 (random/rand-nth* coll)]
        (is (= result1 result2))))))

(deftest shuffle-test
  (testing "shuffle* is reproducible with seed"
    (random/init! 777)
    (let [coll [1 2 3 4 5]
          shuffled1 (random/shuffle* coll)]
      (random/init! 777)
      (let [shuffled2 (random/shuffle* coll)]
        (is (= shuffled1 shuffled2)))))

  (testing "shuffle* contains all original elements"
    (random/init! 123)
    (let [coll [1 2 3 4 5]
          shuffled (random/shuffle* coll)]
      (is (= (set coll) (set shuffled))))))
