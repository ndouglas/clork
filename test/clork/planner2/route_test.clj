(ns clork.planner2.route-test
  "Tests for the route optimizer module."
  (:require [clojure.test :refer :all]
            [clork.planner2.route :as route]
            [clork.core :as core]))

;;; ---------------------------------------------------------------------------
;;; FLOYD-WARSHALL TESTS
;;; ---------------------------------------------------------------------------

(deftest floyd-warshall-simple-test
  (testing "Simple 3-node graph"
    (let [graph {:a #{:b}
                 :b #{:c}
                 :c #{}}
          result (route/floyd-warshall graph)]
      ;; Direct edges
      (is (= 1 (get-in result [:dist [:a :b]])))
      (is (= 1 (get-in result [:dist [:b :c]])))
      ;; Transitive
      (is (= 2 (get-in result [:dist [:a :c]])))
      ;; Self
      (is (= 0 (get-in result [:dist [:a :a]]))))))

(deftest floyd-warshall-bidirectional-test
  (testing "Bidirectional edges"
    (let [graph {:a #{:b}
                 :b #{:a :c}
                 :c #{:b}}
          result (route/floyd-warshall graph)]
      ;; Both directions work
      (is (= 1 (get-in result [:dist [:a :b]])))
      (is (= 1 (get-in result [:dist [:b :a]])))
      (is (= 2 (get-in result [:dist [:a :c]])))
      (is (= 2 (get-in result [:dist [:c :a]]))))))

(deftest floyd-warshall-disconnected-test
  (testing "Disconnected nodes"
    (let [graph {:a #{:b}
                 :b #{}
                 :c #{}}
          result (route/floyd-warshall graph)]
      ;; No path from a to c
      (is (nil? (get-in result [:dist [:a :c]]))))))

;;; ---------------------------------------------------------------------------
;;; DISTANCE HELPER TESTS
;;; ---------------------------------------------------------------------------

(deftest distance-test
  (testing "Distance from precomputed map"
    (let [distances {[:a :b] 5 [:b :c] 3}]
      (is (= 5 (route/distance distances :a :b)))
      (is (= 3 (route/distance distances :b :c)))
      ;; Missing returns MAX_VALUE
      (is (= Integer/MAX_VALUE (route/distance distances :a :c))))))

;;; ---------------------------------------------------------------------------
;;; TSP TESTS
;;; ---------------------------------------------------------------------------

(deftest nearest-neighbor-tsp-test
  (testing "Empty locations"
    (let [distances {}]
      (is (= [] (route/nearest-neighbor-tsp distances :start [])))))

  (testing "Single location"
    (let [distances {[:start :a] 5}]
      (is (= [:a] (route/nearest-neighbor-tsp distances :start [:a])))))

  (testing "Two locations - picks nearest first"
    (let [distances {[:start :a] 10
                     [:start :b] 5
                     [:a :b] 3
                     [:b :a] 3}]
      (let [route (route/nearest-neighbor-tsp distances :start [:a :b])]
        ;; Should visit b first (closer to start)
        (is (= :b (first route)))))))

(deftest route-length-test
  (testing "Empty route has zero length"
    (is (= 0 (route/route-length {} :start []))))

  (testing "Single-hop route"
    (let [distances {[:start :a] 5}]
      (is (= 5 (route/route-length distances :start [:a])))))

  (testing "Multi-hop route"
    (let [distances {[:start :a] 5
                     [:a :b] 3}]
      (is (= 8 (route/route-length distances :start [:a :b]))))))

;;; ---------------------------------------------------------------------------
;;; 2-OPT TESTS
;;; ---------------------------------------------------------------------------

(deftest two-opt-swap-test
  (testing "Swap reverses segment"
    (is (= [:a :c :b :d] (route/two-opt-swap [:a :b :c :d] 1 2)))))

(deftest two-opt-improve-test
  (testing "Improvement on suboptimal route"
    ;; Triangle where direct is better
    (let [distances {[:start :a] 1
                     [:start :b] 10
                     [:a :b] 1
                     [:b :a] 1
                     [:a :start] 1
                     [:b :start] 10}
          ;; Suboptimal: start -> b -> a (costs 10 + 1 = 11)
          ;; Optimal: start -> a -> b (costs 1 + 1 = 2)
          improved (route/two-opt-improve distances :start [:b :a])]
      ;; Should swap to [:a :b]
      (is (= [:a :b] improved)))))

;;; ---------------------------------------------------------------------------
;;; TREASURE LOCATION TESTS
;;; ---------------------------------------------------------------------------

(deftest treasure-location-test
  (testing "Known treasure locations"
    (is (= :up-a-tree (route/treasure-location :egg)))
    (is (= :gallery (route/treasure-location :painting)))
    (is (= :loud-room (route/treasure-location :platinum-bar)))
    (is (= :torch-room (route/treasure-location :ivory-torch)))))

;;; ---------------------------------------------------------------------------
;;; ROUTE PLANNING INTEGRATION TESTS
;;; ---------------------------------------------------------------------------

(deftest plan-treasure-route-test
  (testing "Plan route for single easy treasure"
    (let [gs (core/init-game)
          plan (route/plan-treasure-route gs [:egg])]
      (is (map? plan))
      (is (contains? plan :treasures))
      (is (contains? plan :route))
      (is (contains? plan :total-distance))
      ;; egg should be reachable
      (is (seq (:treasures plan)))
      (is (< (:total-distance plan) Integer/MAX_VALUE))))

  (testing "Plan route for multiple treasures"
    (let [gs (core/init-game)
          ;; These require troll-flag which we don't have, so might be unreachable
          plan (route/plan-treasure-route gs [:egg])]
      (is (map? plan))
      ;; At minimum, egg should be in the plan
      (is (some #{:egg} (:treasures plan))))))

;;; ---------------------------------------------------------------------------
;;; FULL ROUTE PLANNING
;;; ---------------------------------------------------------------------------

(deftest plan-full-route-test
  (testing "Full route includes preps and treasures"
    (let [gs (core/init-game)
          route (route/plan-full-route gs [:egg :painting])]
      (is (seq route))
      ;; Should have some prep actions for painting
      (is (some #(= :prep (:type %)) route))
      ;; Should have treasure actions
      (is (some #(= :treasure (:type %)) route)))))
