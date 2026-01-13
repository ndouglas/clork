(ns clork.intel.routing-test
  (:require [clojure.test :refer :all]
            [clork.intel.routing :as routing]
            [clork.game-state :as gs]
            [clork.debug.scenarios :as scenarios]))

;;; ---------------------------------------------------------------------------
;;; GRAPH BUILDING TESTS
;;; ---------------------------------------------------------------------------

(deftest test-build-navigation-graph
  (testing "builds graph with edges and rooms"
    (let [gs (scenarios/equipped-adventurer)
          graph (routing/build-navigation-graph gs)]
      (is (set? (:rooms graph)))
      (is (seq (:edges graph)))
      (is (map? (:adjacency graph)))
      ;; Should have many rooms
      (is (> (count (:rooms graph)) 50)))))

(deftest test-special-edges-without-flags
  (testing "special edges filtered by available flags"
    (let [no-flags (routing/get-special-edges #{})
          with-troll (routing/get-special-edges #{:troll-flag})]
      ;; Without troll-flag, troll passages shouldn't be available
      (is (not (some #(and (= (:from %) :troll-room)
                           (= (:to %) :east-west-passage))
                     no-flags)))
      ;; With troll-flag, they should be
      (is (some #(and (= (:from %) :troll-room)
                      (= (:to %) :east-west-passage))
                with-troll)))))

(deftest test-special-edges-lld
  (testing "land-of-living-dead requires lld-flag"
    (let [no-flags (routing/get-special-edges #{})
          with-lld (routing/get-special-edges #{:lld-flag})]
      (is (not (some #(= (:to %) :land-of-living-dead) no-flags)))
      (is (some #(= (:to %) :land-of-living-dead) with-lld)))))

;;; ---------------------------------------------------------------------------
;;; FLOYD-WARSHALL TESTS
;;; ---------------------------------------------------------------------------

(deftest test-floyd-warshall-basic
  (testing "Floyd-Warshall computes distances"
    (let [gs (scenarios/equipped-adventurer)
          graph (routing/build-navigation-graph gs)
          fw (routing/floyd-warshall graph)]
      (is (map? (:dist fw)))
      (is (map? (:next fw)))
      (is (set? (:rooms fw))))))

(deftest test-get-distance
  (testing "get-distance returns correct distances"
    (let [gs (scenarios/equipped-adventurer)
          graph (routing/build-navigation-graph gs)
          fw (routing/floyd-warshall graph)]
      ;; Same room should be distance 0
      (is (= 0 (routing/get-distance fw :west-of-house :west-of-house)))
      ;; Adjacent rooms should be distance 1
      (is (= 1 (routing/get-distance fw :west-of-house :north-of-house))))))

(deftest test-reconstruct-path
  (testing "path reconstruction works"
    (let [gs (scenarios/equipped-adventurer)
          graph (routing/build-navigation-graph gs)
          fw (routing/floyd-warshall graph)
          path (routing/reconstruct-path-fw fw :west-of-house :north-of-house)]
      (is (vector? path))
      (is (= :west-of-house (first path)))
      (is (= :north-of-house (last path))))))

;;; ---------------------------------------------------------------------------
;;; SHORTEST PATH TESTS
;;; ---------------------------------------------------------------------------

(deftest test-shortest-path-simple
  (testing "finds shortest path between adjacent rooms"
    (let [gs (scenarios/equipped-adventurer :west-of-house)
          result (routing/shortest-path gs :west-of-house :north-of-house)]
      (is (some? result))
      (is (= 1 (:distance result)))
      (is (= :west-of-house (:from result)))
      (is (= :north-of-house (:to result))))))

(deftest test-shortest-path-longer
  (testing "finds shortest path between distant rooms"
    (let [gs (scenarios/equipped-adventurer)
          result (routing/shortest-path gs :west-of-house :kitchen)]
      (is (some? result))
      (is (> (:distance result) 1))
      (is (= :west-of-house (first (:path result))))
      (is (= :kitchen (last (:path result)))))))

(deftest test-shortest-path-with-flags
  (testing "path changes based on available flags"
    (let [gs (scenarios/equipped-adventurer :entrance-to-hades)
          ;; Without lld-flag, can't reach land-of-living-dead
          without-lld (routing/shortest-path gs :entrance-to-hades :land-of-living-dead
                                             :available-flags #{})
          ;; With lld-flag, should be reachable
          with-lld (routing/shortest-path gs :entrance-to-hades :land-of-living-dead
                                          :available-flags #{:lld-flag})]
      (is (nil? without-lld))
      (is (some? with-lld)))))

;;; ---------------------------------------------------------------------------
;;; ROUTE GENERATION TESTS
;;; ---------------------------------------------------------------------------

(deftest test-path-to-actions
  (testing "converts path to action sequence"
    (let [gs (scenarios/equipped-adventurer)
          path [:west-of-house :north-of-house]
          actions (routing/path-to-actions gs path)]
      (is (seq actions))
      (is (every? map? actions))
      (is (= :walk (:verb (first actions)))))))

(deftest test-route-to
  (testing "generates complete route with commands"
    (let [gs (scenarios/equipped-adventurer :west-of-house)
          result (routing/route-to gs :kitchen)]
      (is (some? result))
      (is (vector? (:actions result)))
      (is (vector? (:commands result)))
      (is (number? (:distance result))))))

;;; ---------------------------------------------------------------------------
;;; TSP OPTIMIZATION TESTS
;;; ---------------------------------------------------------------------------

(deftest test-optimize-visit-order-simple
  (testing "optimizes visit order for multiple locations"
    (let [gs (scenarios/equipped-adventurer :west-of-house)
          locations [:north-of-house :south-of-house :behind-house]
          result (routing/optimize-visit-order gs locations)]
      (is (vector? (:order result)))
      (is (= (set locations) (set (:order result))))
      (is (number? (:total-distance result))))))

(deftest test-optimize-visit-order-with-unreachable
  (testing "identifies unreachable locations"
    (let [gs (scenarios/equipped-adventurer :west-of-house)
          ;; land-of-living-dead is unreachable without lld-flag
          locations [:north-of-house :land-of-living-dead]
          result (routing/optimize-visit-order gs locations
                                               :available-flags #{})]
      ;; north-of-house should be in order
      (is (some #{:north-of-house} (:order result)))
      ;; land-of-living-dead should be unreachable
      (is (some #{:land-of-living-dead} (:unreachable result))))))

(deftest test-optimize-with-return
  (testing "includes return distance when specified"
    (let [gs (scenarios/equipped-adventurer :west-of-house)
          locations [:north-of-house]
          without-return (routing/optimize-visit-order gs locations)
          with-return (routing/optimize-visit-order gs locations
                                                    :return-to :west-of-house)]
      ;; With return should have longer total distance
      (is (>= (:total-distance with-return) (:total-distance without-return))))))

;;; ---------------------------------------------------------------------------
;;; CONVENIENCE FUNCTION TESTS
;;; ---------------------------------------------------------------------------

(deftest test-distance-between
  (testing "computes distance between rooms"
    (let [gs (scenarios/equipped-adventurer)]
      (is (= 0 (routing/distance-between gs :west-of-house :west-of-house)))
      (is (= 1 (routing/distance-between gs :west-of-house :north-of-house)))
      (is (number? (routing/distance-between gs :west-of-house :kitchen))))))

(deftest test-reachable-from
  (testing "finds all reachable rooms"
    (let [gs (scenarios/equipped-adventurer)
          reachable (routing/reachable-from gs :west-of-house)]
      (is (set? reachable))
      (is (contains? reachable :west-of-house))
      (is (contains? reachable :north-of-house))
      (is (> (count reachable) 10)))))

(deftest test-rooms-requiring-flag
  (testing "finds rooms that become reachable with a flag"
    (let [gs (scenarios/equipped-adventurer)
          new-rooms (routing/rooms-requiring-flag gs :entrance-to-hades :lld-flag)]
      ;; land-of-living-dead should require lld-flag
      (is (contains? new-rooms :land-of-living-dead)))))

;;; ---------------------------------------------------------------------------
;;; TREASURE ROUTING TESTS
;;; ---------------------------------------------------------------------------

(deftest test-plan-treasure-route
  (testing "plans route for treasure collection"
    (let [gs (scenarios/equipped-adventurer :west-of-house)
          treasures [:jeweled-egg :portrait]
          result (routing/plan-treasure-route gs treasures)]
      (is (vector? (:order result)))
      (is (map? (:locations result)))
      (is (number? (:total-distance result))))))

(deftest test-treasure-locations-defined
  (testing "treasure locations map is populated"
    (is (map? routing/treasure-locations))
    (is (contains? routing/treasure-locations :jeweled-egg))
    (is (contains? routing/treasure-locations :portrait))
    (is (contains? routing/treasure-locations :crystal-skull))))
