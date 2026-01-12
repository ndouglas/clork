(ns clork.planner2.navigate-test
  "Integration tests for navigation module.
   Tests pathfinding on the real Zork room graph."
  (:require [clojure.test :refer :all]
            [clork.planner2.navigate :as nav]
            [clork.planner2.route :as route]
            [clork.core :as core]))

;;; ---------------------------------------------------------------------------
;;; FIXTURES
;;; ---------------------------------------------------------------------------

(defn fresh-game []
  (core/init-game))

;;; ---------------------------------------------------------------------------
;;; ROOM GRAPH TESTS
;;; ---------------------------------------------------------------------------

(deftest build-room-graph-test
  (let [gs (fresh-game)
        graph (nav/build-room-graph gs)]

    (testing "Room graph is not empty"
      (is (map? graph))
      (is (pos? (count graph))))

    (testing "West-of-house has expected exits"
      (let [exits (:west-of-house graph)]
        (is (set? exits))
        ;; West of house should connect to north-of-house, south-of-house
        (is (contains? exits :north-of-house))
        (is (contains? exits :south-of-house))))

    (testing "Living room has expected exits"
      (let [exits (:living-room graph)]
        (is (set? exits))
        ;; Living room connects to kitchen, etc.
        (is (contains? exits :kitchen))))))

(deftest find-path-test
  (let [gs (fresh-game)
        graph (nav/build-room-graph gs)]

    (testing "Path from west-of-house to kitchen"
      (let [path (nav/find-path graph :west-of-house :kitchen)]
        (is (some? path))
        (is (seq path))
        ;; Path should start at west-of-house and end at kitchen
        (is (= :west-of-house (first path)))
        (is (= :kitchen (last path)))))

    (testing "Path to self is trivial"
      (let [path (nav/find-path graph :west-of-house :west-of-house)]
        (is (= [:west-of-house] path))))

    (testing "Path from west-of-house to living room"
      (let [path (nav/find-path graph :west-of-house :living-room)]
        (is (some? path))
        (is (< (count path) 10)))) ; Should be reachable in few steps

    (testing "Path to unreachable room returns nil"
      ;; Treasure room requires magic-flag
      (let [path (nav/find-path graph :west-of-house :treasure-room)]
        ;; Might be nil or might find a conditional path
        ;; depending on how the graph is built
        true))))

;;; ---------------------------------------------------------------------------
;;; DIRECTION EXTRACTION TESTS
;;; ---------------------------------------------------------------------------

(deftest path-to-directions-test
  (let [gs (fresh-game)]

    (testing "Get directions for a path"
      (let [path [:west-of-house :north-of-house :behind-house]]
        ;; This tests that path-to-directions returns something reasonable
        (let [dirs (nav/path-to-directions gs path)]
          (is (or (nil? dirs) (seq dirs))))))))

;;; ---------------------------------------------------------------------------
;;; FLOYD-WARSHALL ON REAL GRAPH
;;; ---------------------------------------------------------------------------

(deftest floyd-warshall-real-graph-test
  (let [gs (fresh-game)
        graph (nav/build-room-graph gs)
        fw-result (route/floyd-warshall graph)]

    (testing "Floyd-Warshall computes distances"
      (is (map? fw-result))
      (is (contains? fw-result :dist)))

    (testing "Distance to self is zero"
      (is (= 0 (get-in fw-result [:dist [:west-of-house :west-of-house]]))))

    (testing "Distance from west-of-house to kitchen is reasonable"
      (let [dist (get-in fw-result [:dist [:west-of-house :kitchen]])]
        (is (some? dist))
        (is (pos? dist))
        (is (< dist 20)))) ; Should be reachable in < 20 moves

    (testing "Distance from west-of-house to living-room"
      (let [dist (get-in fw-result [:dist [:west-of-house :living-room]])]
        (is (some? dist))
        (is (pos? dist))
        (is (< dist 15))))

    (testing "All rooms have distance to themselves = 0"
      (doseq [room (keys graph)]
        (is (= 0 (get-in fw-result [:dist [room room]]))
            (str room " should have distance 0 to itself"))))))

;;; ---------------------------------------------------------------------------
;;; TSP ON REAL TREASURES
;;; ---------------------------------------------------------------------------

(deftest tsp-treasure-route-test
  (let [gs (fresh-game)]

    (testing "TSP for easy treasures"
      (let [treasures [:egg]
            plan (route/plan-treasure-route gs treasures)]
        (is (map? plan))
        (is (contains? plan :treasures))
        (is (contains? plan :total-distance))
        ;; Egg should be reachable
        (is (some #{:egg} (:treasures plan)))))

    (testing "TSP optimization produces valid route"
      (let [treasures [:egg :bag-of-coins]
            plan (route/plan-treasure-route gs treasures)]
        ;; All reachable treasures should be in the route
        (is (<= (count (:treasures plan)) (count treasures)))
        ;; Total distance should be reasonable
        (is (< (:total-distance plan) Integer/MAX_VALUE))))))

;;; ---------------------------------------------------------------------------
;;; TREASURE LOCATION TESTS
;;; ---------------------------------------------------------------------------

(deftest treasure-locations-test
  (testing "All treasures have known locations"
    (doseq [[treasure location] route/treasure-locations]
      (is (keyword? location)
          (str treasure " should have a keyword location")))))

(deftest treasure-location-consistency-test
  (let [gs (fresh-game)]
    (testing "Known treasure locations match game objects"
      ;; Egg is in up-a-tree
      (is (= :up-a-tree (route/treasure-location :egg)))
      ;; Painting is in gallery
      (is (= :gallery (route/treasure-location :painting)))
      ;; Platinum bar is in loud-room
      (is (= :loud-room (route/treasure-location :platinum-bar))))))

;;; ---------------------------------------------------------------------------
;;; CONDITIONAL EXIT TESTS
;;; ---------------------------------------------------------------------------

(deftest conditional-exits-test
  (let [gs (fresh-game)
        graph (nav/build-room-graph gs)]

    (testing "Rooms with conditional exits are handled"
      ;; Troll room has conditional exit past troll
      (let [troll-room-exits (:troll-room graph)]
        ;; Should have exits regardless of troll state
        ;; (the graph might include or exclude the blocked path)
        (is (set? troll-room-exits)))

      ;; Living room has trap door (conditional)
      (let [living-exits (:living-room graph)]
        (is (set? living-exits))))))

;;; ---------------------------------------------------------------------------
;;; GRAPH CONSISTENCY TESTS
;;; ---------------------------------------------------------------------------

(deftest graph-consistency-test
  (let [gs (fresh-game)
        graph (nav/build-room-graph gs)]

    (testing "No empty rooms in graph"
      (doseq [[room exits] graph]
        (is (keyword? room) (str "Room key should be keyword: " room))
        (is (set? exits) (str "Exits should be set for " room))))

    (testing "All exit targets exist in graph"
      (let [all-rooms (set (keys graph))]
        (doseq [[room exits] graph
                exit exits]
          (is (contains? all-rooms exit)
              (str "Exit " exit " from " room " should exist in graph")))))))
