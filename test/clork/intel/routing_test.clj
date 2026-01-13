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

(deftest test-special-edges-teleports
  (testing "special edges contain teleports"
    (let [edges (routing/get-special-edges #{})]
      ;; Prayer teleport from south-temple to forest
      (is (some #(and (= (:from %) :south-temple)
                      (= (:to %) :forest-1))
                edges))
      ;; Mirror teleport between mirror rooms
      (is (some #(and (= (:from %) :mirror-room-1)
                      (= (:to %) :mirror-room-2))
                edges)))))

(deftest test-conditional-exits-in-rooms
  (testing "flag-gated exits handled by room exits, not special edges"
    (let [gs (scenarios/equipped-adventurer)
          ;; Without lld-flag, can't reach land-of-living-dead through room exits
          graph-no-flag (routing/build-navigation-graph gs :available-flags #{})
          graph-with-flag (routing/build-navigation-graph gs :available-flags #{:lld-flag})
          ;; Check if entrance-to-hades -> land-of-living-dead edge exists
          has-lld-edge? (fn [g]
                          (some #(and (= (:from %) :entrance-to-hades)
                                      (= (:to %) :land-of-living-dead))
                                (:edges g)))]
      (is (not (has-lld-edge? graph-no-flag)) "LLD edge should NOT exist without flag")
      (is (has-lld-edge? graph-with-flag) "LLD edge should exist with flag"))))

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
          ;; Kitchen requires kitchen-window-open flag
          result (routing/shortest-path gs :west-of-house :kitchen
                                        :available-flags #{:kitchen-window-open})]
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
          ;; Kitchen requires kitchen-window-open flag
          result (routing/route-to gs :kitchen
                                   :available-flags #{:kitchen-window-open})]
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
      ;; Kitchen requires kitchen-window-open flag
      (is (number? (routing/distance-between gs :west-of-house :kitchen
                                             :available-flags #{:kitchen-window-open}))))))

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

;;; ---------------------------------------------------------------------------
;;; DOOR-GATED PASSAGE TESTS
;;; ---------------------------------------------------------------------------

(deftest test-door-gated-passages
  (testing "door-gated passages require virtual door-open flags"
    (let [gs (scenarios/equipped-adventurer)
          ;; Trap door: living-room <-> cellar
          graph-closed (routing/build-navigation-graph gs :available-flags #{})
          graph-open (routing/build-navigation-graph gs :available-flags #{:trap-door-open})
          has-trapdoor-edge? (fn [g]
                               (some #(and (= (:from %) :living-room)
                                           (= (:to %) :cellar))
                                     (:edges g)))]
      (is (not (has-trapdoor-edge? graph-closed)) "Trap door edge shouldn't exist when closed")
      (is (has-trapdoor-edge? graph-open) "Trap door edge should exist when open"))))

(deftest test-kitchen-window-passage
  (testing "kitchen window passage requires kitchen-window-open flag"
    (let [gs (scenarios/equipped-adventurer)
          graph-closed (routing/build-navigation-graph gs :available-flags #{})
          graph-open (routing/build-navigation-graph gs :available-flags #{:kitchen-window-open})
          has-window-edge? (fn [g]
                             (some #(and (= (:from %) :behind-house)
                                         (= (:to %) :kitchen))
                                   (:edges g)))]
      (is (not (has-window-edge? graph-closed)) "Window edge shouldn't exist when closed")
      (is (has-window-edge? graph-open) "Window edge should exist when open"))))

;;; ---------------------------------------------------------------------------
;;; FLAG EXTRACTION TESTS
;;; ---------------------------------------------------------------------------

(deftest test-extract-available-flags-game-flags
  (testing "extracts game-level flags from state"
    (let [gs (-> (scenarios/equipped-adventurer)
                 (assoc :troll-flag true)
                 (assoc :lld-flag true))
          flags (routing/extract-available-flags gs)]
      (is (contains? flags :troll-flag))
      (is (contains? flags :lld-flag)))))

(deftest test-extract-available-flags-door-flags
  (testing "extracts virtual door flags for open doors"
    (let [gs (-> (scenarios/equipped-adventurer)
                 (gs/set-thing-flag :trap-door :open))
          flags (routing/extract-available-flags gs)]
      (is (contains? flags :trap-door-open)))))

;;; ---------------------------------------------------------------------------
;;; FRIGID RIVER BOAT ROUTING TESTS
;;; ---------------------------------------------------------------------------

(deftest test-river-edges-require-boat
  (testing "river edges require :boat-ready flag"
    (let [gs (scenarios/equipped-adventurer)
          ;; Without boat-ready, river rooms unreachable
          graph-no-boat (routing/build-navigation-graph gs :available-flags #{})
          graph-with-boat (routing/build-navigation-graph gs :available-flags #{:boat-ready})
          ;; Check if dam-base -> river-1 launch edge exists
          has-launch-edge? (fn [g]
                             (some #(and (= (:from %) :dam-base)
                                         (= (:to %) :river-1))
                                   (:edges g)))]
      (is (not (has-launch-edge? graph-no-boat)) "Launch edge shouldn't exist without boat")
      (is (has-launch-edge? graph-with-boat) "Launch edge should exist with boat"))))

(deftest test-river-downstream-flow
  (testing "river flow edges go downstream only"
    (let [gs (scenarios/equipped-adventurer)
          graph (routing/build-navigation-graph gs :available-flags #{:boat-ready})
          edges (:edges graph)
          ;; Check downstream edges exist
          has-edge? (fn [from to]
                      (some #(and (= (:from %) from) (= (:to %) to)) edges))]
      ;; Downstream edges should exist
      (is (has-edge? :river-1 :river-2))
      (is (has-edge? :river-2 :river-3))
      (is (has-edge? :river-3 :river-4))
      (is (has-edge? :river-4 :river-5))
      ;; Upstream edges should NOT exist (river flows one way)
      (is (not (has-edge? :river-2 :river-1)))
      (is (not (has-edge? :river-5 :river-4))))))

(deftest test-river-landing-points
  (testing "can land from river at appropriate points"
    (let [gs (scenarios/equipped-adventurer)
          graph (routing/build-navigation-graph gs :available-flags #{:boat-ready})
          edges (:edges graph)
          has-edge? (fn [from to]
                      (some #(and (= (:from %) from) (= (:to %) to)) edges))]
      ;; Landing edges
      (is (has-edge? :river-1 :dam-base))
      (is (has-edge? :river-3 :white-cliffs-north))
      (is (has-edge? :river-4 :sandy-beach))
      (is (has-edge? :river-5 :shore)))))

(deftest test-boat-ready-flag-extraction
  (testing "extracts :boat-ready when player in inflated boat"
    (let [gs (-> (scenarios/equipped-adventurer :dam-base)
                 ;; Put inflated boat at dam-base
                 (gs/move-object :inflated-boat :dam-base :test)
                 ;; Put player in boat
                 (assoc-in [:objects :adventurer :in] :inflated-boat))
          flags (routing/extract-available-flags gs)]
      (is (contains? flags :boat-ready)))))

(deftest test-path-through-river
  (testing "can find path through river with boat"
    (let [gs (scenarios/equipped-adventurer :dam-base)
          ;; With boat, should be able to reach shore via river
          result (routing/shortest-path gs :dam-base :shore
                                        :available-flags #{:boat-ready})]
      (is (some? result) "Should find path to shore via river")
      ;; Path should go through river rooms
      (is (some #{:river-1} (:path result))))))

;;; ---------------------------------------------------------------------------
;;; EMPTY-HANDED PASSAGE TESTS
;;; ---------------------------------------------------------------------------

(deftest test-empty-handed-flag-extraction
  (testing "extracts :empty-handed when inventory has no heavy objects"
    (let [;; Start with no inventory
          gs (scenarios/equipped-adventurer :timber-room)
          ;; Clear inventory by moving items out
          gs-empty (reduce (fn [g obj]
                             (gs/move-object g obj :timber-room :test))
                           gs
                           (gs/get-contents gs :adventurer))
          flags (routing/extract-available-flags gs-empty)]
      (is (contains? flags :empty-handed)))))

(deftest test-empty-handed-passage-blocked
  (testing "timber-room to lower-shaft blocked without :empty-handed"
    (let [gs (scenarios/equipped-adventurer :timber-room)
          ;; With inventory (sword, lamp), should NOT be empty-handed
          graph (routing/build-navigation-graph gs :available-flags #{})
          has-passage? (some #(and (= (:from %) :timber-room)
                                   (= (:to %) :lower-shaft))
                             (:edges graph))]
      (is (not has-passage?) "Passage should be blocked when carrying items"))))

(deftest test-empty-handed-passage-open
  (testing "timber-room to lower-shaft open with :empty-handed"
    (let [gs (scenarios/equipped-adventurer :timber-room)
          graph (routing/build-navigation-graph gs :available-flags #{:empty-handed})
          has-passage? (some #(and (= (:from %) :timber-room)
                                   (= (:to %) :lower-shaft))
                             (:edges graph))]
      (is has-passage? "Passage should be open when empty-handed"))))
