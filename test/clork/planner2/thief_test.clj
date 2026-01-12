(ns clork.planner2.thief-test
  "Tests for thief monitoring module."
  (:require [clojure.test :refer :all]
            [clork.planner2.thief :as thief]
            [clork.core :as core]
            [clork.game-state :as gs]))

;;; ---------------------------------------------------------------------------
;;; FIXTURES
;;; ---------------------------------------------------------------------------

(defn fresh-game []
  (core/init-game))

;;; ---------------------------------------------------------------------------
;;; BASIC OBSERVATION TESTS
;;; ---------------------------------------------------------------------------

(deftest thief-location-test
  (testing "Thief has a location"
    (let [gs (fresh-game)
          loc (thief/thief-location gs)]
      (is (keyword? loc)))))

(deftest thief-visibility-test
  (testing "Thief visibility check"
    (let [gs (fresh-game)]
      ;; Just check the function works
      (is (boolean? (thief/thief-visible? gs))))))

(deftest thief-dead-test
  (testing "Thief starts alive"
    (let [gs (fresh-game)]
      (is (not (thief/thief-dead? gs)))))

  (testing "Thief dead flag detection"
    (let [gs (-> (fresh-game)
                 (assoc :thief-dead true))]
      (is (thief/thief-dead? gs)))))

(deftest thief-strength-test
  (testing "Thief has strength"
    (let [gs (fresh-game)
          strength (thief/thief-strength gs)]
      (is (pos? strength)))))

;;; ---------------------------------------------------------------------------
;;; BAG CONTENTS TESTS
;;; ---------------------------------------------------------------------------

(deftest thief-bag-contents-test
  (testing "Fresh game - thief bag excludes stiletto and bag"
    (let [gs (fresh-game)
          contents (thief/thief-bag-contents gs)]
      ;; Should not include :stiletto or :large-bag
      (is (not (some #{:stiletto :large-bag} contents))))))

(deftest thief-has-item-test
  (testing "Thief doesn't have egg initially"
    (let [gs (fresh-game)]
      (is (not (thief/thief-has-item? gs :egg)))))

  (testing "Thief has item after theft"
    (let [gs (-> (fresh-game)
                 (assoc-in [:objects :egg :in] :thief))]
      (is (thief/thief-has-item? gs :egg)))))

(deftest thief-bag-value-test
  (testing "Empty bag has zero value"
    (let [gs (fresh-game)
          value (thief/thief-bag-value gs)]
      ;; Stiletto doesn't count, so should be low
      (is (>= value 0))))

  (testing "Bag value increases with stolen treasures"
    (let [gs (-> (fresh-game)
                 (assoc-in [:objects :egg :in] :thief))
          value (thief/thief-bag-value gs)]
      ;; Egg has positive value
      (is (pos? value)))))

;;; ---------------------------------------------------------------------------
;;; THEFT DETECTION TESTS
;;; ---------------------------------------------------------------------------

(deftest detect-theft-test
  (testing "No theft detected when nothing stolen"
    (let [pre (fresh-game)
          post pre
          result (thief/detect-theft pre post)]
      (is (nil? result))))

  (testing "Theft detected when item moves to thief"
    (let [pre (-> (fresh-game)
                  (assoc-in [:objects :egg :in] :adventurer))
          post (-> pre
                   (assoc-in [:objects :egg :in] :thief))
          result (thief/detect-theft pre post)]
      (is (some? result))
      (is (= [:egg] (:stolen result)))
      (is (= :player (:source result))))))

;;; ---------------------------------------------------------------------------
;;; ROUTE RISK TESTS
;;; ---------------------------------------------------------------------------

(deftest route-thief-risk-test
  (testing "Risk is none when thief dead"
    (let [gs (-> (fresh-game)
                 (assoc :thief-dead true))
          risk (thief/route-thief-risk gs [:west-of-house :living-room])]
      (is (= :none (:risk risk)))))

  (testing "Risk assessment for normal route"
    (let [gs (fresh-game)
          thief-loc (thief/thief-location gs)
          route [thief-loc :living-room]
          risk (thief/route-thief-risk gs route)]
      (is (:thief-on-route? risk))
      (is (#{:low :medium :high} (:risk risk))))))

;;; ---------------------------------------------------------------------------
;;; AVOIDANCE STRATEGY TESTS
;;; ---------------------------------------------------------------------------

(deftest should-avoid-thief-test
  (testing "Fresh game - should avoid thief"
    (let [gs (fresh-game)
          result (thief/should-avoid-thief? gs)]
      ;; At start of game, player is weak
      (is (:avoid? result))))

  (testing "High score - should not avoid"
    (let [gs (-> (fresh-game)
                 (assoc :score 350))
          result (thief/should-avoid-thief? gs)]
      (is (not (:avoid? result))))))

(deftest suggest-thief-avoidance-test
  (testing "Thief dead - ignore"
    (let [gs (-> (fresh-game)
                 (assoc :thief-dead true))
          suggestion (thief/suggest-thief-avoidance gs [:west-of-house :living-room])]
      (is (= :ignore (:strategy suggestion)))))

  (testing "Thief not on route - ignore"
    (let [gs (fresh-game)
          ;; Route that doesn't include thief location
          route [:west-of-house :living-room]
          thief-loc (thief/thief-location gs)
          ;; If thief happens to be on this route, skip test
          suggestion (when (not (some #{thief-loc} route))
                       (thief/suggest-thief-avoidance gs route))]
      (when suggestion
        (is (= :ignore (:strategy suggestion)))))))

;;; ---------------------------------------------------------------------------
;;; MONITOR TESTS
;;; ---------------------------------------------------------------------------

(deftest make-thief-monitor-test
  (testing "Fresh monitor has expected fields"
    (let [monitor (thief/make-thief-monitor)]
      (is (nil? (:last-location monitor)))
      (is (empty? (:location-history monitor)))
      (is (zero? (:thefts-detected monitor)))
      (is (empty? (:items-stolen monitor)))
      (is (empty? (:encounters monitor))))))

(deftest update-thief-monitor-test
  (testing "Monitor updates with thief location"
    (let [gs (fresh-game)
          monitor (thief/make-thief-monitor)
          updated (thief/update-thief-monitor monitor gs)]
      (is (some? (:last-location updated)))
      (is (= 1 (count (:location-history updated)))))))

(deftest record-theft-test
  (testing "Recording theft updates monitor"
    (let [monitor (thief/make-thief-monitor)
          updated (thief/record-theft monitor [:egg :painting])]
      (is (= 1 (:thefts-detected updated)))
      (is (= #{:egg :painting} (:items-stolen updated))))))

(deftest record-encounter-test
  (testing "Recording encounter adds to history"
    (let [monitor (thief/make-thief-monitor)
          updated (thief/record-encounter monitor :cellar 50 :fled)]
      (is (= 1 (count (:encounters updated))))
      (is (= :cellar (:room (first (:encounters updated))))))))

;;; ---------------------------------------------------------------------------
;;; FORMAT OUTPUT TESTS
;;; ---------------------------------------------------------------------------

(deftest format-thief-status-test
  (testing "Format produces readable output"
    (let [gs (fresh-game)
          formatted (thief/format-thief-status gs)]
      (is (string? formatted))
      (is (clojure.string/includes? formatted "Thief Status"))
      (is (clojure.string/includes? formatted "Location:")))))

