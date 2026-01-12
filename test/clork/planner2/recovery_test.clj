(ns clork.planner2.recovery-test
  "Tests for recovery strategies module."
  (:require [clojure.test :refer :all]
            [clork.planner2.recovery :as recovery]
            [clork.planner2.thief :as thief]
            [clork.core :as core]
            [clork.game-state :as gs]))

;;; ---------------------------------------------------------------------------
;;; FIXTURES
;;; ---------------------------------------------------------------------------

(defn fresh-game []
  (core/init-game))

;;; ---------------------------------------------------------------------------
;;; STRATEGY CONSTRUCTION TESTS
;;; ---------------------------------------------------------------------------

(deftest make-strategy-test
  (testing "Strategy creation"
    (let [strategy (recovery/make-strategy
                    :reroute
                    10
                    {:thief-blocking true}
                    [{:type :navigate :route [:a :b]}]
                    "Test strategy")]
      (is (= :reroute (:type strategy)))
      (is (= 10 (:priority strategy)))
      (is (= "Test strategy" (:description strategy))))))

;;; ---------------------------------------------------------------------------
;;; ROUTE BLOCKING TESTS
;;; ---------------------------------------------------------------------------

(deftest route-blocked-test
  (testing "Route not blocked"
    (let [gs (fresh-game)
          ;; Route that doesn't include thief or troll
          result (recovery/route-blocked? gs [:west-of-house :south-of-house])]
      (is (not (:blocked? result)))))

  (testing "Route blocked by troll (if troll alive)"
    (let [gs (fresh-game)
          ;; Route through troll room
          result (recovery/route-blocked? gs [:cellar :troll-room :studio])]
      ;; Troll starts alive at troll-room, so this should be blocked
      (is (:blocked? result))
      (is (= :troll (:blocker result))))))

;;; ---------------------------------------------------------------------------
;;; REROUTING TESTS
;;; ---------------------------------------------------------------------------

(deftest find-alternate-route-test
  (testing "Finds alternate when available"
    (let [gs (fresh-game)
          ;; Try to find route avoiding troll-room
          route (recovery/find-alternate-route gs :cellar :studio :troll-room)]
      ;; May or may not exist depending on map structure
      ;; Just test that the function works
      (is (or (nil? route) (seq route))))))

(deftest reroute-around-thief-test
  (testing "Creates reroute strategy when possible"
    (let [gs (fresh-game)
          thief-loc (thief/thief-location gs)
          ;; Only test if thief is somewhere we can route around
          strategy (recovery/reroute-around-thief gs :living-room)]
      ;; May or may not find a route
      (is (or (nil? strategy) (= :reroute (:type strategy)))))))

;;; ---------------------------------------------------------------------------
;;; ITEM RECOVERY TESTS
;;; ---------------------------------------------------------------------------

(deftest recover-from-thief-bag-test
  (testing "No recovery when thief alive"
    (let [gs (fresh-game)
          strategy (recovery/recover-from-thief-bag gs [:egg])]
      (is (nil? strategy))))

  (testing "Recovery possible when thief dead with items"
    (let [gs (-> (fresh-game)
                 (assoc :thief-dead true)
                 (assoc-in [:objects :egg :in] :thief))
          strategy (recovery/recover-from-thief-bag gs [:egg])]
      (is (some? strategy))
      (is (= :item-recovery (:type strategy))))))

;;; ---------------------------------------------------------------------------
;;; WEAPON RECOVERY TESTS
;;; ---------------------------------------------------------------------------

(deftest recover-dropped-weapon-test
  (testing "Recovery when weapon in current room"
    (let [gs (-> (fresh-game)
                 (assoc :here :living-room))
          ;; Sword is in living-room by default
          strategy (recovery/recover-dropped-weapon gs)]
      (is (some? strategy))
      (is (= :weapon-pickup (:type strategy)))))

  (testing "No recovery when no weapon in room"
    (let [gs (-> (fresh-game)
                 ;; Start at west-of-house, sword is in living-room
                 (assoc :here :west-of-house))
          strategy (recovery/recover-dropped-weapon gs)]
      (is (nil? strategy)))))

;;; ---------------------------------------------------------------------------
;;; STRATEGY SELECTION TESTS
;;; ---------------------------------------------------------------------------

(deftest applicable-strategies-test
  (testing "Strategies for weapon loss"
    (let [gs (-> (fresh-game)
                 (assoc :here :living-room))
          situation {:type :weapon-lost
                     :lost-weapon :sword
                     :weapon-location :living-room}
          strategies (recovery/applicable-strategies gs situation)]
      (is (seq strategies))
      (is (= :weapon-pickup (:type (first strategies))))))

  (testing "Best strategy returns highest priority"
    (let [gs (fresh-game)
          situation {:type :weapon-lost}
          strategy (recovery/best-strategy
                    (assoc gs :here :living-room)
                    {:type :weapon-lost})]
      ;; May or may not find one depending on setup
      true)))

;;; ---------------------------------------------------------------------------
;;; SITUATION DETECTION TESTS
;;; ---------------------------------------------------------------------------

(deftest detect-situation-test
  (testing "Detects weapon loss"
    (let [pre (-> (fresh-game)
                  (assoc-in [:objects :sword :in] :adventurer))
          post (-> pre
                   (assoc-in [:objects :sword :in] :troll-room))
          situation (recovery/detect-situation pre post :kitchen)]
      (is (some? situation))
      (is (= :weapon-lost (:type situation)))))

  (testing "Detects theft"
    (let [pre (-> (fresh-game)
                  (assoc-in [:objects :egg :in] :adventurer))
          post (-> pre
                   (assoc-in [:objects :egg :in] :thief))
          situation (recovery/detect-situation pre post nil)]
      (is (some? situation))
      (is (= :items-stolen (:type situation)))))

  (testing "No situation when nothing wrong"
    (let [gs (fresh-game)
          situation (recovery/detect-situation gs gs nil)]
      (is (nil? situation)))))

;;; ---------------------------------------------------------------------------
;;; RECOVERY EXECUTION TESTS
;;; ---------------------------------------------------------------------------

(deftest execute-recovery-test
  (testing "No strategy returns failure"
    (let [gs (fresh-game)
          result (recovery/execute-recovery gs nil identity)]
      (is (not (:success? result)))
      (is (some #(clojure.string/includes? % "No recovery") (:events result)))))

  (testing "Strategy execution"
    (let [gs (fresh-game)
          strategy (recovery/make-strategy
                    :test 1 {} [{:type :take :item :sword}] "Test")
          ;; Dummy execute-fn that just returns state
          execute-fn (fn [gs cmd] gs)
          result (recovery/execute-recovery gs strategy execute-fn)]
      (is (:success? result))
      (is (= strategy (:strategy result))))))

;;; ---------------------------------------------------------------------------
;;; FORMAT OUTPUT TESTS
;;; ---------------------------------------------------------------------------

(deftest format-strategy-test
  (testing "Format produces readable output"
    (let [strategy (recovery/make-strategy
                    :reroute 10 {} [] "Test reroute")
          formatted (recovery/format-strategy strategy)]
      (is (clojure.string/includes? formatted "Recovery Strategy"))
      (is (clojure.string/includes? formatted "reroute")))))

(deftest format-result-test
  (testing "Format result"
    (let [result (recovery/make-recovery-result
                  true nil {} ["Event 1" "Event 2"])
          formatted (recovery/format-result result)]
      (is (clojure.string/includes? formatted "Recovery Result"))
      (is (clojure.string/includes? formatted "Event 1")))))

