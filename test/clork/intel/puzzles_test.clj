(ns clork.intel.puzzles-test
  "Tests for the puzzle solver system."
  (:require [clojure.test :refer :all]
            [clork.core :as core]
            [clork.game-state :as gs]
            [clork.intel.puzzles :as puzzles]
            [clork.debug.scenarios :as scenarios]))

;;; ---------------------------------------------------------------------------
;;; PUZZLE LIBRARY TESTS
;;; ---------------------------------------------------------------------------

(deftest test-puzzle-library-structure
  (testing "puzzle library contains expected puzzles"
    (let [puzzle-ids (puzzles/all-puzzles)]
      (is (contains? (set puzzle-ids) :exorcism))
      (is (contains? (set puzzle-ids) :troll-battle))
      (is (contains? (set puzzle-ids) :rainbow-solid))
      (is (contains? (set puzzle-ids) :loud-room-echo))
      (is (contains? (set puzzle-ids) :dome-rope))
      (is (contains? (set puzzle-ids) :dam-open)))))

(deftest test-puzzle-has-required-fields
  (testing "each puzzle has required fields"
    (doseq [puzzle-id (puzzles/all-puzzles)]
      (let [puzzle (puzzles/get-puzzle puzzle-id)]
        (is (some? (:id puzzle)) (str puzzle-id " missing :id"))
        (is (some? (:description puzzle)) (str puzzle-id " missing :description"))
        (is (some? (:preconditions puzzle)) (str puzzle-id " missing :preconditions"))
        (is (some? (:steps puzzle)) (str puzzle-id " missing :steps"))))))

;;; ---------------------------------------------------------------------------
;;; QUERY FUNCTION TESTS
;;; ---------------------------------------------------------------------------

(deftest test-puzzles-for-flag
  (testing "find puzzles that set lld-flag"
    (let [puzzles (puzzles/puzzles-for-flag :lld-flag)]
      (is (= [:exorcism] puzzles))))

  (testing "find puzzles that set troll-flag"
    (let [puzzles (puzzles/puzzles-for-flag :troll-flag)]
      (is (= [:troll-battle] puzzles))))

  (testing "find puzzles that set rainbow-flag"
    (let [puzzles (puzzles/puzzles-for-flag :rainbow-flag)]
      (is (= [:rainbow-solid] puzzles)))))

(deftest test-puzzles-unlocking
  (testing "find puzzles that unlock land-of-living-dead"
    (let [puzzles (puzzles/puzzles-unlocking :land-of-living-dead)]
      (is (= [:exorcism] puzzles))))

  (testing "find puzzles that unlock pot-of-gold"
    (let [puzzles (puzzles/puzzles-unlocking :pot-of-gold)]
      (is (= [:rainbow-solid] puzzles)))))

(deftest test-puzzle-items-needed
  (testing "exorcism requires bell, candles, matchbook, book"
    (let [items (puzzles/puzzle-items-needed :exorcism)]
      (is (= #{:brass-bell :candles :matchbook :black-book} (set items)))))

  (testing "rainbow requires sceptre"
    (let [items (puzzles/puzzle-items-needed :rainbow-solid)]
      (is (= [:sceptre] items))))

  (testing "loud-room requires no items"
    (let [items (puzzles/puzzle-items-needed :loud-room-echo)]
      (is (empty? items)))))

;;; ---------------------------------------------------------------------------
;;; PRECONDITION CHECKING TESTS
;;; ---------------------------------------------------------------------------

(deftest test-check-object-accessible
  (testing "object-accessible when held"
    (let [gs (-> (core/init-game)
                 (gs/move-object :brass-lantern :adventurer :test))
          result (puzzles/check-puzzle-precond gs {:type :object-accessible :object :brass-lantern})]
      (is (:satisfied result))))

  (testing "object-accessible when in room"
    (let [gs (-> (core/init-game)
                 (assoc :here :west-of-house))
          result (puzzles/check-puzzle-precond gs {:type :object-accessible :object :mailbox})]
      (is (:satisfied result))))

  (testing "object not accessible when elsewhere"
    (let [gs (-> (core/init-game)
                 (assoc :here :west-of-house))
          result (puzzles/check-puzzle-precond gs {:type :object-accessible :object :brass-lantern})]
      (is (not (:satisfied result))))))

(deftest test-check-can-reach
  (testing "can reach adjacent room"
    (let [gs (-> (core/init-game)
                 (assoc :here :west-of-house))
          result (puzzles/check-puzzle-precond gs {:type :can-reach :room :north-of-house})]
      (is (:satisfied result))))

  (testing "can reach current room"
    (let [gs (-> (core/init-game)
                 (assoc :here :living-room))
          result (puzzles/check-puzzle-precond gs {:type :can-reach :room :living-room})]
      (is (:satisfied result)))))

(deftest test-check-flag-set
  (testing "flag-set when flag is set"
    (let [gs (-> (core/init-game)
                 (gs/set-game-flag :troll-flag))
          result (puzzles/check-puzzle-precond gs {:type :flag-set :flag :troll-flag})]
      (is (:satisfied result))))

  (testing "flag-set when flag is not set"
    (let [gs (core/init-game)
          result (puzzles/check-puzzle-precond gs {:type :flag-set :flag :troll-flag})]
      (is (not (:satisfied result))))))

;;; ---------------------------------------------------------------------------
;;; CAN-SOLVE-PUZZLE TESTS
;;; ---------------------------------------------------------------------------

(deftest test-can-solve-puzzle-loud-room
  (testing "can solve loud-room when at loud-room"
    (let [gs (-> (core/init-game)
                 (assoc :here :loud-room))]
      (is (puzzles/can-solve-puzzle? gs :loud-room-echo)))))

(deftest test-cannot-solve-puzzle-missing-items
  (testing "cannot solve exorcism without items"
    (let [gs (-> (core/init-game)
                 (assoc :here :entrance-to-hades))]
      (is (not (puzzles/can-solve-puzzle? gs :exorcism))))))

(deftest test-can-solve-puzzle-with-items
  (testing "can solve exorcism with all items at location"
    (let [gs (-> (core/init-game)
                 (assoc :here :entrance-to-hades)
                 (gs/move-object :brass-bell :adventurer :test)
                 (gs/move-object :candles :adventurer :test)
                 (gs/move-object :matchbook :adventurer :test)
                 (gs/move-object :black-book :adventurer :test))]
      (is (puzzles/can-solve-puzzle? gs :exorcism)))))

(deftest test-puzzle-blockers
  (testing "puzzle-blockers returns missing items"
    (let [gs (-> (core/init-game)
                 (assoc :here :entrance-to-hades)
                 (gs/move-object :brass-bell :adventurer :test))
          blockers (puzzles/puzzle-blockers gs :exorcism)]
      (is (> (count blockers) 0))
      ;; Should be missing candles, matchbook, black-book
      (is (some #(= :candles (get-in % [:precondition :object])) blockers)))))

;;; ---------------------------------------------------------------------------
;;; PUZZLE EXECUTION TESTS
;;; ---------------------------------------------------------------------------

(deftest test-execute-loud-room-puzzle
  (testing "executing loud-room puzzle sets loud-flag"
    (let [gs (-> (core/init-game)
                 (assoc :here :loud-room))
          result (puzzles/execute-puzzle gs :loud-room-echo)]
      (is (:success result))
      (is (gs/game-flag? (:game-state result) :loud-flag)))))

(deftest test-execute-puzzle-wrong-location
  (testing "cannot execute puzzle at wrong location"
    (let [gs (-> (core/init-game)
                 (assoc :here :west-of-house))
          result (puzzles/execute-puzzle gs :loud-room-echo)]
      (is (not (:success result)))
      (is (re-find #"Must be at" (:error result))))))

(deftest test-execute-puzzle-missing-preconditions
  (testing "cannot execute puzzle with missing preconditions"
    (let [gs (-> (core/init-game)
                 (assoc :here :entrance-to-hades))
          result (puzzles/execute-puzzle gs :exorcism)]
      (is (not (:success result)))
      (is (= "Puzzle preconditions not met" (:error result))))))

(deftest test-execute-dome-rope-puzzle
  (testing "executing dome-rope puzzle sets dome-flag"
    (let [gs (-> (core/init-game)
                 (assoc :here :dome-room)
                 (gs/move-object :rope :adventurer :test))
          result (puzzles/execute-puzzle gs :dome-rope)]
      (is (:success result))
      (is (gs/game-flag? (:game-state result) :dome-flag)))))

(deftest test-execute-rainbow-puzzle
  (testing "executing rainbow puzzle sets rainbow-flag"
    (let [gs (-> (core/init-game)
                 (assoc :here :aragain-falls)
                 (gs/move-object :sceptre :adventurer :test))
          result (puzzles/execute-puzzle gs :rainbow-solid)]
      (is (:success result))
      (is (gs/game-flag? (:game-state result) :rainbow-flag)))))

;;; ---------------------------------------------------------------------------
;;; EXORCISM PUZZLE INTEGRATION TEST
;;; ---------------------------------------------------------------------------

(deftest test-execute-exorcism-full
  (testing "full exorcism puzzle execution"
    (let [gs (-> (core/init-game)
                 (assoc :here :entrance-to-hades)
                 (gs/move-object :brass-bell :adventurer :test)
                 (gs/move-object :candles :adventurer :test)
                 (gs/move-object :matchbook :adventurer :test)
                 (gs/move-object :black-book :adventurer :test)
                 ;; Turn on matchbook for lighting candles
                 (gs/set-thing-flag :matchbook :on))
          result (puzzles/execute-puzzle gs :exorcism)]
      (is (:success result) (str "Failed: " (:error result)))
      (is (gs/game-flag? (:game-state result) :lld-flag)))))

;;; ---------------------------------------------------------------------------
;;; VERIFY POSTCONDITIONS TESTS
;;; ---------------------------------------------------------------------------

(deftest test-verify-postconditions-not-satisfied
  (testing "postconditions not satisfied before puzzle"
    (let [gs (core/init-game)
          result (puzzles/verify-postconditions gs :exorcism)]
      (is (not (:all-satisfied result))))))

(deftest test-verify-postconditions-satisfied
  (testing "postconditions satisfied after puzzle"
    (let [gs (-> (core/init-game)
                 (gs/set-game-flag :lld-flag))
          result (puzzles/verify-postconditions gs :exorcism)]
      (is (:all-satisfied result)))))

;;; ---------------------------------------------------------------------------
;;; PUZZLE PLANNING SUPPORT TESTS
;;; ---------------------------------------------------------------------------

(deftest test-puzzles-needed-for-flag
  (testing "find puzzles needed for lld-flag"
    (let [gs (core/init-game)
          needed (puzzles/puzzles-needed-for-flag gs :lld-flag)]
      (is (= 1 (count needed)))
      (is (= :exorcism (:puzzle (first needed)))))))

(deftest test-puzzle-dependency-order
  (testing "puzzle dependency ordering"
    (let [gs (core/init-game)
          ;; Trap door should come before puzzles that require cellar access
          order (puzzles/puzzle-dependency-order gs [:trap-door-open :loud-room-echo])]
      ;; Both should be present
      (is (= 2 (count order))))))

(deftest test-route-to-puzzle
  (testing "route to puzzle location when reachable"
    (let [gs (-> (core/init-game)
                 (assoc :here :dam-room))
          ;; Dam-open puzzle is at dam-room - already there, so nil route
          route (puzzles/route-to-puzzle gs :dam-open)]
      ;; Already at destination, route is nil
      (is (nil? route))))

  (testing "route to puzzle location when not at destination"
    (let [gs (-> (core/init-game)
                 (assoc :here :loud-room))
          ;; Dam-open puzzle is at dam-room - need to travel
          route (puzzles/route-to-puzzle gs :dam-open)]
      ;; Should return a path to dam-room (adjacent room)
      (is (some? route))
      (is (= :dam-room (last (:path route)))))))

;;; ---------------------------------------------------------------------------
;;; DESCRIBE PUZZLE TESTS
;;; ---------------------------------------------------------------------------

(deftest test-describe-puzzle
  (testing "describe-puzzle returns description"
    (let [desc (puzzles/describe-puzzle :exorcism)]
      (is (string? desc))
      (is (re-find #"spirits" desc))
      (is (re-find #"entrance-to-hades" desc)))))

(deftest test-puzzle-status
  (testing "puzzle-status returns current status"
    (let [gs (core/init-game)
          status (puzzles/puzzle-status gs :exorcism)]
      (is (= :exorcism (:puzzle-id status)))
      (is (not (:can-solve status)))
      (is (not (:already-solved status)))
      (is (seq (:blockers status))))))
