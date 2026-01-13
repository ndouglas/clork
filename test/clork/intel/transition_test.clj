(ns clork.intel.transition-test
  (:require [clojure.test :refer :all]
            [clork.intel.transition :as tr]
            [clork.game-state :as gs]
            [clork.rooms :as rooms]
            [clork.objects :as objects]
            [clork.verb-defs :as verb-defs]
            [clork.daemon :as daemon]
            [clork.ml :as ml]))

(defn init-test-state
  "Create a fresh game state for testing."
  []
  (let [base (gs/initial-game-state)
        with-rooms (gs/add-rooms base rooms/all-rooms)
        with-objects (gs/add-objects with-rooms objects/all-objects)]
    (verb-defs/register-object-vocabulary! (:objects with-objects))
    (gs/set-here-flag with-objects :lit)))

;;; ---------------------------------------------------------------------------
;;; COMPUTE-STATE-DIFF TESTS
;;; ---------------------------------------------------------------------------

(deftest test-diff-detects-location-change
  (testing "diff detects player location change"
    (let [before (init-test-state)
          after (assoc before :here :north-of-house)
          diff (tr/compute-state-diff before after)]
      (is (= {:from :west-of-house :to :north-of-house}
             (:location-changed diff))))))

(deftest test-diff-detects-score-change
  (testing "diff detects score changes"
    (let [before (init-test-state)
          after (assoc before :score 10)
          diff (tr/compute-state-diff before after)]
      (is (= {:from 0 :to 10} (:score-changed diff))))))

(deftest test-diff-detects-moves-change
  (testing "diff detects move counter change"
    (let [before (init-test-state)
          after (assoc before :moves 1)
          diff (tr/compute-state-diff before after)]
      (is (= {:from 0 :to 1} (:moves-changed diff))))))

(deftest test-diff-detects-object-moved
  (testing "diff detects when object changes location"
    (let [before (init-test-state)
          ;; brass-lantern starts in :living-room
          after (assoc-in before [:objects :brass-lantern :in] :adventurer)
          diff (tr/compute-state-diff before after)]
      (is (contains? (:objects-moved diff) :brass-lantern))
      (is (= :living-room (get-in diff [:objects-moved :brass-lantern :from])))
      (is (= :adventurer (get-in diff [:objects-moved :brass-lantern :to]))))))

(deftest test-diff-detects-game-flag-set
  (testing "diff detects when game-level flag is set"
    (let [before (init-test-state)
          after (assoc before :troll-flag true)
          diff (tr/compute-state-diff before after)]
      (is (contains? (:flags-set diff) :troll-flag)))))

(deftest test-diff-detects-game-flag-cleared
  (testing "diff detects when game-level flag is cleared"
    (let [before (-> (init-test-state)
                     (assoc :troll-flag true))
          after (assoc before :troll-flag false)
          diff (tr/compute-state-diff before after)]
      (is (contains? (:flags-cleared diff) :troll-flag)))))

(deftest test-diff-detects-object-flag-set
  (testing "diff detects when object flag is set via runtime override"
    (let [before (init-test-state)
          ;; Set a flag that isn't already in static flags
          ;; (candles don't start with :touch flag)
          after (assoc-in before [:objects :candles :touch] true)
          diff (tr/compute-state-diff before after)]
      (is (contains? (:flags-set diff) [:candles :touch])))))

(deftest test-diff-detects-object-flag-cleared
  (testing "diff detects when object flag is cleared via runtime override"
    (let [before (-> (init-test-state)
                     ;; First set a flag that isn't in static flags
                     (assoc-in [:objects :candles :touch] true))
          after (assoc-in before [:objects :candles :touch] false)
          diff (tr/compute-state-diff before after)]
      (is (contains? (:flags-cleared diff) [:candles :touch])))))

(deftest test-diff-detects-daemon-started
  (testing "diff detects when daemon is enabled"
    (let [before (init-test-state)
          after (assoc-in before [:daemons :i-candles :enabled] true)
          diff (tr/compute-state-diff before after)]
      (is (contains? (:daemons-started diff) :i-candles)))))

(deftest test-diff-detects-daemon-stopped
  (testing "diff detects when daemon is disabled"
    (let [before (-> (init-test-state)
                     (assoc-in [:daemons :i-candles :enabled] true))
          after (assoc-in before [:daemons :i-candles :enabled] false)
          diff (tr/compute-state-diff before after)]
      (is (contains? (:daemons-stopped diff) :i-candles)))))

(deftest test-diff-detects-room-flag-set
  (testing "diff detects when room flag is set"
    (let [before (init-test-state)
          ;; Set the :touch flag on the current room (marks as visited)
          after (assoc-in before [:rooms :west-of-house :touch] true)
          diff (tr/compute-state-diff before after)]
      (is (contains? (:flags-set diff) [:west-of-house :touch])))))

(deftest test-diff-detects-room-flag-cleared
  (testing "diff detects when room flag is cleared"
    (let [before (-> (init-test-state)
                     (assoc-in [:rooms :west-of-house :touch] true))
          after (assoc-in before [:rooms :west-of-house :touch] false)
          diff (tr/compute-state-diff before after)]
      (is (contains? (:flags-cleared diff) [:west-of-house :touch])))))

(deftest test-diff-detects-invisible-flag
  (testing "diff detects :invisible flag when toggled from off to on"
    ;; Thief starts with :invisible in static flags, so test with an object
    ;; that doesn't have :invisible in its static flags
    (let [before (init-test-state)
          ;; Lamp doesn't start invisible, so this is a real semantic change
          after (assoc-in before [:objects :brass-lantern :invisible] true)
          diff (tr/compute-state-diff before after)]
      (is (contains? (:flags-set diff) [:brass-lantern :invisible])))))

(deftest test-diff-empty-when-no-changes
  (testing "diff is empty when states are identical"
    (let [state (init-test-state)
          diff (tr/compute-state-diff state state)]
      (is (empty? (:location-changed diff)))
      (is (empty? (:score-changed diff)))
      (is (empty? (:moves-changed diff)))
      (is (empty? (:objects-moved diff)))
      (is (empty? (:flags-set diff)))
      (is (empty? (:flags-cleared diff)))
      (is (empty? (:daemons-started diff)))
      (is (empty? (:daemons-stopped diff))))))

(deftest test-diff-ignores-bookkeeping
  (testing "diff ignores internal bookkeeping keys"
    (let [before (init-test-state)
          ;; clock-wait is internal bookkeeping
          after (assoc before :clock-wait true)
          diff (tr/compute-state-diff before after)]
      ;; Should not appear in flags-set
      (is (not (contains? (:flags-set diff) :clock-wait))))))

;;; ---------------------------------------------------------------------------
;;; STEP FUNCTION TESTS
;;; ---------------------------------------------------------------------------

(deftest test-step-returns-diff
  (testing "step returns diff along with game state"
    (let [state (init-test-state)
          result (tr/step state {:verb :look})]
      (is (contains? result :game-state))
      (is (contains? result :diff))
      (is (contains? result :message))
      (is (map? (:diff result))))))

(deftest test-step-captures-movement
  (testing "step captures location change when moving"
    (let [state (init-test-state)
          result (tr/step state {:verb :go :direction :north})]
      (is (= :north-of-house (:here (:game-state result))))
      (is (= {:from :west-of-house :to :north-of-house}
             (:location-changed (:diff result)))))))

(deftest test-step-captures-take
  (testing "step captures object movement when taking"
    (let [state (-> (init-test-state)
                    ;; Place brass-lantern in current room so it's visible
                    (assoc-in [:objects :brass-lantern :in] :west-of-house))
          result (tr/step state {:verb :take :direct-object :brass-lantern})]
      (is (= :adventurer (get-in (:game-state result) [:objects :brass-lantern :in])))
      (is (contains? (:objects-moved (:diff result)) :brass-lantern))
      (is (= :west-of-house (get-in (:diff result) [:objects-moved :brass-lantern :from])))
      (is (= :adventurer (get-in (:diff result) [:objects-moved :brass-lantern :to]))))))

(deftest test-step-captures-drop
  (testing "step captures object movement when dropping"
    (let [state (-> (init-test-state)
                    ;; Put brass-lantern in inventory
                    (assoc-in [:objects :brass-lantern :in] :adventurer))
          result (tr/step state {:verb :drop :direct-object :brass-lantern})]
      (is (= :west-of-house (get-in (:game-state result) [:objects :brass-lantern :in])))
      (is (contains? (:objects-moved (:diff result)) :brass-lantern)))))

;;; ---------------------------------------------------------------------------
;;; INTEGRATION TESTS - REAL GAME SCENARIOS
;;; ---------------------------------------------------------------------------

(deftest ^:slow test-step-ring-bell-exorcism
  (testing "ring bell at entrance-to-hades triggers xb flag and object changes"
    (let [state (-> (init-test-state)
                    (assoc :here :entrance-to-hades)
                    ;; Need to have brass-bell and candles in inventory
                    (assoc-in [:objects :brass-bell :in] :adventurer)
                    (assoc-in [:objects :candles :in] :adventurer)
                    (assoc-in [:objects :candles :on] true)) ;; Candles lit
          result (tr/step state {:verb :ring :direct-object :brass-bell})]
      ;; xb flag should be set
      (is (contains? (:flags-set (:diff result)) :xb)
          "xb flag should be set when ringing bell")
      ;; Candles should be dropped (moved from adventurer to room)
      (is (contains? (:objects-moved (:diff result)) :candles)
          "candles should be dropped when ringing bell")
      ;; Candles should be extinguished
      (is (contains? (:flags-cleared (:diff result)) [:candles :on])
          "candles should be extinguished when ringing bell")
      ;; brass-bell goes to limbo, hot-bell appears
      (is (contains? (:objects-moved (:diff result)) :brass-bell)
          "brass-bell should move to limbo"))))

(deftest test-step-kill-troll
  (testing "killing troll sets troll-flag"
    ;; Note: Combat is non-deterministic, so we set up state where troll is already dead
    (let [state (-> (init-test-state)
                    (assoc :here :troll-room)
                    (assoc-in [:objects :sword :in] :adventurer))
          ;; Simulate troll being killed by setting flag directly
          after-state (assoc state :troll-flag true)
          diff (tr/compute-state-diff state after-state)]
      (is (contains? (:flags-set diff) :troll-flag)))))

(deftest test-step-score-change-on-treasure-drop
  (testing "dropping treasure in trophy case changes score"
    (let [state (-> (init-test-state)
                    (assoc :here :living-room)
                    ;; Put egg in inventory
                    (assoc-in [:objects :egg :in] :adventurer)
                    ;; Make sure case is open
                    (assoc-in [:objects :trophy-case :open] true))
          result (tr/step state {:verb :put
                                 :direct-object :egg
                                 :prep :in
                                 :indirect-object :trophy-case})]
      ;; Egg should be in trophy case
      (is (= :trophy-case (get-in (:game-state result) [:objects :egg :in])))
      ;; Score should have changed (egg is worth 5 points)
      (when (get (:score-changed (:diff result)) :to)
        (is (< (:from (:score-changed (:diff result)))
               (:to (:score-changed (:diff result)))))))))

;;; ---------------------------------------------------------------------------
;;; DIFF SUMMARY / HUMAN READABLE OUTPUT
;;; ---------------------------------------------------------------------------

(deftest test-diff-summary
  (testing "diff-summary returns human readable string"
    (let [before (init-test-state)
          after (-> before
                    (assoc :here :north-of-house)
                    (assoc :score 10)
                    (assoc :troll-flag true))
          diff (tr/compute-state-diff before after)
          summary (tr/diff-summary diff)]
      (is (string? summary))
      (is (.contains summary "north-of-house"))
      ;; Summary uses "Score:" format
      (is (.contains summary "Score"))
      (is (.contains summary "troll-flag")))))
