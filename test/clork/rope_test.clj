(ns clork.rope-test
  "Tests for rope actions: tying to railing, climbing, untying."
  (:require [clojure.test :refer [deftest is testing]]
            [clork.game-state :as gs]
            [clork.core :as core]
            [clork.parser :as parser]
            [clork.verb-defs :as verb-defs]
            [clork.daemon :as daemon]))

;;; ---------------------------------------------------------------------------
;;; Test Utilities
;;; ---------------------------------------------------------------------------

(defn make-test-state
  "Create a test game state with rooms and objects initialized."
  []
  (core/init-game))

(defn run-command
  "Parse and execute a command, returning [output new-state]."
  [game-state input]
  (let [output (java.io.StringWriter.)
        gs (binding [*out* output]
             (-> game-state
                 (assoc :input input)
                 parser/parser-from-input
                 verb-defs/perform
                 daemon/clocker))]
    [(str output) gs]))

;;; ---------------------------------------------------------------------------
;;; Basic Rope State Tests
;;; ---------------------------------------------------------------------------

(deftest rope-starts-in-attic-test
  (testing "rope starts in attic"
    (let [gs (make-test-state)
          rope-loc (gs/get-thing-loc-id gs :rope)]
      (is (= :attic rope-loc)))))

(deftest dome-flag-starts-false-test
  (testing "dome-flag starts false (rope not tied)"
    (let [gs (make-test-state)]
      (is (not (get gs :dome-flag false))))))

;;; ---------------------------------------------------------------------------
;;; Tie Rope Tests
;;; ---------------------------------------------------------------------------

(deftest tie-rope-to-railing-test
  (testing "tying rope to railing in dome room sets dome-flag"
    (let [gs (-> (make-test-state)
                 (assoc :here :dome-room)
                 (assoc :lit true)  ; Need light to see objects
                 (assoc-in [:objects :rope :in] :adventurer))
          [output gs'] (run-command gs "tie rope to railing")]
      ;; Should succeed and set dome-flag
      (is (clojure.string/includes? output "drops over"))
      (is (get gs' :dome-flag false)))))

(deftest tie-rope-already-tied-test
  (testing "tying rope when already tied gives message"
    (let [gs (-> (make-test-state)
                 (assoc :here :dome-room)
                 (assoc :lit true)
                 (assoc :dome-flag true)
                 (assoc-in [:objects :rope :in] :dome-room))
          [output _] (run-command gs "tie rope to railing")]
      ;; Should mention already tied
      (is (clojure.string/includes? output "already tied")))))

;;; ---------------------------------------------------------------------------
;;; Untie Rope Tests
;;; ---------------------------------------------------------------------------

(deftest untie-rope-when-tied-test
  (testing "untying rope when tied clears dome-flag"
    (let [gs (-> (make-test-state)
                 (assoc :here :dome-room)
                 (assoc :lit true)  ; Need light to see objects
                 (assoc :dome-flag true)
                 (assoc-in [:objects :rope :in] :dome-room))
          [output gs'] (run-command gs "untie rope")]
      ;; Should untie and clear dome-flag
      (is (clojure.string/includes? output "now untied"))
      (is (not (get gs' :dome-flag false))))))

(deftest untie-rope-not-tied-test
  (testing "untying rope when not tied gives message"
    (let [gs (-> (make-test-state)
                 (assoc :here :dome-room)
                 (assoc :lit true)
                 (assoc :dome-flag false)
                 (assoc-in [:objects :rope :in] :dome-room))
          [output _] (run-command gs "untie rope")]
      ;; Should say not tied
      (is (clojure.string/includes? output "not tied")))))

;;; ---------------------------------------------------------------------------
;;; Drop Rope Tests
;;; ---------------------------------------------------------------------------

(deftest drop-rope-in-dome-falls-test
  (testing "dropping rope in dome room moves it to torch room"
    (let [gs (-> (make-test-state)
                 (assoc :here :dome-room)
                 (assoc :lit true)
                 (assoc :dome-flag false)
                 (assoc-in [:objects :rope :in] :adventurer))
          [output gs'] (run-command gs "drop rope")]
      ;; Should drop the rope to torch room
      (is (clojure.string/includes? output "drops gently"))
      (is (= :torch-room (gs/get-thing-loc-id gs' :rope))))))

;;; ---------------------------------------------------------------------------
;;; Take Rope Tests
;;; ---------------------------------------------------------------------------

(deftest take-rope-when-tied-fails-test
  (testing "taking rope when tied gives message"
    (let [gs (-> (make-test-state)
                 (assoc :here :dome-room)
                 (assoc :lit true)
                 (assoc :dome-flag true)
                 (assoc-in [:objects :rope :in] :dome-room))
          [output gs'] (run-command gs "take rope")]
      ;; Should say rope is tied to railing
      (is (clojure.string/includes? output "tied to"))
      ;; Rope should still be in room
      (is (= :dome-room (gs/get-thing-loc-id gs' :rope))))))
