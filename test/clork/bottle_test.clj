(ns clork.bottle-test
  "Tests for bottle actions: throwing, breaking, shaking."
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
;;; Basic Bottle State Tests
;;; ---------------------------------------------------------------------------

(deftest bottle-starts-with-water-test
  (testing "water starts inside bottle"
    (let [gs (make-test-state)
          water-loc (gs/get-thing-loc-id gs :water)]
      (is (= :bottle water-loc)))))

(deftest bottle-starts-closed-test
  (testing "bottle starts closed"
    (let [gs (make-test-state)]
      (is (not (gs/set-thing-flag? gs :bottle :open))))))

;;; ---------------------------------------------------------------------------
;;; Throw Bottle Tests
;;; ---------------------------------------------------------------------------

(deftest throw-bottle-shatters-test
  (testing "throwing bottle shatters it"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :bottle :in] :adventurer))
          [output gs'] (run-command gs "throw bottle")]
      (is (clojure.string/includes? output "shatters")))))

(deftest throw-bottle-moves-to-limbo-test
  (testing "throwing bottle moves it to limbo"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :bottle :in] :adventurer))
          [_ gs'] (run-command gs "throw bottle")
          bottle-loc (gs/get-thing-loc-id gs' :bottle)]
      (is (= :limbo bottle-loc)))))

(deftest throw-bottle-with-water-spills-test
  (testing "throwing bottle with water spills the water"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :bottle :in] :adventurer)
                 (assoc-in [:objects :water :in] :bottle))
          [output gs'] (run-command gs "throw bottle")]
      (is (clojure.string/includes? output "water spills"))
      (is (= :limbo (gs/get-thing-loc-id gs' :water))))))

;;; ---------------------------------------------------------------------------
;;; Shake Bottle Tests
;;; ---------------------------------------------------------------------------

(deftest shake-closed-bottle-does-nothing-test
  (testing "shaking closed bottle does nothing special"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :bottle :in] :adventurer))
          [output _] (run-command gs "shake bottle")]
      ;; Should get generic shake response, not spill message
      (is (not (clojure.string/includes? output "spills"))))))

(deftest shake-open-bottle-with-water-spills-test
  (testing "shaking open bottle with water spills it"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :bottle :in] :adventurer)
                 (assoc-in [:objects :water :in] :bottle)
                 (gs/set-thing-flag :bottle :open))
          [output gs'] (run-command gs "shake bottle")]
      (is (clojure.string/includes? output "spills"))
      (is (= :limbo (gs/get-thing-loc-id gs' :water))))))

(deftest shake-open-empty-bottle-nothing-test
  (testing "shaking open empty bottle does nothing"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :bottle :in] :adventurer)
                 (assoc-in [:objects :water :in] :limbo)
                 (gs/set-thing-flag :bottle :open))
          [output _] (run-command gs "shake bottle")]
      ;; Should not mention spilling since no water
      (is (not (clojure.string/includes? output "spills"))))))

;;; ---------------------------------------------------------------------------
;;; Mung/Destroy Bottle Tests
;;; ---------------------------------------------------------------------------

(deftest mung-bottle-destroys-test
  (testing "munging bottle destroys it"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :bottle :in] :adventurer))
          [output gs'] (run-command gs "destroy bottle")]
      (is (clojure.string/includes? output "destroys"))
      (is (= :limbo (gs/get-thing-loc-id gs' :bottle))))))
