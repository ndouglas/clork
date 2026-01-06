(ns clork.painting-test
  "Tests for painting actions: vandalism/destruction."
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
;;; Basic Painting State Tests
;;; ---------------------------------------------------------------------------

(deftest painting-starts-in-gallery-test
  (testing "painting starts in gallery"
    (let [gs (make-test-state)
          painting-loc (gs/get-thing-loc-id gs :painting)]
      (is (= :gallery painting-loc)))))

(deftest painting-starts-with-tvalue-test
  (testing "painting starts with trophy value of 6"
    (let [gs (make-test-state)
          painting (gs/get-thing gs :painting)]
      (is (= 6 (:tvalue painting))))))

;;; ---------------------------------------------------------------------------
;;; Mung/Destroy Painting Tests
;;; ---------------------------------------------------------------------------

(deftest mung-painting-destroys-value-test
  (testing "munging painting sets tvalue to 0"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :painting :in] :adventurer))
          [output gs'] (run-command gs "destroy painting")
          painting (gs/get-thing gs' :painting)]
      (is (clojure.string/includes? output "vandal"))
      (is (= 0 (:tvalue painting))))))

(deftest mung-painting-changes-ldesc-test
  (testing "munging painting changes ldesc"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :painting :in] :adventurer))
          [_ gs'] (run-command gs "destroy painting")
          painting (gs/get-thing gs' :painting)]
      (is (clojure.string/includes? (:ldesc painting) "worthless")))))

(deftest painting-still-exists-after-mung-test
  (testing "painting still exists after munging (just worthless)"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :painting :in] :adventurer))
          [_ gs'] (run-command gs "destroy painting")
          painting-loc (gs/get-thing-loc-id gs' :painting)]
      ;; Painting should still be in inventory, not limbo
      (is (= :adventurer painting-loc)))))
