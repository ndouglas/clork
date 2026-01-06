(ns clork.chimney-test
  "Tests for chimney climbing from studio to kitchen."
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
;;; Chimney Climb Tests
;;; ---------------------------------------------------------------------------

(deftest chimney-climb-basic-test
  (testing "go up from studio takes player to kitchen"
    (let [gs (-> (make-test-state)
                 (assoc :here :studio)
                 (assoc :lit true)
                 (assoc-in [:objects :brass-lantern :in] :adventurer)
                 (gs/set-thing-flag :brass-lantern :on))
          [output gs'] (run-command gs "go up")]
      (println "Output:" output)
      (println "Location before:" (:here gs))
      (println "Location after:" (:here gs'))
      (is (= :kitchen (:here gs'))))))

(deftest go-up-chimney-command-test
  (testing "go up chimney command moves player from studio to kitchen"
    (let [gs (-> (make-test-state)
                 (assoc :here :studio)
                 (assoc :lit true)
                 (assoc-in [:objects :brass-lantern :in] :adventurer)
                 (assoc-in [:objects :adventurer :in] :studio)
                 (gs/set-thing-flag :brass-lantern :on))
          [output gs'] (run-command gs "go up chimney")]
      ;; Should move to kitchen
      (is (= :kitchen (:here gs')))
      (is (clojure.string/includes? output "Kitchen")))))

(deftest up-command-test
  (testing "simple 'up' command from studio"
    (let [gs (-> (make-test-state)
                 (assoc :here :studio)
                 (assoc :lit true)
                 (assoc-in [:objects :brass-lantern :in] :adventurer)
                 (gs/set-thing-flag :brass-lantern :on))
          [output gs'] (run-command gs "up")]
      (println "Output for 'up':" output)
      (println "Location before:" (:here gs))
      (println "Location after:" (:here gs')))))

(deftest climb-chimney-command-test
  (testing "'climb chimney' command from studio"
    (let [gs (-> (make-test-state)
                 (assoc :here :studio)
                 (assoc :lit true)
                 (assoc-in [:objects :brass-lantern :in] :adventurer)
                 (gs/set-thing-flag :brass-lantern :on))
          [output gs'] (run-command gs "climb chimney")]
      (println "Output for 'climb chimney':" output)
      (println "Location after:" (:here gs')))))
