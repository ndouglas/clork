(ns clork.pray-test
  "Tests for pray verb - teleport from south-temple to forest."
  (:require [clojure.test :refer [deftest is testing]]
            [clork.game-state :as gs]
            [clork.core :as core]
            [clork.parser :as parser]
            [clork.verb-defs :as verb-defs]
            [clork.verbs-misc :as verbs-misc]
            [clork.daemon :as daemon]))

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

(deftest v-pray-direct-call-test
  (testing "v-pray directly called should teleport to forest-1"
    (let [gs (-> (make-test-state)
                 (assoc :here :south-temple)
                 (assoc :lit true))
          _ (println "Before v-pray - :here =" (:here gs))
          gs' (verbs-misc/v-pray gs)
          _ (println "After v-pray - :here =" (:here gs'))]
      (is (= :forest-1 (:here gs'))))))

(deftest pray-via-parser-test
  (testing "pray command via parser should teleport to forest-1"
    (let [gs (-> (make-test-state)
                 (assoc :here :south-temple)
                 (assoc :lit true))
          _ (println "Before run-command - :here =" (:here gs))
          [output gs'] (run-command gs "pray")
          _ (println "Output:" output)
          _ (println "After run-command - :here =" (:here gs'))]
      (is (clojure.string/includes? output "answered"))
      (is (= :forest-1 (:here gs'))))))
