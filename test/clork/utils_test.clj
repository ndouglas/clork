(ns clork.utils-test
  "Test utilities and helpers for Clork tests."
  (:require [clojure.test :refer :all]
            [clork.utils :as utils]
            [clork.game-state :as gs]
            [clork.parser :as parser]
            [clork.rooms :as rooms]
            [clork.objects :as objects]))

;;; ---------------------------------------------------------------------------
;;; TEST HELPERS
;;; ---------------------------------------------------------------------------

(defmacro with-captured-output
  "Execute body while capturing stdout. Returns [output-string result].

   Usage:
     (let [[output state] (with-captured-output (v-verbose game-state))]
       (is (= \"Maximum verbosity.\" output))
       (is (= true (:verbose state))))"
  [& body]
  `(let [sw# (java.io.StringWriter.)]
     (binding [*out* sw#]
       (let [result# (do ~@body)]
         [(str sw#) result#]))))

(defn make-test-state
  "Create a game state suitable for testing with rooms and objects loaded."
  []
  (-> (gs/initial-game-state)
      (gs/add-rooms [rooms/west-of-house])
      (gs/add-objects [objects/adventurer objects/mailbox])))

(defn parse-test-input
  "Parse a test command and return the resulting game state.

   Simulates what the parser does: tokenizes input, sets len, and parses.
   Useful for testing verbs without dealing with stdin."
  [game-state input]
  (let [lexv (parser/lexv-from-input input)
        gs (-> game-state
               (parser/parser-init)
               (parser/parser-set-winner-to-player)
               (assoc :input input)
               (assoc-in [:parser :lexv] lexv)
               (assoc-in [:parser :len] (count (:tokens lexv))))
        ;; Save lexv for AGAIN and run parse-command
        gs (-> gs
               (assoc-in [:parser :again-lexv] (get-in gs [:parser :lexv]))
               (assoc-in [:parser :dir] nil)
               (assoc-in [:parser :ncn] 0)
               (assoc-in [:parser :getflags] 0))]
    (parser/parse-command gs)))

;;; ---------------------------------------------------------------------------
;;; UTILS TESTS
;;; ---------------------------------------------------------------------------

(deftest crlf-test
  (testing "(crlf game-state) prints a newline and returns game-state"
    (let [game-state {:test true}]
      (is (= "\n" (with-out-str (utils/crlf game-state))))
      (is (= game-state (utils/crlf game-state))))))
