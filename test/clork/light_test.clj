(ns clork.light-test
  "Tests for light source system: lantern, matches, candles."
  (:require [clojure.test :refer [deftest is testing]]
            [clork.game-state :as gs]
            [clork.core :as core]
            [clork.parser :as parser]
            [clork.verb-defs :as verb-defs]
            [clork.daemon :as daemon]
            [clork.light :as light]))

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

(defn run-commands
  "Run multiple commands in sequence, returning final state."
  [game-state commands]
  (reduce (fn [gs cmd]
            (second (run-command gs cmd)))
          game-state
          commands))

(defmacro with-captured-output
  "Execute body and capture stdout, returning [output result]."
  [& body]
  `(let [s# (java.io.StringWriter.)]
     (binding [*out* s#]
       (let [result# (do ~@body)]
         [(str s#) result#]))))

;;; ---------------------------------------------------------------------------
;;; Lantern Tests
;;; ---------------------------------------------------------------------------

(deftest lantern-examine-off-test
  (testing "examining lantern when off shows 'turned off'"
    (let [gs (-> (make-test-state)
                 ;; Move lantern to player
                 (assoc-in [:objects :brass-lantern :in] :adventurer))
          [output _] (run-command gs "examine lantern")]
      (is (clojure.string/includes? output "turned off")))))

(deftest lantern-turn-on-test
  (testing "turning on lantern shows status and sets :on flag"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :brass-lantern :in] :adventurer))
          [output gs'] (run-command gs "turn on lantern")]
      (is (clojure.string/includes? output "now on"))
      (is (gs/set-thing-flag? gs' :brass-lantern :on)))))

(deftest lantern-examine-on-test
  (testing "examining lantern when on shows 'on'"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :brass-lantern :in] :adventurer)
                 (gs/set-thing-flag :brass-lantern :on))
          [output _] (run-command gs "examine lantern")]
      (is (clojure.string/includes? output "lamp is on")))))

(deftest lantern-turn-off-test
  (testing "turning off lantern shows status and clears :on flag"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :brass-lantern :in] :adventurer)
                 (gs/set-thing-flag :brass-lantern :on))
          [output gs'] (run-command gs "turn off lantern")]
      (is (clojure.string/includes? output "now off"))
      (is (not (gs/set-thing-flag? gs' :brass-lantern :on))))))

(deftest lantern-already-on-test
  (testing "turning on lantern that's already on shows 'already on'"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :brass-lantern :in] :adventurer)
                 (gs/set-thing-flag :brass-lantern :on))
          [output _] (run-command gs "turn on lantern")]
      (is (clojure.string/includes? output "already on")))))

(deftest lantern-already-off-test
  (testing "turning off lantern that's already off shows 'already off'"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :brass-lantern :in] :adventurer))
          [output _] (run-command gs "turn off lantern")]
      (is (clojure.string/includes? output "already off")))))

(deftest lantern-daemon-enabled-on-turn-on-test
  (testing "turning on lantern enables the I-LANTERN daemon"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :brass-lantern :in] :adventurer))
          [_ gs'] (run-command gs "turn on lantern")]
      (is (daemon/daemon-enabled? gs' :i-lantern)))))

(deftest lantern-daemon-disabled-on-turn-off-test
  (testing "turning off lantern disables the I-LANTERN daemon"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :brass-lantern :in] :adventurer)
                 (gs/set-thing-flag :brass-lantern :on)
                 (daemon/enable :i-lantern))
          [_ gs'] (run-command gs "turn off lantern")]
      (is (not (daemon/daemon-enabled? gs' :i-lantern))))))

;;; ---------------------------------------------------------------------------
;;; Match Tests
;;; ---------------------------------------------------------------------------

(deftest match-examine-unlit-test
  (testing "examining matchbook when unlit shows match count"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :matchbook :in] :adventurer))
          [output _] (run-command gs "examine match")]
      (is (clojure.string/includes? output "6 matches")))))

(deftest match-light-test
  (testing "lighting match shows 'starts to burn' and sets flags"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :matchbook :in] :adventurer))
          [output gs'] (run-command gs "light match")]
      (is (clojure.string/includes? output "starts to burn"))
      (is (gs/set-thing-flag? gs' :matchbook :on))
      (is (gs/set-thing-flag? gs' :matchbook :flame)))))

(deftest match-examine-lit-test
  (testing "examining match when lit shows 'burning'"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :matchbook :in] :adventurer)
                 (gs/set-thing-flag :matchbook :on)
                 (gs/set-thing-flag :matchbook :flame))
          [output _] (run-command gs "examine match")]
      (is (clojure.string/includes? output "burning")))))

(deftest match-count-decrements-test
  (testing "lighting match decrements match count"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :matchbook :in] :adventurer))
          [_ gs'] (run-command gs "light match")]
      (is (= 5 (:match-count gs'))))))

(deftest match-burns-out-test
  (testing "match burns out after 2 turns via daemon"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :matchbook :in] :adventurer))
          ;; Light the match
          [_ gs1] (run-command gs "light match")
          ;; Wait a turn (daemon counts down)
          [_ gs2] (run-command gs1 "wait")
          ;; Next turn - daemon fires
          [output gs3] (run-command gs2 "wait")]
      ;; Match should have gone out by now
      (is (not (gs/set-thing-flag? gs3 :matchbook :on))))))

(deftest match-out-of-matches-test
  (testing "lighting match when out shows 'run out of matches'"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :matchbook :in] :adventurer)
                 (assoc :match-count 0))
          [output _] (run-command gs "light match")]
      (is (clojure.string/includes? output "run out of matches")))))

(deftest match-already-lit-test
  (testing "lighting match when already lit shows 'already lit'"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :matchbook :in] :adventurer)
                 (gs/set-thing-flag :matchbook :on))
          [output _] (run-command gs "light match")]
      (is (clojure.string/includes? output "already lit")))))

;;; ---------------------------------------------------------------------------
;;; Candles Tests
;;; ---------------------------------------------------------------------------

(deftest candles-examine-lit-test
  (testing "examining candles when lit shows 'lit'"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :candles :in] :adventurer)
                 (gs/set-thing-flag :candles :on))
          [output _] (run-command gs "examine candles")]
      (is (clojure.string/includes? output "lit")))))

(deftest candles-examine-unlit-test
  (testing "examining candles when unlit shows 'not lit'"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :candles :in] :adventurer)
                 (gs/unset-thing-flag :candles :on))
          [output _] (run-command gs "examine candles")]
      (is (clojure.string/includes? output "not lit")))))

(deftest candles-turn-off-test
  (testing "turning off candles when lit works"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :candles :in] :adventurer)
                 (gs/set-thing-flag :candles :on))
          [output gs'] (run-command gs "turn off candles")]
      (is (clojure.string/includes? output "out"))
      (is (not (gs/set-thing-flag? gs' :candles :on))))))

(deftest candles-need-fire-test
  (testing "lighting candles without fire source fails"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :candles :in] :adventurer)
                 (gs/unset-thing-flag :candles :on))
          [output _] (run-command gs "turn on candles")]
      (is (clojure.string/includes? output "flame")))))

(deftest candles-light-with-match-test
  (testing "lighting candles with lit match works"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :candles :in] :adventurer)
                 (assoc-in [:objects :matchbook :in] :adventurer)
                 (gs/unset-thing-flag :candles :on)
                 (gs/set-thing-flag :matchbook :on)
                 (gs/set-thing-flag :matchbook :flame))
          [output gs'] (run-command gs "turn on candles")]
      (is (clojure.string/includes? output "lit"))
      (is (gs/set-thing-flag? gs' :candles :on)))))

;;; ---------------------------------------------------------------------------
;;; Object Action Handler Pattern Tests
;;; ---------------------------------------------------------------------------

(deftest examine-calls-object-action-test
  (testing "examine verb calls object's :action handler"
    ;; This tests the fix we made to v-examine
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :brass-lantern :in] :adventurer)
                 (gs/set-thing-flag :brass-lantern :on))
          [output _] (run-command gs "examine lantern")]
      ;; Should show custom message from lantern-action, not default
      (is (clojure.string/includes? output "lamp is on"))
      (is (not (clojure.string/includes? output "nothing special"))))))

(deftest lamp-on-calls-object-action-test
  (testing "lamp-on verb calls object's :action handler"
    ;; This tests the fix we made to v-lamp-on
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :matchbook :in] :adventurer))
          [output _] (run-command gs "light match")]
      ;; Should show custom message from match-action
      (is (clojure.string/includes? output "starts to burn")))))
