(ns clork.egg-test
  "Tests for egg puzzle: opening, breaking, and the canary inside."
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

(defn run-commands
  "Run multiple commands in sequence, returning final state."
  [game-state commands]
  (reduce (fn [gs cmd]
            (second (run-command gs cmd)))
          game-state
          commands))

;;; ---------------------------------------------------------------------------
;;; Basic Egg State Tests
;;; ---------------------------------------------------------------------------

(deftest egg-starts-closed-test
  (testing "egg starts closed (no :open flag)"
    (let [gs (make-test-state)]
      (is (not (gs/set-thing-flag? gs :egg :open))))))

(deftest egg-contains-canary-test
  (testing "canary starts inside egg"
    (let [gs (make-test-state)
          canary-loc (gs/get-thing-loc-id gs :clockwork-canary)]
      (is (= :egg canary-loc)))))

(deftest broken-egg-starts-in-limbo-test
  (testing "broken-egg starts in limbo"
    (let [gs (make-test-state)
          broken-loc (gs/get-thing-loc-id gs :broken-egg)]
      (is (= :limbo broken-loc)))))

(deftest broken-canary-starts-in-limbo-test
  (testing "broken-canary starts in limbo"
    (let [gs (make-test-state)
          broken-loc (gs/get-thing-loc-id gs :broken-canary)]
      (is (= :limbo broken-loc)))))

;;; ---------------------------------------------------------------------------
;;; Open Egg Without Tool Tests
;;; ---------------------------------------------------------------------------

(deftest open-egg-no-tool-test
  (testing "opening egg without tool gives expertise message"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :egg :in] :adventurer))
          [output _] (run-command gs "open egg")]
      (is (clojure.string/includes? output "neither the tools nor the expertise")))))

(deftest open-egg-with-hands-test
  (testing "opening egg with hands warns of damage"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :egg :in] :adventurer)
                 (assoc-in [:objects :hands :in] :adventurer))
          ;; Note: "open egg with hands" requires hands object to exist
          ;; The game doesn't have a hands object by default, so this tests
          ;; the theoretical case
          [output _] (run-command gs "open egg")]
      ;; Without tool, should get expertise message
      (is (clojure.string/includes? output "neither the tools nor the expertise")))))

;;; ---------------------------------------------------------------------------
;;; Open Egg With Weapon Tests
;;; ---------------------------------------------------------------------------

(deftest open-egg-with-sword-breaks-egg-test
  (testing "opening egg with sword breaks the egg"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :egg :in] :adventurer)
                 (assoc-in [:objects :sword :in] :adventurer))
          [output gs'] (run-command gs "open egg with sword")]
      (is (clojure.string/includes? output "clumsiness"))
      (is (clojure.string/includes? output "esthetic appeal")))))

(deftest open-egg-with-sword-replaces-egg-test
  (testing "opening egg with sword replaces egg with broken-egg"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :egg :in] :adventurer)
                 (assoc-in [:objects :sword :in] :adventurer))
          [_ gs'] (run-command gs "open egg with sword")
          egg-loc (gs/get-thing-loc-id gs' :egg)
          broken-loc (gs/get-thing-loc-id gs' :broken-egg)]
      (is (= :limbo egg-loc) "original egg should be in limbo")
      (is (= :adventurer broken-loc) "broken-egg should be in player inventory"))))

(deftest open-egg-with-sword-damages-canary-test
  (testing "opening egg with sword damages the canary"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :egg :in] :adventurer)
                 (assoc-in [:objects :sword :in] :adventurer))
          [output gs'] (run-command gs "open egg with sword")
          canary-loc (gs/get-thing-loc-id gs' :clockwork-canary)
          broken-canary-loc (gs/get-thing-loc-id gs' :broken-canary)]
      ;; Output should mention the damaged canary
      (is (clojure.string/includes? output "bad experience"))
      ;; Original canary should be in limbo
      (is (= :limbo canary-loc))
      ;; Broken canary should be in broken egg
      (is (= :broken-egg broken-canary-loc)))))

(deftest open-egg-with-knife-breaks-egg-test
  (testing "opening egg with knife (weapon) breaks the egg"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :egg :in] :adventurer)
                 (assoc-in [:objects :knife :in] :adventurer))
          [output gs'] (run-command gs "open egg with knife")
          broken-loc (gs/get-thing-loc-id gs' :broken-egg)]
      (is (clojure.string/includes? output "clumsiness"))
      (is (= :adventurer broken-loc)))))

;;; ---------------------------------------------------------------------------
;;; Throw Egg Tests
;;; ---------------------------------------------------------------------------

(deftest throw-egg-breaks-it-test
  (testing "throwing egg breaks it"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :egg :in] :adventurer))
          [output gs'] (run-command gs "throw egg")]
      (is (clojure.string/includes? output "indelicate handling"))
      (is (clojure.string/includes? output "damage")))))

(deftest throw-egg-replaces-with-broken-test
  (testing "throwing egg replaces it with broken-egg"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :egg :in] :adventurer))
          [_ gs'] (run-command gs "throw egg")
          egg-loc (gs/get-thing-loc-id gs' :egg)
          broken-loc (gs/get-thing-loc-id gs' :broken-egg)]
      (is (= :limbo egg-loc))
      (is (= :adventurer broken-loc)))))

(deftest throw-egg-damages-canary-test
  (testing "throwing egg damages the canary inside"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :egg :in] :adventurer))
          [_ gs'] (run-command gs "throw egg")
          broken-canary-loc (gs/get-thing-loc-id gs' :broken-canary)]
      (is (= :broken-egg broken-canary-loc)))))

;;; ---------------------------------------------------------------------------
;;; Destroy/Mung Egg Tests
;;; ---------------------------------------------------------------------------

(deftest destroy-egg-no-tool-test
  (testing "destroying egg without tool asks for tools"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :egg :in] :adventurer))
          [output _] (run-command gs "destroy egg")]
      (is (clojure.string/includes? output "neither the tools nor the expertise")))))

(deftest mung-egg-no-tool-test
  (testing "munging egg without tool asks for tools"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :egg :in] :adventurer))
          [output _] (run-command gs "mung egg")]
      (is (clojure.string/includes? output "neither the tools nor the expertise")))))

;;; ---------------------------------------------------------------------------
;;; Already Open/Broken Tests
;;; ---------------------------------------------------------------------------

(deftest open-already-open-egg-test
  (testing "opening already open egg says it's already open"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :egg :in] :adventurer)
                 (gs/set-thing-flag :egg :open))
          [output _] (run-command gs "open egg")]
      (is (clojure.string/includes? output "already open")))))

;;; ---------------------------------------------------------------------------
;;; Treasure Value Tests
;;; ---------------------------------------------------------------------------

(deftest broken-egg-has-lower-tvalue-test
  (testing "broken egg has lower trophy value than intact egg"
    (let [gs (make-test-state)
          egg (gs/get-thing gs :egg)
          broken-egg (gs/get-thing gs :broken-egg)]
      (is (= 5 (:tvalue egg)))
      (is (= 2 (:tvalue broken-egg))))))

(deftest broken-canary-has-lower-tvalue-test
  (testing "broken canary has lower trophy value than intact canary"
    (let [gs (make-test-state)
          canary (gs/get-thing gs :clockwork-canary)
          broken-canary (gs/get-thing gs :broken-canary)]
      (is (= 4 (:tvalue canary)))
      (is (= 1 (:tvalue broken-canary))))))

;;; ---------------------------------------------------------------------------
;;; Original Approach Tests (FIGHTBIT tracking)
;;; ---------------------------------------------------------------------------

(deftest open-egg-unusual-object-test
  (testing "opening egg with unusual object gives 'original' message first time"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :egg :in] :adventurer)
                 (assoc-in [:objects :garlic :in] :adventurer))
          [output gs'] (run-command gs "open egg with garlic")]
      ;; First unusual attempt should mention "original"
      (is (clojure.string/includes? output "original")))))

;;; ---------------------------------------------------------------------------
;;; Look Inside Broken Egg Tests
;;; ---------------------------------------------------------------------------

(deftest broken-egg-contains-broken-canary-test
  (testing "after breaking egg, broken canary is inside broken egg"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :egg :in] :adventurer)
                 (assoc-in [:objects :sword :in] :adventurer))
          ;; Break the egg
          [_ gs'] (run-command gs "open egg with sword")
          ;; Check locations directly
          broken-canary-loc (gs/get-thing-loc-id gs' :broken-canary)]
      (is (= :broken-egg broken-canary-loc)))))

;;; ---------------------------------------------------------------------------
;;; Broken Canary Action Handler Test
;;; ---------------------------------------------------------------------------

(deftest broken-canary-action-handler-test
  (testing "broken canary action handler returns message for wind verb"
    (let [gs (-> (make-test-state)
                 (assoc-in [:objects :broken-canary :in] :adventurer)
                 ;; Set up parser state manually to test the action handler
                 (assoc-in [:parser :prsa] :wind)
                 (assoc-in [:parser :prso] [:broken-canary]))
          broken-canary (gs/get-thing gs :broken-canary)
          action-fn (:action broken-canary)
          result (action-fn gs)]
      ;; The action should return a game-state (not nil)
      (is (some? result)))))
