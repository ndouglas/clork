(ns clork.property-test
  "Property-based testing for game state invariants.

   Uses test.check to generate random command sequences and verify
   that game state remains valid after each command."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.string :as str]
            [clork.core :as core]
            [clork.game-state :as game-state]
            [clork.script :as script]
            [clork.random :as random]))

;;; ---------------------------------------------------------------------------
;;; TEST RUNNER
;;; ---------------------------------------------------------------------------

(defn run-script
  "Run Clork with the given script content and config options.
   Returns {:game-state final-state :exit-code code :output output-str}."
  [script-content & {:keys [strict fail-on-parser-error max-turns seed]
                     :or {strict false
                          fail-on-parser-error false
                          max-turns nil
                          seed 12345}}]
  (let [config {:script-mode? true
                :strict strict
                :fail-on-death false
                :fail-on-parser-error (or strict fail-on-parser-error)
                :max-turns max-turns
                :quiet true
                :seed seed}
        result (atom nil)]
    ;; Initialize random with seed for reproducibility
    (random/init! seed)
    ;; Capture output and run game
    (let [output (with-out-str
                   (with-in-str script-content
                     (let [final-state (core/go (game-state/initial-game-state) config)
                           exit-code (script/determine-exit-code final-state config)]
                       (reset! result {:game-state final-state
                                       :exit-code exit-code}))))]
      (assoc @result :output output))))

;;; ---------------------------------------------------------------------------
;;; GENERATORS
;;; ---------------------------------------------------------------------------

(def direction-commands
  "Commands that move the player."
  ["n" "s" "e" "w" "ne" "nw" "se" "sw" "u" "d"])

(def basic-commands
  "Simple commands that should always work."
  ["look" "inventory" "score" "diagnose" "verbose" "brief"])

(def object-verbs
  "Verbs that operate on objects."
  ["take" "drop" "examine" "open" "close"])

(def common-objects
  "Objects likely to exist in the game."
  ["lamp" "sword" "leaflet" "mailbox" "rug" "window" "door"
   "trophy case" "rope" "knife" "sack" "bottle" "egg" "nest"])

;; Generator for movement commands
(def gen-direction
  (gen/elements direction-commands))

;; Generator for basic commands
(def gen-basic-command
  (gen/elements basic-commands))

;; Generator for object interaction commands
(def gen-object-command
  (gen/let [verb (gen/elements object-verbs)
            obj (gen/elements common-objects)]
    (str verb " " obj)))

;; Generator for any safe command (won't crash the game)
(def gen-safe-command
  (gen/frequency [[5 gen-direction]
                  [3 gen-basic-command]
                  [2 gen-object-command]]))

;; Generator for a sequence of commands
(def gen-command-sequence
  (gen/vector gen-safe-command 1 20))

;;; ---------------------------------------------------------------------------
;;; PROPERTIES
;;; ---------------------------------------------------------------------------

(defn valid-state-after-commands?
  "Check if game state is valid after executing a sequence of commands."
  [commands]
  (let [script (str (str/join "\n" commands) "\n$quit\n")
        result (run-script script :seed 42 :max-turns 100)
        gs (:game-state result)
        validation (game-state/validate-state gs)]
    (:valid? validation)))

(defn no-nil-in-output?
  "Check that output doesn't contain unexpected 'nil' strings."
  [commands]
  (let [script (str (str/join "\n" commands) "\n$quit\n")
        result (run-script script :seed 42 :max-turns 100)
        output (:output result)]
    ;; Allow "nil" in context but not standalone
    (not (re-find #"(?m)^nil$" output))))

(defn rooms-never-empty?
  "Check that rooms are never empty during execution."
  [commands]
  (let [script (str (str/join "\n" commands) "\n$quit\n")
        result (run-script script :seed 42 :max-turns 100)
        gs (:game-state result)]
    (pos? (count (:rooms gs {})))))

;;; ---------------------------------------------------------------------------
;;; PROPERTY TESTS
;;; ---------------------------------------------------------------------------

(defspec state-remains-valid-after-random-commands
  50  ;; number of test iterations
  (prop/for-all [commands gen-command-sequence]
                (valid-state-after-commands? commands)))

(defspec output-never-contains-bare-nil
  50
  (prop/for-all [commands gen-command-sequence]
                (no-nil-in-output? commands)))

(defspec rooms-collection-never-empty
  50
  (prop/for-all [commands gen-command-sequence]
                (rooms-never-empty? commands)))

;;; ---------------------------------------------------------------------------
;;; TARGETED PROPERTY TESTS
;;; ---------------------------------------------------------------------------
;;; These test specific scenarios that have caused bugs in the past.

;; Movement commands followed by look
(def gen-movement-then-look
  (gen/let [dirs (gen/vector gen-direction 1 5)]
    (conj (vec dirs) "look")))

(defspec movement-then-look-preserves-state
  50
  (prop/for-all [commands gen-movement-then-look]
                (valid-state-after-commands? commands)))

;; Commands that might trigger death and resurrection
(def gen-dark-room-sequence
  "Commands that might lead to grue death."
  (gen/elements [["open window" "enter" "w" "open trap door" "d" "look"]
                 ["open window" "enter" "w" "open trap door" "d" "w"]
                 ["open window" "enter" "w" "open trap door" "d" "s"]]))

(defspec dark-room-death-preserves-state
  20
  (prop/for-all [commands gen-dark-room-sequence]
                (let [script (str (str/join "\n" commands) "\n$quit\n")
          ;; Use seeds that are likely to trigger grue attack
                      results (for [seed [42 123 456 789 999]]
                                (run-script script :seed seed :max-turns 50))
                      validations (map #(game-state/validate-state (:game-state %)) results)]
                  (every? :valid? validations))))

;;; ---------------------------------------------------------------------------
;;; REGRESSION TESTS AS PROPERTIES
;;; ---------------------------------------------------------------------------

(deftest property-test-sanity-check
  (testing "property test generators produce valid data"
    ;; Just verify the generators work
    (is (every? string? (gen/sample gen-direction 10)))
    (is (every? string? (gen/sample gen-basic-command 10)))
    (is (every? string? (gen/sample gen-object-command 10)))
    (is (every? vector? (gen/sample gen-command-sequence 5)))))

(deftest explicit-regression-death-resurrection
  (testing "death and resurrection preserves state validity"
    ;; This is the exact scenario that originally failed
    (let [commands ["open window" "enter" "w"]
          script (str (str/join "\n" commands) "\n$quit\n")
          result (run-script script :seed 42)
          gs (:game-state result)
          validation (game-state/validate-state gs)]
      (is (:valid? validation)
          (str "State should be valid. Errors: " (:errors validation))))))
