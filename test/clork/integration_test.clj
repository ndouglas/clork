(ns clork.integration-test
  "Integration tests that run Clork with script input.

   These tests verify end-to-end behavior using the script mode feature."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.java.io :as io]
            [clork.core :as core]
            [clork.game-state :as game-state]
            [clork.script :as script]
            [clork.random :as random]))

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

(deftest smoke-test-integration
  (testing "basic commands complete without errors"
    (let [script "look\nverbose\nbrief\nsuperbrief\ninventory\ndiagnose\nscore\nversion\n$quit\n"
          result (run-script script :strict true)]
      (is (= 0 (:exit-code result)) "Smoke test should pass with exit code 0")
      (is (true? (:quit (:game-state result))) "Game should have quit normally"))))

(deftest parser-error-detection-test
  (testing "unknown words trigger parser errors"
    (let [script "xyzzy\n$quit\n"
          result (run-script script :fail-on-parser-error true)]
      (is (= 2 (:exit-code result)) "Unknown word should cause exit code 2")
      (is (pos? (:parser-error-count (:game-state result)))
          "Parser error count should be > 0")))

  (testing "multiple errors are counted"
    (let [script "foo\nbar\nbaz\n$quit\n"
          result (run-script script :fail-on-parser-error true)]
      (is (= 3 (:parser-error-count (:game-state result)))
          "Should count 3 parser errors"))))

(deftest max-turns-test
  (testing "max turns causes exit"
    ;; Use game actions (open) since meta-verbs like look don't count as moves
    (let [script "open mailbox\nopen mailbox\nopen mailbox\nopen mailbox\nopen mailbox\n"
          result (run-script script :max-turns 3)]
      (is (= 4 (:exit-code result)) "Should exit with max-turns code")
      (is (true? (:max-turns-exceeded (:game-state result)))
          "max-turns-exceeded flag should be set"))))

(deftest comment-handling-test
  (testing "comments and blank lines are skipped"
    (let [script "; This is a comment\n\n# Another comment\nlook\n\n; More comments\n$quit\n"
          result (run-script script :strict true)]
      (is (= 0 (:exit-code result))
          "Comments and blank lines should not cause errors"))))

(deftest seed-reproducibility-test
  (testing "same seed produces identical results"
    (let [script "look\n$quit\n"
          result1 (run-script script :seed 42)
          result2 (run-script script :seed 42)]
      ;; Compare relevant parts of game state (excluding things that might vary)
      (is (= (:score (:game-state result1))
             (:score (:game-state result2)))
          "Same seed should produce same game state"))))

(deftest ^:pending alpha-milestone-test
  ;; This test is pending until we implement movement and object interaction
  (testing "early game walkthrough completes"
    (let [script (slurp "test/scripts/alpha-milestone.txt")
          result (run-script script :strict true)]
      (is (= 0 (:exit-code result))
          "Alpha milestone walkthrough should pass when basic verbs are implemented"))))

(deftest ^:pending beta-milestone-test
  ;; This test is pending until the full game is winnable
  (testing "complete game walkthrough to victory"
    (let [script (slurp "test/scripts/beta-milestone.txt")
          result (run-script script :strict true)]
      (is (= 0 (:exit-code result))
          "Beta milestone should pass when game is fully winnable")
      (is (true? (:won (:game-state result)))
          "Player should have won the game"))))

(deftest move-counting-test
  (testing "game actions increment move count"
    ;; open, read, drop are game actions that should count as moves
    (let [script "open mailbox\nread leaflet\ndrop leaflet\n$quit\n"
          result (run-script script :strict true)]
      (is (= 3 (:moves (:game-state result)))
          "open, read, and drop should each count as a move")))

  (testing "meta-verbs don't increment move count"
    ;; Only system verbs like score, verbose, brief don't count
    ;; Note: look and inventory DO count as moves (thief can attack while you look!)
    (let [script "score\nverbose\nbrief\nsuperbrief\n$quit\n"
          result (run-script script :strict true)]
      (is (= 0 (:moves (:game-state result)))
          "Meta-verbs should not count as moves")))

  (testing "look and inventory count as moves"
    ;; Unlike score/verbose, look and inventory run daemons and count as moves
    (let [script "look\ninventory\n$quit\n"
          result (run-script script :strict true)]
      (is (= 2 (:moves (:game-state result)))
          "look and inventory should count as moves")))

  (testing "mixed commands count correctly"
    ;; look, open, inventory count; score doesn't
    (let [script "look\nopen mailbox\nscore\ninventory\n$quit\n"
          result (run-script script :strict true)]
      (is (= 3 (:moves (:game-state result)))
          "look, open, and inventory should count as moves"))))
