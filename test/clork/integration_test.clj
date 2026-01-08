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
    (let [script "frobulate\n$quit\n"  ; Use a nonsense word (not xyzzy - that's a valid verb!)
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
    (let [script (slurp "scripts/alpha-milestone.txt")
          result (run-script script :strict true)]
      (is (= 0 (:exit-code result))
          "Alpha milestone walkthrough should pass when basic verbs are implemented"))))

(deftest ^:pending beta-milestone-test
  ;; This test is pending until the full game is winnable
  (testing "complete game walkthrough to victory"
    (let [script (slurp "scripts/beta-milestone.txt")
          result (run-script script :strict true)]
      (is (= 0 (:exit-code result))
          "Beta milestone should pass when game is fully winnable")
      (is (true? (:won (:game-state result)))
          "Player should have won the game"))))

(deftest take-all-test
  (testing "take all finds objects in room"
    (let [script "take all\n$quit\n"
          result (run-script script)]
      ;; Check that the mailbox was reported as a response to take all
      ;; The expected output is "small mailbox: It is securely anchored."
      (is (re-find #"small mailbox: It is securely anchored\." (:output result))
          (str "take all should find and report on the mailbox. Output:\n" (:output result))))))

;;; ---------------------------------------------------------------------------
;;; STATE INTEGRITY TESTS
;;; ---------------------------------------------------------------------------
;;; Tests that validate game state remains intact through various operations.
;;; Added after bug where room actions returning nil corrupted state.

(deftest state-integrity-after-room-transition-test
  (testing "state remains valid after moving through rooms with actions"
    ;; Walk through multiple rooms, including behind-house which has an action
    ;; that returns nil for :m-enter. This was the root cause of the bug.
    ;; Start at west-of-house, go south then east to behind-house, open window, enter
    (let [script "s\ne\nopen window\nenter\nw\ne\n$quit\n"
          result (run-script script :strict true)]
      (is (= 0 (:exit-code result))
          "Should complete without errors")
      ;; Validate state integrity
      (let [gs (:game-state result)
            validation (game-state/validate-state gs)]
        (is (:valid? validation)
            (str "Game state should be valid after room transitions. Errors: "
                 (:errors validation)))
        (is (pos? (count (:rooms gs)))
            "Rooms map should not be empty")
        (is (pos? (count (:objects gs)))
            "Objects map should not be empty")))))

(deftest death-resurrection-movement-test
  ;; This is the exact scenario that uncovered the bug:
  ;; After dying from a grue, the resurrection should place the player
  ;; in a lit room where they can move without immediately dying again.
  (testing "after death and resurrection, player can look and move"
    ;; Navigate to dark area and get killed by grue, then verify recovery
    ;; Using a specific seed that will trigger grue death
    (let [script (str "open window\n"    ; open window
                      "enter\n"           ; enter kitchen
                      "w\n"               ; go west to living room
                      "open trap door\n"  ; open trap door
                      "d\n"               ; go down into dark
                      "w\n"               ; walk west in dark (may die)
                      "look\n"            ; after resurrection, look
                      "$quit\n")
          result (run-script script :seed 42)]
      ;; Verify state is still valid
      (let [gs (:game-state result)
            validation (game-state/validate-state gs)]
        (is (:valid? validation)
            (str "Game state should be valid after death/resurrection. Errors: "
                 (:errors validation))))
      ;; Output should NOT contain nil from room descriptions
      (is (not (re-find #"\nnil\n" (:output result)))
          "Output should not contain 'nil' from room description"))))

(deftest room-action-nil-return-test
  (testing "rooms with actions that return nil for unknown rargs don't corrupt state"
    ;; Test specifically that entering behind-house (which has an action
    ;; that returns nil for :m-enter) doesn't corrupt state
    (let [script "n\nn\ne\n$quit\n"  ; west-of-house -> north -> north -> behind-house
          result (run-script script :strict true)]
      (let [gs (:game-state result)
            validation (game-state/validate-state gs)]
        (is (:valid? validation)
            (str "Game state should be valid after entering behind-house. Errors: "
                 (:errors validation)))))))

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

(deftest victory-test
  (testing "game ends with victory when entering barrow after winning"
    ;; Set up initial state with :won flag set (simulates score reaching 350)
    (random/init! 12345)
    (let [initial-gs (-> (game-state/initial-game-state)
                         (assoc :won true)
                         (assoc :score 350)
                         (assoc :script-config {:script-mode? true
                                                :quiet true}))]
      ;; Run script that goes SW to stone-barrow and enters
      (let [output (with-out-str
                     (with-in-str "sw\nenter\n"
                       (core/go initial-gs {:script-mode? true :quiet true})))]
        ;; Check that the victory message was displayed
        (is (re-find #"Inside the Barrow" output)
            (str "Should display victory message. Output:\n" output))
        (is (re-find #"ZORK trilogy" output)
            (str "Should mention ZORK trilogy. Output:\n" output))))))
