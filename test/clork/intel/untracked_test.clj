(ns clork.intel.untracked-test
  "Tests for untracked change detection.

   These tests verify that the game code properly uses the tracked functions
   (gs/set-game-flag, gs/unset-game-flag, gs/set-thing-flag, gs/unset-thing-flag,
   gs/move-object) instead of direct assoc/assoc-in.

   Any test failure here indicates 'backdoor' code that bypasses the planner
   infrastructure and needs to be fixed."
  (:require [clojure.test :refer :all]
            [clork.core :as core]
            [clork.parser :as parser]
            [clork.verb-defs :as verb-defs]
            [clork.daemon :as daemon]
            [clork.game-state :as gs]
            [clork.intel.transition :as transition]))

;;; ---------------------------------------------------------------------------
;;; TEST UTILITIES
;;; ---------------------------------------------------------------------------

(defn run-command
  "Execute a command and return tracking information."
  [game-state command]
  (let [clean-state (gs/clear-changes (assoc game-state :input command))
        output (java.io.StringWriter.)
        after (binding [*out* output]
                (-> clean-state
                    parser/parser-from-input
                    verb-defs/perform
                    daemon/clocker))
        untracked (transition/detect-untracked-changes clean-state after)]
    {:command command
     :output (str output)
     :untracked untracked
     :has-untracked? (transition/has-untracked-changes? untracked)
     :game-state after}))

(defn run-commands
  "Execute a sequence of commands and collect any untracked changes."
  [commands]
  (loop [gs (core/init-game)
         cmds commands
         results []]
    (if (empty? cmds)
      results
      (let [cmd (first cmds)
            result (try
                     (run-command gs cmd)
                     (catch Exception e
                       {:command cmd
                        :error (.getMessage e)
                        :game-state gs}))]
        (recur (or (:game-state result) gs)
               (rest cmds)
               (if (:has-untracked? result)
                 (conj results (dissoc result :game-state))
                 results))))))

;;; ---------------------------------------------------------------------------
;;; SAMPLE COMMAND SEQUENCES
;;; ---------------------------------------------------------------------------

(def core-mechanics-commands
  "Commands that exercise core game mechanics."
  ["open mailbox"
   "read leaflet"
   "drop leaflet"
   "s" "e"  ; to behind house
   "open window"
   "in"  ; kitchen
   "w"   ; living room
   "take lamp"
   "turn on lamp"
   "move rug"
   "open trap door"
   "down"  ; cellar
   "n"     ; troll room
   "s" "up" ; back to living room
   "e" "e" ; to attic
   "take rope"
   "take knife"
   "down" "w" ; back to living room
   "turn off lamp"
   "turn on lamp"
   "e" ; kitchen
   "take bottle"
   "take sack"
   "open sack"
   "u" ; attic
   "down"
   "w" ; living room
   "w" ; outside house
   "n" "n" "n" ; forest path
   ])

(def light-mechanics-commands
  "Commands that test light source tracking."
  ["s" "e" "open window" "in" "w" "take lamp"
   "turn on lamp"
   "turn off lamp"
   "turn on lamp"
   "move rug" "open trap door" "d"
   "turn off lamp"
   "turn on lamp"])

;;; ---------------------------------------------------------------------------
;;; TESTS
;;; ---------------------------------------------------------------------------

(deftest no-untracked-changes-in-core-mechanics
  (testing "Core game mechanics use tracked functions"
    (let [results (run-commands core-mechanics-commands)]
      (when (seq results)
        (println "\n=== UNTRACKED CHANGES DETECTED ===")
        (doseq [{:keys [command untracked]} results]
          (println "Command:" command)
          (println (transition/untracked-summary untracked))
          (println "---")))
      (is (empty? results)
          (str "Found " (count results) " commands with untracked changes")))))

(deftest no-untracked-changes-in-light-mechanics
  (testing "Light mechanics use tracked functions"
    (let [results (run-commands light-mechanics-commands)]
      (when (seq results)
        (println "\n=== UNTRACKED CHANGES DETECTED ===")
        (doseq [{:keys [command untracked]} results]
          (println "Command:" command)
          (println (transition/untracked-summary untracked))
          (println "---")))
      (is (empty? results)
          (str "Found " (count results) " commands with untracked changes")))))

;;; ---------------------------------------------------------------------------
;;; HELPERS FOR OTHER TESTS
;;; ---------------------------------------------------------------------------

(defn assert-no-untracked
  "Helper for use in other tests - asserts that executing a command
   produces no untracked changes. Returns the game-state for chaining."
  ([game-state command]
   (assert-no-untracked game-state command nil))
  ([game-state command message]
   (let [{:keys [has-untracked? untracked game-state]} (run-command game-state command)]
     (is (not has-untracked?)
         (str (or message (str "Command '" command "' produced untracked changes:"))
              "\n" (transition/untracked-summary untracked)))
     game-state)))

(defn run-commands-strict
  "Execute commands and fail immediately on any untracked changes.
   Returns the final game state."
  [game-state commands]
  (reduce
    (fn [gs cmd]
      (assert-no-untracked gs cmd))
    game-state
    commands))
