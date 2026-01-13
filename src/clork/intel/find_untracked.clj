(ns clork.intel.find-untracked
  "Script to find untracked state changes in the codebase.

   Runs through a playthrough and reports any changes that bypassed
   the event sourcing system, indicating 'backdoor' code that needs
   to be updated to use the tracked functions."
  (:require [clork.core :as core]
            [clork.parser :as parser]
            [clork.verb-defs :as verb-defs]
            [clork.daemon :as daemon]
            [clork.game-state :as gs]
            [clork.intel.transition :as transition]
            [clojure.string :as str]))

;;; ---------------------------------------------------------------------------
;;; PLAYTHROUGH COMMANDS
;;; ---------------------------------------------------------------------------
;;; A representative set of commands that exercises many game mechanics.

(def sample-commands
  "Commands that exercise various game mechanics."
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
   ;; Attack troll to trigger combat flags
   "attack troll with sword"
   "attack troll with sword"
   "s" "up" ; back to living room
   "take sword"  ; if we dropped it
   "e" "e" ; to attic
   "take rope"
   "take knife"
   "down" "w" ; back to living room
   ;; Try prayer mechanic (won't work here but exercises code)
   "pray"
   ;; Light mechanics
   "turn off lamp"
   "turn on lamp"
   ;; Container mechanics
   "e" ; kitchen
   "take bottle"
   "take sack"
   "open sack"
   ;; More movement
   "u" ; attic
   "down"
   "w" ; living room
   "w" ; outside house
   "n" "n" "n" ; forest path
   ])

;;; ---------------------------------------------------------------------------
;;; ANALYSIS FUNCTIONS
;;; ---------------------------------------------------------------------------

(defn run-command-with-tracking
  "Execute a command and return tracking information."
  [game-state command]
  (let [;; Clear changes before executing
        clean-state (gs/clear-changes (assoc game-state :input command))
        ;; Parse and execute the command
        output (java.io.StringWriter.)
        after (binding [*out* output]
                (-> clean-state
                    parser/parser-from-input
                    verb-defs/perform
                    daemon/clocker))
        ;; Get the action that was parsed
        action (get-in after [:parser :prsa])
        ;; Detect untracked changes
        untracked (transition/detect-untracked-changes clean-state after)]
    {:command command
     :action action
     :output (str output)
     :untracked untracked
     :has-untracked? (transition/has-untracked-changes? untracked)
     :game-state after}))

(defn find-all-untracked
  "Run through commands and collect all untracked changes."
  [& {:keys [commands verbose?] :or {commands sample-commands verbose? false}}]
  (loop [gs (core/init-game)
         cmds commands
         cmd-num 1
         results []]
    (if (empty? cmds)
      results
      (let [cmd (first cmds)
            result (try
                     (run-command-with-tracking gs cmd)
                     (catch Exception e
                       {:command cmd
                        :error (.getMessage e)
                        :game-state gs}))]
        (when (and verbose? (:has-untracked? result))
          (println (str "Command " cmd-num ": " cmd))
          (println (transition/untracked-summary (:untracked result)))
          (println "---"))
        (recur (or (:game-state result) gs)
               (rest cmds)
               (inc cmd-num)
               (if (:has-untracked? result)
                 (conj results (dissoc result :game-state))
                 results))))))

(defn summarize-untracked
  "Summarize all untracked changes found."
  [results]
  (let [all-flags-set (apply clojure.set/union
                             (map #(get-in % [:untracked :flags-set]) results))
        all-flags-cleared (apply clojure.set/union
                                 (map #(get-in % [:untracked :flags-cleared]) results))
        all-daemons-started (apply clojure.set/union
                                   (map #(get-in % [:untracked :daemons-started]) results))
        all-daemons-stopped (apply clojure.set/union
                                   (map #(get-in % [:untracked :daemons-stopped]) results))
        location-changes (filter #(get-in % [:untracked :location-changed]) results)
        score-changes (filter #(get-in % [:untracked :score-changed]) results)]
    {:total-untracked (count results)
     :flags-set all-flags-set
     :flags-cleared all-flags-cleared
     :daemons-started all-daemons-started
     :daemons-stopped all-daemons-stopped
     :location-change-commands (mapv :command location-changes)
     :score-change-commands (mapv :command score-changes)
     :commands-with-untracked (mapv :command results)}))

(defn print-summary
  "Print a human-readable summary."
  [summary]
  (println "=== UNTRACKED CHANGES SUMMARY ===")
  (println)
  (println "Total commands with untracked changes:" (:total-untracked summary))
  (println)
  (when (seq (:flags-set summary))
    (println "Untracked flags SET:")
    (doseq [f (:flags-set summary)]
      (println "  -" (if (vector? f)
                       (str (name (first f)) ":" (name (second f)))
                       (name f))))
    (println))
  (when (seq (:flags-cleared summary))
    (println "Untracked flags CLEARED:")
    (doseq [f (:flags-cleared summary)]
      (println "  -" (if (vector? f)
                       (str (name (first f)) ":" (name (second f)))
                       (name f))))
    (println))
  (when (seq (:daemons-started summary))
    (println "Untracked daemons STARTED:")
    (doseq [d (:daemons-started summary)]
      (println "  -" (name d)))
    (println))
  (when (seq (:daemons-stopped summary))
    (println "Untracked daemons STOPPED:")
    (doseq [d (:daemons-stopped summary)]
      (println "  -" (name d)))
    (println))
  (when (seq (:location-change-commands summary))
    (println "Commands with untracked LOCATION changes:")
    (doseq [c (:location-change-commands summary)]
      (println "  -" c))
    (println))
  (when (seq (:score-change-commands summary))
    (println "Commands with untracked SCORE changes:")
    (doseq [c (:score-change-commands summary)]
      (println "  -" c))
    (println))
  (println "Commands with any untracked changes:")
  (doseq [c (:commands-with-untracked summary)]
    (println "  -" c)))

(defn run-analysis
  "Main entry point - run analysis and print summary."
  []
  (println "Running untracked change detection...")
  (println)
  (let [results (find-all-untracked :verbose? true)]
    (println)
    (print-summary (summarize-untracked results))
    results))

;; To run from REPL:
;; (require '[clork.intel.find-untracked :as fu])
;; (fu/run-analysis)
