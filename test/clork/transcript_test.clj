(ns clork.transcript-test
  "Tests that verify game output matches the MIT Zork I transcript.

   The transcript comes from: https://web.mit.edu/marleigh/www/portfolio/Files/zork/transcript.html
   It represents an actual playthrough of Zork I (Revision 88 / Serial number 840726).

   This test executes each command from the transcript and verifies the output
   matches exactly. The test uses a random seed to try to reproduce the exact
   combat outcomes and random events from the original playthrough.

   If a mismatch is found, the test stops and reports the difference."
  (:require [clojure.test :refer :all]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clork.game-state :as gs]
            [clork.core :as core]
            [clork.parser :as parser]
            [clork.verb-defs :as verb-defs]
            [clork.verbs-look :as look]
            [clork.utils :as utils]
            [clork.daemon :as daemon]
            [clork.random :as random]))

;;; ---------------------------------------------------------------------------
;;; TRANSCRIPT DATA LOADING
;;; ---------------------------------------------------------------------------

(defn load-transcript-data
  "Load the parsed transcript JSON file."
  []
  (with-open [r (io/reader "test/scripts/mit-transcript.json")]
    (json/read r :key-fn keyword)))

;;; ---------------------------------------------------------------------------
;;; OUTPUT CAPTURE AND NORMALIZATION
;;; ---------------------------------------------------------------------------

(defn normalize-output
  "Normalize output for comparison focusing on paragraph structure.

   We CATCH these bugs:
   - Missing paragraph breaks (action jammed into room text)
   - Excessive blank lines (6 newlines in a row)
   - Missing content

   We IGNORE these differences:
   - Line wrapping within paragraphs (original wraps at 80 cols)
   - Single vs double blank lines between paragraphs
   - Trailing whitespace"
  [s]
  (when s
    (let [paragraphs (str/split (str/trim s) #"\n\n+")
          normalized (map (fn [para]
                            (-> para
                                str/trim
                                ;; Join lines within paragraph (unwrap)
                                (str/replace #"\n" " ")
                                ;; Normalize multiple spaces
                                (str/replace #"  +" " ")))
                          paragraphs)]
      (str/join "\n\n" normalized))))

;;; ---------------------------------------------------------------------------
;;; COMMAND EXECUTION
;;; ---------------------------------------------------------------------------

(defn execute-command
  "Execute a single command and return [new-state output-string].
   Simulates one iteration of the main loop."
  [game-state input]
  (let [output (java.io.StringWriter.)]
    (binding [*out* output]
      (let [;; Initialize parser state
            lexv (parser/lexv-from-input input)
            gs (-> game-state
                   (parser/parser-init)
                   (parser/parser-set-winner-to-player)
                   (assoc :input input)
                   (assoc-in [:parser :lexv] lexv)
                   (assoc-in [:parser :len] (count (:tokens lexv)))
                   (assoc-in [:parser :again-lexv] lexv)
                   (assoc-in [:parser :dir] nil)
                   (assoc-in [:parser :ncn] 0)
                   (assoc-in [:parser :getflags] 0))
            ;; Parse the command
            gs (parser/parse-command gs)]
        (if (parser/get-parser-error gs)
          ;; Parsing failed - error already in output
          [gs (str output)]
          ;; Parsing succeeded - perform the action, then run daemons
          (let [gs (-> gs
                       (verb-defs/perform)
                       (daemon/clocker)
                       (utils/crlf))]
            [gs (str output)]))))))

;;; ---------------------------------------------------------------------------
;;; COMPARISON AND REPORTING
;;; ---------------------------------------------------------------------------

(defn find-first-diff
  "Find the position of the first character difference between two strings.
   Returns [position expected-char actual-char] or nil if equal."
  [expected actual]
  (let [len (min (count expected) (count actual))]
    (loop [i 0]
      (cond
        (>= i len)
        (when (not= (count expected) (count actual))
          [i
           (if (< i (count expected)) (get expected i) :eof)
           (if (< i (count actual)) (get actual i) :eof)])

        (not= (get expected i) (get actual i))
        [i (get expected i) (get actual i)]

        :else
        (recur (inc i))))))

(defn char-repr
  "Human-readable representation of a character."
  [c]
  (cond
    (= c :eof) "<EOF>"
    (= c \newline) "\\n"
    (= c \return) "\\r"
    (= c \tab) "\\t"
    (= c \space) "<space>"
    :else (str c)))

(defn context-around
  "Get context around a position in a string."
  [s pos radius]
  (let [start (max 0 (- pos radius))
        end (min (count s) (+ pos radius))]
    (subs s start end)))

(defn report-mismatch
  "Generate a detailed mismatch report."
  [command-num command expected actual]
  (let [[pos exp-char act-char] (find-first-diff expected actual)
        exp-context (when pos (context-around expected pos 40))
        act-context (when pos (context-around actual pos 40))]
    (str "\n"
         "=== TRANSCRIPT MISMATCH at command #" command-num " ===\n"
         "Command: " command "\n"
         "\n"
         "First difference at position " pos ":\n"
         "  Expected char: " (char-repr exp-char) "\n"
         "  Actual char:   " (char-repr act-char) "\n"
         "\n"
         "Expected context:\n"
         "  \"" exp-context "\"\n"
         "\n"
         "Actual context:\n"
         "  \"" act-context "\"\n"
         "\n"
         "--- Full Expected ---\n"
         expected "\n"
         "\n"
         "--- Full Actual ---\n"
         actual "\n")))

;;; ---------------------------------------------------------------------------
;;; INITIAL STATE SETUP
;;; ---------------------------------------------------------------------------

(defn create-initial-state
  "Create initial game state for transcript testing."
  [seed]
  (random/init! seed)
  (let [output (java.io.StringWriter.)]
    (binding [*out* output]
      ;; Use core/init-game which properly sets up rooms, objects, daemons, etc.
      (core/init-game nil))))

;;; ---------------------------------------------------------------------------
;;; MAIN TEST RUNNER
;;; ---------------------------------------------------------------------------

;; Seed changes at specific command numbers.
;; This is a temporary workaround for random events (combat, thief, loud room).
;; Once we've verified structural formatting, we'll generate a new transcript
;; from an actual playthrough with a single fixed seed.
(def seed-changes
  "Map of command-number -> new-seed to apply BEFORE that command.
   Used to work around random events that don't match the MIT transcript."
  {1 315     ;; Initial seed
   93 2})    ;; Before Loud Room scramble - picks :round-room

;; Maximum commands to run before stopping.
;; Set to nil to run full transcript.
;; Currently limited to 227 because:
;; - Commands 228+ require the river flow daemon (I-RIVER) for boat drifting
;; - After implementing river daemon, extend this to continue testing
(def max-verified-commands 227)

(defn run-transcript-test
  "Run through the transcript, comparing outputs.
   Returns {:success true :commands-run N} or {:success false :error <report>}."
  [seed max-commands]
  (let [data (load-transcript-data)
        commands (:commands data)
        commands-to-run (if max-commands
                          (take max-commands commands)
                          commands)]
    (loop [gs (create-initial-state seed)
           remaining commands-to-run
           cmd-num 1]
      (if (empty? remaining)
        {:success true :commands-run (dec cmd-num)}
        (let [;; Check if we need to change seeds before this command
              _ (when-let [new-seed (get seed-changes cmd-num)]
                  (random/init! new-seed))
              {:keys [command response]} (first remaining)
              expected (normalize-output response)
              [new-gs actual-raw] (execute-command gs command)
              actual (normalize-output actual-raw)]
          (if (= expected actual)
            (recur new-gs (rest remaining) (inc cmd-num))
            {:success false
             :command-num cmd-num
             :command command
             :expected expected
             :actual actual
             :report (report-mismatch cmd-num command expected actual)}))))))

;;; ---------------------------------------------------------------------------
;;; TESTS
;;; ---------------------------------------------------------------------------

(deftest ^:transcript transcript-first-10-commands
  (testing "First 10 commands match transcript"
    (let [result (run-transcript-test 315 10)]
      (when-not (:success result)
        (println (:report result)))
      (is (:success result)
          (str "Failed at command #" (:command-num result))))))

(deftest ^:transcript ^:slow transcript-full-test
  (testing "Full transcript matches"
    (let [result (run-transcript-test 315 max-verified-commands)]
      (when-not (:success result)
        (println (:report result)))
      (is (:success result)
          (str "Failed at command #" (:command-num result))))))

;;; ---------------------------------------------------------------------------
;;; SEED FINDER (for finding seeds that match random events)
;;; ---------------------------------------------------------------------------

(defn try-seeds
  "Try multiple seeds to find one that gets furthest in the transcript.
   Returns the best seed found."
  [seed-start seed-end max-commands]
  (let [data (load-transcript-data)
        commands (take (or max-commands 50) (:commands data))]
    (loop [seed seed-start
           best-seed seed-start
           best-count 0]
      (if (> seed seed-end)
        {:best-seed best-seed :commands-matched best-count}
        (let [result (run-transcript-test seed (count commands))
              matched (if (:success result)
                        (count commands)
                        (dec (:command-num result)))]
          (if (> matched best-count)
            (do
              (println (str "Seed " seed ": " matched " commands matched"))
              (recur (inc seed) seed matched))
            (recur (inc seed) best-seed best-count)))))))

;;; ---------------------------------------------------------------------------
;;; DEBUG MODE - Interactive debugging for transcript issues
;;; ---------------------------------------------------------------------------

(defn- format-flag-state
  "Format the flag state of an object for debugging."
  [game-state obj-id]
  (let [obj (gs/get-thing game-state obj-id)
        static-flags (or (:flags obj) #{})
        ;; Find runtime overrides
        runtime-overrides (reduce (fn [m k]
                                    (let [v (get obj k)]
                                      (if (or (true? v) (false? v))
                                        (assoc m k v)
                                        m)))
                                  {}
                                  (keys obj))
        loc (:in obj)]
    (str obj-id
         " @ " loc
         " | static: #{" (str/join " " (map name (sort static-flags))) "}"
         (when (seq runtime-overrides)
           (str " | overrides: " runtime-overrides)))))

(defn debug-transcript
  "Run transcript with debugging options.

   Options:
     :seed        - Random seed (default 315)
     :max-cmd     - Max commands to run (default nil = all)
     :break-at    - Command number to stop at and dump state
     :watch       - Collection of object IDs to track
     :verbose     - Print every command's output

   Example:
     (debug-transcript {:break-at 174 :watch [:candles :brass-lantern]})
     (debug-transcript {:break-at 115 :watch [:candles] :verbose true})"
  [{:keys [seed max-cmd break-at watch verbose]
    :or {seed 315 max-cmd nil watch [] verbose false}}]
  (let [data (load-transcript-data)
        commands (:commands data)
        commands-to-run (if max-cmd
                          (take max-cmd commands)
                          commands)
        watch-set (set watch)]
    (println "=== TRANSCRIPT DEBUG MODE ===")
    (println (str "Seed: " seed))
    (println (str "Break at: " (or break-at "none")))
    (println (str "Watching: " (if (empty? watch) "none" (str/join ", " (map name watch)))))
    (println)
    (loop [gs (create-initial-state seed)
           remaining commands-to-run
           cmd-num 1
           prev-watch-state {}]
      (if (empty? remaining)
        (do
          (println (str "\n=== COMPLETED " (dec cmd-num) " COMMANDS ==="))
          {:success true :commands-run (dec cmd-num) :final-state gs})
        (let [;; Check if we need to change seeds
              _ (when-let [new-seed (get seed-changes cmd-num)]
                  (println (str "*** Changing seed to " new-seed " at command #" cmd-num))
                  (random/init! new-seed))
              {:keys [command response]} (first remaining)
              ;; Capture watch state BEFORE command
              watch-before (into {} (for [obj-id watch-set]
                                      [obj-id (format-flag-state gs obj-id)]))
              ;; Execute command
              [new-gs actual-raw] (execute-command gs command)
              actual (normalize-output actual-raw)
              expected (normalize-output response)
              ;; Capture watch state AFTER command
              watch-after (into {} (for [obj-id watch-set]
                                     [obj-id (format-flag-state new-gs obj-id)]))]
          ;; Print verbose output
          (when verbose
            (println (str "\n--- Command #" cmd-num ": " command " ---"))
            (println actual-raw))
          ;; Print watch changes
          (doseq [obj-id watch-set]
            (let [before (get watch-before obj-id)
                  after (get watch-after obj-id)]
              (when (not= before after)
                (println (str "\n*** WATCH: " obj-id " changed at command #" cmd-num))
                (println (str "    Before: " before))
                (println (str "    After:  " after)))))
          ;; Check for break
          (if (= cmd-num break-at)
            (do
              (println (str "\n=== BREAK AT COMMAND #" cmd-num " ==="))
              (println (str "Command: " command))
              (println (str "\nExpected output:\n" expected))
              (println (str "\nActual output:\n" actual))
              (println (str "\nMatch: " (= expected actual)))
              (println "\nWatched objects:")
              (doseq [obj-id watch-set]
                (println (str "  " (format-flag-state new-gs obj-id))))
              (println "\nCurrent location:" (:here new-gs))
              {:break true :command-num cmd-num :state new-gs})
            ;; Check for mismatch
            (if (not= expected actual)
              (do
                (println (str "\n=== MISMATCH AT COMMAND #" cmd-num " ==="))
                (println (report-mismatch cmd-num command expected actual))
                {:success false :command-num cmd-num :state new-gs})
              ;; Continue
              (recur new-gs (rest remaining) (inc cmd-num) watch-after))))))))

(comment
  ;; Run first 10 commands with seed 42
  (run-transcript-test 42 10)

  ;; Try seeds 0-100 for first 20 commands
  (try-seeds 0 100 20)

  ;; Load and inspect transcript data
  (def data (load-transcript-data))
  (count (:commands data))
  (first (:commands data))

  ;; Debug specific command with watch
  (debug-transcript {:break-at 174 :watch [:candles :brass-lantern]})

  ;; Trace candle state through commands 100-120
  (debug-transcript {:max-cmd 120 :watch [:candles] :verbose false})
  )
