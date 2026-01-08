(ns clork.playthrough-test
  "Tests a complete Clork playthrough with a single fixed seed.

   Unlike the MIT transcript test, this uses our own transcript generated
   by playing the game with a known seed, so no state adjustments are needed.

   The goal is to win the game in as few moves as possible (under 250).

   Someone's walkthrough said it could be done in < 225; can we beat that?

   Workflow:
    - Run the test to see what happens:
      - if it passes and the game is won in < 250 moves, you're done!
      - if it passes and the game is won but in >= 250 moves:
        - consult the walkthroughs and the transcript to find ways to optimize
      - if it passes and the game is not won (score < 350):
        - consult the walkthroughs in ./scripts/walkthroughs/ to find next steps
        - use add-command! to add commands to the transcript JSON
        - if the result looks wrong:
          - use show-state to inspect the current game state
          - debug as needed
        - if the command succeeds:
          - take the output and paste into the JSON file
        - if the command fails:
          - debug as needed
      - if it fails:
        - inspect the report printed to the console
        - debug as needed
    - repeat
   "
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
;;; TRANSCRIPT DATA
;;; ---------------------------------------------------------------------------

(defn load-playthrough
  "Load the playthrough transcript JSON file."
  []
  (with-open [r (io/reader "test/scripts/clork-playthrough.json")]
    (json/read r :key-fn keyword)))

;;; ---------------------------------------------------------------------------
;;; OUTPUT NORMALIZATION
;;; ---------------------------------------------------------------------------

(defn normalize-output
  "Normalize output for comparison.
   Handles line wrapping differences while preserving paragraph structure."
  [s]
  (when s
    (let [paragraphs (str/split (str/trim s) #"\n\n+")
          normalized (map (fn [para]
                            (-> para
                                str/trim
                                (str/replace #"\n" " ")
                                (str/replace #"  +" " ")))
                          paragraphs)]
      (str/join "\n\n" normalized))))

;;; ---------------------------------------------------------------------------
;;; MOVE COUNTING
;;; ---------------------------------------------------------------------------
;;; Must match main_loop.clj behavior - meta-verbs don't count as moves.

(def ^:private meta-verbs
  "Verbs that don't count as moves (meta/system commands)."
  #{:verbose :brief :super-brief :version :diagnose :score :quit :verify
    :restart :save :restore :script :unscript})

(defn- increment-moves-if-needed
  "Increment the move counter if the action is not a meta-verb."
  [game-state]
  (let [action (get-in game-state [:parser :prsa])]
    (if (contains? meta-verbs action)
      game-state
      (update game-state :moves (fnil inc 0)))))

;;; ---------------------------------------------------------------------------
;;; COMMAND EXECUTION
;;; ---------------------------------------------------------------------------

(defn execute-command
  "Execute a single command and return [new-state output-string]."
  [game-state input]
  (let [output (java.io.StringWriter.)]
    (binding [*out* output]
      (let [lexv (parser/lexv-from-input input)
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
            gs (parser/parse-command gs)]
        (if (parser/get-parser-error gs)
          [gs (str output)]
          (let [gs (verb-defs/perform gs)
                gs (increment-moves-if-needed gs)
                gs (if (:clock-wait gs)
                     (dissoc gs :clock-wait)
                     (daemon/clocker gs))
                gs (utils/crlf gs)]
            [gs (str output)]))))))

;;; ---------------------------------------------------------------------------
;;; INITIAL STATE
;;; ---------------------------------------------------------------------------

(defn create-initial-state
  "Create initial game state for playthrough testing."
  [seed]
  (random/init! seed)
  (let [output (java.io.StringWriter.)]
    (binding [*out* output]
      (core/init-game nil))))

;;; ---------------------------------------------------------------------------
;;; COMPARISON AND REPORTING
;;; ---------------------------------------------------------------------------

(defn find-first-diff
  "Find the first character difference between two strings."
  [expected actual]
  (let [len (min (count expected) (count actual))]
    (loop [i 0]
      (cond
        (>= i len)
        (when (not= (count expected) (count actual))
          [i (if (< i (count expected)) (get expected i) :eof)
           (if (< i (count actual)) (get actual i) :eof)])

        (not= (get expected i) (get actual i))
        [i (get expected i) (get actual i)]

        :else (recur (inc i))))))

(defn char-repr [c]
  (cond
    (= c :eof) "<EOF>"
    (= c \newline) "\\n"
    (= c \space) "<space>"
    :else (str c)))

(defn report-mismatch
  "Generate a detailed mismatch report."
  [cmd-num command expected actual]
  (let [[pos exp-char act-char] (find-first-diff expected actual)]
    (str "\n"
         "=== MISMATCH at command #" cmd-num " ===\n"
         "Command: " command "\n"
         "\n"
         "First difference at position " pos ":\n"
         "  Expected: " (char-repr exp-char) "\n"
         "  Actual:   " (char-repr act-char) "\n"
         "\n"
         "--- Expected ---\n"
         expected "\n"
         "\n"
         "--- Actual ---\n"
         actual "\n")))

;;; ---------------------------------------------------------------------------
;;; TEST RUNNER
;;; ---------------------------------------------------------------------------

;; Set to nil to run full playthrough, or a number to limit
(def max-commands nil)

(defn run-playthrough
  "Run through the playthrough transcript.
   Returns {:success true :commands-run N} or {:success false :error ...}."
  [& {:keys [max-cmds verbose] :or {max-cmds nil verbose false}}]
  (let [data (load-playthrough)
        seed (get data :seed 42)
        commands (:commands data)
        commands-to-run (if max-cmds (take max-cmds commands) commands)]
    (loop [gs (create-initial-state seed)
           remaining commands-to-run
           cmd-num 1]
      (if (empty? remaining)
        {:success true :commands-run (dec cmd-num)}
        (let [{:keys [command response]} (first remaining)
              expected (normalize-output response)
              [new-gs actual-raw] (execute-command gs command)
              actual (normalize-output actual-raw)]
          (when verbose
            (println (str "#" cmd-num ": " command))
            (println actual)
            (println))
          (if (= expected actual)
            (recur new-gs (rest remaining) (inc cmd-num))
            {:success false
             :command-num cmd-num
             :command command
             :expected expected
             :actual actual
             :report (report-mismatch cmd-num command expected actual)
             :game-state new-gs}))))))

;;; ---------------------------------------------------------------------------
;;; TESTS
;;; ---------------------------------------------------------------------------

(deftest playthrough-test
  (testing "Clork playthrough matches transcript"
    (let [result (run-playthrough :max-cmds max-commands)]
      (when-not (:success result)
        (println (:report result)))
      (is (:success result)
          (str "Failed at command #" (:command-num result))))))

;;; ---------------------------------------------------------------------------
;;; INTERACTIVE HELPERS
;;; ---------------------------------------------------------------------------

(defn add-command!
  "Helper to add a command to the playthrough.
   Executes the command and returns the output for copy/paste into JSON."
  [command]
  (let [data (load-playthrough)
        seed (get data :seed 42)
        commands (:commands data)]
    (random/init! seed)
    ;; Run through all existing commands
    (loop [gs (create-initial-state seed)
           remaining commands]
      (if (empty? remaining)
        ;; Execute the new command
        (let [[new-gs output] (execute-command gs command)
              normalized (normalize-output output)]
          (println "Command:" command)
          (println "Response:")
          (println normalized)
          (println)
          (println "JSON:")
          (println (str "    {\n"
                        "      \"command\": \"" command "\",\n"
                        "      \"response\": \"" (str/escape normalized {\" "\\\"" \newline "\\n"}) "\"\n"
                        "    }"))
          {:output normalized :game-state new-gs})
        ;; Execute existing command
        (let [[new-gs _] (execute-command gs (:command (first remaining)))]
          (recur new-gs (rest remaining)))))))

(defn show-state
  "Show current game state after running all commands in the playthrough."
  []
  (let [data (load-playthrough)
        seed (get data :seed 42)
        commands (:commands data)]
    (random/init! seed)
    (loop [gs (create-initial-state seed)
           remaining commands]
      (if (empty? remaining)
        (do
          (println "Location:" (:here gs))
          (println "Score:" (:score gs 0))
          (println "Moves:" (:moves gs 0))
          (println "Inventory:" (gs/get-contents gs :adventurer))
          gs)
        (let [[new-gs _] (execute-command gs (:command (first remaining)))]
          (recur new-gs (rest remaining)))))))
