(ns clork.planner2.executor
  "Unified Executor for the Prep-Optimized Planner.

   Executes optimized plans while handling reactive situations
   like combat RNG, thief encounters, and unexpected deaths.

   Uses the existing reactive planner (core.clj) for goal execution
   while following the optimized schedule from route.clj and schedule.clj.

   See IMPLEMENTATION_PLAN.md for the full architecture."
  (:require [clork.planner2.core :as planner]
            [clork.planner2.goals :as goals]
            [clork.planner2.observe :as obs]
            [clork.planner2.navigate :as nav]
            [clork.planner2.prep :as prep]
            [clork.planner2.deps :as deps]
            [clork.planner2.route :as route]
            [clork.planner2.schedule :as schedule]
            [clork.random :as rng]
            [clork.ml :as ml]
            [clojure.string :as str]))

;;; ---------------------------------------------------------------------------
;;; EXECUTION TRACE - Records everything for debugging
;;; ---------------------------------------------------------------------------

(defn make-trace-entry
  "Create a trace entry for logging."
  [event-type data]
  {:timestamp (System/currentTimeMillis)
   :event event-type
   :data data})

(defn trace-event
  "Add an event to the execution trace."
  [exec-state event-type data]
  (update exec-state :trace conj (make-trace-entry event-type data)))

(defn trace-schedule-start
  "Record schedule execution start."
  [exec-state entry]
  (trace-event exec-state :schedule-entry
               {:index (:schedule-index exec-state)
                :type (:type entry)
                :details (select-keys entry [:to :id :treasure :location :name])}))

(defn trace-goal-start
  "Record goal execution start."
  [exec-state goal]
  (trace-event exec-state :goal-start
               {:goal (goals/goal->string goal)
                :turn (:turn-count exec-state)
                :room (obs/current-room (:game-state exec-state))}))

(defn trace-goal-complete
  "Record goal execution result."
  [exec-state goal result]
  (trace-event exec-state :goal-complete
               {:goal (goals/goal->string goal)
                :status (:status result)
                :turns-used (:turns-used result 0)
                :error (:error result)}))

(defn trace-checkpoint
  "Save a checkpoint of current state for debugging."
  [exec-state label]
  (let [gs (:game-state exec-state)]
    (update exec-state :checkpoints conj
            {:label label
             :turn (:turn-count exec-state)
             :schedule-index (:schedule-index exec-state)
             :room (obs/current-room gs)
             :inventory (count (obs/inventory gs))
             :collected (count (:collected-treasures exec-state))
             :deposited (count (:deposited-treasures exec-state))
             :preps-done (count (:completed-preps exec-state))})))

;;; ---------------------------------------------------------------------------
;;; EXECUTION STATE
;;; ---------------------------------------------------------------------------

(defn make-execution-state
  "Create initial execution state for a speedrun."
  [game-state treasures & {:keys [debug] :or {debug false}}]
  {:game-state game-state
   :treasures treasures
   :schedule (schedule/generate-schedule game-state treasures)
   :schedule-index 0
   :timer-state (schedule/make-timer-state)
   :collected-treasures #{}
   :deposited-treasures #{}
   :completed-preps #{}
   :status :running
   :turn-count 0
   :deaths 0
   :replans 0
   ;; Debug/trace state
   :debug debug
   :trace []           ; Event log for debugging
   :checkpoints []})

(defn current-schedule-entry
  "Get the current schedule entry to execute."
  [exec-state]
  (get (:schedule exec-state) (:schedule-index exec-state)))

(defn advance-schedule
  "Move to the next schedule entry."
  [exec-state]
  (update exec-state :schedule-index inc))

(defn schedule-complete?
  "Check if the schedule has been fully executed."
  [exec-state]
  (>= (:schedule-index exec-state) (count (:schedule exec-state))))

;;; ---------------------------------------------------------------------------
;;; GOAL CONVERSION
;;; ---------------------------------------------------------------------------

(defn schedule-entry->goal
  "Convert a schedule entry to a planner goal."
  [entry]
  (case (:type entry)
    :move (goals/at-room (:to entry))
    :prep (goals/flag-set (:id entry))
    :collect (goals/have-item (:treasure entry))
    :deposit-all (goals/all-treasures-deposited)
    :atomic-sequence nil  ; Handle specially
    :parallel-work nil    ; Handle specially
    nil))

(defn entry-requires-combat?
  "Check if a schedule entry involves combat."
  [entry]
  (and (= :prep (:type entry))
       (prep/combat-prep? (:id entry))))

;;; ---------------------------------------------------------------------------
;;; REACTIVE EXECUTION
;;; ---------------------------------------------------------------------------

(defn execute-goal
  "Execute a single goal using the reactive planner.
   Returns updated execution state."
  [exec-state goal max-turns]
  (let [state-with-trace (if (:debug exec-state)
                           (trace-goal-start exec-state goal)
                           exec-state)
        result (planner/run-goal (:game-state state-with-trace) goal
                                 :max-turns max-turns)
        updated (-> state-with-trace
                    (assoc :game-state (:game-state result))
                    (update :turn-count + (:turns-used result 0))
                    (assoc :last-result result))]
    (if (:debug updated)
      (trace-goal-complete updated goal result)
      updated)))

(defn execute-schedule-entry
  "Execute a single schedule entry.
   Returns updated execution state."
  [exec-state]
  (let [entry (current-schedule-entry exec-state)
        state (if (:debug exec-state)
                (-> exec-state
                    (trace-schedule-start entry)
                    (trace-checkpoint (str "before-" (name (:type entry)))))
                exec-state)]
    (case (:type entry)
      ;; Move to a location
      :move
      (let [goal (goals/at-room (:to entry))
            result (execute-goal exec-state goal 50)]
        (if (= :complete (:status (:last-result result)))
          (advance-schedule result)
          (assoc result :status :stuck)))

      ;; Execute a prep action
      :prep
      (let [prep-id (:id entry)
            ;; Check for required items from prep registry
            required-items (when-let [reqs (prep/prep-requires prep-id)]
                             (filter #(and (keyword? %)
                                           (not (contains? prep/prep-actions %)))
                                     reqs))
            ;; Get any missing items first
            items-state (reduce
                         (fn [state item]
                           (if (obs/has-item? (:game-state state) item)
                             state
                             (let [result (execute-goal state (goals/have-item item) 50)]
                               (if (= :complete (:status (:last-result result)))
                                 result
                                 (reduced (assoc result :status :stuck))))))
                         exec-state
                         (or required-items []))]
        (if (= :stuck (:status items-state))
          items-state
          (let [;; Get location from entry or prep registry
                location (or (:location entry) (prep/prep-location prep-id))
                ;; Navigate to location
                nav-result (when location
                             (execute-goal items-state (goals/at-room location) 50))]
            (if (and nav-result (not= :complete (:status (:last-result nav-result))))
              (assoc nav-result :status :stuck)
              (let [;; Use nav-result if we navigated, else items-state
                    base-state (or nav-result items-state)
                    game-state (:game-state base-state)
                    ;; Get action from entry or prep registry
                    action (or (:action entry) (prep/prep-action prep-id))]
                (cond
                  ;; Combat preps use kill-enemy goal
                  (= :combat action)
                  (let [goal (goals/kill-enemy (get-in prep/prep-actions [prep-id :target]))
                        result (execute-goal base-state goal 100)]
                    (if (= :complete (:status (:last-result result)))
                      (-> result
                          (update :completed-preps conj prep-id)
                          advance-schedule)
                      (assoc result :status :stuck)))

                  ;; Direct action from entry - execute it immediately
                  (map? action)
                  (let [action-result (ml/execute-action game-state action)
                        new-state (:game-state action-result)]
                    (-> base-state
                        (assoc :game-state new-state)
                        (update :turn-count inc)
                        (update :completed-preps conj prep-id)
                        advance-schedule))

                  ;; No action - just advance
                  :else
                  (-> base-state
                      (update :completed-preps conj prep-id)
                      advance-schedule)))))))

      ;; Collect a treasure
      :collect
      (let [treasure (:treasure entry)
            goal (goals/have-item treasure)
            result (execute-goal exec-state goal 50)]
        (if (= :complete (:status (:last-result result)))
          (-> result
              (update :collected-treasures conj treasure)
              advance-schedule)
          (assoc result :status :stuck)))

      ;; Deposit all collected treasures
      :deposit-all
      (let [to-deposit (:collected-treasures exec-state)
            ;; Deposit each treasure individually
            final-state
            (reduce
             (fn [state treasure]
               (let [goal (goals/item-deposited treasure)
                     result (execute-goal state goal 30)]
                 (if (= :complete (:status (:last-result result)))
                   (update result :deposited-treasures conj treasure)
                   (reduced result))))
             exec-state
             to-deposit)]
        (-> final-state
            (assoc :collected-treasures #{})
            advance-schedule))

      ;; Parallel work during a timed effect
      :parallel-work
      (let [treasures (:treasures entry)
            ;; Collect each treasure in sequence
            final-state
            (reduce
             (fn [state treasure]
               (let [goal (goals/have-item treasure)
                     result (execute-goal state goal 20)]
                 (if (= :complete (:status (:last-result result)))
                   (update result :collected-treasures conj treasure)
                   (reduced (assoc result :status :partial)))))
             exec-state
             treasures)]
        (advance-schedule final-state))

      ;; Deposit run - triggered when inventory is full
      :deposit-run
      (let [to-deposit (:collected-treasures exec-state)
            ;; Navigate to living room
            nav-result (execute-goal exec-state (goals/at-room :living-room) 50)]
        (if (not= :complete (:status (:last-result nav-result)))
          (assoc nav-result :status :stuck)
          ;; Deposit each treasure
          (let [final-state
                (reduce
                 (fn [state treasure]
                   (let [goal (goals/item-deposited treasure)
                         result (execute-goal state goal 30)]
                     (if (= :complete (:status (:last-result result)))
                       (update result :deposited-treasures conj treasure)
                       (reduced result))))
                 nav-result
                 to-deposit)]
            (-> final-state
                (assoc :collected-treasures #{})
                advance-schedule))))

      ;; Atomic sequence (like exorcism)
      :atomic-sequence
      (let [steps (:steps entry)
            location (:location entry)
            ;; Navigate to location first
            nav-result (execute-goal exec-state (goals/at-room location) 50)]
        (if (not= :complete (:status (:last-result nav-result)))
          (assoc nav-result :status :stuck)
          ;; Execute all steps in sequence without interruption
          (let [final-state
                (reduce
                 (fn [state step]
                   (let [goal (schedule-entry->goal step)
                         result (execute-goal state goal 10)]
                     (if (= :complete (:status (:last-result result)))
                       (update result :completed-preps conj (:id step))
                       (reduced (assoc result :status :failed)))))
                 nav-result
                 steps)]
            (if (= :failed (:status final-state))
              final-state
              (advance-schedule final-state)))))

      ;; Unknown entry type
      (advance-schedule exec-state))))

;;; ---------------------------------------------------------------------------
;;; UNEXPECTED EVENT HANDLING
;;; ---------------------------------------------------------------------------

(defn detect-death
  "Check if player has died."
  [exec-state]
  (not (obs/player-alive? (:game-state exec-state))))

(defn detect-thief
  "Check if thief has stolen something important."
  [exec-state]
  ;; TODO: Implement thief detection
  false)

(defn handle-death
  "Handle player death by restarting from checkpoint.
   In Zork, death teleports you to forest after a few turns."
  [exec-state]
  (-> exec-state
      (update :deaths inc)
      (assoc :status :recovering)))

(defn replan-from-current
  "Generate a new plan from current game state."
  [exec-state]
  (let [remaining-treasures (remove (:deposited-treasures exec-state)
                                    (:treasures exec-state))
        new-schedule (schedule/generate-schedule
                      (:game-state exec-state)
                      remaining-treasures)]
    (-> exec-state
        (assoc :schedule new-schedule)
        (assoc :schedule-index 0)
        (update :replans inc)
        (assoc :status :running))))

;;; ---------------------------------------------------------------------------
;;; MAIN EXECUTION LOOP
;;; ---------------------------------------------------------------------------

(defn- verbose-print
  "Print verbose execution info."
  [exec-state entry]
  (let [gs (:game-state exec-state)]
    (println (str "[Turn " (:turn-count exec-state) "] "
                  "Entry " (:schedule-index exec-state) "/" (count (:schedule exec-state))
                  " | Room: " (name (obs/current-room gs))
                  " | " (case (:type entry)
                          :move (str "MOVE -> " (name (:to entry)))
                          :prep (str "PREP: " (name (:id entry)))
                          :collect (str "COLLECT: " (name (:treasure entry)))
                          :deposit-all "DEPOSIT ALL"
                          :parallel-work (str "PARALLEL: " (mapv name (:treasures entry)))
                          :atomic-sequence (str "ATOMIC: " (:name entry))
                          (str (:type entry)))))))

(defn- verbose-result
  "Print result of an operation."
  [label status & {:keys [error turns]}]
  (println (str "  -> " label ": " (name status)
                (when turns (str " (" turns " turns)"))
                (when error (str " ERROR: " error)))))

(defn run-speedrun
  "Execute a full speedrun for the given treasures.

   Options:
   - :max-turns - Maximum turns before timeout (default 500)
   - :max-deaths - Maximum deaths before giving up (default 3)
   - :verbose - Print progress (default false)
   - :debug - Enable full execution tracing (default false)

   Returns execution state with:
   - :status - :complete, :timeout, :failed, :stuck
   - :deposited-treasures - Set of successfully deposited treasures
   - :turn-count - Total turns used
   - :deaths - Number of deaths
   - :trace - Event log (if :debug true)
   - :checkpoints - State snapshots (if :debug true)
   - :failure-info - Details about why execution failed (if failed)"
  [game-state treasures & {:keys [max-turns max-deaths verbose debug]
                           :or {max-turns 500 max-deaths 3 verbose false debug false}}]
  (when verbose
    (println "=== SPEEDRUN START ===")
    (println "Treasures:" (mapv name treasures))
    (println ""))

  (loop [exec-state (make-execution-state game-state treasures :debug debug)
         last-entry nil]
    (let [entry (current-schedule-entry exec-state)]
      ;; Verbose output for new entries
      (when (and verbose entry (not= entry last-entry))
        (verbose-print exec-state entry))

      (cond
        ;; Schedule complete
        (schedule-complete? exec-state)
        (do
          (when verbose
            (println "\n=== SPEEDRUN COMPLETE ===")
            (println "Turns:" (:turn-count exec-state))
            (println "Deposited:" (count (:deposited-treasures exec-state)) "/" (count treasures)))
          (assoc exec-state :status :complete))

        ;; Timeout
        (> (:turn-count exec-state) max-turns)
        (do
          (when verbose
            (println "\n=== TIMEOUT ===")
            (println "Max turns exceeded:" max-turns))
          (-> exec-state
              (assoc :status :timeout)
              (assoc :failure-info {:reason :timeout
                                    :at-entry (:schedule-index exec-state)
                                    :entry entry
                                    :turn (:turn-count exec-state)})))

        ;; Too many deaths
        (> (:deaths exec-state) max-deaths)
        (do
          (when verbose
            (println "\n=== TOO MANY DEATHS ===")
            (println "Deaths:" (:deaths exec-state)))
          (-> exec-state
              (assoc :status :failed)
              (assoc :failure-info {:reason :too-many-deaths
                                    :deaths (:deaths exec-state)
                                    :at-entry (:schedule-index exec-state)})))

        ;; Death detected
        (detect-death exec-state)
        (do
          (when verbose
            (println "  *** DEATH DETECTED - Replanning ***"))
          (let [recovered (handle-death exec-state)
                replanned (replan-from-current recovered)]
            (recur replanned entry)))

        ;; Stuck - try replanning
        (= :stuck (:status exec-state))
        (do
          (when verbose
            (println "  *** STUCK - Replanning (attempt" (inc (:replans exec-state)) ") ***"))
          (let [replanned (replan-from-current exec-state)]
            (if (> (:replans replanned) 5)
              (do
                (when verbose
                  (println "\n=== FAILED - Too many replans ==="))
                (-> exec-state
                    (assoc :status :failed)
                    (assoc :failure-info {:reason :too-many-replans
                                          :replans (:replans exec-state)
                                          :stuck-at entry
                                          :last-result (:last-result exec-state)})))
              (recur replanned entry))))

        ;; Normal execution
        :else
        (let [result (execute-schedule-entry exec-state)]
          (when (and verbose (:last-result result))
            (verbose-result "Goal"
                            (:status (:last-result result))
                            :turns (:turns-used (:last-result result) 0)
                            :error (:error (:last-result result))))
          (recur result entry))))))

;;; ---------------------------------------------------------------------------
;;; CONVENIENCE FUNCTIONS
;;; ---------------------------------------------------------------------------

(defn run-all-treasures
  "Execute a speedrun for all treasures."
  [game-state & opts]
  (let [treasures (keys route/treasure-locations)]
    (apply run-speedrun game-state treasures opts)))

(defn run-easy-treasures
  "Execute a speedrun for easy treasures only (no complex puzzles)."
  [game-state & opts]
  (let [easy [:egg :painting :bag-of-coins :platinum-bar]]
    (apply run-speedrun game-state easy opts)))

(defn run-medium-treasures
  "Execute a speedrun for medium difficulty treasures."
  [game-state & opts]
  (let [medium [:egg :painting :bag-of-coins :platinum-bar
                :ivory-torch :sceptre]]
    (apply run-speedrun game-state medium opts)))

;;; ---------------------------------------------------------------------------
;;; RESULTS ANALYSIS
;;; ---------------------------------------------------------------------------

(defn analyze-result
  "Analyze speedrun results."
  [result]
  {:status (:status result)
   :turns (:turn-count result)
   :deaths (:deaths result)
   :replans (:replans result)
   :treasures-deposited (count (:deposited-treasures result))
   :treasures-collected (count (:collected-treasures result))
   :completion-rate (/ (count (:deposited-treasures result))
                       (count (:treasures result)))})

(defn print-result
  "Print speedrun results."
  [result]
  (let [analysis (analyze-result result)]
    (println "=== Speedrun Results ===")
    (println "Status:" (name (:status analysis)))
    (println "Turns used:" (:turns analysis))
    (println "Deaths:" (:deaths analysis))
    (println "Replans:" (:replans analysis))
    (println "Treasures deposited:" (:treasures-deposited analysis)
             "/" (count (:treasures result)))
    (println "Completion rate:" (str (int (* 100 (:completion-rate analysis))) "%"))
    (when (seq (:deposited-treasures result))
      (println "Deposited:" (vec (map name (:deposited-treasures result)))))))

;;; ---------------------------------------------------------------------------
;;; TRACE ANALYSIS - For debugging failed runs
;;; ---------------------------------------------------------------------------

(defn print-trace
  "Print the execution trace for debugging.
   Options:
   - :limit N - Show only last N entries (default: all)
   - :filter :event-type - Show only specific event types"
  [result & {:keys [limit filter-type]}]
  (let [trace (:trace result [])
        filtered (if filter-type
                   (filter #(= filter-type (:event %)) trace)
                   trace)
        limited (if limit
                  (take-last limit filtered)
                  filtered)]
    (println "=== Execution Trace ===")
    (println "Total events:" (count trace))
    (when filter-type
      (println "Filtered to:" (name filter-type)))
    (println "")
    (doseq [entry limited]
      (let [event (:event entry)
            data (:data entry)]
        (case event
          :schedule-entry
          (println (str "[SCHED] #" (:index data) " " (name (:type data))
                        (when-let [details (:details data)]
                          (str " " details))))

          :goal-start
          (println (str "[GOAL+] " (:goal data) " at " (name (:room data))
                        " (turn " (:turn data) ")"))

          :goal-complete
          (println (str "[GOAL-] " (:goal data) " -> " (name (:status data))
                        (when (:error data) (str " ERROR: " (:error data)))))

          (println (str "[" (name event) "] " data)))))))

(defn print-checkpoints
  "Print execution checkpoints."
  [result]
  (let [checkpoints (:checkpoints result [])]
    (println "=== Checkpoints ===")
    (println "Total:" (count checkpoints))
    (println "")
    (doseq [cp checkpoints]
      (println (str "Turn " (:turn cp) " | " (:label cp)
                    " | Room: " (name (:room cp))
                    " | Inv: " (:inventory cp)
                    " | Collected: " (:collected cp)
                    " | Deposited: " (:deposited cp))))))

(defn print-failure-info
  "Print detailed information about why a run failed."
  [result]
  (when-let [info (:failure-info result)]
    (println "=== Failure Analysis ===")
    (println "Reason:" (name (:reason info)))
    (case (:reason info)
      :timeout
      (do
        (println "Failed at turn:" (:turn info))
        (println "Entry index:" (:at-entry info))
        (println "Entry:" (:entry info)))

      :too-many-deaths
      (do
        (println "Total deaths:" (:deaths info))
        (println "At entry index:" (:at-entry info)))

      :too-many-replans
      (do
        (println "Total replans:" (:replans info))
        (println "Stuck at entry:" (:stuck-at info))
        (println "Last result:" (:last-result info)))

      (println "Info:" info))))

(defn diagnose
  "Full diagnostic output for a failed run."
  [result]
  (print-result result)
  (println "")
  (print-failure-info result)
  (when (seq (:checkpoints result))
    (println "")
    (print-checkpoints result))
  (when (seq (:trace result))
    (println "")
    (print-trace result :limit 20)))

;;; ---------------------------------------------------------------------------
;;; SCHEDULE INSPECTION - Preview before running
;;; ---------------------------------------------------------------------------

(defn preview-schedule
  "Generate and preview a schedule without executing it."
  [game-state treasures]
  (let [sched (schedule/generate-schedule game-state treasures)]
    (println "=== Schedule Preview ===")
    (println "Treasures:" (mapv name treasures))
    (println "Total entries:" (count sched))
    (println "Estimated turns:" (schedule/schedule-turns sched))
    (println "")
    (doseq [[idx entry] (map-indexed vector sched)]
      (println (str (inc idx) ". "
                    (case (:type entry)
                      :move (str "MOVE to " (name (:to entry)))
                      :prep (str "PREP " (name (:id entry))
                                 (when-let [loc (:location entry)]
                                   (str " at " (if (keyword? loc) (name loc) loc))))
                      :collect (str "COLLECT " (name (:treasure entry))
                                    (when-let [loc (:location entry)]
                                      (str " at " (name loc))))
                      :deposit-all "DEPOSIT ALL treasures"
                      :parallel-work (str "PARALLEL WORK during wait: "
                                          (mapv name (:treasures entry)))
                      :atomic-sequence (str "ATOMIC SEQUENCE: " (:name entry)
                                            " (" (count (:steps entry)) " steps)")
                      (str (:type entry) " " (dissoc entry :type))))))
    sched))

(defn validate-schedule
  "Validate a schedule for obvious issues."
  [game-state treasures]
  (let [sched (schedule/generate-schedule game-state treasures)
        issues []]
    (println "=== Schedule Validation ===")
    (let [issues
          (cond-> issues
            (empty? sched)
            (conj {:level :error :msg "Schedule is empty!"})

            (not (some #(= :deposit-all (:type %)) sched))
            (conj {:level :warn :msg "No deposit-all entry found"})

            (not= (count (filter #(= :collect (:type %)) sched))
                  (count treasures))
            (conj {:level :warn
                   :msg (str "Collect entries (" (count (filter #(= :collect (:type %)) sched))
                             ") != treasures (" (count treasures) ")")}))]
      (if (empty? issues)
        (println "No issues found!")
        (doseq [issue issues]
          (println (str "[" (str/upper-case (name (:level issue))) "] " (:msg issue)))))
      {:valid (not (some #(= :error (:level %)) issues))
       :issues issues})))

;;; ---------------------------------------------------------------------------
;;; MONTE CARLO TESTING
;;; ---------------------------------------------------------------------------
;;; Test strategy robustness by running with multiple RNG seeds.
;;; This helps identify strategies that are dependent on lucky RNG.

(defn analyze-monte-carlo
  "Analyze Monte Carlo test results.
   Returns aggregated statistics."
  [results total-treasures]
  (let [completed (filter #(= :complete (:status %)) results)
        failed (remove #(= :complete (:status %)) results)
        turns (map :turns completed)
        deposited (map :deposited results)]
    {:total-trials (count results)
     :successes (count completed)
     :failures (count failed)
     :success-rate (if (pos? (count results))
                     (double (/ (count completed) (count results)))
                     0.0)
     :avg-turns (if (seq turns)
                  (double (/ (reduce + turns) (count turns)))
                  nil)
     :min-turns (when (seq turns) (apply min turns))
     :max-turns (when (seq turns) (apply max turns))
     :avg-deposited (if (seq deposited)
                      (double (/ (reduce + deposited) (count deposited)))
                      0.0)
     :perfect-runs (count (filter #(= total-treasures (:deposited %)) results))
     :failure-reasons (->> failed
                           (map :failure-reason)
                           frequencies)}))

(defn monte-carlo-test
  "Test a strategy across multiple RNG seeds.

   Parameters:
     game-state - Initial game state
     treasures - Vector of treasures to collect
     num-trials - Number of trials to run (default 10)

   Options:
     :base-seed - Starting seed (default 12345)
     :max-turns - Max turns per trial (default 500)
     :verbose - Print progress (default false)

   Returns map with:
     :results - Vector of individual trial results
     :summary - Aggregated statistics"
  [game-state treasures & {:keys [num-trials base-seed max-turns verbose]
                           :or {num-trials 10 base-seed 12345 max-turns 500 verbose false}}]
  (when verbose
    (println (str "=== Monte Carlo Test: " num-trials " trials ===" ))
    (println (str "Treasures: " (mapv name treasures))))

  (let [results
        (vec
         (for [trial (range num-trials)]
           (let [seed (+ base-seed (* trial 1000))]
             ;; Initialize RNG with this specific seed
             (rng/init! seed)

             (when verbose
               (println (str "\nTrial " (inc trial) "/" num-trials " (seed: " seed ")")))

             (let [result (run-speedrun game-state treasures
                                        :max-turns max-turns
                                        :verbose false)]
               {:trial (inc trial)
                :seed seed
                :status (:status result)
                :turns (:turn-count result)
                :deposited (count (:deposited-treasures result))
                :deaths (:deaths result 0)
                :replans (:replans result 0)
                :failure-reason (get-in result [:failure-info :reason])}))))]

    {:results results
     :summary (analyze-monte-carlo results (count treasures))}))

(defn print-monte-carlo-results
  "Print Monte Carlo test results in a human-readable format."
  [mc-result]
  (let [{:keys [results summary]} mc-result]
    (println "\n=== Monte Carlo Results ===")
    (println (str "Trials: " (:total-trials summary)))
    (println (str "Successes: " (:successes summary)
                  " (" (format "%.1f%%" (* 100 (:success-rate summary))) ")"))
    (println (str "Failures: " (:failures summary)))

    (when (:avg-turns summary)
      (println (str "Turns (avg/min/max): "
                    (format "%.1f" (:avg-turns summary)) " / "
                    (:min-turns summary) " / "
                    (:max-turns summary))))

    (println (str "Avg treasures deposited: " (format "%.1f" (:avg-deposited summary))))
    (println (str "Perfect runs: " (:perfect-runs summary)))

    (when (seq (:failure-reasons summary))
      (println "\nFailure reasons:")
      (doseq [[reason count] (:failure-reasons summary)]
        (println (str "  " (or reason "unknown") ": " count))))

    ;; Show individual results
    (println "\nIndividual trials:")
    (doseq [r results]
      (println (str "  #" (:trial r) ": "
                    (name (:status r))
                    " - " (:deposited r) " treasures"
                    (when (= :complete (:status r))
                      (str " in " (:turns r) " turns"))
                    (when (:failure-reason r)
                      (str " (" (:failure-reason r) ")")))))))

(defn quick-robustness-check
  "Quick check if a strategy is robust (>80% success rate).

   Returns :robust, :fragile, or :broken based on success rate."
  [game-state treasures & {:keys [num-trials] :or {num-trials 5}}]
  (let [mc-result (monte-carlo-test game-state treasures
                                    :num-trials num-trials
                                    :verbose false)
        rate (get-in mc-result [:summary :success-rate] 0)]
    (cond
      (>= rate 0.8) :robust
      (>= rate 0.5) :fragile
      :else :broken)))
