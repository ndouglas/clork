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
            [clork.planner2.schedule :as schedule]))

;;; ---------------------------------------------------------------------------
;;; EXECUTION STATE
;;; ---------------------------------------------------------------------------

(defn make-execution-state
  "Create initial execution state for a speedrun."
  [game-state treasures]
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
   :replans 0})

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
  (let [result (planner/run-goal (:game-state exec-state) goal
                                 :max-turns max-turns)]
    (-> exec-state
        (assoc :game-state (:game-state result))
        (update :turn-count + (:turns-used result 0))
        (assoc :last-result result))))

(defn execute-schedule-entry
  "Execute a single schedule entry.
   Returns updated execution state."
  [exec-state]
  (let [entry (current-schedule-entry exec-state)]
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
            ;; First navigate to location
            location (prep/prep-location prep-id)
            nav-result (when location
                         (execute-goal exec-state (goals/at-room location) 50))]
        (if (and nav-result (not= :complete (:status (:last-result nav-result))))
          (assoc nav-result :status :stuck)
          (let [state (or (:game-state nav-result) (:game-state exec-state))
                ;; Then execute the prep
                action (prep/prep-action prep-id)
                goal (cond
                       (= :combat action)
                       (goals/kill-enemy (get-in prep/prep-actions [prep-id :target]))

                       ;; For map actions, use the flag-set goal which the
                       ;; reactive planner can decompose appropriately
                       (map? action)
                       (goals/flag-set prep-id)

                       :else nil)
                max-turns (if (entry-requires-combat? entry) 100 20)
                result (if goal
                         (execute-goal (assoc exec-state :game-state state) goal max-turns)
                         exec-state)]
            (if (or (nil? goal) (= :complete (:status (:last-result result))))
              (-> result
                  (update :completed-preps conj prep-id)
                  advance-schedule)
              (assoc result :status :stuck)))))

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

(defn run-speedrun
  "Execute a full speedrun for the given treasures.

   Options:
   - :max-turns - Maximum turns before timeout (default 500)
   - :max-deaths - Maximum deaths before giving up (default 3)
   - :verbose - Print progress (default false)

   Returns execution state with:
   - :status - :complete, :timeout, :failed
   - :deposited-treasures - Set of successfully deposited treasures
   - :turn-count - Total turns used
   - :deaths - Number of deaths"
  [game-state treasures & {:keys [max-turns max-deaths verbose]
                           :or {max-turns 500 max-deaths 3 verbose false}}]
  (loop [exec-state (make-execution-state game-state treasures)]
    (when verbose
      (println "Turn" (:turn-count exec-state)
               "- Schedule entry" (:schedule-index exec-state) "of" (count (:schedule exec-state))
               "- Collected:" (count (:collected-treasures exec-state))
               "- Deposited:" (count (:deposited-treasures exec-state))))

    (cond
      ;; Schedule complete
      (schedule-complete? exec-state)
      (assoc exec-state :status :complete)

      ;; Timeout
      (> (:turn-count exec-state) max-turns)
      (assoc exec-state :status :timeout)

      ;; Too many deaths
      (> (:deaths exec-state) max-deaths)
      (assoc exec-state :status :failed)

      ;; Death detected
      (detect-death exec-state)
      (let [recovered (handle-death exec-state)
            replanned (replan-from-current recovered)]
        (recur replanned))

      ;; Stuck - try replanning
      (= :stuck (:status exec-state))
      (let [replanned (replan-from-current exec-state)]
        (if (> (:replans replanned) 5)
          (assoc exec-state :status :failed)
          (recur replanned)))

      ;; Normal execution
      :else
      (recur (execute-schedule-entry exec-state)))))

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
