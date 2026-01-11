(ns clork.debug.plan
  "Debug commands for speedrun planning.

   $plan treasure <id>  - Plan to get and deposit a specific treasure
   $plan flag <flag>    - Plan to achieve a specific flag
   $plan win            - Plan complete game victory
   $plan kill-thief     - Plan to kill the thief

   $run win             - Execute complete speedrun (validates the plan)"
  (:require [clork.utils :as utils]
            [clork.planner.actions :as actions]
            [clork.planner.backward :as backward]
            [clork.planner.constraints :as constraints]
            [clork.planner.optimizer :as optimizer]
            [clork.planner.validator :as validator]
            [clojure.string :as str]
            [clojure.set :as set]))

;; =============================================================================
;; Planning Helpers
;; =============================================================================

(defn- build-planning-context
  "Build registry and initial state for planning."
  [game-state]
  (let [registry (actions/build-action-registry game-state)
        initial-state (constraints/initial-planning-state game-state)
        ;; Plan prerequisite flags
        troll-plan (backward/plan-flag-achievement registry :troll-flag initial-state)
        state-after-troll (reduce actions/apply-action-effects initial-state (:plan troll-plan))
        cyclops-plan (backward/plan-flag-achievement registry :cyclops-flag state-after-troll)
        state-after-cyclops (reduce actions/apply-action-effects state-after-troll (:plan cyclops-plan))]
    {:registry registry
     :initial-state initial-state
     :state-after-cyclops state-after-cyclops
     :troll-plan troll-plan
     :cyclops-plan cyclops-plan
     :game-state game-state}))

(defn- print-command-sequence
  "Print a command sequence with actions and their commands."
  [game-state by-action]
  (reduce (fn [gs {:keys [action nav-commands action-commands]}]
            (let [action-name (if (keyword? action) (name action) (str action))
                  ;; Combine navigation and action commands
                  all-commands (into (vec nav-commands) action-commands)
                  cmd-count (count all-commands)]
              (-> gs
                  (utils/tell (str "  " action-name " (" cmd-count " commands):\n"))
                  (as-> g
                    (reduce (fn [gs2 cmd]
                              (utils/tell gs2 (str "    - \"" cmd "\"\n")))
                            g all-commands)))))
          game-state
          by-action))

(defn- generate-full-plan
  "Generate a full plan with navigation commands.
   Returns {:success? bool :by-action [...] :total-moves n :error ...}"
  [game-state plan start-room initial-flags initial-inventory]
  (try
    (let [cmd-seq (optimizer/plan-to-command-sequence
                   game-state plan start-room
                   :initial-flags initial-flags
                   :initial-inventory initial-inventory)]
      {:success? true
       :by-action (:by-action cmd-seq)
       :total-moves (:total-moves cmd-seq)
       :commands (:commands cmd-seq)})
    (catch Exception e
      {:success? false
       :error (.getMessage e)})))

;; =============================================================================
;; $plan treasure <id>
;; =============================================================================

(defn- cmd-plan-treasure
  "Plan to collect and deposit a specific treasure."
  [game-state args]
  (if (empty? args)
    (-> game-state
        (utils/tell "Usage: $plan treasure <treasure-id>\n")
        (utils/tell "Available treasures:\n")
        (utils/tell (str "  " (str/join ", " (map name (sort actions/treasures))) "\n")))
    (let [treasure-id (keyword (first args))]
      (if-not (contains? actions/treasures treasure-id)
        (utils/tell game-state (str "Unknown treasure: " (first args) "\n"))
        (let [gs1 (utils/tell game-state (str "Planning for " (name treasure-id) "...\n"))
              ctx (build-planning-context game-state)
              ;; Get flags achieved by troll+cyclops plans
              flags-after-setup (set/union
                                 (:flags (:state-after-cyclops ctx))
                                 #{:troll-flag :magic-flag :cyclops-flag
                                   :rug-moved :trap-door-open})
              plan-result (backward/plan-treasure-collection
                           (:registry ctx) treasure-id (:state-after-cyclops ctx))]
          (if (:success? plan-result)
            (let [full-plan (generate-full-plan
                             (:game-state ctx)
                             (:plan plan-result)
                             :west-of-house
                             flags-after-setup
                             #{:brass-lantern :sword})]
              (if (:success? full-plan)
                (-> gs1
                    (utils/tell (str "\nPlan for " (name treasure-id)
                                     " (" (:total-moves full-plan) " total commands):\n"))
                    (print-command-sequence (:by-action full-plan)))
                (-> gs1
                    (utils/tell (str "Navigation failed: " (:error full-plan) "\n")))))
            (-> gs1
                (utils/tell (str "Failed to plan for " (name treasure-id) "\n"))
                (utils/tell (str "Error: " (:error plan-result) "\n")))))))))

;; =============================================================================
;; $plan flag <flag>
;; =============================================================================

(defn- cmd-plan-flag
  "Plan to achieve a specific flag."
  [game-state args]
  (let [common-flags [:troll-flag :cyclops-flag :magic-flag :dome-flag :lld-flag
                      :thief-dead :rainbow-flag :loud-room-solved :boat-inflated
                      :dam-opened :low-tide :won]]
    (if (empty? args)
      (-> game-state
          (utils/tell "Usage: $plan flag <flag-name>\n")
          (utils/tell "Common flags:\n")
          (utils/tell (str "  " (str/join ", " (map name common-flags)) "\n")))
      (let [flag-id (keyword (first args))
            gs1 (utils/tell game-state (str "Planning for " (name flag-id) "...\n"))
            ctx (build-planning-context game-state)
            plan-result (backward/plan-flag-achievement
                         (:registry ctx) flag-id (:initial-state ctx))]
        (if (:success? plan-result)
          (let [full-plan (generate-full-plan
                           (:game-state ctx)
                           (:plan plan-result)
                           :west-of-house
                           #{}
                           #{:brass-lantern :sword})]
            (if (:success? full-plan)
              (-> gs1
                  (utils/tell (str "\nPlan for " (name flag-id)
                                   " (" (:total-moves full-plan) " total commands):\n"))
                  (print-command-sequence (:by-action full-plan)))
              (-> gs1
                  (utils/tell (str "Navigation failed: " (:error full-plan) "\n")))))
          (-> gs1
              (utils/tell (str "Failed to plan for " (name flag-id) "\n"))
              (utils/tell (str "Error: " (:error plan-result) "\n"))))))))

;; =============================================================================
;; $plan kill-thief
;; =============================================================================

(defn- cmd-plan-kill-thief
  "Plan to kill the thief (earliest possible)."
  [game-state _args]
  (let [gs1 (utils/tell game-state "Planning to kill thief...\n")
        ctx (build-planning-context game-state)
        flags-after-setup (set/union
                           (:flags (:state-after-cyclops ctx))
                           #{:troll-flag :magic-flag :cyclops-flag
                             :rug-moved :trap-door-open})
        plan-result (backward/plan-flag-achievement
                     (:registry ctx) :thief-dead (:state-after-cyclops ctx))]
    (if (:success? plan-result)
      (let [full-plan (generate-full-plan
                       (:game-state ctx)
                       (:plan plan-result)
                       :west-of-house
                       flags-after-setup
                       #{:brass-lantern :sword})]
        (if (:success? full-plan)
          (-> gs1
              (utils/tell (str "\nPlan to kill thief (" (:total-moves full-plan) " total commands):\n"))
              (print-command-sequence (:by-action full-plan))
              (utils/tell "\nNote: Combat success depends on score (fight-strength = 2 + score/70).\n")
              (utils/tell "Pre-thief deposits recommended for better odds.\n"))
          (-> gs1
              (utils/tell (str "Navigation failed: " (:error full-plan) "\n")))))
      (-> gs1
          (utils/tell "Failed to plan thief kill\n")
          (utils/tell (str "Error: " (:error plan-result) "\n"))))))

;; =============================================================================
;; $plan win - Complete Speedrun Generation
;; =============================================================================

(defn- plan-all-treasures-in-order
  "Plan all treasures in TSP-optimized order.
   Returns {:success? bool :plans [...] :final-state state :failed [...]}"
  [registry state-after-cyclops available-flags game-state]
  ;; Use TSP to find optimal order for reachable treasures
  ;; Then append unreachable treasures (the planner will handle their dependencies)
  (let [tsp-result (backward/optimize-treasure-order
                    game-state
                    (vec actions/treasures)
                    available-flags
                    :living-room)
        ;; Start with TSP-optimized reachable treasures, then add unreachable ones
        ;; The backward planner will figure out the actual order via dependencies
        all-treasures (concat (:optimized-order tsp-result)
                              (:unreachable tsp-result))]
    ;; Plan each treasure in order, updating state as we go
    (loop [remaining all-treasures
           current-state state-after-cyclops
           plans []
           failed []]
      (if (empty? remaining)
        {:success? true :plans plans :final-state current-state :failed failed}
        (let [treasure (first remaining)
              plan-result (backward/plan-treasure-collection
                           registry treasure current-state)]
          (if (:success? plan-result)
            (let [new-state (reduce actions/apply-action-effects
                                    current-state
                                    (:plan plan-result))]
              (recur (rest remaining)
                     new-state
                     (conj plans {:treasure treasure
                                  :plan (:plan plan-result)})
                     failed))
            ;; Track failed treasure and continue
            (recur (rest remaining)
                   current-state
                   plans
                   (conj failed treasure))))))))

(defn- cmd-plan-win
  "Plan complete game victory - generates full executable command sequence.

   Uses BATCHED treasure collection: collects multiple treasures before depositing
   to minimize round trips to the living room."
  [game-state _args]
  (let [gs1 (utils/tell game-state "Planning complete speedrun (batched)...\n")

        ;; Build planning context
        registry (actions/build-action-registry game-state)
        initial-state (constraints/initial-planning-state game-state)

        ;; Phase 1: Kill troll
        _ (println "  Phase 1: Planning troll kill...")
        troll-plan (backward/plan-flag-achievement registry :troll-flag initial-state)
        state-after-troll (reduce actions/apply-action-effects
                                  initial-state
                                  (:plan troll-plan))

        ;; Phase 2: Defeat cyclops (opens return route)
        _ (println "  Phase 2: Planning cyclops defeat...")
        cyclops-plan (backward/plan-flag-achievement registry :cyclops-flag state-after-troll)
        state-after-cyclops (reduce actions/apply-action-effects
                                    state-after-troll
                                    (:plan cyclops-plan))

        ;; Available flags after setup phases
        available-flags (set/union (:flags state-after-cyclops)
                                   #{:troll-flag :magic-flag :cyclops-flag
                                     :rug-moved :trap-door-open :coffin-cure})

        ;; Phase 3: All treasures using BATCHED collection
        ;; First, get the TSP-optimized treasure order
        _ (println "  Phase 3: Planning all 19 treasures (batched)...")
        tsp-result (backward/optimize-treasure-order
                    game-state
                    (vec actions/treasures)
                    available-flags
                    :living-room)
        ordered-treasures (concat (:optimized-order tsp-result)
                                  (:unreachable tsp-result))
        _ (println "    TSP order:" (take 5 ordered-treasures) "...")

        ;; Use batched planning - collect multiple treasures, deposit together
        ;; max-items 7 allows: lamp + sword + 5 treasures (or + garlic + 4 treasures)
        treasure-result (backward/plan-treasures-batched
                         registry
                         ordered-treasures
                         state-after-cyclops
                         :max-items 7)
        _ (println "    Batches:" (count (:batches treasure-result)))
        _ (doseq [{:keys [acquired]} (:batches treasure-result)]
            (println "      -" (count acquired) "treasures:" (take 3 acquired) "..."))

        ;; Phase 4: Kill thief (if not already done during treasure collection)
        _ (println "  Phase 4: Planning thief kill...")
        state-after-treasures (:final-state treasure-result)
        thief-already-dead? (contains? (:flags state-after-treasures) :thief-dead)
        thief-plan (if thief-already-dead?
                     {:plan []}
                     (backward/plan-flag-achievement
                      registry :thief-dead state-after-treasures))

        ;; Phase 5: Enter stone barrow (requires :won flag, sets :finished)
        _ (println "  Phase 5: Planning stone barrow entry...")
        state-after-thief (reduce actions/apply-action-effects
                                  state-after-treasures
                                  (:plan thief-plan))
        barrow-plan (backward/plan-flag-achievement
                     registry :finished state-after-thief)

        ;; Combine all action plans
        all-actions (concat (:plan troll-plan)
                            (:plan cyclops-plan)
                            (:plan treasure-result)
                            (:plan thief-plan)
                            (:plan barrow-plan))

        ;; Generate full command sequence
        _ (println "  Generating command sequence...")
        cmd-seq (optimizer/plan-to-command-sequence
                 game-state
                 all-actions
                 :west-of-house
                 :initial-flags #{}
                 :initial-inventory #{})

        all-commands (:commands cmd-seq)
        total-moves (count all-commands)]

    (if (empty? all-commands)
      (utils/tell gs1 "Failed to generate speedrun plan.\n")

      ;; Success - print the complete command sequence
      (let [;; Group commands by phase for summary
            phase-boundaries
            [{:name "Kill Troll" :actions (count (:plan troll-plan))}
             {:name "Defeat Cyclops" :actions (count (:plan cyclops-plan))}
             {:name "Collect Treasures" :actions (count (:plan treasure-result))}
             {:name "Kill Thief" :actions (count (:plan thief-plan))}
             {:name "Enter Barrow" :actions (count (:plan barrow-plan))}]

            ;; Format the numbered command list
            numbered-commands
            (str/join "\n"
                      (map-indexed (fn [i cmd]
                                     (format "%3d. %s" (inc i) cmd))
                                   all-commands))]

        (-> gs1
            (utils/tell (str "\n=== COMPLETE SPEEDRUN (" total-moves " commands) ===\n\n"))
            (utils/tell "Phases:\n")
            (as-> g
              (reduce (fn [gs {:keys [name actions]}]
                        (utils/tell gs (str "  - " name " (" actions " actions)\n")))
                      g phase-boundaries))
            (utils/tell (str "\nTreasures planned: "
                             (reduce + (map #(count (:acquired %)) (:batches treasure-result)))
                             "/19\n"))
            (utils/tell (str "Batches: " (count (:batches treasure-result)) "\n"))
            (as-> g
              (reduce (fn [gs batch]
                        (utils/tell gs (str "  - " (count (:acquired batch)) " treasures: "
                                            (str/join ", " (map name (take 3 (:acquired batch))))
                                            (when (> (count (:acquired batch)) 3) "...") "\n")))
                      g (:batches treasure-result)))
            (utils/tell "\n--- Commands ---\n")
            (utils/tell numbered-commands)
            (utils/tell "\n\n--- End of Speedrun ---\n")
            (utils/tell "\nNote: Combat outcomes depend on RNG. Save before troll/thief fights.\n")
            (utils/tell "Estimated completion: ~250-300 turns depending on combat luck.\n"))))))


;; =============================================================================
;; $run win - Execute Speedrun
;; =============================================================================

(defn- generate-speedrun-commands
  "Generate the command list for a complete speedrun.
   Returns {:success? bool :commands [...] :error ...}"
  [game-state]
  (try
    (let [;; Build planning context
          registry (actions/build-action-registry game-state)
          initial-state (constraints/initial-planning-state game-state)

          ;; Phase 1: Kill troll
          troll-plan (backward/plan-flag-achievement registry :troll-flag initial-state)
          state-after-troll (reduce actions/apply-action-effects
                                    initial-state
                                    (:plan troll-plan))

          ;; Phase 2: Defeat cyclops
          cyclops-plan (backward/plan-flag-achievement registry :cyclops-flag state-after-troll)
          state-after-cyclops (reduce actions/apply-action-effects
                                      state-after-troll
                                      (:plan cyclops-plan))

          ;; Available flags after setup
          available-flags (set/union (:flags state-after-cyclops)
                                     #{:troll-flag :magic-flag :cyclops-flag
                                       :rug-moved :trap-door-open :coffin-cure})

          ;; Phase 3: All treasures (batched)
          tsp-result (backward/optimize-treasure-order
                      game-state
                      (vec actions/treasures)
                      available-flags
                      :living-room)
          ordered-treasures (concat (:optimized-order tsp-result)
                                    (:unreachable tsp-result))
          treasure-result (backward/plan-treasures-batched
                           registry
                           ordered-treasures
                           state-after-cyclops
                           :max-items 7)

          ;; Phase 4: Kill thief (if needed)
          state-after-treasures (:final-state treasure-result)
          thief-already-dead? (contains? (:flags state-after-treasures) :thief-dead)
          thief-plan (if thief-already-dead?
                       {:plan []}
                       (backward/plan-flag-achievement
                        registry :thief-dead state-after-treasures))

          ;; Phase 5: Enter barrow
          state-after-thief (reduce actions/apply-action-effects
                                    state-after-treasures
                                    (:plan thief-plan))
          barrow-plan (backward/plan-flag-achievement
                       registry :finished state-after-thief)

          ;; Combine all actions
          all-actions (concat (:plan troll-plan)
                              (:plan cyclops-plan)
                              (:plan treasure-result)
                              (:plan thief-plan)
                              (:plan barrow-plan))

          ;; Generate command sequence
          cmd-seq (optimizer/plan-to-command-sequence
                   game-state
                   all-actions
                   :west-of-house
                   :initial-flags #{}
                   :initial-inventory #{})]
      {:success? true
       :commands (:commands cmd-seq)
       :treasures-planned (reduce + (map #(count (:acquired %)) (:batches treasure-result)))})
    (catch Exception e
      {:success? false
       :error (.getMessage e)})))

(defn- format-combat-results
  "Format combat results for display."
  [gs combat-results]
  (if (seq combat-results)
    (reduce (fn [g {:keys [enemy attacks-used]}]
              (utils/tell g (str "  - " enemy ": " attacks-used " attacks\n")))
            (utils/tell gs "\nCombat results:\n")
            combat-results)
    gs))

(defn- cmd-run-win
  "Execute a complete speedrun with adaptive combat.
   Generates the plan and executes it, handling RNG dynamically."
  [game-state _args]
  (let [gs1 (-> game-state
                (utils/tell "=== SPEEDRUN EXECUTION ===\n\n")
                (utils/tell "Generating plan...\n"))
        plan-result (generate-speedrun-commands game-state)]

    (if-not (:success? plan-result)
      (utils/tell gs1 (str "Failed to generate plan: " (:error plan-result) "\n"))

      ;; Execute the plan
      (let [commands (:commands plan-result)
            _ (println (str "Plan generated: " (count commands) " commands"))
            _ (println (str "Treasures to collect: " (:treasures-planned plan-result) "/19"))
            _ (println "Executing speedrun...")

            ;; Progress tracking
            step-count (atom 0)
            last-progress (atom 0)

            ;; Execute with adaptive combat
            result (validator/execute-speedrun-adaptive
                    game-state
                    commands
                    :on-progress
                    (fn [_step _cmd _output]
                      (swap! step-count inc)
                      (when (>= (- @step-count @last-progress) 50)
                        (println (str "  Progress: " @step-count " commands..."))
                        (reset! last-progress @step-count))))]

        (if (:success? result)
          ;; Success!
          (let [final-gs (:final-state result)
                score (get final-gs :score 0)]
            (-> gs1
                (utils/tell "\n=== SPEEDRUN COMPLETE! ===\n\n")
                (utils/tell (str "Final room: " (name (:final-room result)) "\n"))
                (utils/tell (str "Commands executed: " (:commands-executed result) "\n"))
                (utils/tell (str "Actual moves (with combat): " (:actual-commands result) "\n"))
                (utils/tell (str "Score: " score " / 350\n"))
                (format-combat-results (:combat-results result))
                (merge final-gs)))

          ;; Failed
          (let [error (:error result)
                output-str (when-let [o (:output error)] (str/trim o))]
            (-> gs1
                (utils/tell "\n=== SPEEDRUN FAILED ===\n\n")
                (utils/tell (str "Failed at step " (:step error) "\n"))
                (utils/tell (str "Command: " (:command error) "\n"))
                (utils/tell (str "Room: " (name (or (:from error) (:final-room result))) "\n"))
                (utils/tell (str "Error: " (:message error) "\n"))
                (cond-> output-str (utils/tell (str "Output: " output-str "\n"))))))))))


;; =============================================================================
;; $plan (main dispatcher)
;; =============================================================================

(defn cmd-plan
  "Main $plan command dispatcher."
  [game-state args]
  (if (empty? args)
    (-> game-state
        (utils/tell "Usage: $plan <subcommand> [args]\n\n")
        (utils/tell "Subcommands:\n")
        (utils/tell "  treasure <id>  - Plan to get and deposit a treasure\n")
        (utils/tell "  flag <flag>    - Plan to achieve a flag\n")
        (utils/tell "  kill-thief     - Plan earliest thief kill\n")
        (utils/tell "  win            - Plan complete game victory\n\n")
        (utils/tell "Examples:\n")
        (utils/tell "  $plan treasure egg\n")
        (utils/tell "  $plan treasure huge-diamond\n")
        (utils/tell "  $plan flag dome-flag\n"))
    (let [subcmd (first args)
          sub-args (rest args)]
      (case subcmd
        "treasure" (cmd-plan-treasure game-state sub-args)
        "flag" (cmd-plan-flag game-state sub-args)
        "kill-thief" (cmd-plan-kill-thief game-state sub-args)
        "win" (cmd-plan-win game-state sub-args)
        ;; Default: assume it's a treasure name
        (if (contains? actions/treasures (keyword subcmd))
          (cmd-plan-treasure game-state args)
          (utils/tell game-state (str "Unknown subcommand: " subcmd "\n")))))))

(def subcommands
  "Subcommand definitions for help system."
  {:treasure {:handler cmd-plan-treasure
              :help "Plan to collect a specific treasure"}
   :flag {:handler cmd-plan-flag
          :help "Plan to achieve a specific flag"}
   :kill-thief {:handler cmd-plan-kill-thief
                :help "Plan earliest thief kill"}
   :win {:handler cmd-plan-win
         :help "Plan complete game victory"}})

;; =============================================================================
;; $run (execute plans)
;; =============================================================================

(defn cmd-run
  "Main $run command dispatcher - executes plans instead of just displaying them."
  [game-state args]
  (if (empty? args)
    (-> game-state
        (utils/tell "Usage: $run <subcommand>\n\n")
        (utils/tell "Subcommands:\n")
        (utils/tell "  win  - Execute complete speedrun (with adaptive combat)\n\n")
        (utils/tell "Unlike $plan, $run actually executes the commands.\n")
        (utils/tell "Combat is handled adaptively - attacks continue until victory.\n"))
    (let [subcmd (first args)
          sub-args (rest args)]
      (case subcmd
        "win" (cmd-run-win game-state sub-args)
        (utils/tell game-state (str "Unknown subcommand: " subcmd "\n"))))))

(def run-subcommands
  "Subcommand definitions for $run."
  {:win {:handler cmd-run-win
         :help "Execute complete speedrun"}})
