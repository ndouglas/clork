(ns clork.debug.plan
  "Debug commands for speedrun planning using the reactive planner.

   $plan goal <goal-spec>  - Plan a specific goal
   $plan egg               - Plan to get the egg
   $plan deposit <id>      - Plan to deposit a treasure
   $plan kill <enemy>      - Plan to kill an enemy
   $plan win               - Plan complete game victory

   $run goal <goal-spec>   - Execute a goal reactively
   $run win                - Execute complete speedrun"
  (:require [clork.utils :as utils]
            [clork.planner2.core :as planner]
            [clork.planner2.goals :as goals]
            [clork.planner2.observe :as obs]
            [clork.planner2.navigate :as nav]
            [clojure.string :as str]))

;; =============================================================================
;; Goal Parsing
;; =============================================================================

(defn parse-goal
  "Parse a goal specification from command args.
   Examples:
   - 'egg' -> {:type :have-item :item :egg}
   - 'at kitchen' -> {:type :at-room :room :kitchen}
   - 'kill troll' -> {:type :kill-enemy :enemy :troll}
   - 'deposit egg' -> {:type :item-deposited :item :egg}
   - 'lantern-on' -> {:type :lantern-on}
   - 'win' -> {:type :win}"
  [args]
  (when (seq args)
    (let [first-arg (first args)
          rest-args (rest args)]
      (case first-arg
        "at" (when (seq rest-args)
               (goals/at-room (keyword (first rest-args))))
        "kill" (when (seq rest-args)
                 (goals/kill-enemy (keyword (first rest-args))))
        "deposit" (when (seq rest-args)
                    (goals/item-deposited (keyword (first rest-args))))
        "get" (when (seq rest-args)
                (goals/have-item (keyword (first rest-args))))
        "lantern-on" (goals/lantern-on)
        "win" (goals/win)
        ;; Default: treat as item name
        (goals/have-item (keyword first-arg))))))

;; =============================================================================
;; $plan Commands
;; =============================================================================

(defn- cmd-plan-goal
  "Plan a specific goal and show the execution trace."
  [game-state args]
  (if (empty? args)
    (-> game-state
        (utils/tell "Usage: $plan goal <goal-spec>\n")
        (utils/tell "Examples:\n")
        (utils/tell "  $plan goal egg          - Plan to get the egg\n")
        (utils/tell "  $plan goal at kitchen   - Plan to go to kitchen\n")
        (utils/tell "  $plan goal kill troll   - Plan to kill troll\n")
        (utils/tell "  $plan goal deposit egg  - Plan to deposit egg\n")
        (utils/tell "  $plan goal lantern-on   - Plan to turn on lantern\n")
        (utils/tell "  $plan goal win          - Plan to win the game\n"))
    (if-let [goal (parse-goal args)]
      (let [gs1 (utils/tell game-state (str "Planning: " (goals/goal->string goal) "\n"))
            result (planner/run-goal game-state goal :max-turns 200)]
        (-> gs1
            (utils/tell (str "\nStatus: " (name (:status result)) "\n"))
            (utils/tell (str "Turns: " (:turn result) "\n"))
            (cond->
              (:error result)
              (utils/tell (str "Error: " (:error result) "\n"))

              (= :complete (:status result))
              (utils/tell (str "Final room: " (name (obs/current-room (:game-state result))) "\n")))))
      (utils/tell game-state (str "Could not parse goal: " (str/join " " args) "\n")))))

(defn- cmd-plan-egg
  "Plan to get the egg."
  [game-state _args]
  (cmd-plan-goal game-state ["egg"]))

(defn- cmd-plan-deposit
  "Plan to deposit a treasure."
  [game-state args]
  (if (empty? args)
    (-> game-state
        (utils/tell "Usage: $plan deposit <treasure>\n")
        (utils/tell "Treasures: egg, painting, bag-of-coins, jeweled-scarab, ...\n"))
    (cmd-plan-goal game-state (cons "deposit" args))))

(defn- cmd-plan-kill
  "Plan to kill an enemy."
  [game-state args]
  (if (empty? args)
    (-> game-state
        (utils/tell "Usage: $plan kill <enemy>\n")
        (utils/tell "Enemies: troll, cyclops, thief\n"))
    (cmd-plan-goal game-state (cons "kill" args))))

(defn- cmd-plan-win
  "Show plan for complete game victory."
  [game-state _args]
  (-> game-state
      (utils/tell "Planning complete speedrun...\n")
      (utils/tell "\nThe reactive planner executes goals dynamically.\n")
      (utils/tell "Use '$run win' to execute a full speedrun.\n")
      (utils/tell "\nGoal decomposition:\n")
      (utils/tell "  win\n")
      (utils/tell "    -> all-treasures-deposited\n")
      (utils/tell "         -> item-deposited :egg\n")
      (utils/tell "              -> have-item :egg -> at-room :up-a-tree\n")
      (utils/tell "              -> at-room :living-room\n")
      (utils/tell "              -> container-open :trophy-case\n")
      (utils/tell "         -> item-deposited :painting\n")
      (utils/tell "              -> have-item :painting -> ... (lantern, navigation)\n")
      (utils/tell "         -> ... (17 more treasures)\n")
      (utils/tell "    -> at-room :stone-barrow\n")))

(defn cmd-plan
  "Main $plan command dispatcher."
  [game-state args]
  (if (empty? args)
    (-> game-state
        (utils/tell "Usage: $plan <subcommand> [args]\n\n")
        (utils/tell "Subcommands:\n")
        (utils/tell "  goal <spec>     - Plan a specific goal\n")
        (utils/tell "  egg             - Plan to get the egg\n")
        (utils/tell "  deposit <id>    - Plan to deposit a treasure\n")
        (utils/tell "  kill <enemy>    - Plan to kill an enemy\n")
        (utils/tell "  win             - Show complete game plan\n\n")
        (utils/tell "Examples:\n")
        (utils/tell "  $plan egg\n")
        (utils/tell "  $plan deposit egg\n")
        (utils/tell "  $plan kill troll\n")
        (utils/tell "  $plan goal at troll-room\n"))
    (let [subcmd (first args)
          sub-args (rest args)]
      (case subcmd
        "goal" (cmd-plan-goal game-state sub-args)
        "egg" (cmd-plan-egg game-state sub-args)
        "deposit" (cmd-plan-deposit game-state sub-args)
        "kill" (cmd-plan-kill game-state sub-args)
        "win" (cmd-plan-win game-state sub-args)
        ;; Default: treat as treasure name shortcut
        (if (contains? goals/treasures (keyword subcmd))
          (cmd-plan-goal game-state [(str subcmd)])
          (utils/tell game-state (str "Unknown subcommand: " subcmd "\n")))))))

;; =============================================================================
;; $run Commands
;; =============================================================================

(defn- cmd-run-goal
  "Execute a goal reactively and show results."
  [game-state args]
  (if (empty? args)
    (-> game-state
        (utils/tell "Usage: $run goal <goal-spec>\n")
        (utils/tell "See $plan goal for goal specifications.\n"))
    (if-let [goal (parse-goal args)]
      (let [gs1 (-> game-state
                    (utils/tell (str "\n=== Executing: " (goals/goal->string goal) " ===\n\n")))
            result (planner/run-goal game-state goal :max-turns 300 :verbose? true)
            final-gs (:game-state result)]
        (-> gs1
            (utils/tell (str "\n=== Result ===\n"))
            (utils/tell (str "Status: " (name (:status result)) "\n"))
            (utils/tell (str "Turns: " (:turn result) "\n"))
            (utils/tell (str "Score: " (obs/score final-gs) "\n"))
            (utils/tell (str "Room: " (name (obs/current-room final-gs)) "\n"))
            (cond->
              (:error result)
              (utils/tell (str "Error: " (:error result) "\n")))
            ;; Merge the final game state so the game continues from there
            (merge final-gs)))
      (utils/tell game-state (str "Could not parse goal: " (str/join " " args) "\n")))))

(defn- cmd-run-win
  "Execute a complete speedrun."
  [game-state _args]
  (let [;; Use fresh game state for speedrun
        init-game (resolve 'clork.core/init-game)
        fresh-gs (init-game)
        gs1 (-> game-state
                (utils/tell "\n=== SPEEDRUN EXECUTION ===\n\n")
                (utils/tell "Starting from fresh game state...\n")
                (utils/tell "This may take a few minutes due to combat RNG.\n\n"))]

    ;; Execute the win goal
    (let [result (planner/run-goal fresh-gs (goals/win) :max-turns 1000 :verbose? true)
          final-gs (:game-state result)]
      (-> gs1
          (utils/tell (str "\n=== SPEEDRUN " (if (= :complete (:status result)) "COMPLETE" "FAILED") " ===\n\n"))
          (utils/tell (str "Status: " (name (:status result)) "\n"))
          (utils/tell (str "Moves: " (:turn result) "\n"))
          (utils/tell (str "Score: " (obs/score final-gs) "/350\n"))
          (utils/tell (str "Room: " (name (obs/current-room final-gs)) "\n"))
          (utils/tell (str "Deaths: " (obs/deaths final-gs) "\n"))
          (cond->
            (:error result)
            (utils/tell (str "Error: " (:error result) "\n"))

            (= :complete (:status result))
            (utils/tell "\nCongratulations! The game has been won.\n"))
          ;; Don't merge - keep original game state
          ))))

(defn cmd-run
  "Main $run command dispatcher - executes goals reactively."
  [game-state args]
  (if (empty? args)
    (-> game-state
        (utils/tell "Usage: $run <subcommand> [args]\n\n")
        (utils/tell "Subcommands:\n")
        (utils/tell "  goal <spec>  - Execute a specific goal\n")
        (utils/tell "  win          - Execute complete speedrun\n\n")
        (utils/tell "Unlike $plan, $run actually executes actions.\n")
        (utils/tell "The game state will be updated based on results.\n"))
    (let [subcmd (first args)
          sub-args (rest args)]
      (case subcmd
        "goal" (cmd-run-goal game-state sub-args)
        "win" (cmd-run-win game-state sub-args)
        (utils/tell game-state (str "Unknown subcommand: " subcmd "\n"))))))

;; =============================================================================
;; Exports for help system
;; =============================================================================

(def subcommands
  "Subcommand definitions for help system."
  {:goal {:handler cmd-plan-goal
          :help "Plan a specific goal"}
   :egg {:handler cmd-plan-egg
         :help "Plan to get the egg"}
   :deposit {:handler cmd-plan-deposit
             :help "Plan to deposit a treasure"}
   :kill {:handler cmd-plan-kill
          :help "Plan to kill an enemy"}
   :win {:handler cmd-plan-win
         :help "Show complete game plan"}})

(def run-subcommands
  "Subcommand definitions for $run."
  {:goal {:handler cmd-run-goal
          :help "Execute a specific goal"}
   :win {:handler cmd-run-win
         :help "Execute complete speedrun"}})
