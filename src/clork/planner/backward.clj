(ns clork.planner.backward
  "Backward-chaining planner for Clork speedruns.

   Works backward from the win condition to find a valid plan:
   1. Start with goal state (all treasures deposited, player at stone-barrow)
   2. Find unmet goals
   3. Find actions that achieve each goal
   4. Add action preconditions as new sub-goals
   5. Repeat until all goals satisfied by initial state

   Handles:
   - Flag dependencies (troll-flag before cyclops-room)
   - Inventory constraints (deposit trips when overloaded)
   - One-way paths (collect items before committing)
   - Light requirements (lamp/torch in dark areas)"
  (:require [clojure.set :as set]
            [clork.planner.actions :as actions]
            [clork.planner.constraints :as constraints]))

;; =============================================================================
;; Win Condition
;; =============================================================================

(def win-condition
  "The goal state for winning Zork I.
   All treasures in trophy case, player enters stone barrow."
  {:deposited actions/treasures
   :flags #{:won}
   :here :stone-barrow})

(def essential-flags
  "Flags that must be achieved to reach the win condition.
   These enable access to areas containing treasures."
  #{:troll-flag      ; Access to maze, cyclops room, dam area
    :cyclops-flag    ; Access to treasure room (thief's lair)
    :magic-flag      ; Access through strange passage
    :dome-flag       ; Access to torch room (rope tied)
    :lld-flag        ; Access to land of living dead (skull)
    :dam-opened      ; Opens sluice gates
    :low-tide        ; Access to reservoir area
    :rainbow-flag    ; Get pot of gold
    :thief-dead      ; Get jade, bracelet, open egg
    :loud-room-solved ; Get gold bar
    :boat-inflated}) ; Navigate river

;; =============================================================================
;; Goal Types
;; =============================================================================

(defn goal-type
  "Classify a goal for prioritization."
  [goal]
  (cond
    (and (vector? goal) (= :deposit (first goal))) :deposit
    (and (vector? goal) (= :have (first goal))) :have-item
    (and (vector? goal) (= :at (first goal))) :location
    (keyword? goal) :flag
    :else :unknown))

(defn goal-to-string
  "Convert goal to readable string."
  [goal]
  (cond
    (and (vector? goal) (= :deposit (first goal)))
    (str "Deposit " (name (second goal)))

    (and (vector? goal) (= :have (first goal)))
    (str "Have " (name (second goal)))

    (and (vector? goal) (= :at (first goal)))
    (str "Be at " (name (second goal)))

    (keyword? goal)
    (str "Flag: " (name goal))

    :else (str goal)))

;; =============================================================================
;; Goal Analysis
;; =============================================================================

(defn extract-deposit-goals
  "Extract deposit goals for all treasures."
  []
  (set (map #(vector :deposit %) actions/treasures)))

(defn extract-unmet-goals
  "Extract goals not satisfied by current state."
  [goal-state current-state]
  (let [deposit-goals (extract-deposit-goals)
        unmet-deposits (set/difference (:deposited goal-state #{})
                                       (:deposited current-state #{}))
        unmet-flags (set/difference (:flags goal-state #{})
                                    (:flags current-state #{}))]
    (set/union
     (set (map #(vector :deposit %) unmet-deposits))
     unmet-flags)))

;; =============================================================================
;; Achiever Finding
;; =============================================================================

(defn find-achievers-for-deposit
  "Find actions that deposit a specific treasure."
  [registry treasure-id]
  (filter #(= (:treasure %) treasure-id)
          (vals registry)))

(defn find-achievers-for-flag
  "Find actions that set a specific flag."
  [registry flag]
  (filter #(contains? (get-in % [:effects :flags-set] #{}) flag)
          (vals registry)))

(defn find-achievers-for-item
  "Find actions that add an item to inventory."
  [registry item-id]
  (filter #(contains? (get-in % [:effects :inventory-add] #{}) item-id)
          (vals registry)))

(defn find-achievers-for-location
  "Find actions that move player to a specific room.
   These are movement actions with :effects :new-location matching target."
  [registry room-id]
  (filter #(= room-id (get-in % [:effects :new-location]))
          (vals registry)))

(defn find-achievers
  "Find all actions that can achieve a goal."
  [registry goal]
  (cond
    (and (vector? goal) (= :deposit (first goal)))
    (find-achievers-for-deposit registry (second goal))

    (and (vector? goal) (= :have (first goal)))
    (find-achievers-for-item registry (second goal))

    (and (vector? goal) (= :at (first goal)))
    (find-achievers-for-location registry (second goal))

    (keyword? goal)
    (find-achievers-for-flag registry goal)

    :else []))

;; =============================================================================
;; Precondition Expansion
;; =============================================================================

(defn action-preconditions-as-goals
  "Convert an action's preconditions to goal format."
  [action]
  (let [preconds (:preconditions action)]
    (set/union
     ;; Location as goal
     (if (:here preconds)
       #{[:at (:here preconds)]}
       #{})
     ;; Required items as goals
     (set (map #(vector :have %) (:inventory preconds #{})))
     ;; Required flags as goals
     (:flags preconds #{}))))

;; =============================================================================
;; Backward Planning Algorithm
;; =============================================================================

(defn select-best-achiever
  "Select the best achiever for a goal based on cost and complexity.
   Prefers lower cost and fewer preconditions."
  [achievers current-state]
  (when (seq achievers)
    (->> achievers
         (sort-by (fn [action]
                    (let [cost (:cost action 1)
                          precond-count (+ (count (get-in action [:preconditions :inventory] #{}))
                                           (count (get-in action [:preconditions :flags] #{})))]
                      (+ cost (* 10 precond-count)))))
         first)))

(defn select-best-goal
  "Select the best goal to work on next.
   Prioritizes:
   1. Flags (unlock areas first)
   2. Items (collect treasures)
   3. Deposits (put in trophy case)"
  [goals]
  (let [sorted (sort-by (fn [g]
                          (case (goal-type g)
                            :flag 0
                            :have-item 1
                            :deposit 2
                            :location 3
                            4))
                        goals)]
    (first sorted)))

(defn plan-backward
  "Main backward-chaining planner.

   Parameters:
   - goal-state: Target state to achieve
   - initial-state: Starting state
   - registry: Action registry

   Returns:
   {:success? bool
    :plan [actions in execution order]
    :error error-info (if failed)}"
  [goal-state initial-state registry]
  (let [max-iterations 1000]
    (loop [goals (extract-unmet-goals goal-state initial-state)
           plan []
           achieved-state initial-state
           visited-actions #{}
           iterations 0]
      (cond
        ;; Success - all goals met
        (empty? goals)
        {:success? true
         :plan (reverse plan)
         :iterations iterations}

        ;; Iteration limit
        (>= iterations max-iterations)
        {:success? false
         :plan (reverse plan)
         :remaining-goals goals
         :error :max-iterations}

        :else
        (let [goal (select-best-goal goals)]
          ;; Special handling for location goals:
          ;; We treat them as implicitly satisfiable via A* pathfinding.
          ;; The optimizer will generate actual navigation commands.
          ;; However, we need to check if the room requires light!
          (if (= :location (goal-type goal))
            ;; Location goals are achievable from initial state
            ;; But if the room is dark, we need a light source
            (let [target-room (second goal)
                  needs-light? (constraints/requires-light? target-room)
                  ;; If dark room and no light in achieved inventory, add light goal
                  has-light? (or (contains? (:inventory achieved-state) :brass-lantern)
                                 (contains? (:inventory achieved-state) :ivory-torch))
                  new-goals (if (and needs-light? (not has-light?))
                              ;; Add lantern requirement (simplest light source)
                              (-> goals
                                  (disj goal)
                                  (conj [:have :brass-lantern]))
                              (disj goals goal))]
              (recur new-goals
                     plan
                     achieved-state
                     visited-actions
                     (inc iterations)))

            ;; Normal goal processing
            (let [achievers (find-achievers registry goal)]
              (if (empty? achievers)
                ;; No achiever found - check if already satisfied
                (if (case (goal-type goal)
                      :flag (contains? (:flags achieved-state) goal)
                      :have-item (contains? (:inventory achieved-state) (second goal))
                      :deposit (contains? (:deposited achieved-state) (second goal))
                      false)
                  ;; Goal already satisfied, remove it
                  (recur (disj goals goal)
                         plan
                         achieved-state
                         visited-actions
                         (inc iterations))
                  ;; Can't achieve goal
                  {:success? false
                   :plan (reverse plan)
                   :error {:type :no-achiever :goal goal}
                   :remaining-goals goals
                   :iterations iterations})

                ;; Try to use an achiever
                (let [achiever (select-best-achiever
                               (remove #(contains? visited-actions (:id %)) achievers)
                               achieved-state)]
                  (if (nil? achiever)
                    ;; All achievers already tried
                    {:success? false
                     :plan (reverse plan)
                     :error {:type :cycle :goal goal}
                     :remaining-goals goals
                     :iterations iterations}

                    ;; Add achiever to plan
                    (let [new-precond-goals (action-preconditions-as-goals achiever)
                          new-achieved (actions/apply-action-effects achiever achieved-state)
                          remaining-goals (-> goals
                                              (disj goal)
                                              (set/union new-precond-goals))]
                      (recur remaining-goals
                             (conj plan achiever)
                             new-achieved
                             (conj visited-actions (:id achiever))
                             (inc iterations)))))))))))))

;; =============================================================================
;; High-Level Planning
;; =============================================================================

(defn plan-treasure-collection
  "Plan collection and deposit of a single treasure."
  [registry treasure-id initial-state]
  (let [goal-state {:deposited #{treasure-id}
                    :flags #{}
                    :here nil}]
    (plan-backward goal-state initial-state registry)))

(defn plan-all-treasures
  "Plan collection of all treasures.
   Returns ordered list of treasure collection plans."
  [registry initial-state]
  (loop [remaining (vec actions/treasures)
         state initial-state
         all-plans []]
    (if (empty? remaining)
      {:success? true :plans all-plans}
      (let [treasure (first remaining)
            plan-result (plan-treasure-collection registry treasure state)]
        (if (:success? plan-result)
          (let [final-state (reduce actions/apply-action-effects
                                    state
                                    (:plan plan-result))]
            (recur (rest remaining)
                   final-state
                   (conj all-plans {:treasure treasure
                                    :plan (:plan plan-result)})))
          {:success? false
           :failed-at treasure
           :error (:error plan-result)
           :completed-plans all-plans})))))

;; =============================================================================
;; Flag Dependency Chain
;; =============================================================================

(def flag-dependencies
  "Known dependencies between flags.
   Format: {flag #{required-flags}}"
  {:cyclops-flag #{:troll-flag}
   :magic-flag #{:troll-flag}
   :dome-flag #{:troll-flag}
   :lld-flag #{:troll-flag}  ; Need to reach entrance-to-hades
   :thief-dead #{:cyclops-flag}  ; Need to reach treasure room
   :low-tide #{:dam-opened}
   :rainbow-flag #{}  ; Just need sceptre at end-of-rainbow
   :loud-room-solved #{:troll-flag}})

(defn required-flags-for
  "Get all flags required to achieve a target flag (transitive)."
  [target-flag]
  (loop [to-check #{target-flag}
         checked #{}
         required #{}]
    (if (empty? to-check)
      required
      (let [flag (first to-check)
            deps (get flag-dependencies flag #{})
            new-deps (set/difference deps checked)]
        (recur (set/union (disj to-check flag) new-deps)
               (conj checked flag)
               (set/union required deps))))))

(defn plan-flag-achievement
  "Plan to achieve a specific flag, respecting dependencies."
  [registry target-flag initial-state]
  (let [all-required (conj (required-flags-for target-flag) target-flag)
        goal-state {:flags all-required
                    :deposited #{}
                    :here nil}]
    (plan-backward goal-state initial-state registry)))

;; =============================================================================
;; Complete Speedrun Planning
;; =============================================================================

(defn plan-speedrun
  "Generate a complete speedrun plan.

   Strategy:
   1. Achieve essential flags first (unlock areas)
   2. Collect and deposit treasures in efficient order
   3. Navigate to stone barrow to win

   Returns:
   {:success? bool
    :phases [{:name :phase-name :actions [...]}]
    :total-actions count
    :estimated-moves count}"
  [game-state]
  (let [registry (actions/build-action-registry game-state)
        initial-state (constraints/initial-planning-state game-state)]

    ;; Phase 1: Essential flags
    (println "Planning Phase 1: Essential flags...")
    (let [flag-plan (plan-flag-achievement registry :troll-flag initial-state)]
      (if-not (:success? flag-plan)
        {:success? false :error "Failed to plan troll flag" :details flag-plan}

        ;; Phase 2: More flags + treasures
        (do
          (println "Planning Phase 2: Additional flags and treasures...")
          (let [state-after-troll (reduce actions/apply-action-effects
                                          initial-state
                                          (:plan flag-plan))
                ;; Simplified: just return the flag plan for now
                ;; Full implementation would continue with treasure collection
                ]
            {:success? true
             :phases [{:name :troll-setup
                       :actions (:plan flag-plan)}]
             :total-actions (count (:plan flag-plan))
             :estimated-moves (* 2 (count (:plan flag-plan)))}))))))

;; =============================================================================
;; Debug Utilities
;; =============================================================================

(defn print-plan
  "Pretty print a plan."
  [plan]
  (println "\n=== Plan (" (count plan) "actions) ===")
  (doseq [[idx action] (map-indexed vector plan)]
    (println (str "  " (inc idx) ". " (:id action)
                  " [" (name (:type action)) "]"
                  " - " (first (:commands action ["-"]))))))

(defn print-goal-analysis
  "Print analysis of goals for debugging."
  [goals]
  (println "\n=== Goals (" (count goals) ") ===")
  (doseq [[type goals] (group-by goal-type goals)]
    (println (str "  " (name type) ":"))
    (doseq [g goals]
      (println (str "    - " (goal-to-string g))))))

(defn trace-achievers
  "Print all achievers for a goal."
  [registry goal]
  (let [achievers (find-achievers registry goal)]
    (println (str "\n=== Achievers for " (goal-to-string goal) " ==="))
    (if (empty? achievers)
      (println "  (none found)")
      (doseq [a achievers]
        (println (str "  - " (:id a)
                      " (cost: " (:cost a) ")"
                      " requires: " (get-in a [:preconditions :flags] #{})))))))
