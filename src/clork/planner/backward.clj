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
   - Light requirements (lamp/torch in dark areas)
   - Location dependencies (flags required to reach destinations)"
  (:require [clojure.set :as set]
            [clork.planner.actions :as actions]
            [clork.planner.constraints :as constraints]
            [clork.planner.optimizer :as optimizer]))

;; Forward declaration for plan ordering
(declare sort-plan-by-dependencies)

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
         :plan (sort-plan-by-dependencies (reverse plan))
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
          ;; However, we need to check constraints AND location dependencies!
          (if (= :location (goal-type goal))
            (let [target-room (second goal)
                  ;; Check: Light requirements for dark rooms
                  ;; Need both the lantern AND have it turned on
                  needs-light? (constraints/requires-light? target-room)
                  has-lantern? (or (contains? (:inventory achieved-state) :brass-lantern)
                                   (contains? (:inventory achieved-state) :ivory-torch))
                  lantern-on? (contains? (:flags achieved-state) :lantern-on)

                  ;; Check: Location dependencies (flags required to reach this room)
                  ;; This ensures actions at flag-gated destinations get properly ordered
                  required-flags (optimizer/flags-required-for-room target-room)
                  missing-location-flags (set/difference required-flags (:flags achieved-state))

                  ;; NOTE: We no longer add magic-flag requirement for underground return.
                  ;; The chimney (studio -> kitchen) provides an alternate exit from underground
                  ;; that doesn't require any flags. The optimizer's navigation will find
                  ;; the chimney route via A* when cellar->living-room is blocked.

                  ;; Build new goals
                  base-goals (disj goals goal)
                  ;; Add light requirements if needed:
                  ;; 1. Need to have the lantern
                  ;; 2. Need to turn it on (adds :lantern-on flag goal)
                  with-lantern (if (and needs-light? (not has-lantern?))
                                 (conj base-goals [:have :brass-lantern])
                                 base-goals)
                  with-light (if (and needs-light? (not lantern-on?))
                               (conj with-lantern :lantern-on)
                               with-lantern)
                  ;; Add location dependency flags as goals
                  final-goals (set/union with-light missing-location-flags)]
              (recur final-goals
                     plan
                     achieved-state
                     visited-actions
                     (inc iterations)))

            ;; Normal goal processing
            ;; FIRST check if goal is already satisfied by achieved state
            (let [already-satisfied? (case (goal-type goal)
                                       :flag (contains? (:flags achieved-state) goal)
                                       :have-item (contains? (:inventory achieved-state) (second goal))
                                       :deposit (contains? (:deposited achieved-state) (second goal))
                                       false)]
              (if already-satisfied?
                ;; Goal already satisfied, just remove it
                (recur (disj goals goal)
                       plan
                       achieved-state
                       visited-actions
                       (inc iterations))

                ;; Goal not satisfied, find achievers
                (let [achievers (find-achievers registry goal)]
                  (if (empty? achievers)
                    ;; No achiever found
                    {:success? false
                     :plan (reverse plan)
                     :error {:type :no-achiever :goal goal}
                     :remaining-goals goals
                     :iterations iterations}

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
                              new-achieved (actions/apply-action-effects achieved-state achiever)
                              remaining-goals (-> goals
                                                  (disj goal)
                                                  (set/union new-precond-goals))]
                          (recur remaining-goals
                                 (conj plan achiever)
                                 new-achieved
                                 (conj visited-actions (:id achiever))
                                 (inc iterations)))))))))))))))

;; =============================================================================
;; Plan Ordering (Topological Sort)
;; =============================================================================

(defn action-provides
  "Get the set of flags an action provides."
  [action]
  (get-in action [:effects :flags-set] #{}))

(defn action-requires
  "Get the set of flags an action requires."
  [action]
  (get-in action [:preconditions :flags] #{}))

(defn action-adds-to-inventory
  "Get items an action adds to inventory."
  [action]
  (get-in action [:effects :inventory-add] #{}))

(defn action-requires-in-inventory
  "Get items an action requires in inventory."
  [action]
  (get-in action [:preconditions :inventory] #{}))

(defn action-deposits-item
  "Get the treasure ID if this is a deposit action."
  [action]
  (:treasure action))

(def underground-rooms
  "Rooms below the one-way trap door."
  #{:cellar :troll-room :east-of-chasm :gallery :studio
    :cyclops-room :treasure-room :maze-1 :maze-2 :maze-3
    :maze-4 :maze-5 :maze-6 :maze-7 :maze-8 :maze-9
    :maze-10 :maze-11 :maze-12 :maze-13 :maze-14 :maze-15})

(defn action-location
  "Get the location where an action must be performed."
  [action]
  (get-in action [:preconditions :here]))

(defn action-is-underground?
  "Check if an action takes place in an underground room."
  [action]
  (contains? underground-rooms (action-location action)))

(defn action-is-surface?
  "Check if an action takes place on the surface (above trap door)."
  [action]
  (let [loc (action-location action)]
    (and loc (not (contains? underground-rooms loc)))))

(defn sort-plan-by-dependencies
  "Topologically sort a plan so:
   1. Actions that provide flags come before actions that require them
   2. Actions that add items come before actions that need them
   3. Take actions come before deposit actions for same item
   4. If plan has underground actions, deposit actions depend on magic-flag provider
   5. Actions at flag-gated locations depend on flag providers
   Uses Kahn's algorithm."
  [plan]
  (if (< (count plan) 2)
    plan
    (let [;; Build maps of what each action provides
          flag-providers (into {} (for [a plan
                                        flag (action-provides a)]
                                    [flag (:id a)]))

          item-providers (into {} (for [a plan
                                        item (action-adds-to-inventory a)]
                                    [item (:id a)]))

          ;; Check if plan visits underground (requires return route)
          has-underground? (some action-is-underground? plan)
          magic-provider (get flag-providers :magic-flag)

          ;; Calculate dependencies for each action
          action-deps
          (into {}
                (for [a plan]
                  [(:id a)
                   (set/union
                    ;; Flag dependencies (explicit preconditions)
                    (set (for [flag (action-requires a)
                               :when (contains? flag-providers flag)]
                           (get flag-providers flag)))
                    ;; Inventory dependencies (need item before requiring it)
                    (set (for [item (action-requires-in-inventory a)
                               :when (contains? item-providers item)]
                           (get item-providers item)))
                    ;; Location dependencies:
                    ;; Actions at flag-gated locations depend on flag providers
                    (let [location (action-location a)
                          required-flags (when location
                                           (optimizer/flags-required-for-room location))]
                      (set (for [flag required-flags
                                 :when (contains? flag-providers flag)]
                             (get flag-providers flag))))
                    ;; Deposit depends on take for same item
                    (when-let [treasure (action-deposits-item a)]
                      (if-let [provider (get item-providers treasure)]
                        #{provider}
                        #{}))
                    ;; Underground return constraint:
                    ;; Deposit actions depend on magic-flag when plan goes underground
                    ;; (must unlock trap door before returning to living room)
                    (when (and has-underground?
                               magic-provider
                               (= :deposit (:type a)))
                      #{magic-provider}))]))

          ;; Kahn's algorithm with strategic ordering
          id->action (into {} (map (juxt :id identity) plan))
          original-order (into {} (map-indexed (fn [i a] [(:id a) i]) plan))

          ;; Categorize actions by type for smarter ordering:
          ;; - "supply" actions (take items) should come first
          ;; - "work" actions (kill, puzzle) come in the middle
          ;; - "deposit" actions come last (after all items collected)
          action-category (fn [action]
                            (case (:type action)
                              :take 0      ; Supply phase - get items first
                              :deposit 2   ; Deposit phase - last
                              1))          ; Work phase - middle

          ;; Scoring function for action priority:
          ;; 1. Category (take -> work -> deposit)
          ;; 2. Within same category, group by surface/underground
          ;;    - Take actions: surface first (get items before going down)
          ;;    - Work actions: underground first (do all underground work before returning)
          ;;    - Deposit actions: surface (they're all at living-room anyway)
          ;; 3. Ties broken by original order
          action-priority (fn [action-id]
                            (let [action (get id->action action-id)
                                  category (action-category action)
                                  is-surface? (action-is-surface? action)
                                  ;; For take: surface before underground
                                  ;; For work: underground before surface (do all underground work together)
                                  ;; For deposit: doesn't matter (all surface)
                                  location-bonus (case category
                                                   0 (if is-surface? 0 100)   ; take: surface first
                                                   1 (if is-surface? 100 0)   ; work: underground first
                                                   2 0)]                       ; deposit: no preference
                              (+ (* 1000 category)
                                 location-bonus
                                 (get original-order action-id 99))))]

      (loop [result []
             remaining (set (map :id plan))
             deps action-deps]
        (if (empty? remaining)
          result
          ;; Find actions with no unsatisfied dependencies
          ;; Prefer surface actions, then by original order
          (let [ready-actions (filter #(empty? (set/intersection (get deps %) remaining))
                                      remaining)
                ;; Sort by priority (surface first, then original order)
                ready (first (sort-by action-priority ready-actions))]
            (if ready
              (recur (conj result (get id->action ready))
                     (disj remaining ready)
                     deps)
              ;; No ready action - cycle detected, return original order
              plan)))))))

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
;; Treasure Categories
;; =============================================================================

(def easy-treasures
  "Treasures that can be collected with just troll-flag.
   These are accessible via trap door + maze without additional puzzles."
  [:bag-of-coins   ; maze-5 - just need to navigate maze
   :jeweled-egg    ; tree/forest - on surface, no flags needed
   :painting])     ; gallery - accessible via cellar

(def medium-treasures
  "Treasures requiring additional flags or puzzle solutions."
  [:ivory-torch    ; torch-room - needs dome-flag (rope tied)
   :emerald        ; reservoir - needs dam-opened
   :platinum-bar   ; loud-room - needs loud-room-solved
   :sceptre        ; temple - needs navigation through maze
   :gold-coffin    ; egyptian room - just navigation
   :crystal-skull  ; hades - needs lld-flag (exorcism)
   :pot-of-gold])  ; end-of-rainbow - needs rainbow-flag

(def hard-treasures
  "Treasures requiring killing the thief."
  [:jade-figurine       ; treasure-room - needs thief-dead
   :sapphire-bracelet   ; treasure-room - needs thief-dead
   :clockwork-canary    ; in egg, opened by thief
   :huge-diamond        ; coal mine shaft - needs thief-dead or timing
   :scarab])            ; coal mine - needs complex navigation

;; =============================================================================
;; Treasure Planning
;; =============================================================================

(defn plan-treasure-list
  "Plan collection of a specific list of treasures.
   Returns {:success? bool :plans [...] :final-state state}"
  [registry initial-state treasure-list]
  (loop [remaining treasure-list
         state initial-state
         plans []]
    (if (empty? remaining)
      {:success? true :plans plans :final-state state}
      (let [treasure (first remaining)
            _ (println "  Planning for" treasure "...")
            plan-result (plan-treasure-collection registry treasure state)]
        (if (:success? plan-result)
          (let [final-state (reduce actions/apply-action-effects
                                    state
                                    (:plan plan-result))]
            (recur (rest remaining)
                   final-state
                   (conj plans {:treasure treasure
                                :plan (:plan plan-result)})))
          ;; Skip this treasure and continue with others
          (do
            (println "  ! Could not plan for" treasure "- skipping")
            (recur (rest remaining)
                   state
                   plans)))))))

;; =============================================================================
;; Complete Speedrun Planning
;; =============================================================================

(defn plan-speedrun
  "Generate a complete speedrun plan.

   Strategy:
   1. Achieve troll-flag (opens underground)
   2. Achieve cyclops-flag (opens strange-passage for return trips)
   3. Collect treasures with return route available

   Returns:
   {:success? bool
    :phases [{:name :phase-name :actions [...]}]
    :total-actions count
    :estimated-moves count}"
  [game-state]
  (let [registry (actions/build-action-registry game-state)
        initial-state (constraints/initial-planning-state game-state)]

    ;; Phase 1: Kill troll
    (println "Planning Phase 1: Kill troll...")
    (let [troll-plan (plan-flag-achievement registry :troll-flag initial-state)]
      (if-not (:success? troll-plan)
        {:success? false :error "Failed to plan troll flag" :details troll-plan}

        (let [state-after-troll (reduce actions/apply-action-effects
                                        initial-state
                                        (:plan troll-plan))
              troll-phase {:name :kill-troll :actions (:plan troll-plan)}]

          ;; Phase 2: Defeat cyclops (opens strange-passage for return trips)
          ;; This is REQUIRED before collecting underground treasures because:
          ;; - Trap door bars after going down (can't return via cellar)
          ;; - Chimney has inventory limit of 2 (can't carry treasures + lamp + sword)
          ;; - Strange passage provides unlimited-inventory return route
          (println "Planning Phase 2: Defeat cyclops (open return route)...")
          (let [cyclops-plan (plan-flag-achievement registry :cyclops-flag state-after-troll)]
            (if-not (:success? cyclops-plan)
              ;; Can't defeat cyclops - return just troll phase
              (do
                (println "  ! Could not plan cyclops defeat - limited treasure collection")
                {:success? true
                 :phases [troll-phase]
                 :total-actions (count (:plan troll-plan))
                 :estimated-moves (* 2 (count (:plan troll-plan)))})

              (let [state-after-cyclops (reduce actions/apply-action-effects
                                                state-after-troll
                                                (:plan cyclops-plan))
                    cyclops-phase {:name :defeat-cyclops :actions (:plan cyclops-plan)}
                    phases [troll-phase cyclops-phase]]

                ;; Phase 3: Collect treasures (with return route available)
                (println "Planning Phase 3: Collect treasures...")
                (let [treasure-result (plan-treasure-list registry state-after-cyclops easy-treasures)]
                  (if (empty? (:plans treasure-result))
                    ;; No treasures planned
                    {:success? true
                     :phases phases
                     :total-actions (+ (count (:plan troll-plan))
                                       (count (:plan cyclops-plan)))
                     :estimated-moves (* 2 (+ (count (:plan troll-plan))
                                              (count (:plan cyclops-plan))))}

                    ;; Success with treasures
                    (let [all-treasure-actions (mapcat :plan (:plans treasure-result))
                          all-phases (conj phases {:name :collect-treasures
                                                   :actions all-treasure-actions
                                                   :details (:plans treasure-result)})
                          total-actions (+ (count (:plan troll-plan))
                                           (count (:plan cyclops-plan))
                                           (count all-treasure-actions))]
                      {:success? true
                       :phases all-phases
                       :total-actions total-actions
                       :estimated-moves (* 2 total-actions)
                       :treasures-planned (map :treasure (:plans treasure-result))})))))))))))

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
