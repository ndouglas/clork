(ns clork.planner2.core
  "Core reactive planner for Clork speedruns.

   The planner maintains a goal stack and executes actions one at a time,
   observing the game state after each action to decide what to do next.

   Key concepts:
   - Goal Stack: LIFO stack of goals to achieve
   - Reactive Loop: Observe -> Select Action -> Execute -> Repeat
   - State Verification: After each action, verify we're in expected state
   - Recovery: Push recovery goals when things go wrong"
  (:require [clork.planner2.observe :as obs]
            [clork.planner2.goals :as goals]
            [clork.planner2.navigate :as nav]
            [clork.ml :as ml]))

;;; ---------------------------------------------------------------------------
;;; PLANNER STATE
;;; ---------------------------------------------------------------------------

(defn make-planner
  "Create a new planner state.

   Options:
   - :trace? - Enable execution tracing (default true)
   - :max-turns - Maximum turns before giving up (default 1000)"
  [game-state goal & {:keys [trace? max-turns]
                      :or {trace? true max-turns 1000}}]
  {:game-state game-state
   :goal-stack [goal]
   :trace []
   :turn 0
   :max-turns max-turns
   :trace? trace?
   :status :running
   :error nil})

;;; ---------------------------------------------------------------------------
;;; ACTION SELECTION
;;; ---------------------------------------------------------------------------

(defmulti select-action-for-goal
  "Select an action that makes progress toward a goal.

   Returns one of:
   - {:action <action-map>} - Execute this action
   - {:decompose [<sub-goals>]} - Push sub-goals onto stack
   - {:satisfied true} - Goal already achieved
   - {:stuck <reason>} - Cannot make progress"
  (fn [_game-state goal] (:type goal)))

;; at-room: Use navigation
(defmethod select-action-for-goal :at-room
  [game-state {:keys [room]}]
  (cond
    ;; Already there
    (obs/at-room? game-state room)
    {:satisfied true}

    ;; Try to navigate
    :else
    (if-let [nav-plan (nav/plan-navigation game-state room)]
      ;; Have a path - check requirements
      (let [current (obs/current-room game-state)
            path (:path nav-plan)
            next-room (when (> (count path) 1) (second path))
            direction (first (:directions nav-plan))
            ;; Only check if the NEXT room is dark, not the whole path
            ;; We'll handle later dark rooms when we get there
            next-room-dark? (and next-room (nav/dark-room? next-room))
            have-light? (obs/lantern-on? game-state)
            ;; Special case: if we're going to get the lantern, don't require light
            ;; (the lantern is the light source we need!)
            lantern-room (obs/find-object-room game-state :brass-lantern)
            going-for-lantern? (= room lantern-room)]
        (cond
          ;; Need light for next room but don't have it
          ;; Exception: if we're going to get the lantern itself
          (and next-room-dark? (not have-light?) (not going-for-lantern?))
          {:decompose [(goals/lantern-on)]}

          ;; Need pre-entry actions (open door, move rug)
          (nav/pre-entry-actions game-state current next-room)
          {:action (first (nav/pre-entry-actions game-state current next-room))}

          ;; Can move directly
          :else
          {:action {:verb :go :direction direction}}))
      {:stuck "No path to destination"})))

;; have-item: Take if visible, otherwise decompose
(defmethod select-action-for-goal :have-item
  [game-state {:keys [item]}]
  (let [;; Check if item is inside a container
        item-loc (obs/object-location game-state item)
        ;; item-loc is a container if it's not a room and not the player
        is-container? (and item-loc
                           (not= item-loc :adventurer)
                           (not (contains? (:rooms game-state) item-loc)))
        container-visible? (and is-container?
                                (obs/object-visible? game-state item-loc))
        container-open? (when container-visible?
                          (obs/object-open? game-state item-loc))]
    (cond
      ;; Already have it
      (obs/has-item? game-state item)
      {:satisfied true}

      ;; Item is visible - take it
      (obs/object-visible? game-state item)
      (if (obs/can-take? game-state item)
        {:action {:verb :take :direct-object item}}
        {:stuck (str "Cannot take " item)})

      ;; Item is in a visible but closed container - open it first
      (and container-visible? (not container-open?))
      {:action {:verb :open :direct-object item-loc}}

      ;; Item is in an open visible container - take it from there
      (and container-visible? container-open?)
      {:action {:verb :take :direct-object item}}

      ;; Need to go to item location
      :else
      (if-let [item-room (obs/find-object-room game-state item)]
        ;; Check if we're already at the room - if so, might need to open container
        (if (obs/at-room? game-state item-room)
          ;; We're here but can't see item - might be in a closed container we can't see
          ;; or item might not exist
          {:stuck (str "Cannot find " item " at " item-room)}
          {:decompose [(goals/at-room item-room)]})
        {:stuck (str "Cannot find " item)}))))

;; item-deposited: Multi-step process
(defmethod select-action-for-goal :item-deposited
  [game-state {:keys [item]}]
  (cond
    ;; Already deposited
    (obs/item-deposited? game-state item)
    {:satisfied true}

    ;; Don't have item
    (not (obs/has-item? game-state item))
    {:decompose [(goals/have-item item)]}

    ;; Not at living room
    (not (obs/at-room? game-state :living-room))
    {:decompose [(goals/at-room :living-room)]}

    ;; Trophy case not open
    (not (obs/object-open? game-state :trophy-case))
    {:action {:verb :open :direct-object :trophy-case}}

    ;; Ready to deposit
    :else
    {:action {:verb :put :direct-object item :prep :in :indirect-object :trophy-case}}))

;; kill-enemy: Fight until dead
(defmethod select-action-for-goal :kill-enemy
  [game-state {:keys [enemy]}]
  (let [enemy-room (case enemy
                     :troll :troll-room
                     :cyclops :cyclops-room
                     nil)
        have-weapon? (or (obs/has-item? game-state :sword)
                         (obs/has-item? game-state :nasty-knife))
        weapon (cond
                 (obs/has-item? game-state :nasty-knife) :nasty-knife
                 (obs/has-item? game-state :sword) :sword
                 :else :sword)]
    (cond
      ;; Enemy dead
      (goals/goal-satisfied? game-state {:type :kill-enemy :enemy enemy})
      {:satisfied true}

      ;; Need weapon
      (not have-weapon?)
      {:decompose [(goals/have-item :sword)]}

      ;; Need light
      (and (not (obs/lit? game-state))
           (not (obs/lantern-on? game-state)))
      {:decompose [(goals/lantern-on)]}

      ;; Need to be at enemy room (for stationary enemies)
      (and enemy-room (not (obs/at-room? game-state enemy-room)))
      {:decompose [(goals/at-room enemy-room)]}

      ;; Cyclops is special - say "ulysses" instead of fighting
      (= enemy :cyclops)
      {:action {:verb :say :direct-object :ulysses}}

      ;; Ready to attack - must specify the weapon!
      :else
      {:action {:verb :attack :direct-object enemy :prep :with :indirect-object weapon}})))

;; container-open: Open it
(defmethod select-action-for-goal :container-open
  [game-state {:keys [container]}]
  (cond
    ;; Already open
    (obs/object-open? game-state container)
    {:satisfied true}

    ;; Container visible - open it
    (or (obs/object-visible? game-state container)
        (obs/has-item? game-state container))
    {:action {:verb :open :direct-object container}}

    ;; Need to find container
    :else
    (if-let [container-room (obs/find-object-room game-state container)]
      {:decompose [(goals/at-room container-room)]}
      {:stuck (str "Cannot find " container)})))

;; lantern-on: Turn it on
(defmethod select-action-for-goal :lantern-on
  [game-state _goal]
  (cond
    ;; Already on
    (obs/lantern-on? game-state)
    {:satisfied true}

    ;; Don't have lantern
    (not (obs/has-item? game-state :brass-lantern))
    {:decompose [(goals/have-item :brass-lantern)]}

    ;; Turn it on
    :else
    {:action {:verb :lamp-on :direct-object :brass-lantern}}))

;; flag-set: Generic flag achievement (puzzle-specific)
(defmethod select-action-for-goal :flag-set
  [game-state {:keys [flag]}]
  (if (obs/flag-set? game-state flag)
    {:satisfied true}
    {:stuck (str "Don't know how to achieve flag: " flag)}))

;; all-treasures-deposited: Find next treasure to deposit
(defmethod select-action-for-goal :all-treasures-deposited
  [game-state _goal]
  (let [undeposited (remove #(obs/item-deposited? game-state %) goals/treasures)]
    (if (empty? undeposited)
      {:satisfied true}
      ;; Pick first undeposited treasure
      {:decompose [(goals/item-deposited (first undeposited))]})))

;; win: Ultimate goal
(defmethod select-action-for-goal :win
  [game-state _goal]
  (cond
    ;; Already won
    (obs/game-finished? game-state)
    {:satisfied true}

    ;; Not all treasures deposited
    (not (goals/goal-satisfied? game-state (goals/all-treasures-deposited)))
    {:decompose [(goals/all-treasures-deposited)]}

    ;; Need to go to barrow
    (not (obs/at-room? game-state :stone-barrow))
    {:decompose [(goals/at-room :stone-barrow)]}

    ;; Enter barrow (this should trigger the win)
    :else
    {:action {:verb :go :direction :south}}))

(defmethod select-action-for-goal :default
  [_game-state goal]
  {:stuck (str "Unknown goal type: " (:type goal))})

;;; ---------------------------------------------------------------------------
;;; EXECUTION TRACE
;;; ---------------------------------------------------------------------------

(defn trace-entry
  "Create a trace entry for an action."
  [game-state goal action result message]
  {:turn (:moves game-state)
   :room (obs/current-room game-state)
   :goal goal
   :action action
   :message message
   :inventory (obs/inventory game-state)
   :score (obs/score game-state)})

;;; ---------------------------------------------------------------------------
;;; REACTIVE EXECUTION LOOP
;;; ---------------------------------------------------------------------------

(defn step
  "Execute one step of the planner.

   Returns updated planner state with:
   - :game-state updated after action
   - :goal-stack modified as needed
   - :trace with new entry
   - :status :running, :complete, :stuck, or :error"
  [{:keys [game-state goal-stack trace turn max-turns trace?] :as planner}]
  (cond
    ;; Exceeded max turns
    (>= turn max-turns)
    (assoc planner
           :status :timeout
           :error (str "Exceeded " max-turns " turns"))

    ;; No goals remaining
    (empty? goal-stack)
    (assoc planner :status :complete)

    ;; Player dead
    (not (obs/player-alive? game-state))
    (assoc planner
           :status :dead
           :error "Player died")

    ;; Game over
    (obs/game-over? game-state)
    (if (obs/game-finished? game-state)
      (assoc planner :status :complete)
      (assoc planner
             :status :game-over
             :error "Game ended without winning"))

    :else
    (let [current-goal (peek goal-stack)
          selection (select-action-for-goal game-state current-goal)]
      (cond
        ;; Goal satisfied - pop and continue
        (:satisfied selection)
        (let [new-stack (pop goal-stack)]
          (-> planner
              (assoc :goal-stack new-stack)
              (update :trace conj {:type :goal-satisfied :goal current-goal})))

        ;; Need to decompose into sub-goals
        (:decompose selection)
        (let [sub-goals (:decompose selection)
              ;; Check for cycles - if any sub-goal is already in the stack, we're stuck
              existing-goal-set (set goal-stack)
              has-cycle? (some #(contains? existing-goal-set %) sub-goals)]
          (if has-cycle?
            ;; Cycle detected - this means we have a dependency issue
            ;; (e.g., need lantern to go to room where lantern is)
            ;; For now, skip the problematic prerequisite and try direct action
            (let [;; Remove sub-goals that cause cycles
                  safe-sub-goals (remove #(contains? existing-goal-set %) sub-goals)]
              (if (empty? safe-sub-goals)
                ;; All sub-goals cause cycles - try to proceed anyway
                (-> planner
                    (assoc :goal-stack (pop goal-stack))
                    (update :trace conj {:type :cycle-skip
                                         :goal current-goal
                                         :sub-goals sub-goals}))
                ;; Some safe sub-goals remain
                (let [new-stack (into (pop goal-stack)
                                      (cons current-goal (reverse safe-sub-goals)))]
                  (-> planner
                      (assoc :goal-stack new-stack)
                      (update :trace conj {:type :decompose-partial
                                           :goal current-goal
                                           :sub-goals safe-sub-goals
                                           :skipped (filter #(contains? existing-goal-set %) sub-goals)})))))
            ;; No cycles - normal decomposition
            (let [new-stack (into (pop goal-stack)
                                  (cons current-goal (reverse sub-goals)))]
              (-> planner
                  (assoc :goal-stack new-stack)
                  (update :trace conj {:type :decompose
                                       :goal current-goal
                                       :sub-goals sub-goals})))))

        ;; Got stuck
        (:stuck selection)
        (assoc planner
               :status :stuck
               :error (:stuck selection)
               :stuck-goal current-goal)

        ;; Have an action to execute
        (:action selection)
        (let [action (:action selection)
              result (ml/execute-action game-state action)
              new-game-state (:game-state result)
              message (:message result)
              entry (when trace?
                      (trace-entry game-state current-goal action result message))]
          (-> planner
              (assoc :game-state new-game-state)
              (update :turn inc)
              (cond-> trace? (update :trace conj entry))))

        ;; Unexpected result
        :else
        (assoc planner
               :status :error
               :error (str "Unexpected selection result: " selection))))))

(defn run
  "Run the planner until completion, stuck, or max turns.

   Returns final planner state."
  [planner]
  (loop [p planner]
    (if (= (:status p) :running)
      (recur (step p))
      p)))

;;; ---------------------------------------------------------------------------
;;; CONVENIENCE FUNCTIONS
;;; ---------------------------------------------------------------------------

(defn run-goal
  "Run a goal from the current game state.

   Returns {:status :complete/:stuck/:error
            :game-state final-state
            :moves count
            :trace [...]}

   Options:
   - :trace? - Enable tracing (default true)
   - :max-turns - Max turns (default 1000)
   - :verbose? - Print each step (default false)"
  [game-state goal & {:keys [trace? max-turns verbose?]
                      :or {trace? true max-turns 1000 verbose? false}}]
  (let [planner (make-planner game-state goal :trace? trace? :max-turns max-turns)]
    (if verbose?
      (loop [p planner]
        (when (= (:status p) :running)
          (let [current-goal (peek (:goal-stack p))]
            (println (str "Turn " (:turn p) ": " (goals/goal->string current-goal)))))
        (if (= (:status p) :running)
          (recur (step p))
          (do
            (println (str "\nFinal status: " (:status p)))
            (when (:error p)
              (println (str "Error: " (:error p))))
            p)))
      (run planner))))

(defn speedrun
  "Attempt a full speedrun from initial state.

   Options:
   - :trace? - Enable tracing (default true)
   - :verbose? - Print each step (default false)"
  [game-state & {:keys [trace? verbose?]
                 :or {trace? true verbose? false}}]
  (run-goal game-state (goals/win) :trace? trace? :verbose? verbose?))

;;; ---------------------------------------------------------------------------
;;; DEBUGGING
;;; ---------------------------------------------------------------------------

(defn explain-status
  "Explain the current planner status."
  [{:keys [status goal-stack error stuck-goal game-state turn]}]
  (println "\n=== Planner Status ===")
  (println "  Status:" status)
  (println "  Turn:" turn)
  (println "  Room:" (obs/current-room game-state))
  (println "  Score:" (obs/score game-state))
  (when error
    (println "  Error:" error))
  (when stuck-goal
    (println "  Stuck on:" (goals/goal->string stuck-goal)))
  (when (seq goal-stack)
    (println "\n  Goal Stack:")
    (doseq [[i g] (map-indexed vector (reverse goal-stack))]
      (println (str "    " i ". " (goals/goal->string g))))))

(defn print-trace
  "Print the execution trace."
  [{:keys [trace]}]
  (println "\n=== Execution Trace ===")
  (doseq [entry trace]
    (case (:type entry)
      :goal-satisfied
      (println (str "  [DONE] " (goals/goal->string (:goal entry))))

      :decompose
      (println (str "  [DECOMPOSE] " (goals/goal->string (:goal entry))
                    " -> " (mapv goals/goal->string (:sub-goals entry))))

      ;; Action trace
      (println (str "  [T" (:turn entry) "] "
                    (obs/current-room (:game-state entry))
                    " | " (:action entry)
                    " | " (subs (or (:message entry) "") 0 (min 50 (count (or (:message entry) "")))))))))

(defn step-runner
  "Create an interactive step runner for debugging.

   Usage:
   (def r (step-runner game-state (goals/have-item :egg)))
   (step! r)  ; Execute one step
   (status r) ; Show current status"
  [game-state goal]
  (atom (make-planner game-state goal)))

(defn step!
  "Execute one step of a step-runner."
  [runner-atom]
  (swap! runner-atom step)
  (explain-status @runner-atom))

(defn status
  "Show status of a step-runner."
  [runner-atom]
  (explain-status @runner-atom))
