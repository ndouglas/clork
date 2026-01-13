(ns clork.intel.goals
  "Stage 3: Goal Regression / Causal Reasoning

   Provides backward chaining over the affordance registry to answer
   'why can't I achieve X?' with actionable explanations.

   Key functions:
   - check-goal: Is a goal currently satisfied?
   - explain-goal: Why isn't this goal achievable? Returns causal tree.
   - find-root-causes: What are the deepest unmet preconditions?
   - suggest-actions: What actions would help achieve this goal?"
  (:require [clork.game-state :as gs]
            [clork.intel.affordances :as aff]
            [clork.intel.routing :as routing]))

;;; ---------------------------------------------------------------------------
;;; GOAL CHECKING
;;; ---------------------------------------------------------------------------
;;; Goals are conditions we want to achieve. They mirror precondition types.

(defmulti check-goal
  "Check if a goal is currently satisfied in the game state.
   Returns {:satisfied true/false :goal goal :details ...}"
  (fn [_game-state goal] (:type goal)))

(defmethod check-goal :game-flag
  [game-state {:keys [flag] :as goal}]
  ;; Check both stored game flags AND virtual flags (like :boat-ready, :trap-door-open)
  (let [stored-flag (gs/game-flag? game-state flag)
        virtual-flags (routing/extract-available-flags game-state)
        satisfied (or stored-flag (contains? virtual-flags flag))]
    {:satisfied satisfied
     :goal goal
     :details (if satisfied
                (str "Flag " (name flag) " is set")
                (str "Flag " (name flag) " is NOT set"))}))

(defmethod check-goal :game-not-flag
  [game-state {:keys [flag] :as goal}]
  ;; Check both stored game flags AND virtual flags
  (let [stored-flag (gs/game-flag? game-state flag)
        virtual-flags (routing/extract-available-flags game-state)
        satisfied (not (or stored-flag (contains? virtual-flags flag)))]
    {:satisfied satisfied
     :goal goal
     :details (if satisfied
                (str "Flag " (name flag) " is not set (as required)")
                (str "Flag " (name flag) " is set (but shouldn't be)"))}))

(defmethod check-goal :object-flag
  [game-state {:keys [object flag] :as goal}]
  (let [satisfied (gs/set-thing-flag? game-state object flag)]
    {:satisfied satisfied
     :goal goal
     :details (if satisfied
                (str "Object " (name object) " has flag " (name flag))
                (str "Object " (name object) " lacks flag " (name flag)))}))

(defmethod check-goal :object-not-flag
  [game-state {:keys [object flag] :as goal}]
  (let [satisfied (not (gs/set-thing-flag? game-state object flag))]
    {:satisfied satisfied
     :goal goal
     :details (if satisfied
                (str "Object " (name object) " lacks flag " (name flag) " (as required)")
                (str "Object " (name object) " has flag " (name flag) " (but shouldn't)"))}))

(defmethod check-goal :object-at
  [game-state {:keys [object location] :as goal}]
  (let [actual-loc (gs/get-thing-location game-state object)
        satisfied (= actual-loc location)]
    {:satisfied satisfied
     :goal goal
     :details (if satisfied
                (str "Object " (name object) " is at " (name location))
                (str "Object " (name object) " is at " (name (or actual-loc :nowhere))
                     " (need: " (name location) ")"))}))

(defmethod check-goal :object-held
  [game-state {:keys [object] :as goal}]
  (let [inventory (set (gs/get-contents game-state :adventurer))
        satisfied (contains? inventory object)]
    {:satisfied satisfied
     :goal goal
     :details (if satisfied
                (str "Holding " (name object))
                (str "Not holding " (name object)))}))

(defmethod check-goal :object-visible
  [game-state {:keys [object] :as goal}]
  (let [here (:here game-state)
        inventory (set (gs/get-contents game-state :adventurer))
        room-contents (set (gs/get-contents game-state here))
        satisfied (or (contains? inventory object)
                      (contains? room-contents object))]
    {:satisfied satisfied
     :goal goal
     :details (if satisfied
                (str "Object " (name object) " is visible")
                (str "Object " (name object) " is not visible"))}))

(defmethod check-goal :at-location
  [game-state {:keys [room] :as goal}]
  (let [here (:here game-state)
        satisfied (= here room)]
    {:satisfied satisfied
     :goal goal
     :details (if satisfied
                (str "At " (name room))
                (str "At " (name here) " (need: " (name room) ")"))}))

(defmethod check-goal :player-at
  [game-state {:keys [room] :as goal}]
  ;; Alias for :at-location
  (check-goal game-state (assoc goal :type :at-location)))

(defmethod check-goal :at-any-location
  [game-state {:keys [rooms] :as goal}]
  (let [here (:here game-state)
        satisfied (contains? (set rooms) here)]
    {:satisfied satisfied
     :goal goal
     :details (if satisfied
                (str "At " (name here) " (one of valid rooms)")
                (str "At " (name here) " (need one of: " (clojure.string/join ", " (map name rooms)) ")"))}))

(defmethod check-goal :player-in-vehicle
  [game-state {:keys [vehicle] :as goal}]
  (let [winner (:winner game-state)
        winner-loc (gs/get-thing-location game-state winner)
        satisfied (= winner-loc vehicle)]
    {:satisfied satisfied
     :goal goal
     :details (if satisfied
                (str "In vehicle " (name vehicle))
                (str "Not in vehicle " (name vehicle) " (at " (name (or winner-loc :unknown)) ")"))}))

(defmethod check-goal :player-not-in-vehicle
  [game-state goal]
  (let [winner (:winner game-state)
        winner-loc (gs/get-thing-location game-state winner)
        here (:here game-state)
        satisfied (= winner-loc here)]
    {:satisfied satisfied
     :goal goal
     :details (if satisfied
                "Not in any vehicle"
                (str "Currently in vehicle: " (name (or winner-loc :unknown))))}))

(defmethod check-goal :boat-ready
  [game-state goal]
  ;; :boat-ready is a virtual flag that's true when:
  ;; 1. Player is in the inflated boat (:inflated-boat or its synonym :magic-boat)
  ;; 2. The boat can be launched
  (let [winner (:winner game-state)
        winner-loc (gs/get-thing-location game-state winner)
        in-boat (#{:inflated-boat :magic-boat} winner-loc)
        satisfied in-boat]
    {:satisfied satisfied
     :goal goal
     :details (if satisfied
                "In inflated boat, ready to launch"
                (if winner-loc
                  (str "Currently at " (name winner-loc) ", not in boat")
                  "Not in boat - need to inflate and board"))}))

(defmethod check-goal :treasures-collected
  [game-state {:keys [treasures] :as goal}]
  ;; Check if all specified treasures are either held or in trophy case
  (let [inventory (set (gs/get-contents game-state :adventurer))
        in-case (set (gs/get-contents game-state :trophy-case))
        collected (clojure.set/union inventory in-case)
        required (set treasures)
        have (clojure.set/intersection required collected)
        missing (clojure.set/difference required have)
        satisfied (empty? missing)]
    {:satisfied satisfied
     :goal goal
     :details (if satisfied
                (str "All " (count treasures) " treasures collected")
                (str "Missing " (count missing) " treasures: "
                     (clojure.string/join ", " (map name missing))))
     :collected have
     :missing missing}))

(defmethod check-goal :score-at-least
  [game-state {:keys [score] :as goal}]
  (let [current-score (or (:score game-state) 0)
        satisfied (>= current-score score)]
    {:satisfied satisfied
     :goal goal
     :details (if satisfied
                (str "Score " current-score " >= " score)
                (str "Score " current-score " < " score " (need " (- score current-score) " more)"))
     :current-score current-score
     :needed (- score current-score)}))

(defmethod check-goal :light-available
  [game-state goal]
  ;; Check if player has access to a working light source
  (let [inventory (set (gs/get-contents game-state :adventurer))
        ;; Check brass lantern
        has-lantern (contains? inventory :brass-lantern)
        lantern-on (and has-lantern (gs/set-thing-flag? game-state :brass-lantern :on))
        ;; Check torch
        has-torch (contains? inventory :ivory-torch)
        torch-lit (and has-torch (gs/set-thing-flag? game-state :ivory-torch :on))
        ;; Check candles
        has-candles (contains? inventory :candles)
        candles-lit (and has-candles (gs/set-thing-flag? game-state :candles :on))
        ;; Any working light?
        satisfied (or lantern-on torch-lit candles-lit)]
    {:satisfied satisfied
     :goal goal
     :details (cond
                lantern-on "Brass lantern is on"
                torch-lit "Torch is lit"
                candles-lit "Candles are lit"
                has-lantern "Have lantern but it's off"
                has-torch "Have torch but it's not lit"
                has-candles "Have candles but they're not lit"
                :else "No light source available")
     :light-sources {:lantern {:held has-lantern :on lantern-on}
                     :torch {:held has-torch :on torch-lit}
                     :candles {:held has-candles :on candles-lit}}}))

(defmethod check-goal :inventory-has-room
  [game-state goal]
  ;; Check if inventory has room for more items
  (let [inventory (gs/get-contents game-state :adventurer)
        count-held (count inventory)
        max-items 7  ; Typical Zork inventory limit
        satisfied (< count-held max-items)]
    {:satisfied satisfied
     :goal goal
     :details (if satisfied
                (str "Inventory has room (" count-held "/" max-items ")")
                (str "Inventory full (" count-held "/" max-items ")"))
     :items-held count-held
     :max-items max-items}))

(defmethod check-goal :lamp-has-power
  [game-state goal]
  ;; Check if the brass lantern has fuel remaining
  ;; Lamp has 4 stages (indices 0-3), burns out at stage 3 (ticks=0)
  ;; Total fuel: 100+70+15 = 185 turns
  (let [inventory (set (gs/get-contents game-state :adventurer))
        has-lantern (contains? inventory :brass-lantern)
        burned-out (gs/set-thing-flag? game-state :brass-lantern :burned-out)
        ;; Stage index: 0=full, 1=dim, 2=dimmer, 3=nearly out/out
        stage-idx (get game-state :lamp-stage-index 0)
        ;; Calculate approximate remaining turns
        remaining-turns (case stage-idx
                          0 185  ; Full power
                          1 85   ; 70+15
                          2 15   ; Nearly out
                          3 0    ; Dead
                          0)     ; Default
        satisfied (and has-lantern (not burned-out) (pos? remaining-turns))]
    {:satisfied satisfied
     :goal goal
     :details (cond
                (not has-lantern) "Don't have the brass lantern"
                burned-out "Lamp has burned out"
                (zero? remaining-turns) "Lamp has no fuel left"
                (<= remaining-turns 15) (str "Lamp nearly out (~" remaining-turns " turns)")
                :else (str "Lamp has fuel (~" remaining-turns " turns)"))
     :has-lantern has-lantern
     :burned-out burned-out
     :stage-index stage-idx
     :remaining-turns remaining-turns}))

(defmethod check-goal :object-accessible
  [game-state {:keys [object] :as goal}]
  ;; Object is accessible if held, in room, or in open container in room
  (let [here (:here game-state)
        inventory (set (gs/get-contents game-state :adventurer))
        room-contents (set (gs/get-contents game-state here))
        held (contains? inventory object)
        in-room (contains? room-contents object)
        ;; Check if in open container in room
        obj-loc (gs/get-thing-loc-id game-state object)
        in-container (and obj-loc
                          (contains? room-contents obj-loc)
                          (gs/set-thing-flag? game-state obj-loc :open))
        satisfied (or held in-room in-container)]
    {:satisfied satisfied
     :goal goal
     :details (cond
                held (str (name object) " is held")
                in-room (str (name object) " is in room")
                in-container (str (name object) " is in open container " (name obj-loc))
                :else (str (name object) " is not accessible (at " (name (or obj-loc :unknown)) ")"))
     :location obj-loc}))

(defmethod check-goal :default
  [_game-state goal]
  {:satisfied false
   :goal goal
   :details (str "Unknown goal type: " (:type goal))})

;;; ---------------------------------------------------------------------------
;;; PRECONDITION TO GOAL CONVERSION
;;; ---------------------------------------------------------------------------
;;; Convert affordance preconditions to goals for recursive analysis.

(defn precond->goal
  "Convert a precondition to a goal for recursive checking."
  [precond]
  (case (:type precond)
    :object-visible {:type :object-visible :object (:object precond)}
    :object-held {:type :object-held :object (:object precond)}
    :object-accessible {:type :object-accessible :object (:object precond)}
    :object-in-room {:type :object-at :object (:object precond) :location :here}
    :object-flag {:type :object-flag :object (:object precond) :flag (:flag precond)}
    :object-not-flag {:type :object-not-flag :object (:object precond) :flag (:flag precond)}
    :game-flag {:type :game-flag :flag (:flag precond)}
    :game-not-flag {:type :game-not-flag :flag (:flag precond)}
    :at-location {:type :at-location :room (:room precond)}
    :at-any-location {:type :at-any-location :rooms (:rooms precond)}
    :exit-exists nil  ; Can't easily convert to goal
    :inventory-capacity {:type :inventory-has-room}
    :object-in-container {:type :object-at :object (:object precond) :location (:container precond)}
    :in-vehicle {:type :player-in-vehicle :vehicle (:vehicle precond)}
    :not-in-vehicle {:type :player-not-in-vehicle}
    :boat-ready {:type :boat-ready}
    :light-available {:type :light-available}
    :lamp-has-power {:type :lamp-has-power}
    :treasures-collected {:type :treasures-collected :treasures (:treasures precond)}
    :score-at-least {:type :score-at-least :score (:score precond)}
    nil))

;;; ---------------------------------------------------------------------------
;;; AFFORDANCE ANALYSIS
;;; ---------------------------------------------------------------------------
;;; Analyze what affordances can achieve goals and what blocks them.

(defn analyze-affordance-preconditions
  "Analyze preconditions of an affordance, checking each one.
   Returns list of {:precond ... :status :ok/:blocked :goal-check ...}"
  [game-state affordance]
  (for [precond (:preconds affordance)]
    (let [goal (precond->goal precond)
          check (when goal (check-goal game-state goal))]
      {:precond precond
       :status (cond
                 (nil? goal) :unknown
                 (:satisfied check) :ok
                 :else :blocked)
       :goal-check check})))

(defn find-blocking-preconditions
  "Find which preconditions are blocking an affordance."
  [game-state affordance]
  (->> (analyze-affordance-preconditions game-state affordance)
       (filter #(= :blocked (:status %)))
       (map :precond)))

;;; ---------------------------------------------------------------------------
;;; GOAL EXPLANATION (CAUSAL REASONING)
;;; ---------------------------------------------------------------------------

(declare explain-goal)

(defn- explain-achiever
  "Explain how an achiever (affordance) could achieve a goal, and what blocks it."
  [game-state achiever depth max-depth]
  (let [{:keys [affordance-id affordance via-effect]} achiever
        preconds-analysis (analyze-affordance-preconditions game-state affordance)
        blocked (filter #(= :blocked (:status %)) preconds-analysis)
        all-ok (empty? blocked)]
    {:affordance-id affordance-id
     :verb (:verb affordance)
     :description (:desc affordance)
     :via-effect via-effect
     :preconditions preconds-analysis
     :status (if all-ok :ready :blocked)
     :blocking (when (and (not all-ok) (< depth max-depth))
                 ;; Recursively explain blocking preconditions
                 (for [{:keys [precond]} blocked
                       :let [goal (precond->goal precond)]
                       :when goal]
                   (explain-goal game-state goal (inc depth) max-depth)))}))

(defn explain-goal
  "Explain why a goal isn't achievable. Returns causal tree.

   Options:
   - depth: Current recursion depth (default 0)
   - max-depth: Maximum recursion depth (default 3)

   Returns:
   {:status :satisfied/:blocked/:no-achievers
    :goal goal
    :details explanation
    :achievable-via [{:affordance-id ... :preconditions [...] :blocking [...]}]
    :root-blockers [deepest unmet goals]}"
  ([game-state goal]
   (explain-goal game-state goal 0 3))
  ([game-state goal depth max-depth]
   (let [current (check-goal game-state goal)]
     (if (:satisfied current)
       {:status :satisfied
        :goal goal
        :details (:details current)}
       ;; Goal not satisfied - find what could achieve it
       (let [achievers (aff/achievers-of goal)]
         (if (empty? achievers)
           {:status :no-achievers
            :goal goal
            :details (str "No known affordance achieves: " (pr-str goal))}
           ;; Analyze each achiever
           (let [analysis (mapv #(explain-achiever game-state % depth max-depth) achievers)
                 ready (filter #(= :ready (:status %)) analysis)
                 blocked (filter #(= :blocked (:status %)) analysis)]
             {:status (if (seq ready) :achievable :blocked)
              :goal goal
              :details (:details current)
              :achievable-via analysis
              :ready-achievers (when (seq ready) (mapv :affordance-id ready))})))))))

;;; ---------------------------------------------------------------------------
;;; ROOT CAUSE ANALYSIS
;;; ---------------------------------------------------------------------------

(defn- collect-leaf-blockers
  "Recursively collect leaf-level blockers (goals with no further achievers or at max depth)."
  [explanation]
  (cond
    (= :satisfied (:status explanation))
    []

    (= :no-achievers (:status explanation))
    [(:goal explanation)]

    :else
    (let [achievers (:achievable-via explanation)]
      (if (empty? achievers)
        [(:goal explanation)]
        ;; Collect from all blocked achievers' blocking explanations
        (mapcat (fn [achiever]
                  (if (empty? (:blocking achiever))
                    ;; This achiever has blocked preconditions but we didn't recurse
                    ;; Include both :blocked and :unknown status (can't determine = blocked)
                    (for [{:keys [precond status]} (:preconditions achiever)
                          :when (#{:blocked :unknown} status)
                          :let [goal (precond->goal precond)]
                          :when goal]
                      goal)
                    ;; Recursively collect from sub-explanations
                    (mapcat collect-leaf-blockers (:blocking achiever))))
                achievers)))))

(defn find-root-causes
  "Find the deepest unmet preconditions (root causes) for a goal.

   These are goals that either:
   1. Have no known achievers
   2. Are at the maximum analysis depth

   Returns list of {:goal ... :reason ...}"
  [game-state goal]
  (let [explanation (explain-goal game-state goal 0 5)
        leaf-blockers (collect-leaf-blockers explanation)]
    (distinct
      (for [blocker leaf-blockers]
        {:goal blocker
         :reason (str "No known way to achieve: " (pr-str blocker))}))))

;;; ---------------------------------------------------------------------------
;;; SUGGESTION GENERATION
;;; ---------------------------------------------------------------------------

(defn suggest-actions
  "Suggest actions that would help achieve a goal.

   Returns list of {:action ... :reason ... :priority ...}"
  [game-state goal]
  (let [explanation (explain-goal game-state goal)
        achievers (:achievable-via explanation)]
    (when (seq achievers)
      (->> achievers
           (mapcat (fn [achiever]
                     (let [{:keys [affordance-id verb preconditions]} achiever
                           blocked-preconds (filter #(= :blocked (:status %)) preconditions)]
                       (concat
                         ;; If this achiever is ready, suggest it directly
                         (when (empty? blocked-preconds)
                           [{:action {:verb verb :affordance affordance-id}
                             :reason (str "Directly achieves goal via " (name affordance-id))
                             :priority 1}])
                         ;; Otherwise suggest actions for blocked preconditions
                         (for [{:keys [precond]} blocked-preconds
                               :let [sub-goal (precond->goal precond)]
                               :when sub-goal
                               :let [sub-suggestions (suggest-actions game-state sub-goal)]
                               suggestion sub-suggestions]
                           (update suggestion :priority inc))))))
           (sort-by :priority)
           (take 5)))))

;;; ---------------------------------------------------------------------------
;;; CONVENIENCE FUNCTIONS
;;; ---------------------------------------------------------------------------

(defn why-not-flag?
  "Explain why a game flag is not set."
  [game-state flag]
  (explain-goal game-state {:type :game-flag :flag flag}))

(defn analyze-location-reachability
  "Analyze why a location is or isn't reachable using the routing engine.

   Returns a map with:
   - :reachable? - boolean
   - :distance - number of moves (if reachable)
   - :path - sequence of rooms (if reachable)
   - :missing-flags - set of flags that might help (if unreachable)
   - :suggestion - human-readable suggestion"
  [game-state target-room]
  (let [;; Lazy load routing to avoid circular dependency
        routing-ns (requiring-resolve 'clork.intel.routing/shortest-path)
        extract-flags (requiring-resolve 'clork.intel.routing/extract-available-flags)
        rooms-requiring (requiring-resolve 'clork.intel.routing/rooms-requiring-flag)
        here (:here game-state)
        current-flags (extract-flags game-state)

        ;; Try to find path with current flags
        path-result (routing-ns game-state here target-room
                                :available-flags current-flags)]
    (if path-result
      {:reachable? true
       :distance (:distance path-result)
       :path (:path path-result)
       :suggestion (str "Reachable in " (:distance path-result) " moves")}
      ;; Room not reachable - find what flags might help
      (let [candidate-flags [:troll-flag :lld-flag :cyclops-flag :magic-flag
                             :rainbow-flag :dome-flag :low-tide :coffin-cure
                             :trap-door-open :grate-open :kitchen-window-open]
            helpful-flags (filter
                            (fn [flag]
                              (let [new-flags (conj current-flags flag)]
                                (routing-ns game-state here target-room
                                            :available-flags new-flags)))
                            candidate-flags)]
        {:reachable? false
         :missing-flags (set helpful-flags)
         :suggestion (if (seq helpful-flags)
                       (str "Requires: " (clojure.string/join ", " (map name helpful-flags)))
                       "No known path - may be unreachable or require multiple flags")}))))

(defn why-cant-reach?
  "Explain why a location can't be reached.
   Uses routing engine for pathfinding analysis."
  [game-state room]
  (let [basic-check (check-goal game-state {:type :at-location :room room})]
    (if (:satisfied basic-check)
      {:status :satisfied
       :goal {:type :at-location :room room}
       :details "Already at this location"}
      ;; Use routing analysis for more detail
      (let [routing-info (analyze-location-reachability game-state room)]
        {:status (if (:reachable? routing-info) :reachable :unreachable)
         :goal {:type :at-location :room room}
         :details (:suggestion routing-info)
         :routing routing-info}))))

(defn why-not-held?
  "Explain why an object is not held."
  [game-state object]
  (explain-goal game-state {:type :object-held :object object}))

(defn goal-status-summary
  "Get a quick summary of goal status."
  [game-state goal]
  (let [check (check-goal game-state goal)]
    (if (:satisfied check)
      :satisfied
      (let [achievers (aff/achievers-of goal)]
        (cond
          (empty? achievers) :no-achievers
          (some #(empty? (find-blocking-preconditions game-state (:affordance %))) achievers) :achievable
          :else :blocked)))))

;;; ---------------------------------------------------------------------------
;;; FORMATTING FOR DEBUG OUTPUT
;;; ---------------------------------------------------------------------------

(defn format-explanation
  "Format an explanation for human-readable output."
  ([explanation] (format-explanation explanation 0))
  ([explanation indent]
   (let [prefix (apply str (repeat indent "  "))
         {:keys [status goal details achievable-via ready-achievers]} explanation]
     (str prefix "Goal: " (pr-str goal) "\n"
          prefix "Status: " (name status) "\n"
          prefix "Details: " details "\n"
          (when ready-achievers
            (str prefix "Ready via: " (clojure.string/join ", " (map name ready-achievers)) "\n"))
          (when (and achievable-via (not= status :satisfied))
            (str prefix "Achievable via:\n"
                 (clojure.string/join
                   ""
                   (for [achiever achievable-via]
                     (let [{:keys [affordance-id verb description preconditions status blocking]} achiever]
                       (str prefix "  " (name affordance-id) " (" (name verb) ") - " description "\n"
                            prefix "    Status: " (name status) "\n"
                            prefix "    Preconditions:\n"
                            (clojure.string/join
                              ""
                              (for [{:keys [precond status goal-check]} preconditions]
                                (str prefix "      " (if (= status :ok) "✓" "✗") " "
                                     (pr-str precond) "\n")))
                            (when (seq blocking)
                              (str prefix "    Blocking goals:\n"
                                   (clojure.string/join
                                     ""
                                     (for [sub-explanation blocking]
                                       (format-explanation sub-explanation (+ indent 3))))))))))))))))
