(ns clork.intel.affordances
  "Stage 2: Affordance Registry - declaring actions and their preconditions/effects.

   The critical function is `legal-actions` which returns only meaningful actions
   in a given state, eliminating 99% of the action space for planning.

   Usage:
     (legal-actions game-state) -> [{:verb :take :direct-object :lamp} ...]
     (achievers-of {:type :game-flag :flag :lld-flag}) -> [{:affordance :read-book-exorcism ...}]"
  (:require [clork.game-state :as gs]))

;;; ---------------------------------------------------------------------------
;;; BINDING RESOLUTION
;;; ---------------------------------------------------------------------------
;;; Bindings map template variables like :$obj to actual values during enumeration.

(defn resolve-binding
  "Resolve a value, substituting bindings for template variables.
   Template variables start with :$ (e.g., :$obj, :$dir, :$room)."
  [value bindings]
  (if (and (keyword? value)
           (.startsWith (name value) "$"))
    (get bindings value value)
    value))

;;; ---------------------------------------------------------------------------
;;; PRECONDITION CHECKERS
;;; ---------------------------------------------------------------------------
;;; Each precondition type has a checker function.

(defmulti check-precondition
  "Check if a precondition is satisfied.
   Returns true if satisfied, false otherwise."
  (fn [_game-state precond _bindings] (:type precond)))

(defmethod check-precondition :object-visible
  [game-state {:keys [object]} bindings]
  (let [obj-id (resolve-binding object bindings)
        here (:here game-state)
        inventory (set (gs/get-contents game-state :adventurer))
        room-contents (set (gs/get-contents game-state here))]
    (or (contains? inventory obj-id)
        (contains? room-contents obj-id))))

(defmethod check-precondition :object-held
  [game-state {:keys [object]} bindings]
  (let [obj-id (resolve-binding object bindings)
        inventory (set (gs/get-contents game-state :adventurer))]
    (contains? inventory obj-id)))

(defmethod check-precondition :object-in-room
  [game-state {:keys [object]} bindings]
  (let [obj-id (resolve-binding object bindings)
        here (:here game-state)
        room-contents (set (gs/get-contents game-state here))]
    (contains? room-contents obj-id)))

(defmethod check-precondition :object-flag
  [game-state {:keys [object flag]} bindings]
  (let [obj-id (resolve-binding object bindings)]
    (gs/set-thing-flag? game-state obj-id flag)))

(defmethod check-precondition :object-not-flag
  [game-state {:keys [object flag]} bindings]
  (let [obj-id (resolve-binding object bindings)]
    (not (gs/set-thing-flag? game-state obj-id flag))))

(defmethod check-precondition :game-flag
  [game-state {:keys [flag]} _bindings]
  (gs/game-flag? game-state flag))

(defmethod check-precondition :game-not-flag
  [game-state {:keys [flag]} _bindings]
  (not (gs/game-flag? game-state flag)))

(defmethod check-precondition :at-location
  [game-state {:keys [room]} _bindings]
  (= (:here game-state) room))

(defmethod check-precondition :exit-exists
  [game-state {:keys [dir]} bindings]
  (let [direction (resolve-binding dir bindings)
        here (:here game-state)
        room (gs/get-thing game-state here)
        exits (:exits room {})]
    (and (contains? exits direction)
         ;; Must be a keyword destination, not a string (blocked)
         (keyword? (get exits direction)))))

(defmethod check-precondition :inventory-capacity
  [game-state _precond _bindings]
  ;; Simplified check - just verify we have some capacity
  ;; Full implementation would check load-allowed
  (let [inventory (gs/get-contents game-state :adventurer)
        load-allowed (or (:load-allowed game-state) 100)
        current-weight (reduce + 0 (map #(gs/weight game-state %) inventory))]
    (< current-weight load-allowed)))

(defmethod check-precondition :default
  [_game-state precond _bindings]
  (throw (ex-info (str "Unknown precondition type: " (:type precond))
                  {:precondition precond})))

(defn satisfied?
  "Check if all preconditions of an affordance are satisfied."
  [game-state affordance bindings]
  (every? #(check-precondition game-state % bindings) (:preconds affordance)))

;;; ---------------------------------------------------------------------------
;;; EFFECT MATCHING
;;; ---------------------------------------------------------------------------
;;; Used for goal regression - finding what affordances achieve a goal.

(defn effect-achieves?
  "Check if an effect could achieve a goal.
   Goal format: {:type :game-flag :flag :lld-flag}"
  [effect goal]
  (case (:type goal)
    :game-flag
    (and (= (:type effect) :set-flag)
         (nil? (:object effect))  ; Game flag, not object flag
         (= (:flag effect) (:flag goal)))

    :object-flag
    (and (= (:type effect) :set-flag)
         (= (:object effect) (:object goal))
         (= (:flag effect) (:flag goal)))

    :object-at
    (and (= (:type effect) :move-object)
         (= (:object effect) (:object goal))
         (= (:to effect) (:to goal)))

    :player-at
    (and (= (:type effect) :move-player)
         (= (:to effect) (:room goal)))

    ;; Default - no match
    false))

;;; ---------------------------------------------------------------------------
;;; BASIC AFFORDANCES
;;; ---------------------------------------------------------------------------
;;; Core game interactions: navigation, take, drop, open, close, lamp.

(def basic-affordances
  [;; Navigation - handled specially in legal-actions
   ;; (one action per valid exit, enumerated dynamically)

   ;; Take object
   {:id :take-object
    :verb :take
    :pattern {:object :$obj}
    :preconds [{:type :object-in-room :object :$obj}
               {:type :object-flag :object :$obj :flag :take}
               {:type :inventory-capacity}]
    :effects [{:type :move-object :object :$obj :to :adventurer}]
    :desc "Take a portable object"}

   ;; Drop object
   {:id :drop-object
    :verb :drop
    :pattern {:object :$obj}
    :preconds [{:type :object-held :object :$obj}]
    :effects [{:type :move-object :object :$obj :to :here}]
    :desc "Drop a held object"}

   ;; Open container
   {:id :open-container
    :verb :open
    :pattern {:object :$obj}
    :preconds [{:type :object-visible :object :$obj}
               {:type :object-flag :object :$obj :flag :cont}
               {:type :object-not-flag :object :$obj :flag :open}]
    :effects [{:type :set-flag :object :$obj :flag :open}]
    :desc "Open a closed container"}

   ;; Close container
   {:id :close-container
    :verb :close
    :pattern {:object :$obj}
    :preconds [{:type :object-visible :object :$obj}
               {:type :object-flag :object :$obj :flag :cont}
               {:type :object-flag :object :$obj :flag :open}]
    :effects [{:type :clear-flag :object :$obj :flag :open}]
    :desc "Close an open container"}

   ;; Turn on light source
   {:id :lamp-on
    :verb :lamp-on
    :pattern {:object :$obj}
    :preconds [{:type :object-held :object :$obj}
               {:type :object-flag :object :$obj :flag :light}
               {:type :object-not-flag :object :$obj :flag :on}]
    :effects [{:type :set-flag :object :$obj :flag :on}
              {:type :set-flag :object :$obj :flag :lit}]
    :desc "Turn on a light source"}

   ;; Turn off light source
   {:id :lamp-off
    :verb :lamp-off
    :pattern {:object :$obj}
    :preconds [{:type :object-held :object :$obj}
               {:type :object-flag :object :$obj :flag :on}]
    :effects [{:type :clear-flag :object :$obj :flag :on}
              {:type :clear-flag :object :$obj :flag :lit}]
    :desc "Turn off a light source"}])

;;; ---------------------------------------------------------------------------
;;; PUZZLE AFFORDANCES
;;; ---------------------------------------------------------------------------
;;; Special actions for puzzles - these have specific triggers and effects.

(def puzzle-affordances
  [;; === EXORCISM PUZZLE ===
   ;; Step 1: Ring bell (sets xb, moves bell to limbo, creates hot-bell)
   {:id :ring-bell-exorcism
    :verb :ring
    :pattern {:object :brass-bell}
    :preconds [{:type :at-location :room :entrance-to-hades}
               {:type :object-held :object :brass-bell}
               {:type :game-not-flag :flag :lld-flag}]
    :effects [{:type :set-flag :flag :xb}
              {:type :move-object :object :brass-bell :to :limbo}
              {:type :move-object :object :hot-bell :to :entrance-to-hades}]
    :desc "Ring bell to start exorcism (step 1 of 3)"}

   ;; Step 2: Light candles (sets xc when xb is set)
   {:id :light-candles-exorcism
    :verb :lamp-on
    :pattern {:object :candles}
    :preconds [{:type :at-location :room :entrance-to-hades}
               {:type :object-held :object :candles}
               {:type :game-flag :flag :xb}
               {:type :game-not-flag :flag :xc}]
    :effects [{:type :set-flag :flag :xc}
              {:type :set-flag :object :candles :flag :on}
              {:type :start-daemon :daemon :i-candles}]
    :desc "Light candles to continue exorcism (step 2 of 3)"}

   ;; Step 3: Read book (completes exorcism, sets lld-flag)
   {:id :read-book-exorcism
    :verb :read
    :pattern {:object :black-book}
    :preconds [{:type :at-location :room :entrance-to-hades}
               {:type :object-held :object :black-book}
               {:type :game-flag :flag :xc}
               {:type :game-not-flag :flag :lld-flag}]
    :effects [{:type :set-flag :flag :lld-flag}
              {:type :move-object :object :ghosts :to :limbo}]
    :desc "Read book to complete exorcism (step 3 of 3)"}

   ;; === LOUD ROOM PUZZLE ===
   {:id :echo-loud-room
    :verb :echo
    :pattern {}
    :preconds [{:type :at-location :room :loud-room}
               {:type :game-not-flag :flag :loud-flag}]
    :effects [{:type :set-flag :flag :loud-flag}
              {:type :clear-flag :object :platinum-bar :flag :sacred}]
    :desc "Say 'echo' to solve loud room puzzle"}

   ;; === RAINBOW PUZZLE ===
   {:id :wave-sceptre-rainbow
    :verb :wave
    :pattern {:object :sceptre}
    :preconds [{:type :at-location :room :on-the-rainbow}
               {:type :object-held :object :sceptre}]
    :effects [{:type :set-flag :flag :rainbow-flag}]
    :desc "Wave sceptre to make rainbow solid"}

   ;; === DOME PUZZLE ===
   {:id :tie-rope-dome
    :verb :tie
    :pattern {:object :rope :indirect :railing}
    :preconds [{:type :at-location :room :dome-room}
               {:type :object-held :object :rope}
               {:type :object-not-flag :object :rope :flag :tied}]
    :effects [{:type :set-flag :object :rope :flag :tied}
              {:type :set-flag :flag :dome-flag}]
    :desc "Tie rope to railing to enable descent"}])

;;; ---------------------------------------------------------------------------
;;; ALL AFFORDANCES
;;; ---------------------------------------------------------------------------

(def all-affordances
  "Combined list of all affordances."
  (concat basic-affordances puzzle-affordances))

(def affordance-by-id
  "Map of affordance ID to affordance."
  (into {} (map (juxt :id identity) all-affordances)))

;;; ---------------------------------------------------------------------------
;;; LEGAL ACTIONS FUNCTION
;;; ---------------------------------------------------------------------------

(defn- movement-actions
  "Generate movement actions from room exits."
  [game-state]
  (let [here (:here game-state)
        room (gs/get-thing game-state here)
        exits (:exits room {})]
    (for [[dir dest] exits
          :when (keyword? dest)]  ; Ignore string (blocked) exits
      {:verb :walk :direct-object dir})))

(defn- take-actions
  "Generate take actions for takeable objects in room."
  [game-state]
  (let [here (:here game-state)
        room-contents (gs/get-contents game-state here)
        inventory (set (gs/get-contents game-state :adventurer))]
    (for [obj-id room-contents
          :when (and (gs/set-thing-flag? game-state obj-id :take)
                     (not (contains? inventory obj-id)))]
      {:verb :take :direct-object obj-id})))

(defn- drop-actions
  "Generate drop actions for held objects."
  [game-state]
  (let [inventory (gs/get-contents game-state :adventurer)]
    (for [obj-id inventory]
      {:verb :drop :direct-object obj-id})))

(defn- open-actions
  "Generate open actions for closed containers."
  [game-state]
  (let [here (:here game-state)
        inventory (gs/get-contents game-state :adventurer)
        room-contents (gs/get-contents game-state here)
        visible (concat inventory room-contents)]
    (for [obj-id visible
          :when (and (gs/set-thing-flag? game-state obj-id :cont)
                     (not (gs/set-thing-flag? game-state obj-id :open)))]
      {:verb :open :direct-object obj-id})))

(defn- close-actions
  "Generate close actions for open containers."
  [game-state]
  (let [here (:here game-state)
        inventory (gs/get-contents game-state :adventurer)
        room-contents (gs/get-contents game-state here)
        visible (concat inventory room-contents)]
    (for [obj-id visible
          :when (and (gs/set-thing-flag? game-state obj-id :cont)
                     (gs/set-thing-flag? game-state obj-id :open))]
      {:verb :close :direct-object obj-id})))

(defn- lamp-on-actions
  "Generate lamp-on actions for light sources that are off."
  [game-state]
  (let [inventory (gs/get-contents game-state :adventurer)]
    (for [obj-id inventory
          :when (and (gs/set-thing-flag? game-state obj-id :light)
                     (not (gs/set-thing-flag? game-state obj-id :on)))]
      {:verb :lamp-on :direct-object obj-id})))

(defn- lamp-off-actions
  "Generate lamp-off actions for light sources that are on."
  [game-state]
  (let [inventory (gs/get-contents game-state :adventurer)]
    (for [obj-id inventory
          :when (and (gs/set-thing-flag? game-state obj-id :light)
                     (gs/set-thing-flag? game-state obj-id :on))]
      {:verb :lamp-off :direct-object obj-id})))

(defn- affordance->action
  "Convert an affordance to an action map."
  [affordance]
  (let [{:keys [verb pattern]} affordance]
    (cond-> {:verb verb}
      (:object pattern) (assoc :direct-object (:object pattern))
      (:indirect pattern) (assoc :indirect-object (:indirect pattern)))))

(defn- applicable-special-actions
  "Return special (puzzle) actions applicable in current context."
  [game-state]
  (->> puzzle-affordances
       (filter #(satisfied? game-state % {}))
       (map affordance->action)))

(defn legal-actions
  "Return all meaningful actions in current state.

   This is THE key function for planning tractability.
   Returns 5-20 actions per state, not 500.

   Returns a sequence of action maps like:
   {:verb :take :direct-object :lamp}
   {:verb :walk :direct-object :north}"
  [game-state]
  (concat
    (movement-actions game-state)
    (take-actions game-state)
    (drop-actions game-state)
    (open-actions game-state)
    (close-actions game-state)
    (lamp-on-actions game-state)
    (lamp-off-actions game-state)
    (applicable-special-actions game-state)))

;;; ---------------------------------------------------------------------------
;;; REGISTRY QUERY FUNCTIONS
;;; ---------------------------------------------------------------------------

(defn achievers-of
  "Find affordances that could achieve a goal.

   Goal format:
   {:type :game-flag :flag :lld-flag}
   {:type :object-flag :object :candles :flag :on}
   {:type :object-at :object :lamp :to :adventurer}

   Returns: [{:affordance-id ... :via-effect ...}]"
  [goal]
  (->> all-affordances
       (filter (fn [aff]
                 (some #(effect-achieves? % goal) (:effects aff))))
       (map (fn [aff]
              {:affordance-id (:id aff)
               :affordance aff
               :via-effect (first (filter #(effect-achieves? % goal) (:effects aff)))}))))

(defn effects-of
  "Get declared effects of an affordance by ID.

   Returns: [{:type :set-flag :flag :xb} ...]"
  [affordance-id]
  (:effects (get affordance-by-id affordance-id)))

(defn preconditions-of
  "Get declared preconditions of an affordance by ID.

   Returns: [{:type :at-location :room :entrance-to-hades} ...]"
  [affordance-id]
  (:preconds (get affordance-by-id affordance-id)))

(defn get-affordance
  "Get an affordance by ID."
  [affordance-id]
  (get affordance-by-id affordance-id))

(defn list-affordances
  "List all affordance IDs."
  []
  (map :id all-affordances))

;;; ---------------------------------------------------------------------------
;;; DEBUGGING HELPERS
;;; ---------------------------------------------------------------------------

(defn explain-legal-actions
  "Return legal actions with explanations."
  [game-state]
  (let [actions (legal-actions game-state)]
    {:count (count actions)
     :actions (vec actions)
     :summary {:movement (count (filter #(= :walk (:verb %)) actions))
               :take (count (filter #(= :take (:verb %)) actions))
               :drop (count (filter #(= :drop (:verb %)) actions))
               :open (count (filter #(= :open (:verb %)) actions))
               :close (count (filter #(= :close (:verb %)) actions))
               :lamp-on (count (filter #(= :lamp-on (:verb %)) actions))
               :lamp-off (count (filter #(= :lamp-off (:verb %)) actions))
               :special (count (filter #(not (#{:walk :take :drop :open :close :lamp-on :lamp-off} (:verb %))) actions))}}))
