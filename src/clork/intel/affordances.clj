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

(defmethod check-precondition :at-any-location
  [game-state {:keys [rooms]} _bindings]
  (contains? (set rooms) (:here game-state)))

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

(defmethod check-precondition :object-in-container
  [game-state {:keys [object container]} bindings]
  (let [obj-id (resolve-binding object bindings)
        container-id (resolve-binding container bindings)]
    (= (gs/get-thing-location game-state obj-id) container-id)))

(defmethod check-precondition :in-vehicle
  [game-state {:keys [vehicle]} _bindings]
  (let [winner (:winner game-state)
        winner-loc (gs/get-thing-location game-state winner)]
    (= winner-loc vehicle)))

(defmethod check-precondition :not-in-vehicle
  [game-state _precond _bindings]
  (let [winner (:winner game-state)
        winner-loc (gs/get-thing-location game-state winner)
        here (:here game-state)]
    (= winner-loc here)))

(defmethod check-precondition :player-in-vehicle
  [game-state {:keys [vehicle]} _bindings]
  ;; Alias for :in-vehicle - checks if player is in the specified vehicle
  (let [winner (:winner game-state)
        winner-loc (gs/get-thing-location game-state winner)]
    (= winner-loc vehicle)))

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

    :game-not-flag
    (and (= (:type effect) :clear-flag)
         (nil? (:object effect))  ; Game flag, not object flag
         (= (:flag effect) (:flag goal)))

    :object-flag
    (and (= (:type effect) :set-flag)
         (= (:object effect) (:object goal))
         (= (:flag effect) (:flag goal)))

    :object-not-flag
    (and (= (:type effect) :clear-flag)
         (= (:object effect) (:object goal))
         (= (:flag effect) (:flag goal)))

    :object-at
    (and (= (:type effect) :move-object)
         (= (:object effect) (:object goal))
         (= (:to effect) (:to goal)))

    :object-held
    (and (= (:type effect) :move-object)
         (= (:object effect) (:object goal))
         (= (:to effect) :adventurer))

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
    :desc "Turn off a light source"}

   ;; Read object (generic)
   {:id :read-object
    :verb :read
    :pattern {:object :$obj}
    :preconds [{:type :object-visible :object :$obj}
               {:type :object-flag :object :$obj :flag :read}]
    :effects []  ; Reading doesn't change state
    :desc "Read a readable object"}

   ;; Examine object (generic)
   {:id :examine-object
    :verb :examine
    :pattern {:object :$obj}
    :preconds [{:type :object-visible :object :$obj}]
    :effects []  ; Examining doesn't change state
    :desc "Examine any visible object"}

   ;; Put object in container
   {:id :put-in-container
    :verb :put
    :pattern {:object :$obj :indirect :$container}
    :preconds [{:type :object-held :object :$obj}
               {:type :object-visible :object :$container}
               {:type :object-flag :object :$container :flag :cont}
               {:type :object-flag :object :$container :flag :open}]
    :effects [{:type :move-object :object :$obj :to :$container}]
    :desc "Put held object in open container"}])

;;; ---------------------------------------------------------------------------
;;; COMBAT AFFORDANCES
;;; ---------------------------------------------------------------------------
;;; Fighting villains - troll and cyclops.

(def combat-affordances
  [;; Attack troll with weapon
   {:id :attack-troll
    :verb :attack
    :pattern {:object :troll :with :$weapon}
    :preconds [{:type :at-location :room :troll-room}
               {:type :object-in-room :object :troll}
               {:type :object-held :object :$weapon}
               {:type :object-flag :object :$weapon :flag :weapon}
               {:type :game-not-flag :flag :troll-flag}]
    :effects [{:type :set-flag :flag :troll-flag}]  ; Simplified - combat is probabilistic
    :desc "Attack the troll with a weapon"}

   ;; Kill troll (synonym)
   {:id :kill-troll
    :verb :kill
    :pattern {:object :troll :with :$weapon}
    :preconds [{:type :at-location :room :troll-room}
               {:type :object-in-room :object :troll}
               {:type :object-held :object :$weapon}
               {:type :object-flag :object :$weapon :flag :weapon}
               {:type :game-not-flag :flag :troll-flag}]
    :effects [{:type :set-flag :flag :troll-flag}]
    :desc "Kill the troll with a weapon"}

   ;; Attack cyclops with weapon
   {:id :attack-cyclops
    :verb :attack
    :pattern {:object :cyclops :with :$weapon}
    :preconds [{:type :at-location :room :cyclops-room}
               {:type :object-in-room :object :cyclops}
               {:type :object-held :object :$weapon}
               {:type :object-flag :object :$weapon :flag :weapon}
               {:type :game-not-flag :flag :cyclops-flag}]
    :effects []  ; Fighting cyclops doesn't defeat him directly
    :desc "Attack the cyclops with a weapon (not recommended)"}

   ;; Say Odysseus to scare cyclops
   {:id :say-odysseus
    :verb :odysseus
    :pattern {}
    :preconds [{:type :at-location :room :cyclops-room}
               {:type :object-in-room :object :cyclops}
               {:type :game-not-flag :flag :cyclops-flag}]
    :effects [{:type :set-flag :flag :cyclops-flag}
              {:type :set-flag :flag :magic-flag}
              {:type :move-object :object :cyclops :to :limbo}]
    :desc "Say 'Odysseus' to scare the cyclops away"}

   ;; Give lunch to cyclops (makes him sleepy)
   {:id :give-lunch-to-cyclops
    :verb :give
    :pattern {:object :lunch :indirect :cyclops}
    :preconds [{:type :at-location :room :cyclops-room}
               {:type :object-in-room :object :cyclops}
               {:type :object-held :object :lunch}
               {:type :game-not-flag :flag :cyclops-flag}]
    :effects [{:type :move-object :object :lunch :to :limbo}]
    :desc "Give lunch to the cyclops"}])

;;; ---------------------------------------------------------------------------
;;; TROPHY CASE / SCORING AFFORDANCES
;;; ---------------------------------------------------------------------------
;;; Putting treasures in the trophy case for points.

(def scoring-affordances
  [;; Put treasure in trophy case
   {:id :put-treasure-in-case
    :verb :put
    :pattern {:object :$treasure :indirect :trophy-case}
    :preconds [{:type :at-location :room :living-room}
               {:type :object-held :object :$treasure}
               {:type :object-flag :object :$treasure :flag :treasure}]
    :effects [{:type :move-object :object :$treasure :to :trophy-case}
              {:type :add-score}]  ; Score based on treasure value
    :desc "Put a treasure in the trophy case for points"}])

;;; ---------------------------------------------------------------------------
;;; DAM PUZZLE AFFORDANCES
;;; ---------------------------------------------------------------------------
;;; The Flood Control Dam #3 buttons and bolt mechanism.

(def dam-affordances
  [;; Press yellow button (enables bolt turning)
   {:id :press-yellow-button
    :verb :push
    :pattern {:object :yellow-button}
    :preconds [{:type :at-location :room :dam-room}]
    :effects [{:type :set-flag :flag :gate-flag}]
    :desc "Press yellow button to enable bolt mechanism"}

   ;; Press brown button (disables bolt turning)
   {:id :press-brown-button
    :verb :push
    :pattern {:object :brown-button}
    :preconds [{:type :at-location :room :dam-room}]
    :effects [{:type :clear-flag :flag :gate-flag}]
    :desc "Press brown button to disable bolt mechanism"}

   ;; Turn bolt with wrench (open gates)
   {:id :turn-bolt-open
    :verb :turn
    :pattern {:object :bolt :with :wrench}
    :preconds [{:type :at-location :room :dam-room}
               {:type :object-held :object :wrench}
               {:type :game-flag :flag :gate-flag}
               {:type :game-not-flag :flag :gates-open}]
    :effects [{:type :set-flag :flag :gates-open}]
    :desc "Turn bolt with wrench to open sluice gates"}

   ;; Turn bolt with wrench (close gates)
   {:id :turn-bolt-close
    :verb :turn
    :pattern {:object :bolt :with :wrench}
    :preconds [{:type :at-location :room :dam-room}
               {:type :object-held :object :wrench}
               {:type :game-flag :flag :gate-flag}
               {:type :game-flag :flag :gates-open}]
    :effects [{:type :clear-flag :flag :gates-open}]
    :desc "Turn bolt with wrench to close sluice gates"}

   ;; Press blue button (causes leak in maintenance room)
   {:id :press-blue-button
    :verb :push
    :pattern {:object :blue-button}
    :preconds [{:type :at-location :room :dam-room}]
    :effects [{:type :start-daemon :daemon :i-flood}]
    :desc "Press blue button to cause leak in maintenance room"}

   ;; Press red button (toggles maintenance room lights)
   {:id :press-red-button
    :verb :push
    :pattern {:object :red-button}
    :preconds [{:type :at-location :room :dam-room}]
    :effects []  ; Just toggles lights
    :desc "Press red button to toggle maintenance room lights"}])

;;; ---------------------------------------------------------------------------
;;; BOAT AFFORDANCES
;;; ---------------------------------------------------------------------------
;;; Inflatable boat mechanics.

(def boat-affordances
  [;; Inflate boat with pump
   ;; Transforms :inflatable-boat into :inflated-boat
   {:id :inflate-boat
    :verb :inflate
    :pattern {:object :inflatable-boat :with :pump}
    :preconds [{:type :object-visible :object :inflatable-boat}
               {:type :object-held :object :pump}]
    :effects [{:type :move-object :object :inflatable-boat :to :limbo}
              {:type :move-object :object :inflated-boat :to :here}]
    :desc "Inflate the boat with the pump"}

   ;; Deflate boat
   ;; Transforms :inflated-boat back into :inflatable-boat
   {:id :deflate-boat
    :verb :deflate
    :pattern {:object :inflated-boat}
    :preconds [{:type :object-visible :object :inflated-boat}
               {:type :not-in-vehicle}]  ; Can't deflate while in boat
    :effects [{:type :move-object :object :inflated-boat :to :limbo}
              {:type :move-object :object :inflatable-boat :to :here}]
    :desc "Deflate the inflated boat"}

   ;; Board boat
   {:id :board-boat
    :verb :board
    :pattern {:object :inflated-boat}
    :preconds [{:type :object-in-room :object :inflated-boat}
               {:type :not-in-vehicle}]
    :effects [{:type :move-player :to :inflated-boat}]
    :desc "Get into the inflated boat"}

   ;; Disembark boat
   {:id :disembark-boat
    :verb :disembark
    :pattern {}
    :preconds [{:type :in-vehicle :vehicle :inflated-boat}]
    :effects [{:type :move-player :to :here}]
    :desc "Get out of the boat"}

   ;; Launch boat (Frigid River launch points)
   {:id :launch-boat-frigid
    :verb :launch
    :pattern {:object :inflated-boat}
    :preconds [{:type :at-any-location :rooms [:dam-base :white-cliffs-north
                                               :white-cliffs-south :sandy-beach
                                               :shore]}
               {:type :player-in-vehicle :vehicle :inflated-boat}]
    :effects [{:type :set-flag :flag :boat-in-water}]
    :desc "Launch the boat into the Frigid River"}

   ;; Launch boat (Reservoir launch points)
   {:id :launch-boat-reservoir
    :verb :launch
    :pattern {:object :inflated-boat}
    :preconds [{:type :at-any-location :rooms [:reservoir-north :reservoir-south]}
               {:type :player-in-vehicle :vehicle :inflated-boat}]
    :effects [{:type :set-flag :flag :boat-in-water}]
    :desc "Launch the boat into the Reservoir"}

   ;; Land boat (Frigid River landing points)
   ;; Landing exits boat to shore room
   {:id :land-boat
    :verb :land
    :pattern {}
    :preconds [{:type :player-in-vehicle :vehicle :inflated-boat}
               {:type :at-any-location :rooms [:river-3 :river-4 :river-5]}]
    :effects [{:type :move-player :to :here}
              {:type :clear-flag :flag :boat-in-water}]
    :desc "Land the boat at shore"}])

;;; ---------------------------------------------------------------------------
;;; ENVIRONMENT PUZZLE AFFORDANCES
;;; ---------------------------------------------------------------------------
;;; Rug, trap door, grating, mirror.

(def environment-affordances
  [;; Move rug to reveal trap door
   {:id :move-rug
    :verb :move
    :pattern {:object :oriental-rug}
    :preconds [{:type :at-location :room :living-room}
               {:type :game-not-flag :flag :rug-moved}]
    :effects [{:type :set-flag :flag :rug-moved}]
    :desc "Move the rug to reveal the trap door"}

   ;; Open trap door
   {:id :open-trap-door
    :verb :open
    :pattern {:object :trap-door}
    :preconds [{:type :at-location :room :living-room}
               {:type :game-flag :flag :rug-moved}
               {:type :game-not-flag :flag :trap-door-open}]
    :effects [{:type :set-flag :flag :trap-door-open}]
    :desc "Open the trap door (after moving rug)"}

   ;; Close trap door
   {:id :close-trap-door
    :verb :close
    :pattern {:object :trap-door}
    :preconds [{:type :at-location :room :living-room}
               {:type :game-flag :flag :trap-door-open}]
    :effects [{:type :clear-flag :flag :trap-door-open}]
    :desc "Close the trap door"}

   ;; Unlock grating with keys
   {:id :unlock-grating
    :verb :unlock
    :pattern {:object :grate :with :skeleton-key}
    :preconds [{:type :at-location :room :grating-clearing}
               {:type :object-held :object :skeleton-key}
               {:type :object-flag :object :grate :flag :locked}]
    :effects [{:type :clear-flag :object :grate :flag :locked}]
    :desc "Unlock the grating with the skeleton key"}

   ;; Open grating
   {:id :open-grating
    :verb :open
    :pattern {:object :grate}
    :preconds [{:type :at-location :room :grating-clearing}
               {:type :object-not-flag :object :grate :flag :locked}
               {:type :object-not-flag :object :grate :flag :open}]
    :effects [{:type :set-flag :object :grate :flag :open}]
    :desc "Open the unlocked grating"}

   ;; Close grating
   {:id :close-grating
    :verb :close
    :pattern {:object :grate}
    :preconds [{:type :at-location :room :grating-clearing}
               {:type :object-flag :object :grate :flag :open}]
    :effects [{:type :clear-flag :object :grate :flag :open}]
    :desc "Close the grating"}

   ;; Lock grating
   {:id :lock-grating
    :verb :lock
    :pattern {:object :grate :with :skeleton-key}
    :preconds [{:type :at-location :room :grating-clearing}
               {:type :object-held :object :skeleton-key}
               {:type :object-not-flag :object :grate :flag :locked}
               {:type :object-not-flag :object :grate :flag :open}]
    :effects [{:type :set-flag :object :grate :flag :locked}]
    :desc "Lock the grating with the skeleton key"}

   ;; Raise basket (from shaft room)
   {:id :raise-basket
    :verb :raise
    :pattern {:object :basket}
    :preconds [{:type :at-location :room :shaft-room}]
    :effects [{:type :set-flag :object :basket :flag :raised}]
    :desc "Raise the basket in the shaft room"}

   ;; Lower basket
   {:id :lower-basket
    :verb :lower
    :pattern {:object :basket}
    :preconds [{:type :at-location :room :shaft-room}
               {:type :object-flag :object :basket :flag :raised}]
    :effects [{:type :clear-flag :object :basket :flag :raised}]
    :desc "Lower the basket in the shaft room"}])

;;; ---------------------------------------------------------------------------
;;; CLIMBING AFFORDANCES
;;; ---------------------------------------------------------------------------

(def climb-affordances
  [;; Climb up tree
   {:id :climb-tree
    :verb :climb
    :pattern {:object :tree}
    :preconds [{:type :at-location :room :forest-path}]
    :effects [{:type :move-player :to :up-a-tree}]
    :desc "Climb up the tree to reach the nest"}

   ;; Climb down from tree
   {:id :climb-down-tree
    :verb :climb-down
    :pattern {:object :tree}
    :preconds [{:type :at-location :room :up-a-tree}]
    :effects [{:type :move-player :to :forest-path}]
    :desc "Climb down from the tree"}

   ;; Climb down rope (in dome room after tying)
   {:id :climb-down-rope
    :verb :climb-down
    :pattern {:object :rope}
    :preconds [{:type :at-location :room :dome-room}
               {:type :game-flag :flag :dome-flag}]  ; Rope must be tied
    :effects [{:type :move-player :to :torch-room}]
    :desc "Climb down the tied rope"}

   ;; Climb up rope (from torch room)
   {:id :climb-up-rope
    :verb :climb-up
    :pattern {:object :rope}
    :preconds [{:type :at-location :room :torch-room}
               {:type :game-flag :flag :dome-flag}]
    :effects [{:type :move-player :to :dome-room}]
    :desc "Climb up the rope to the dome"}

   ;; Untie rope
   {:id :untie-rope
    :verb :untie
    :pattern {:object :rope}
    :preconds [{:type :at-location :room :dome-room}
               {:type :game-flag :flag :dome-flag}]
    :effects [{:type :clear-flag :flag :dome-flag}
              {:type :clear-flag :object :rope :flag :tied}
              {:type :move-object :object :rope :to :adventurer}]
    :desc "Untie the rope from the railing"}])

;;; ---------------------------------------------------------------------------
;;; FOOD/DRINK AFFORDANCES
;;; ---------------------------------------------------------------------------

(def food-affordances
  [;; Eat garlic
   {:id :eat-garlic
    :verb :eat
    :pattern {:object :garlic}
    :preconds [{:type :object-held :object :garlic}]
    :effects [{:type :move-object :object :garlic :to :limbo}]
    :desc "Eat the garlic"}

   ;; Eat lunch
   {:id :eat-lunch
    :verb :eat
    :pattern {:object :lunch}
    :preconds [{:type :object-held :object :lunch}]
    :effects [{:type :move-object :object :lunch :to :limbo}]
    :desc "Eat the lunch"}

   ;; Fill bottle with water
   {:id :fill-bottle
    :verb :fill
    :pattern {:object :bottle}
    :preconds [{:type :object-held :object :bottle}
               {:type :at-any-location :rooms [:stream :reservoir-north
                                               :reservoir-south :stream-view]}]
    :effects [{:type :move-object :object :quantity-of-water :to :bottle}]
    :desc "Fill the bottle with water"}

   ;; Drink from bottle
   {:id :drink-water
    :verb :drink
    :pattern {:object :quantity-of-water}
    :preconds [{:type :object-in-container :object :quantity-of-water :container :bottle}
               {:type :object-held :object :bottle}]
    :effects [{:type :move-object :object :quantity-of-water :to :limbo}]
    :desc "Drink the water from the bottle"}])

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

   ;; Wave sceptre again (unsolid rainbow)
   {:id :wave-sceptre-unsolid
    :verb :wave
    :pattern {:object :sceptre}
    :preconds [{:type :at-any-location :rooms [:on-the-rainbow :end-of-rainbow]}
               {:type :object-held :object :sceptre}
               {:type :game-flag :flag :rainbow-flag}]
    :effects [{:type :clear-flag :flag :rainbow-flag}]
    :desc "Wave sceptre again to make rainbow insubstantial"}

   ;; === DOME PUZZLE ===
   {:id :tie-rope-dome
    :verb :tie
    :pattern {:object :rope :indirect :railing}
    :preconds [{:type :at-location :room :dome-room}
               {:type :object-held :object :rope}
               {:type :object-not-flag :object :rope :flag :tied}]
    :effects [{:type :set-flag :object :rope :flag :tied}
              {:type :set-flag :flag :dome-flag}]
    :desc "Tie rope to railing to enable descent"}

   ;; === COAL MINE PUZZLE ===
   ;; Put coal in machine
   {:id :put-coal-in-machine
    :verb :put
    :pattern {:object :pile-of-coal :indirect :machine}
    :preconds [{:type :at-location :room :machine-room}
               {:type :object-held :object :pile-of-coal}
               {:type :object-flag :object :machine :flag :open}]
    :effects [{:type :move-object :object :pile-of-coal :to :machine}]
    :desc "Put coal in the machine"}

   ;; Turn machine switch (coal -> diamond)
   {:id :turn-machine-switch
    :verb :turn
    :pattern {:object :machine-switch}
    :preconds [{:type :at-location :room :machine-room}
               {:type :object-in-container :object :pile-of-coal :container :machine}
               {:type :object-not-flag :object :machine :flag :open}]
    :effects [{:type :move-object :object :pile-of-coal :to :limbo}
              {:type :move-object :object :diamond :to :machine}]
    :desc "Turn the machine switch to process coal into diamond"}

   ;; === MIRROR TELEPORTATION ===
   {:id :look-in-mirror-1
    :verb :look
    :pattern {:object :mirror}
    :preconds [{:type :at-location :room :mirror-room-1}]
    :effects [{:type :move-player :to :mirror-room-2}]
    :desc "Look in the mirror to teleport (room 1 to 2)"}

   {:id :look-in-mirror-2
    :verb :look
    :pattern {:object :mirror}
    :preconds [{:type :at-location :room :mirror-room-2}]
    :effects [{:type :move-player :to :mirror-room-1}]
    :desc "Look in the mirror to teleport (room 2 to 1)"}

   ;; === COFFIN/SCEPTRE ENDGAME ===
   {:id :open-coffin
    :verb :open
    :pattern {:object :gold-coffin}
    :preconds [{:type :object-visible :object :gold-coffin}
               {:type :object-not-flag :object :gold-coffin :flag :open}]
    :effects [{:type :set-flag :object :gold-coffin :flag :open}]
    :desc "Open the gold coffin"}

   ;; === PRAYER TELEPORT ===
   {:id :pray-at-altar
    :verb :pray
    :pattern {}
    :preconds [{:type :at-location :room :altar}]
    :effects [{:type :move-player :to :forest-1}]
    :desc "Pray at the altar to teleport to the forest"}])

;;; ---------------------------------------------------------------------------
;;; ALL AFFORDANCES
;;; ---------------------------------------------------------------------------

(def all-affordances
  "Combined list of all affordances."
  (concat basic-affordances
          combat-affordances
          scoring-affordances
          dam-affordances
          boat-affordances
          environment-affordances
          climb-affordances
          food-affordances
          puzzle-affordances))

(def affordance-by-id
  "Map of affordance ID to affordance."
  (into {} (map (juxt :id identity) all-affordances)))

;;; ---------------------------------------------------------------------------
;;; TREASURE DETECTION
;;; ---------------------------------------------------------------------------

(def treasure-objects
  "Set of objects that are treasures (have :treasure flag in game)."
  #{:jeweled-egg :clockwork-canary :portrait :platinum-bar :ivory-torch
    :gold-coffin :jade :sapphire-bracelet :diamond :bag-of-coins
    :crystal-skull :jewel-encrusted-trident :chalice :trunk-of-jewels
    :crystal-sphere :torch :pot-of-gold :scarab :bauble :brass-bell
    :figurine})

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

(defn- read-actions
  "Generate read actions for readable objects."
  [game-state]
  (let [here (:here game-state)
        inventory (gs/get-contents game-state :adventurer)
        room-contents (gs/get-contents game-state here)
        visible (concat inventory room-contents)]
    (for [obj-id visible
          :when (gs/set-thing-flag? game-state obj-id :read)]
      {:verb :read :direct-object obj-id})))

(defn- put-in-case-actions
  "Generate actions for putting treasures in trophy case."
  [game-state]
  (when (= (:here game-state) :living-room)
    (let [inventory (gs/get-contents game-state :adventurer)]
      (for [obj-id inventory
            :when (contains? treasure-objects obj-id)]
        {:verb :put :direct-object obj-id :indirect-object :trophy-case}))))

(defn- attack-actions
  "Generate attack actions for villains in room."
  [game-state]
  (let [here (:here game-state)
        room-contents (set (gs/get-contents game-state here))
        inventory (gs/get-contents game-state :adventurer)
        weapons (filter #(gs/set-thing-flag? game-state % :weapon) inventory)
        villains (filter #(and (gs/set-thing-flag? game-state % :actor)
                               (contains? room-contents %))
                         [:troll :cyclops :thief])]
    (for [villain villains
          weapon weapons]
      {:verb :attack :direct-object villain :indirect-object weapon})))

(defn- affordance->action
  "Convert an affordance to an action map."
  [affordance]
  (let [{:keys [verb pattern]} affordance]
    (cond-> {:verb verb}
      (:object pattern) (assoc :direct-object (:object pattern))
      (:indirect pattern) (assoc :indirect-object (:indirect pattern))
      (:with pattern) (assoc :indirect-object (:with pattern)))))

(defn- applicable-special-actions
  "Return special (puzzle) actions applicable in current context."
  [game-state]
  (let [special-affs (concat puzzle-affordances
                             combat-affordances
                             scoring-affordances
                             dam-affordances
                             boat-affordances
                             environment-affordances
                             climb-affordances
                             food-affordances)]
    (->> special-affs
         (filter #(satisfied? game-state % {}))
         (map affordance->action))))

(defn legal-actions
  "Return all meaningful actions in current state.

   This is THE key function for planning tractability.
   Returns a focused set of meaningful actions, not the entire action space.

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
    (read-actions game-state)
    (put-in-case-actions game-state)
    (attack-actions game-state)
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

(defn affordances-by-verb
  "Get all affordances that use a specific verb."
  [verb]
  (filter #(= verb (:verb %)) all-affordances))

(defn affordances-by-category
  "Get affordances by category."
  [category]
  (case category
    :basic basic-affordances
    :combat combat-affordances
    :scoring scoring-affordances
    :dam dam-affordances
    :boat boat-affordances
    :environment environment-affordances
    :climb climb-affordances
    :food food-affordances
    :puzzle puzzle-affordances
    all-affordances))

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
               :read (count (filter #(= :read (:verb %)) actions))
               :attack (count (filter #(= :attack (:verb %)) actions))
               :put (count (filter #(= :put (:verb %)) actions))
               :special (count (filter #(not (#{:walk :take :drop :open :close
                                                :lamp-on :lamp-off :read :attack :put}
                                              (:verb %))) actions))}}))

(defn check-affordance
  "Check if a specific affordance is applicable in current state.
   Returns {:applicable true/false :failed-preconds [...] if not applicable}"
  [game-state affordance-id]
  (if-let [aff (get-affordance affordance-id)]
    (let [preconds (:preconds aff)
          results (map (fn [p]
                         {:precond p
                          :satisfied (check-precondition game-state p {})})
                       preconds)
          failed (filter #(not (:satisfied %)) results)]
      (if (empty? failed)
        {:applicable true :affordance-id affordance-id}
        {:applicable false
         :affordance-id affordance-id
         :failed-preconds (mapv :precond failed)}))
    {:error (str "Unknown affordance: " affordance-id)}))
