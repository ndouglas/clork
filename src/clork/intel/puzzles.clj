(ns clork.intel.puzzles
  "Stage 5: Puzzle Solvers

   Puzzles as verified procedures with pre/postconditions.
   The planner treats these as black-box calls rather than
   action sequences to discover.

   Key functions:
   - can-solve-puzzle?: Check if puzzle preconditions are met
   - execute-puzzle: Run puzzle steps and verify effects
   - puzzle-items-needed: List required items for a puzzle
   - puzzles-for-flag: Find puzzles that achieve a flag"
  (:require [clork.game-state :as gs]
            [clork.intel.transition :as transition]
            [clork.intel.routing :as routing]
            [clork.intel.goals :as goals]))

;;; ---------------------------------------------------------------------------
;;; PUZZLE PRECONDITION CHECKING
;;; ---------------------------------------------------------------------------

(defmulti check-puzzle-precond
  "Check if a puzzle precondition is satisfied.
   Returns {:satisfied bool :details string}"
  (fn [_game-state precond] (:type precond)))

(defmethod check-puzzle-precond :object-accessible
  [game-state {:keys [object]}]
  (let [loc (gs/get-thing-loc-id game-state object)
        held (= loc :adventurer)
        here (:here game-state)
        in-room (= loc here)
        ;; Check if in an open container in the room
        accessible (or held in-room
                       (when-let [container-loc (gs/get-thing-loc-id game-state loc)]
                         (and (= container-loc here)
                              (gs/set-thing-flag? game-state loc :open))))]
    {:satisfied accessible
     :details (cond
                held (str (name object) " is held")
                in-room (str (name object) " is in room")
                accessible (str (name object) " is accessible in container")
                :else (str (name object) " is at " (name (or loc :unknown))))}))

(defmethod check-puzzle-precond :object-held
  [game-state {:keys [object]}]
  (let [loc (gs/get-thing-loc-id game-state object)
        held (= loc :adventurer)]
    {:satisfied held
     :details (if held
                (str "Holding " (name object))
                (str (name object) " not held (at " (name (or loc :unknown)) ")"))}))

(defmethod check-puzzle-precond :can-reach
  [game-state {:keys [room]}]
  (let [here (:here game-state)
        flags (routing/extract-available-flags game-state)
        path-result (routing/shortest-path game-state here room
                                           :available-flags flags)]
    {:satisfied (or (= here room) (some? path-result))
     :details (cond
                (= here room) (str "Already at " (name room))
                path-result (str "Can reach " (name room) " in " (:distance path-result) " moves")
                :else (str "Cannot reach " (name room)))}))

(defmethod check-puzzle-precond :flag-set
  [game-state {:keys [flag]}]
  ;; Check both stored game flags AND virtual flags (like :boat-ready)
  (let [stored-flag (gs/game-flag? game-state flag)
        virtual-flags (routing/extract-available-flags game-state)
        is-set (or stored-flag (contains? virtual-flags flag))]
    {:satisfied is-set
     :details (if is-set
                (str "Flag " (name flag) " is set")
                (str "Flag " (name flag) " is NOT set"))}))

(defmethod check-puzzle-precond :flag-not-set
  [game-state {:keys [flag]}]
  ;; Check both stored game flags AND virtual flags
  (let [stored-flag (gs/game-flag? game-state flag)
        virtual-flags (routing/extract-available-flags game-state)
        is-set (or stored-flag (contains? virtual-flags flag))]
    {:satisfied (not is-set)
     :details (if is-set
                (str "Flag " (name flag) " is set (should not be)")
                (str "Flag " (name flag) " is not set (good)"))}))

(defmethod check-puzzle-precond :at-location
  [game-state {:keys [room]}]
  (let [here (:here game-state)]
    {:satisfied (= here room)
     :details (if (= here room)
                (str "At " (name room))
                (str "At " (name here) ", need " (name room)))}))

(defmethod check-puzzle-precond :player-in-vehicle
  [game-state {:keys [vehicle]}]
  (let [adventurer-loc (gs/get-thing-loc-id game-state :adventurer)
        in-vehicle (= adventurer-loc vehicle)]
    {:satisfied in-vehicle
     :details (if in-vehicle
                (str "Player in " (name vehicle))
                (str "Player not in vehicle (at " (name (or adventurer-loc :unknown)) ")"))}))

(defmethod check-puzzle-precond :object-flag
  [game-state {:keys [object flag]}]
  (let [has-flag (gs/set-thing-flag? game-state object flag)]
    {:satisfied has-flag
     :details (if has-flag
                (str (name object) " has " (name flag) " flag")
                (str (name object) " missing " (name flag) " flag"))}))

(defmethod check-puzzle-precond :default
  [_game-state precond]
  {:satisfied false
   :details (str "Unknown precondition type: " (:type precond))})

;;; ---------------------------------------------------------------------------
;;; PUZZLE LIBRARY
;;; ---------------------------------------------------------------------------
;;; Each puzzle defines:
;;; - :id - Unique identifier
;;; - :description - Human-readable description
;;; - :preconditions - What must be true before attempting
;;; - :postconditions - What will be true after success
;;; - :execution-location - Where the puzzle must be solved
;;; - :required-items - Items needed (subset of preconditions for convenience)
;;; - :steps - Action sequence to solve
;;; - :unlocks - What becomes accessible after solving

(def puzzle-library
  {;;; =========================================================================
   ;;; EXORCISM PUZZLE
   ;;; =========================================================================
   ;;; The exorcism requires a specific sequence: ring bell (sets xb, drops
   ;;; candles), pick up candles, light match, light candles with match (sets
   ;;; xc), read book (sets lld-flag). Must be done at entrance-to-hades.

   :exorcism
   {:id :exorcism
    :description "Banish the spirits to access the Land of the Living Dead"

    :preconditions
    [{:type :object-accessible :object :brass-bell}
     {:type :object-accessible :object :candles}
     {:type :object-accessible :object :matchbook}
     {:type :object-accessible :object :black-book}
     {:type :flag-set :flag :troll-flag}  ; Need to pass troll
     {:type :can-reach :room :entrance-to-hades}
     {:type :flag-not-set :flag :lld-flag}]

    :postconditions
    [{:type :flag-set :flag :lld-flag}]

    :execution-location :entrance-to-hades

    :required-items [:brass-bell :candles :matchbook :black-book]

    :steps
    [;; Ring the bell - sets xb, drops candles if held, swaps bell for hot-bell
     {:action {:verb :ring :direct-object :brass-bell}
      :description "Ring the bell to begin exorcism"
      :expect {:flags-set #{:xb}}}

     ;; Pick up candles (they were dropped by ringing bell)
     {:action {:verb :take :direct-object :candles}
      :description "Pick up the dropped candles"
      :condition {:type :object-not-held :object :candles}}

     ;; Light a match - needed to light candles
     {:action {:verb :lamp-on :direct-object :matchbook}
      :description "Light a match to provide flame"}

     ;; Light the candles - sets xc when xb is true (match auto-used as flame)
     {:action {:verb :lamp-on :direct-object :candles}
      :description "Light the candles"
      :expect {:flags-set #{:xc}}}

     ;; Read the book - completes exorcism, sets lld-flag
     {:action {:verb :read :direct-object :black-book}
      :description "Read from the book to complete exorcism"
      :expect {:flags-set #{:lld-flag}}}]

    :unlocks [:land-of-living-dead :crystal-skull]}

   ;;; =========================================================================
   ;;; TROLL BATTLE
   ;;; =========================================================================
   ;;; Combat is non-deterministic. Must attack until troll is dead.

   :troll-battle
   {:id :troll-battle
    :description "Defeat the troll to pass through the troll room"

    :preconditions
    [{:type :object-held :object :sword}
     {:type :can-reach :room :troll-room}
     {:type :flag-not-set :flag :troll-flag}]

    :postconditions
    [{:type :flag-set :flag :troll-flag}]

    :execution-location :troll-room

    :required-items [:sword]

    :steps
    [;; Attack troll repeatedly until dead
     ;; NOTE: 50 attempts to handle bad RNG - typical kill is 1-5 attempts
     {:action {:verb :attack :direct-object :troll :indirect-object :sword}
      :description "Attack the troll with the sword"
      :repeat-until {:type :flag-set :flag :troll-flag}
      :max-attempts 50}]

    :unlocks [:east-west-passage :round-room]}

   ;;; =========================================================================
   ;;; CYCLOPS PUZZLE
   ;;; =========================================================================
   ;;; Feed the cyclops peppers, then say "ulysses" to make him flee.

   :cyclops-puzzle
   {:id :cyclops-puzzle
    :description "Get past the cyclops by feeding him and invoking Ulysses"

    :preconditions
    [{:type :object-accessible :object :lunch}  ; Contains peppers
     {:type :flag-set :flag :troll-flag}  ; Need to pass troll first
     {:type :can-reach :room :cyclops-room}
     {:type :flag-not-set :flag :cyclops-flag}]

    :postconditions
    [{:type :flag-set :flag :cyclops-flag}
     {:type :flag-set :flag :magic-flag}]  ; Opens strange passage

    :execution-location :cyclops-room

    :required-items [:lunch]

    :steps
    [{:action {:verb :give :direct-object :lunch :indirect-object :cyclops}
      :description "Give the lunch to the cyclops"}
     {:action {:verb :odysseus}
      :description "Say 'Ulysses' to frighten the cyclops"
      :expect {:flags-set #{:cyclops-flag :magic-flag}}}]

    :unlocks [:treasure-room :strange-passage]}

   ;;; =========================================================================
   ;;; RAINBOW PUZZLE
   ;;; =========================================================================
   ;;; Wave the sceptre at the rainbow to make it solid.

   :rainbow-solid
   {:id :rainbow-solid
    :description "Make the rainbow solid to cross it"

    :preconditions
    [{:type :object-held :object :sceptre}
     {:type :flag-set :flag :troll-flag}  ; Need to pass troll (sceptre is in coffin underground)
     {:type :flag-not-set :flag :rainbow-flag}]

    :postconditions
    [{:type :flag-set :flag :rainbow-flag}]

    ;; Wave sceptre at end-of-rainbow (reachable via canyon/cliffs, no boat needed)
    :execution-location :end-of-rainbow

    :required-items [:sceptre]

    :steps
    [{:action {:verb :wave :direct-object :sceptre}
      :description "Wave the sceptre at the rainbow"
      :expect {:flags-set #{:rainbow-flag}}}]

    :unlocks [:pot-of-gold :on-the-rainbow]}

   ;;; =========================================================================
   ;;; LOUD ROOM PUZZLE
   ;;; =========================================================================
   ;;; Say "echo" in the loud room to quiet it and make bar takeable.

   :loud-room-echo
   {:id :loud-room-echo
    :description "Quiet the loud room to take the platinum bar"

    :preconditions
    [{:type :flag-set :flag :troll-flag}  ; Need to pass troll
     {:type :can-reach :room :loud-room}
     {:type :flag-not-set :flag :loud-flag}]

    :postconditions
    [{:type :flag-set :flag :loud-flag}]

    :execution-location :loud-room

    :required-items []

    :steps
    [{:action {:verb :echo}
      :description "Say 'echo' to quiet the room"
      :expect {:flags-set #{:loud-flag}}}]

    :unlocks [:platinum-bar]}

   ;;; =========================================================================
   ;;; DOME ROPE PUZZLE
   ;;; =========================================================================
   ;;; Tie rope to railing to enable descent to torch room.

   :dome-rope
   {:id :dome-rope
    :description "Tie rope to railing to descend from dome"

    :preconditions
    [{:type :object-held :object :rope}
     {:type :flag-set :flag :troll-flag}  ; Need to pass troll
     {:type :can-reach :room :dome-room}
     {:type :flag-not-set :flag :dome-flag}]

    :postconditions
    [{:type :flag-set :flag :dome-flag}]

    :execution-location :dome-room

    :required-items [:rope]

    :steps
    [{:action {:verb :tie :direct-object :rope :indirect-object :railing}
      :description "Tie the rope to the railing"
      :expect {:flags-set #{:dome-flag}}}]

    :unlocks [:torch-room]}

   ;;; =========================================================================
   ;;; DAM PUZZLE
   ;;; =========================================================================
   ;;; Turn the bolt with the wrench to open/close the dam gates.

   :dam-open
   {:id :dam-open
    :description "Open the dam gates to drain the reservoir"

    :preconditions
    [{:type :object-held :object :wrench}
     {:type :flag-set :flag :troll-flag}  ; Need to pass troll to reach dam
     {:type :can-reach :room :dam-room}
     {:type :flag-not-set :flag :gates-open}]

    :postconditions
    [{:type :flag-set :flag :gates-open}
     {:type :flag-set :flag :gate-flag}]

    :execution-location :dam-room

    :required-items [:wrench]

    :steps
    [;; First push yellow button to set gate-flag (enables bolt turning)
     ;; Note: PUSH maps to :move verb in the game
     {:action {:verb :move :direct-object :yellow-button}
      :description "Push yellow button to enable gate mechanism"}
     ;; Now turn the bolt to open gates
     {:action {:verb :turn :direct-object :bolt :indirect-object :wrench}
      :description "Turn the bolt with the wrench to open gates"}]

    :unlocks [:low-tide :trunk-of-jewels]}

   ;;; =========================================================================
   ;;; GRATING PUZZLE
   ;;; =========================================================================
   ;;; Unlock the grating with the keys.

   :grating-unlock
   {:id :grating-unlock
    :description "Unlock the grating to access the underground"

    :preconditions
    [{:type :object-held :object :skeleton-key}
     {:type :can-reach :room :grating-clearing}]

    :postconditions
    [{:type :object-flag :object :grate :flag :open}]

    :execution-location :grating-clearing

    :required-items [:skeleton-key]

    :steps
    [{:action {:verb :unlock :direct-object :grate :indirect-object :skeleton-key}
      :description "Unlock the grating with the skeleton key"}
     {:action {:verb :open :direct-object :grate}
      :description "Open the grating"}]

    :unlocks [:grating-room]}

   ;;; =========================================================================
   ;;; TRAP DOOR PUZZLE
   ;;; =========================================================================
   ;;; Move the rug and open the trap door.

   :trap-door-open
   {:id :trap-door-open
    :description "Reveal and open the trap door in the living room"

    :preconditions
    [{:type :can-reach :room :living-room}
     {:type :flag-not-set :flag :rug-moved}]

    :postconditions
    [{:type :flag-set :flag :rug-moved}
     {:type :object-flag :object :trap-door :flag :open}]

    :execution-location :living-room

    :required-items []

    :steps
    [{:action {:verb :move :direct-object :rug}
      :description "Move the rug to reveal trap door"
      :expect {:flags-set #{:rug-moved}}}
     {:action {:verb :open :direct-object :trap-door}
      :description "Open the trap door"}]

    :unlocks [:cellar]}

   ;;; =========================================================================
   ;;; COAL MINE PUZZLE
   ;;; =========================================================================
   ;;; Navigate the coal mine with proper light source.

   :coal-mine-navigation
   {:id :coal-mine-navigation
    :description "Navigate the coal mine safely"

    :preconditions
    [{:type :object-accessible :object :brass-lantern}
     {:type :can-reach :room :coal-mine-1}]

    :postconditions
    [{:type :at-location :room :coal-mine-4}]  ; Reached deepest coal mine

    :execution-location :coal-mine-1

    :required-items [:brass-lantern]

    ;; Coal mine navigation is complex - this is simplified
    :steps
    [{:action {:verb :lamp-off :direct-object :brass-lantern}
      :description "Turn off lantern (gas danger with open flame)"}]

    :unlocks [:coal-mine-access :coal]}

   ;;; =========================================================================
   ;;; COFFIN CURE PUZZLE
   ;;; =========================================================================
   ;;; Put the coffin in the case without the torch going out.

   :coffin-cure
   {:id :coffin-cure
    :description "Safely store the coffin (avoid torch extinguishing)"

    :preconditions
    [{:type :object-held :object :gold-coffin}
     {:type :can-reach :room :living-room}]

    :postconditions
    [{:type :flag-set :flag :coffin-cure}]

    :execution-location :living-room

    :required-items [:gold-coffin]

    :steps
    [{:action {:verb :put :direct-object :gold-coffin :indirect-object :trophy-case}
      :description "Put the coffin in the trophy case"}]

    :unlocks [:egyptian-treasures]}

   ;;; =========================================================================
   ;;; BOAT PUZZLE
   ;;; =========================================================================
   ;;; Inflate the boat with pump, board it, and launch into the river.

   :boat-ready
   {:id :boat-ready
    :description "Prepare the boat for river navigation"

    :preconditions
    [{:type :object-accessible :object :inflatable-boat}
     {:type :object-accessible :object :pump}
     {:type :flag-set :flag :troll-flag}  ; Need to pass troll to reach dam
     {:type :can-reach :room :dam-base}]

    :postconditions
    [{:type :player-in-vehicle :vehicle :inflated-boat}
     {:type :at-location :room :river-1}]

    :execution-location :dam-base

    :required-items [:pump]

    :steps
    [{:action {:verb :inflate :direct-object :inflatable-boat :indirect-object :pump}
      :description "Inflate the boat with the pump"}
     {:action {:verb :board :direct-object :inflated-boat}
      :description "Board the inflated boat"}
     {:action {:verb :launch}
      :description "Launch the boat into the river"}]

    :unlocks [:river-access :frigid-river-treasures :sandy-beach :shore]}

   ;;; =========================================================================
   ;;; THIEF BATTLE
   ;;; =========================================================================
   ;;; Combat with the thief is different from troll - he's tougher and
   ;;; carries stolen treasure. Killing him drops his loot.

   :thief-battle
   {:id :thief-battle
    :description "Defeat the thief to recover stolen treasures and open the egg"

    :preconditions
    [{:type :object-held :object :sword}
     {:type :can-reach :room :treasure-room}]  ; Thief's lair is treasure-room

    :postconditions
    [{:type :object-accessible :object :stiletto}   ; Thief drops stiletto when dead
     {:type :flag-set :flag :egg-opened}]           ; Egg opens when thief dies with it

    :execution-location :treasure-room  ; Thief fights to death here

    :required-items [:sword]

    :steps
    [{:action {:verb :attack :direct-object :thief :indirect-object :sword}
      :description "Attack the thief with the sword"
      :repeat-until {:type :flag-set :flag :egg-opened}  ; Thief opens egg when he dies
      :max-attempts 30}]  ; Thief is tougher than troll

    :unlocks [:clockwork-canary :treasure-room-access]}

   ;;; =========================================================================
   ;;; MIRROR TELEPORT
   ;;; =========================================================================
   ;;; Looking at the mirror in mirror-room-1 teleports to mirror-room-2
   ;;; and vice versa. Also swaps objects between rooms.

   :mirror-teleport
   {:id :mirror-teleport
    :description "Use the mirror to teleport between rooms"

    :preconditions
    [{:type :can-reach :room :mirror-room-1}]

    :postconditions
    [{:type :at-location :room :mirror-room-2}]  ; Player ends up in mirror-room-2

    ;; Can be done from either mirror room
    :execution-location :mirror-room-1

    :required-items []

    :steps
    [{:action {:verb :look :direct-object :mirror-1}
      :description "Look into the mirror to teleport"}]

    :unlocks [:mirror-room-2 :short-route-to-treasure-room]}

   ;;; =========================================================================
   ;;; EGG OPENING
   ;;; =========================================================================
   ;;; The jeweled egg must be opened carefully. Using wrong tools breaks it.
   ;;; The only way to safely open the egg is via the thief. When the thief
   ;;; dies, he deposits all his loot including the egg, and opens the egg
   ;;; in the process. This puzzle combines giving the egg to the thief
   ;;; and killing him.

   :egg-opening
   {:id :egg-opening
    :description "Open the jeweled egg via the thief"

    :preconditions
    [{:type :object-held :object :egg}
     {:type :object-held :object :sword}  ; Need weapon to kill thief
     {:type :flag-set :flag :cyclops-flag}  ; Need to pass cyclops to reach treasure room
     {:type :can-reach :room :treasure-room}]

    :postconditions
    [{:type :flag-set :flag :egg-opened}
     {:type :object-flag :object :egg :flag :open}
     {:type :object-accessible :object :clockwork-canary}]

    ;; Must be at treasure room - thief is always there when player enters
    :execution-location :treasure-room

    :required-items [:egg :sword]

    ;; Give egg to thief, then kill thief - thief opens egg when he dies
    :steps
    [{:action {:verb :give :direct-object :egg :indirect-object :thief}
      :description "Give the egg to the thief"}
     {:action {:verb :attack :direct-object :thief :indirect-object :sword}
      :description "Attack the thief with the sword"
      :repeat-until {:type :flag-set :flag :egg-opened}
      :max-attempts 30}]

    :unlocks [:canary :bauble :clockwork-canary]}

   ;;; =========================================================================
   ;;; WIND CANARY (BAUBLE)
   ;;; =========================================================================
   ;;; Wind the clockwork canary in the forest to summon the songbird,
   ;;; which drops the brass bauble.

   :wind-canary
   {:id :wind-canary
    :description "Wind the canary in the forest to get the bauble"

    :preconditions
    [{:type :object-held :object :clockwork-canary}
     {:type :flag-not-set :flag :canary-sung}]

    :postconditions
    [{:type :flag-set :flag :canary-sung}
     {:type :object-at :object :brass-bauble :room :forest-path}]

    :execution-location :forest-path

    :required-items [:clockwork-canary]

    :steps
    [{:action {:verb :wind :direct-object :clockwork-canary}
      :description "Wind the clockwork canary to summon the songbird"}]

    :unlocks [:brass-bauble]}

   ;;; =========================================================================
   ;;; KITCHEN WINDOW
   ;;; =========================================================================
   ;;; Open the kitchen window to access the house from behind.

   :kitchen-window
   {:id :kitchen-window
    :description "Open the kitchen window for house access"

    :preconditions
    [{:type :can-reach :room :behind-house}]

    :postconditions
    [{:type :object-flag :object :kitchen-window :flag :open}
     {:type :at-location :room :kitchen}]

    :execution-location :behind-house

    :required-items []

    :steps
    [{:action {:verb :open :direct-object :kitchen-window}
      :description "Open the kitchen window"}
     {:action {:verb :enter}
      :description "Climb through the window"}]

    :unlocks [:kitchen :house-access :living-room]}

   ;;; =========================================================================
   ;;; COAL TO DIAMOND PUZZLE
   ;;; =========================================================================
   ;;; Put coal in the machine and turn the switch to compress it into a diamond.

   :coal-to-diamond
   {:id :coal-to-diamond
    :description "Convert coal into a diamond using the machine"

    :preconditions
    [{:type :object-accessible :object :coal}
     {:type :can-reach :room :machine-room}
     {:type :object-accessible :object :screwdriver}]  ; Need to turn switch

    :postconditions
    [{:type :flag-set :flag :coal-machine-used}
     {:type :object-accessible :object :huge-diamond}]

    :execution-location :machine-room

    :required-items [:coal :screwdriver]

    :steps
    [{:action {:verb :open :direct-object :machine}
      :description "Open the machine lid"}
     {:action {:verb :put :direct-object :coal :indirect-object :machine}
      :description "Put the coal in the machine"}
     {:action {:verb :close :direct-object :machine}
      :description "Close the machine lid"}
     {:action {:verb :turn :direct-object :machine-switch :indirect-object :screwdriver}
      :description "Turn the switch with the screwdriver to activate the machine"}]

    :unlocks [:huge-diamond]}

   ;;; =========================================================================
   ;;; RESERVOIR ACCESS (LOW TIDE)
   ;;; =========================================================================
   ;;; After opening dam gates, wait for water to drain to access reservoir floor.

   :reservoir-low-tide
   {:id :reservoir-low-tide
    :description "Wait for reservoir to drain after opening dam"

    :preconditions
    [{:type :flag-set :flag :gates-open}  ; Dam must be open first
     {:type :flag-set :flag :troll-flag}  ; Need to pass troll
     {:type :can-reach :room :reservoir-north}]

    :postconditions
    [{:type :flag-set :flag :low-tide}]

    :execution-location :reservoir-north

    :required-items []

    :steps
    [;; Just need to wait - the dam daemon handles draining
     ;; Dam takes 8 turns to drain, so allow up to 15 waits for safety
     {:action {:verb :wait}
      :description "Wait for water to drain"
      :repeat-until {:type :flag-set :flag :low-tide}
      :max-attempts 15}]

    :unlocks [:reservoir-bottom :trunk-of-jewels :atlantis-room]}

   ;;; =========================================================================
   ;;; SANDY BEACH LANDING
   ;;; =========================================================================
   ;;; Navigate river and land at sandy beach.

   :sandy-beach-landing
   {:id :sandy-beach-landing
    :description "Navigate the river and land at sandy beach"

    :preconditions
    [{:type :flag-set :flag :boat-ready}  ; Need boat prepared first
     {:type :can-reach :room :river-4}]    ; Need to be far enough downriver

    :postconditions
    [{:type :at-location :room :sandy-beach}]

    :execution-location :river-4  ; Near sandy beach landing point

    :required-items []

    :steps
    [{:action {:verb :land}
      :description "Land the boat at sandy beach"}]

    :unlocks [:sandy-beach :scarab :shovel]}

   ;;; =========================================================================
   ;;; SHORE LANDING
   ;;; =========================================================================
   ;;; Navigate river and land at shore.

   :shore-landing
   {:id :shore-landing
    :description "Navigate the river and land at shore"

    :preconditions
    [{:type :flag-set :flag :boat-ready}
     {:type :can-reach :room :river-5}]

    :postconditions
    [{:type :at-location :room :shore}]

    :execution-location :river-5

    :required-items []

    :steps
    [{:action {:verb :land}
      :description "Land the boat at shore"}]

    :unlocks [:shore :aragain-falls]}

   ;;; =========================================================================
   ;;; BUOY COLLECTION
   ;;; =========================================================================
   ;;; Take and open the buoy at river-4 to get the emerald.
   ;;; Must be done while in boat on the river.

   :buoy-collection
   {:id :buoy-collection
    :description "Take and open the buoy to get the emerald"

    :preconditions
    [{:type :flag-set :flag :boat-ready}
     {:type :at-location :room :river-4}]

    :postconditions
    [{:type :object-held :object :large-emerald}]

    :execution-location :river-4

    :required-items []

    :steps
    [{:action {:verb :take :direct-object :buoy}
      :description "Take the buoy from the water"}
     {:action {:verb :open :direct-object :buoy}
      :description "Open the buoy"}
     {:action {:verb :take :direct-object :large-emerald}
      :description "Take the emerald from the buoy"}]

    :unlocks [:large-emerald]}

   ;;; =========================================================================
   ;;; GAS ROOM ACCESS
   ;;; =========================================================================
   ;;; Safely access the gas room - must turn off flame sources.

   :gas-room-access
   {:id :gas-room-access
    :description "Safely enter the gas room"

    :preconditions
    [{:type :can-reach :room :smelly-room}
     {:type :object-accessible :object :brass-lantern}]

    :postconditions
    [{:type :at-location :room :gas-room}]

    :execution-location :smelly-room

    :required-items [:brass-lantern]

    ;; Must turn off light sources before entering gas room
    :steps
    [{:action {:verb :lamp-off :direct-object :brass-lantern}
      :description "Turn off the lantern (gas danger with flame)"}
     {:action {:verb :go :direct-object :down}
      :description "Go down to the gas room"}]

    :unlocks [:gas-room :sapphire-bracelet]}})

;;; ---------------------------------------------------------------------------
;;; PUZZLE QUERY FUNCTIONS
;;; ---------------------------------------------------------------------------

(defn get-puzzle
  "Get a puzzle by ID."
  [puzzle-id]
  (get puzzle-library puzzle-id))

(defn all-puzzles
  "Get all puzzle IDs."
  []
  (keys puzzle-library))

(defn puzzles-for-flag
  "Find puzzles that set a given flag."
  [flag]
  (->> puzzle-library
       (filter (fn [[_id puzzle]]
                 (some #(and (= (:type %) :flag-set)
                             (= (:flag %) flag))
                       (:postconditions puzzle))))
       (map first)))

(defn puzzles-unlocking
  "Find puzzles that unlock a given thing (room or object)."
  [thing]
  (->> puzzle-library
       (filter (fn [[_id puzzle]]
                 (some #{thing} (:unlocks puzzle))))
       (map first)))

(defn puzzle-items-needed
  "Get items required for a puzzle."
  [puzzle-id]
  (:required-items (get-puzzle puzzle-id)))

;;; ---------------------------------------------------------------------------
;;; PUZZLE PRECONDITION ANALYSIS
;;; ---------------------------------------------------------------------------

(defn check-puzzle-preconditions
  "Check all preconditions for a puzzle.
   Returns {:can-solve bool :preconditions [{:precond ... :result ...}]}"
  [game-state puzzle-id]
  (let [puzzle (get-puzzle puzzle-id)
        results (for [precond (:preconditions puzzle)]
                  {:precond precond
                   :result (check-puzzle-precond game-state precond)})]
    {:can-solve (every? #(get-in % [:result :satisfied]) results)
     :preconditions results
     :missing-items (->> results
                         (filter #(and (= :object-accessible (get-in % [:precond :type]))
                                       (not (get-in % [:result :satisfied]))))
                         (map #(get-in % [:precond :object])))}))

(defn can-solve-puzzle?
  "Check if puzzle preconditions are met."
  [game-state puzzle-id]
  (:can-solve (check-puzzle-preconditions game-state puzzle-id)))

(defn puzzle-blockers
  "Get what's blocking a puzzle from being solvable."
  [game-state puzzle-id]
  (let [check (check-puzzle-preconditions game-state puzzle-id)]
    (->> (:preconditions check)
         (filter #(not (get-in % [:result :satisfied])))
         (map (fn [{:keys [precond result]}]
                {:precondition precond
                 :reason (:details result)})))))

;;; ---------------------------------------------------------------------------
;;; PUZZLE EXECUTION
;;; ---------------------------------------------------------------------------

(defn- step-condition-met?
  "Check if a step's condition is met (for conditional steps)."
  [game-state condition]
  (case (:type condition)
    :object-not-held
    (not= :adventurer (gs/get-thing-loc-id game-state (:object condition)))

    :object-held
    (= :adventurer (gs/get-thing-loc-id game-state (:object condition)))

    :object-accessible
    (let [loc (gs/get-thing-loc-id game-state (:object condition))
          here (:here game-state)]
      (or (= loc :adventurer) (= loc here)))

    :flag-set
    (gs/game-flag? game-state (:flag condition))

    :flag-not-set
    (not (gs/game-flag? game-state (:flag condition)))

    :player-alive
    (not (gs/game-flag? game-state :dead))

    ;; Default: condition met
    true))

(defn- verify-expectations
  "Verify that expected outcomes occurred after a step.
   Returns {:verified bool :failures [...]}"
  [game-state expect]
  (if (nil? expect)
    {:verified true :failures []}
    (let [failures
          (concat
            ;; Check flags that should be set
            (for [flag (:flags-set expect #{})
                  :when (not (gs/game-flag? game-state flag))]
              {:expected :flag-set :flag flag :actual :not-set})

            ;; Check flags that should be cleared
            (for [flag (:flags-cleared expect #{})
                  :when (gs/game-flag? game-state flag)]
              {:expected :flag-cleared :flag flag :actual :still-set})

            ;; Check objects that should be at specific locations
            (for [[obj expected-loc] (:objects-at expect {})
                  :let [actual-loc (gs/get-thing-loc-id game-state obj)]
                  :when (not= actual-loc expected-loc)]
              {:expected :object-at :object obj :location expected-loc :actual actual-loc}))]
      {:verified (empty? failures)
       :failures (vec failures)})))

(defn- player-alive?
  "Check if player is still alive."
  [game-state]
  (not (gs/game-flag? game-state :dead)))

(defn- execute-step
  "Execute a single puzzle step.
   Returns {:success bool :game-state gs :output string :error string :attempts int}"
  [game-state step]
  (let [{:keys [action condition repeat-until max-attempts expect]} step]
    (cond
      ;; Conditional step - skip if condition not met
      (and condition (not (step-condition-met? game-state condition)))
      {:success true :game-state game-state :output "Step skipped (condition not met)" :attempts 0}

      ;; Repeat-until step (for combat)
      repeat-until
      (loop [gs game-state
             attempts 0]
        (cond
          ;; Max attempts reached
          (>= attempts (or max-attempts 20))
          {:success false :game-state gs :error "Max attempts reached" :attempts attempts}

          ;; Player died during combat
          (not (player-alive? gs))
          {:success false :game-state gs :error "Player died during combat" :attempts attempts}

          ;; For combat: check if weapon still held (if attacking)
          (and (= (:verb action) :attack)
               (:indirect-object action)
               (not= :adventurer (gs/get-thing-loc-id gs (:indirect-object action))))
          {:success false :game-state gs :error "Weapon lost during combat" :attempts attempts}

          :else
          (let [result (transition/step gs action)
                new-gs (:game-state result)]
            (if (step-condition-met? new-gs repeat-until)
              {:success true :game-state new-gs :output (:output result) :attempts (inc attempts)}
              (recur new-gs (inc attempts))))))

      ;; Normal step with optional verification
      :else
      (let [result (transition/step game-state action)
            new-gs (:game-state result)
            verification (verify-expectations new-gs expect)]
        (if (:verified verification)
          {:success true :game-state new-gs :output (:output result) :attempts 1}
          {:success false
           :game-state new-gs
           :output (:output result)
           :error "Step did not produce expected results"
           :verification-failures (:failures verification)
           :attempts 1})))))

(defn execute-puzzle
  "Execute a puzzle's steps, verifying each step's effects.
   Returns {:success bool :game-state gs :steps-executed n :error string}"
  [game-state puzzle-id]
  (let [puzzle (get-puzzle puzzle-id)
        exec-loc (:execution-location puzzle)
        here (:here game-state)]
    (cond
      ;; Not at execution location
      (and exec-loc (not= here exec-loc))
      {:success false
       :game-state game-state
       :error (str "Must be at " (name exec-loc) " to execute puzzle (currently at " (name here) ")")}

      ;; Preconditions not met
      (not (can-solve-puzzle? game-state puzzle-id))
      {:success false
       :game-state game-state
       :error "Puzzle preconditions not met"
       :blockers (puzzle-blockers game-state puzzle-id)}

      ;; Execute steps
      :else
      (loop [gs game-state
             steps (:steps puzzle)
             executed 0]
        (if (empty? steps)
          {:success true :game-state gs :steps-executed executed}
          (let [step (first steps)
                result (execute-step gs step)]
            (if (:success result)
              (recur (:game-state result) (rest steps) (inc executed))
              {:success false
               :game-state (:game-state result)
               :steps-executed executed
               :error (:error result)
               :failed-step step})))))))

(defn verify-postconditions
  "Verify that puzzle postconditions are satisfied after execution."
  [game-state puzzle-id]
  (let [puzzle (get-puzzle puzzle-id)
        results (for [postcond (:postconditions puzzle)]
                  {:postcond postcond
                   :result (check-puzzle-precond game-state postcond)})]
    {:all-satisfied (every? #(get-in % [:result :satisfied]) results)
     :postconditions results}))

;;; ---------------------------------------------------------------------------
;;; PUZZLE PLANNING SUPPORT
;;; ---------------------------------------------------------------------------

(defn puzzles-needed-for-flag
  "Find what puzzles are needed to achieve a flag.
   Returns a dependency chain."
  [game-state flag]
  (let [direct-puzzles (puzzles-for-flag flag)]
    (if (seq direct-puzzles)
      (for [puzzle-id direct-puzzles]
        {:puzzle puzzle-id
         :can-solve (can-solve-puzzle? game-state puzzle-id)
         :blockers (puzzle-blockers game-state puzzle-id)})
      [{:no-puzzle-found true
        :flag flag
        :suggestion "This flag may be set by a non-puzzle action"}])))

(defn puzzle-dependency-order
  "Given a set of puzzles, determine execution order based on dependencies.
   Returns puzzles in order (independent first, dependent last)."
  [game-state puzzle-ids]
  ;; Simple topological sort based on flag dependencies
  (let [puzzles (map get-puzzle puzzle-ids)
        ;; Build dependency graph: puzzle A depends on B if A needs a flag B sets
        deps (for [puzzle puzzles
                   precond (:preconditions puzzle)
                   :when (= :flag-set (:type precond))
                   :let [needed-flag (:flag precond)
                         providers (puzzles-for-flag needed-flag)]
                   provider providers
                   :when (contains? (set puzzle-ids) provider)]
               [(:id puzzle) provider])]
    ;; Simple ordering: puzzles with no deps first
    (let [has-deps (set (map first deps))
          no-deps (remove has-deps puzzle-ids)
          with-deps (filter has-deps puzzle-ids)]
      (concat no-deps with-deps))))

(defn route-to-puzzle
  "Generate route to a puzzle's execution location."
  [game-state puzzle-id]
  (let [puzzle (get-puzzle puzzle-id)
        exec-loc (:execution-location puzzle)
        here (:here game-state)
        flags (routing/extract-available-flags game-state)]
    (when (and exec-loc (not= here exec-loc))
      (routing/shortest-path game-state here exec-loc :available-flags flags))))

;;; ---------------------------------------------------------------------------
;;; DEBUG / INSPECTION
;;; ---------------------------------------------------------------------------

(defn describe-puzzle
  "Get a human-readable description of a puzzle."
  [puzzle-id]
  (when-let [puzzle (get-puzzle puzzle-id)]
    (str (:description puzzle) "\n"
         "Location: " (name (:execution-location puzzle)) "\n"
         "Items needed: " (if (seq (:required-items puzzle))
                           (clojure.string/join ", " (map name (:required-items puzzle)))
                           "none") "\n"
         "Unlocks: " (clojure.string/join ", " (map name (:unlocks puzzle))))))

(defn puzzle-status
  "Get current status of a puzzle for debugging."
  [game-state puzzle-id]
  (let [puzzle (get-puzzle puzzle-id)
        check (check-puzzle-preconditions game-state puzzle-id)
        post-check (verify-postconditions game-state puzzle-id)]
    {:puzzle-id puzzle-id
     :description (:description puzzle)
     :can-solve (:can-solve check)
     :already-solved (:all-satisfied post-check)
     :blockers (when-not (:can-solve check)
                 (puzzle-blockers game-state puzzle-id))
     :missing-items (:missing-items check)}))
