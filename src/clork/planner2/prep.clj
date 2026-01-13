(ns clork.planner2.prep
  "Prep Actions Catalog for the Prep-Optimized Planner.

   Prep actions are one-time actions that permanently change game state
   to enable future actions. This catalog documents all such actions
   with their requirements and what they enable.

   See IMPLEMENTATION_PLAN.md for the full architecture.")

;;; ---------------------------------------------------------------------------
;;; PREP ACTIONS CATALOG
;;; ---------------------------------------------------------------------------
;;;
;;; Each prep action has:
;;;   :description  - Human-readable explanation
;;;   :location     - Where the action must be performed
;;;   :requires     - Set of items and/or flags required
;;;   :action       - The action to perform (keyword for special, map for verb)
;;;   :enables      - Set of regions/items/flags unlocked by this action
;;;   :delay        - Optional turns until effect takes place
;;;   :permanent    - If true, cannot be undone (default: true)

(def prep-actions
  "Complete catalog of one-time state-changing actions in Zork I."

  {;;; =========================================================================
   ;;; COMBAT PREP ACTIONS
   ;;; =========================================================================

   :troll-flag
   {:description "Kill troll to access underground passages"
    :location :troll-room
    :requires #{:sword :brass-lantern}  ; need light and weapon
    :action :combat                      ; special handler for combat
    :target :troll
    :enables #{;; Immediate access beyond troll room
               :east-west-passage
               :round-room
               :engravings-cave
               :dome-room
               :north-south-passage
               :chasm-room
               :reservoir-south
               :dam-room
               :dam-base
               :maintenance-room
               :cyclops-room
               :loud-room
               :damp-cave
               :deep-canyon
               ;; Maze access
               :maze-1 :maze-2 :maze-3 :maze-4 :maze-5
               :maze-6 :maze-7 :maze-8 :maze-9 :maze-10
               :maze-11 :maze-12 :maze-13 :maze-14 :maze-15
               :dead-end-1 :dead-end-2 :dead-end-3 :dead-end-4
               :grating-room
               ;; Coal mine access
               :smelly-room :gas-room :coal-mine-1 :coal-mine-2
               :coal-mine-3 :coal-mine-4 :ladder-top :ladder-bottom
               :dead-end-coal-mine :timber-room :drafty-room
               :machine-room :shaft-room
               ;; Other areas
               :mirror-room-1 :mirror-room-2
               :tiny-cave :atlantis-room :narrow-passage
               :small-cave :twisting-passage :egypt-room
               :north-temple :south-temple :entrance-to-hades
               :land-of-the-dead :altar}}

   :thief-flag
   {:description "Kill thief to access his treasures"
    :location :thiefs-lair  ; or anywhere thief appears
    :requires #{:sword}
    :action :combat
    :target :thief
    :enables #{:trunk          ; thief's treasure bag
               :clockwork-canary}} ; thief opens egg

   ;;; =========================================================================
   ;;; PUZZLE PREP ACTIONS - Immediate Effect
   ;;; =========================================================================

   :rug-moved
   {:description "Move rug to reveal trap door"
    :location :living-room
    :requires #{}
    :action {:verb :move :direct-object :rug}
    :enables #{:trap-door-visible}}

   :trap-door-open
   {:description "Open trap door to access cellar"
    :location :living-room
    :requires #{:rug-moved}  ; must move rug first
    :action {:verb :open :direct-object :trap-door}
    :enables #{:cellar}}

   :kitchen-window-open
   {:description "Open kitchen window for entry"
    :location :behind-house
    :requires #{}
    :action {:verb :open :direct-object :kitchen-window}
    :enables #{:kitchen}}

   :grate-revealed
   {:description "Move leaves to reveal grate"
    :location :grating-clearing
    :requires #{}
    :action {:verb :move :direct-object :leaves}
    :enables #{:grate-visible}}

   :grate-unlocked
   {:description "Unlock grate with skeleton key"
    :location :grating-room  ; from below
    :requires #{:skeleton-key :troll-flag}
    :action {:verb :unlock :direct-object :grate :indirect-object :skeleton-key}
    :enables #{:grate-exit}}  ; can exit maze via grate

   :cyclops-flag
   {:description "Say ULYSSES to scare cyclops to sleep"
    :location :cyclops-room
    :requires #{:troll-flag}  ; must reach cyclops room first
    :action {:verb :say :direct-object :ulysses}
    :enables #{}}  ; just makes cyclops safe, magic-flag unlocks passage

   :magic-flag
   {:description "Cyclops flees, creating passage (triggered by ULYSSES)"
    :location :cyclops-room
    :requires #{:cyclops-flag}  ; set simultaneously with cyclops-flag
    :action :auto  ; automatically set when cyclops-flag is set
    :enables #{:strange-passage
               :treasure-room}}

   :dome-flag
   {:description "Tie rope to railing for descent to torch room"
    :location :dome-room
    :requires #{:rope :troll-flag}
    :action {:verb :tie :direct-object :rope :indirect-object :railing}
    :enables #{:torch-room}}

   :loud-flag
   {:description "Say ECHO to quiet loud room"
    :location :loud-room
    :requires #{:troll-flag}  ; must reach loud room
    :action {:verb :say :direct-object :echo}  ; or just type 'echo' in loud room
    :enables #{:platinum-bar}}  ; bar can be taken without echo causing drop

   :rainbow-flag
   {:description "Wave sceptre at rainbow to make it solid"
    :location #{:end-of-rainbow :aragain-falls}  ; either location works
    :requires #{:sceptre}
    :action {:verb :wave :direct-object :sceptre}
    :enables #{:pot-of-gold     ; becomes visible
               :on-rainbow}}    ; can walk on rainbow

   ;;; =========================================================================
   ;;; DAM PUZZLE - Multi-Step with Delay
   ;;; =========================================================================

   :gate-flag
   {:description "Press yellow button to enable dam bolt"
    :location :maintenance-room
    :requires #{:troll-flag}
    :action {:verb :push :direct-object :yellow-button}
    :enables #{:bolt-turnable}}

   :gates-open
   {:description "Turn bolt with wrench to open sluice gates"
    :location :dam-room
    :requires #{:gate-flag :wrench}
    :action {:verb :turn :direct-object :bolt :indirect-object :wrench}
    :triggers :low-tide  ; will set low-tide after delay
    :delay 8             ; 8 turns until reservoir drains
    :enables #{}}        ; no immediate enables, low-tide triggers later

   :low-tide
   {:description "Reservoir drained (triggered by gates-open after delay)"
    :location nil  ; automatic after delay
    :requires #{:gates-open}
    :action :timed  ; automatically set by game clock
    :delay 8        ; turns after gates-open
    :enables #{:reservoir       ; can walk through
               :reservoir-north ; can cross
               :reservoir-south ; can cross
               :atlantis-room   ; accessible via reservoir
               :small-cave      ; via reservoir path
               :trunk}}         ; trunk visible when tide is low

   ;;; =========================================================================
   ;;; EXORCISM PUZZLE - Multi-Step Sequential
   ;;; =========================================================================

   :bell-rang
   {:description "Ring bell at entrance to Hades"
    :location :entrance-to-hades
    ;; Use actual object IDs: brass-bell, black-book (not generic bell, book)
    :requires #{:brass-bell :candles :black-book :troll-flag}
    :action {:verb :ring :direct-object :brass-bell}
    :enables #{:xb-started}  ; starts exorcism sequence
    :note "Bell becomes hot and drops; pick up candles"}

   :candles-lit
   {:description "Light candles during exorcism"
    :location :entrance-to-hades
    ;; Use actual object ID: matchbook (not generic matches)
    :requires #{:bell-rang :candles :matchbook}
    :action {:verb :light :direct-object :candles :indirect-object :matchbook}
    :enables #{:xc-started}
    :time-limit 6}  ; must do within 6 turns of ringing bell

   :lld-flag  ; "Land of Living Dead" flag
   {:description "Read book to complete exorcism"
    :location :entrance-to-hades
    ;; Use actual object ID: black-book (not generic book)
    :requires #{:candles-lit :black-book}
    :action {:verb :read :direct-object :black-book}
    :enables #{:land-of-the-dead  ; ghosts banished
               :crystal-skull}    ; can be taken
    :time-limit 3}  ; must do within 3 turns of lighting candles

   ;;; =========================================================================
   ;;; TEMPLE/PRAYER TELEPORT
   ;;; =========================================================================

   :coffin-cure
   {:description "Enter temple without coffin (enables prayer teleport)"
    :location :south-temple
    :requires #{:dome-flag}  ; must reach temple via dome/torch path
    :action :enter  ; just entering without coffin sets flag
    :condition {:not-carrying :coffin}
    :enables #{:prayer-teleport}}  ; PRAY will work to teleport out

   :prayer-teleport
   {:description "Pray at altar to teleport to forest"
    :location :south-temple
    :requires #{:coffin-cure}
    :action {:verb :pray}
    :destination :forest-1
    :enables #{}  ; teleport action, doesn't enable regions
    :note "One-way escape from temple area"}

   ;;; =========================================================================
   ;;; COAL MINE / MACHINE PUZZLES
   ;;; =========================================================================

   :coal-in-machine
   {:description "Put coal in machine basket"
    :location :machine-room
    :requires #{:coal :troll-flag}
    :action {:verb :put :direct-object :coal :indirect-object :basket}
    :enables #{}}

   :machine-activated
   {:description "Turn machine switch to create diamond"
    :location :machine-room
    :requires #{:coal-in-machine}
    :action {:verb :turn :direct-object :machine-switch}
    :enables #{:huge-diamond}}  ; coal becomes diamond

   :shaft-basket-lowered
   {:description "Lower basket down shaft"
    :location :shaft-room
    :requires #{:troll-flag}
    :action {:verb :lower :direct-object :basket}
    :enables #{:basket-at-bottom}}

   ;;; =========================================================================
   ;;; BOAT PUZZLES
   ;;; =========================================================================

   :boat-inflated
   {:description "Inflate boat with pump"
    :location #{:dam-base :reservoir}  ; various water locations
    :requires #{:pile-of-plastic :air-pump}
    :action {:verb :inflate :direct-object :pile-of-plastic :indirect-object :air-pump}
    :enables #{:inflated-boat}}

   :boat-repaired
   {:description "Repair punctured boat with putty"
    :location nil  ; wherever boat is
    :requires #{:punctured-boat :putty}
    :action {:verb :put :direct-object :putty :indirect-object :punctured-boat}
    :enables #{:inflatable-boat}}  ; boat becomes usable again

   ;;; =========================================================================
   ;;; MIRROR PUZZLES
   ;;; =========================================================================

   :mirror-room-connection
   {:description "Touch mirrors to teleport between mirror rooms"
    :location #{:mirror-room-1 :mirror-room-2}
    :requires #{:troll-flag}
    :action {:verb :touch :direct-object :mirror}
    :destination :mirror-room-opposite  ; teleports to other mirror room
    :enables #{}  ; teleport action, doesn't enable regions
    :note "Reusable teleport between north and south mirror rooms"}})

;;; ---------------------------------------------------------------------------
;;; TIMED EFFECTS
;;; ---------------------------------------------------------------------------

(def timed-effects
  "Effects that have delays or time limits."

  {:low-tide
   {:trigger :gates-open
    :delay 8  ; turns after triggering
    :effect :low-tide
    :description "Reservoir drains 8 turns after opening gates"}

   :reservoir-refill
   {:trigger :gates-closed  ; closing gates after low tide
    :delay 8
    :effect :reservoir-full
    :description "Reservoir refills 8 turns after closing gates"}

   :candles
   {:type :consumable
    :max-turns 40  ; approximate candle life
    :required-for #{:exorcism :light-in-hades}
    :note "Candles burn out; plan exorcism carefully"}

   :lantern
   {:type :consumable
    :max-turns 200  ; approximate lantern life
    :required-for #{:underground-exploration}
    :note "Lantern battery depletes; optimize underground routing"}

   :exorcism-bell
   {:trigger :bell-rang
    :window 6  ; must light candles within 6 turns
    :description "Bell is hot; must proceed quickly"}

   :exorcism-candles
   {:trigger :candles-lit
    :window 3  ; must read book within 3 turns
    :description "Spirits are weakened; must read book quickly"}

   :maintenance-flood
   {:trigger :blue-button-pressed
    :effect :room-flooding
    :turns-to-death 14  ; room becomes fatal
    :fix {:action {:verb :put :direct-object :putty :indirect-object :leak}}
    :description "Blue button starts flood; fix with putty or escape"}})

;;; ---------------------------------------------------------------------------
;;; ITEM LOCATIONS
;;; ---------------------------------------------------------------------------

(def prep-item-locations
  "Where to find items needed for prep actions."

  {:sword {:location :living-room :container :trophy-case}
   :brass-lantern {:location :living-room :container nil}
   :rope {:location :attic :container nil}
   :wrench {:location :maintenance-room :container nil}
   :skeleton-key {:location :maze-5 :container nil}  ; dead thief drops it
   :sceptre {:location :torch-room :container nil :requires #{:dome-flag}}
   ;; Exorcism items - use actual object IDs
   :brass-bell {:location :north-temple :container nil :requires #{:dome-flag}}
   :black-book {:location :south-temple :container nil :requires #{:dome-flag}}
   :candles {:location :south-temple :container nil :requires #{:dome-flag}}
   :matchbook {:location :dam-lobby :container nil :requires #{:troll-flag}}
   :coal {:location :dead-end-coal-mine :container nil :requires #{:troll-flag}}
   :air-pump {:location :maintenance-room :container nil :requires #{:troll-flag}}
   :pile-of-plastic {:location :dam-base :container nil :requires #{:troll-flag}}
   :putty {:location :maintenance-room :container :tube :requires #{:troll-flag}}})

;;; ---------------------------------------------------------------------------
;;; DEPENDENCY HELPERS
;;; ---------------------------------------------------------------------------

(defn prep-requires
  "Get the set of items and flags required for a prep action."
  [prep-id]
  (get-in prep-actions [prep-id :requires] #{}))

(defn prep-enables
  "Get the set of things enabled by a prep action."
  [prep-id]
  (get-in prep-actions [prep-id :enables] #{}))

(defn prep-location
  "Get the location(s) where a prep action must be performed."
  [prep-id]
  (get-in prep-actions [prep-id :location]))

(defn prep-action
  "Get the action specification for a prep action."
  [prep-id]
  (get-in prep-actions [prep-id :action]))

(defn prep-delay
  "Get the delay (in turns) for a timed prep action, or nil if immediate."
  [prep-id]
  (get-in prep-actions [prep-id :delay]))

(defn timed-prep?
  "Check if a prep action has a delayed effect."
  [prep-id]
  (some? (prep-delay prep-id)))

(defn combat-prep?
  "Check if a prep action requires combat."
  [prep-id]
  (= :combat (prep-action prep-id)))

(defn all-preps
  "Get all prep action IDs."
  []
  (keys prep-actions))

(defn preps-enabling
  "Find all prep actions that enable a given region, item, or flag."
  [target]
  (for [[prep-id info] prep-actions
        :when (contains? (:enables info) target)]
    prep-id))

(defn preps-requiring-flag
  "Find all prep actions that require a given flag."
  [flag]
  (for [[prep-id info] prep-actions
        :when (contains? (:requires info) flag)]
    prep-id))

(defn preps-requiring-item
  "Find all prep actions that require a given item."
  [item]
  (for [[prep-id info] prep-actions
        :when (contains? (:requires info) item)]
    prep-id))

;;; ---------------------------------------------------------------------------
;;; TREASURE PREP REQUIREMENTS
;;; ---------------------------------------------------------------------------

(def treasure-prep-requirements
  "Map of treasures to the prep actions required to obtain them."

  {:egg {}  ; no prep needed, just climb tree
   :painting {:required #{:troll-flag}}
   :bag-of-coins {:required #{:troll-flag}}  ; maze
   :platinum-bar {:required #{:troll-flag :loud-flag}}
   :ivory-torch {:required #{:troll-flag :dome-flag}}
   :sceptre {:required #{:troll-flag :dome-flag}}
   :jade-figurine {:required #{:troll-flag :low-tide}}  ; bat room
   :sapphire-bracelet {:required #{:troll-flag :low-tide}}  ; gas room
   :pot-of-gold {:required #{:rainbow-flag}}  ; need sceptre first
   :crystal-skull {:required #{:troll-flag :dome-flag :lld-flag}}  ; exorcism
   :trunk {:required #{:troll-flag :low-tide :thief-flag}}  ; thief's lair
   :jewel-encrusted-trunk {:required #{:troll-flag :low-tide :thief-flag}}  ; alias for trunk
   :clockwork-canary {:required #{:thief-flag}}  ; thief opens egg
   :gold-coffin {:required #{:troll-flag :dome-flag :coffin-cure}}
   :huge-diamond {:required #{:troll-flag :machine-activated}}
   :silver-chalice {:required #{:troll-flag}}  ; just need to reach it
   :brass-bauble {:required #{:troll-flag}}  ; maze area
   :zorkmid-coins {:required #{:troll-flag}}  ; dead end in maze
   :china-figurine {:required #{:troll-flag}}  ; maze area
   ;; Additional treasures from goals.clj
   :jeweled-scarab {:required #{:troll-flag}}  ; sandy-cave, need shovel
   :large-emerald {:required #{:troll-flag}}  ; buoy in reservoir area
   :crystal-trident {:required #{:troll-flag :low-tide}}})  ; atlantis-room

;;; ---------------------------------------------------------------------------
;;; PREP ACTION ORDERING
;;; ---------------------------------------------------------------------------

(def prep-ordering
  "Suggested order for prep actions in an optimized run.
   Based on geographic proximity and dependency chains."

  [;; Phase 1: Above ground and house entry
   :kitchen-window-open  ; enter house
   :rug-moved            ; reveal trap door
   :trap-door-open       ; access cellar

   ;; Phase 2: Kill troll (unlocks most of underground)
   :troll-flag

   ;; Phase 3: Dam puzzle (start early for parallel work)
   :gate-flag            ; enable bolt
   :gates-open           ; start 8-turn drain timer
   ;; ... do other things while dam drains ...
   ;; :low-tide will be set automatically after 8 turns

   ;; Phase 4: Cyclops puzzle (opens treasure room)
   :cyclops-flag
   ;; :magic-flag set automatically

   ;; Phase 5: Dome/rope puzzle (opens temple area)
   :dome-flag

   ;; Phase 6: Loud room (for platinum bar)
   :loud-flag

   ;; Phase 7: Rainbow (for pot of gold)
   ;; Need sceptre from torch-room first
   :rainbow-flag

   ;; Phase 8: Exorcism (for crystal skull)
   ;; Need bell, book, candles from temple
   :bell-rang
   :candles-lit
   :lld-flag

   ;; Phase 9: Coal mine machine (for diamond)
   :coal-in-machine
   :machine-activated

   ;; Phase 10: Thief (for canary and trunk)
   :thief-flag])
