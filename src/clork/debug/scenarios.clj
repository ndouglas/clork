(ns clork.debug.scenarios
  "Pre-configured game scenarios for testing and debugging.

   These scenarios set up game state to specific checkpoints, making it
   easy to test specific areas without navigating through the entire game.

   Usage in tests:
     (require '[clork.debug.scenarios :as scenarios])
     (let [gs (scenarios/equipped-adventurer :loud-room)]
       ...)

   Usage in REPL:
     (scenarios/start-at :loud-room)  ; starts game at loud room

   Common scenarios:
   - equipped-adventurer: Player with lamp (on) and sword, troll dead
   - underground-ready: Above + grating open, basic exploration done
   - dam-area: Set up for dam/reservoir puzzle testing
   - loud-room: Set up for echo puzzle testing
   - thief-lair: Set up for cyclops/treasure room testing"
  (:require [clork.game-state :as gs]
            [clork.flags :as flags]))

;;; ---------------------------------------------------------------------------
;;; BASE SCENARIO BUILDERS
;;; ---------------------------------------------------------------------------

(defn init-game
  "Initialize a fresh game state.
   Uses requiring-resolve to avoid cyclic dependency with clork.core."
  []
  ((requiring-resolve 'clork.core/init-game)))

(defn set-location
  "Move player to a specific room."
  [game-state room-id]
  (gs/set-location game-state room-id :scenario-setup))

(defn give-item
  "Give an item to the player (move to :adventurer)."
  [game-state item-id]
  (gs/move-object game-state item-id :adventurer :scenario-setup))

(defn give-items
  "Give multiple items to the player."
  [game-state & item-ids]
  (reduce give-item game-state item-ids))

(defn turn-on-lamp
  "Turn on the brass lantern and set it as lit."
  [game-state]
  (-> game-state
      (gs/set-thing-flag :brass-lantern :on)
      (gs/set-thing-flag :brass-lantern :lit)))

(defn kill-troll
  "Remove troll as obstacle (for testing - just removes it from the game).
   Sets TROLL-FLAG to allow passage through troll room."
  [game-state]
  (-> game-state
      (gs/set-game-flag :troll-flag)           ; Troll defeated - allows passage
      (gs/move-object :troll nil :test-kill)))  ; Remove from game entirely

(defn open-grating
  "Open the grating in the clearing."
  [game-state]
  (-> game-state
      (gs/set-thing-flag :grate :open)))

(defn solve-echo-puzzle
  "Mark the echo puzzle as solved."
  [game-state]
  (-> game-state
      (gs/set-game-flag :loud-flag)
      (gs/unset-thing-flag :platinum-bar :sacred)))

(defn defeat-cyclops
  "Remove cyclops as obstacle (for testing - just removes it from the game).
   Sets flags to allow passage through cyclops room and into strange passage."
  [game-state]
  (-> game-state
      (gs/set-game-flag :cyclops-flag)   ; Cyclops defeated - allows up exit
      (gs/set-game-flag :magic-flag)     ; East wall opened - allows east exit
      (gs/move-object :cyclops nil :test-kill)))  ; Remove from game entirely

(defn open-dam-gates
  "Open the dam sluice gates (water draining)."
  [game-state]
  (-> game-state
      (gs/set-game-flag :gates-open)
      (gs/set-game-flag :gate-flag)))

(defn drain-reservoir
  "Drain the reservoir (low tide)."
  [game-state]
  (-> game-state
      (gs/set-game-flag :low-tide)
      (gs/set-game-flag :gates-open)))

(defn fill-reservoir
  "Fill the reservoir (high water)."
  [game-state]
  (-> game-state
      (gs/unset-game-flag :low-tide)
      (gs/unset-game-flag :gates-open)))

;;; ---------------------------------------------------------------------------
;;; COMPOSITE SCENARIOS
;;; ---------------------------------------------------------------------------

(defn equipped-adventurer
  "Player with lamp (on) and sword, troll dead.
   Optionally specify starting room (default: :west-of-house).

   This is the base scenario for most underground exploration tests."
  ([]
   (equipped-adventurer :west-of-house))
  ([room-id]
   (-> (init-game)
       (give-items :brass-lantern :sword)
       (turn-on-lamp)
       (kill-troll)
       (set-location room-id))))

(defn underground-ready
  "Full underground access: equipped player, troll dead, grating open.
   Optionally specify starting room (default: :cellar).

   Good for testing any underground area."
  ([]
   (underground-ready :cellar))
  ([room-id]
   (-> (equipped-adventurer room-id)
       (open-grating))))

(defn dam-area
  "Set up for dam/reservoir puzzle testing.
   Player at dam-room with wrench, gates operational.

   Options:
   - :drained - reservoir empty (low-tide true)
   - :filled - reservoir full (low-tide false)
   - :draining - gates open, water flowing out"
  ([]
   (dam-area :dam-room))
  ([room-id]
   (dam-area room-id :drained))
  ([room-id water-state]
   (let [base (-> (underground-ready room-id)
                  (give-item :wrench)
                  (gs/set-game-flag :gate-flag))]  ; Yellow button pressed
     (case water-state
       :drained (drain-reservoir base)
       :filled (fill-reservoir base)
       :draining (open-dam-gates base)
       base))))

(defn loud-room-scenario
  "Set up for echo puzzle testing.
   Player at loud-room with lamp, troll dead.

   Options via second arg:
   - :quiet - room is quiet (gates closed, low-tide)
   - :loud - room is deafening (gates open, water high)
   - :solved - echo puzzle already solved"
  ([]
   (loud-room-scenario :loud-room :quiet))
  ([room-id]
   (loud-room-scenario room-id :quiet))
  ([room-id state]
   (let [base (underground-ready room-id)]
     (case state
       :quiet (-> base
                  (gs/unset-game-flag :gates-open)
                  (gs/set-game-flag :low-tide))
       :loud (-> base
                 (gs/set-game-flag :gates-open)
                 (gs/unset-game-flag :low-tide))
       :solved (-> base
                   (solve-echo-puzzle))
       base))))

(defn thief-lair
  "Set up for cyclops/treasure room testing.
   Player at cyclops-room, troll dead.

   Options:
   - :cyclops-present - cyclops is there (default)
   - :cyclops-gone - cyclops defeated, passage to treasure open"
  ([]
   (thief-lair :cyclops-room))
  ([room-id]
   (thief-lair room-id :cyclops-present))
  ([room-id state]
   (let [base (underground-ready room-id)]
     (case state
       :cyclops-present base
       :cyclops-gone (defeat-cyclops base)
       base))))

(defn maze-explorer
  "Set up for maze exploration.
   Player in maze with lamp and items for marking rooms."
  ([]
   (maze-explorer :maze-1))
  ([room-id]
   (-> (underground-ready room-id)
       (give-items :garlic :lunch))))

;;; ---------------------------------------------------------------------------
;;; CONVENIENCE FUNCTIONS
;;; ---------------------------------------------------------------------------

(defn start-at
  "Create a fully equipped game state at the given room.
   Shorthand for (equipped-adventurer room-id).

   Examples:
     (start-at :loud-room)
     (start-at :dam-room)
     (start-at :cyclops-room)"
  [room-id]
  (underground-ready room-id))

(defn scenario
  "Get a named scenario by keyword.

   Available scenarios:
   - :equipped - equipped adventurer at west-of-house
   - :underground - underground ready at cellar
   - :dam - dam area, reservoir drained
   - :dam-full - dam area, reservoir full
   - :loud-quiet - loud room, quiet state
   - :loud-noisy - loud room, deafening state
   - :loud-solved - loud room, puzzle solved
   - :cyclops - cyclops room, cyclops present
   - :treasure - cyclops room, cyclops gone
   - :maze - maze entrance"
  [scenario-kw]
  (case scenario-kw
    :equipped (equipped-adventurer)
    :underground (underground-ready)
    :dam (dam-area)
    :dam-full (dam-area :dam-room :filled)
    :loud-quiet (loud-room-scenario)
    :loud-noisy (loud-room-scenario :loud-room :loud)
    :loud-solved (loud-room-scenario :loud-room :solved)
    :cyclops (thief-lair)
    :treasure (thief-lair :treasure-room :cyclops-gone)
    :maze (maze-explorer)
    ;; Default: equipped at the given room if it's a keyword
    (if (keyword? scenario-kw)
      (underground-ready scenario-kw)
      (throw (ex-info (str "Unknown scenario: " scenario-kw)
                      {:scenario scenario-kw})))))

;;; ---------------------------------------------------------------------------
;;; SCENARIO LISTING
;;; ---------------------------------------------------------------------------

(def available-scenarios
  "Map of scenario names to descriptions."
  {:equipped "Equipped adventurer (lamp, sword) at west-of-house, troll dead"
   :underground "Underground ready (equipped + grating open) at cellar"
   :dam "Dam area with wrench, reservoir drained"
   :dam-full "Dam area with wrench, reservoir full"
   :loud-quiet "Loud room in quiet state"
   :loud-noisy "Loud room with deafening noise"
   :loud-solved "Loud room with echo puzzle solved"
   :cyclops "Cyclops room with cyclops present"
   :treasure "Treasure room with cyclops defeated"
   :maze "Maze entrance with marking items"})

(defn list-scenarios
  "Print available scenarios."
  []
  (println "Available test scenarios:")
  (println "")
  (doseq [[k v] (sort available-scenarios)]
    (println (format "  %-15s %s" (name k) v)))
  (println "")
  (println "Usage: (scenario :name) or (start-at :room-id)"))

;;; ---------------------------------------------------------------------------
;;; DEBUG COMMAND INTERFACE
;;; ---------------------------------------------------------------------------

(defn- tell [game-state msg]
  ;; Import utils/tell dynamically to avoid circular deps
  ((requiring-resolve 'clork.utils/tell) game-state msg))

(defn cmd-scenario
  "Handle $scenario command.

   Usage:
     $scenario           - list available scenarios
     $scenario <name>    - load a named scenario
     $scenario <room-id> - start equipped at specified room

   Examples:
     $scenario loud-quiet
     $scenario dam-full
     $scenario loud-room"
  [game-state args]
  (if (empty? args)
    ;; No args: list scenarios
    (-> game-state
        (tell "Available scenarios:\n")
        (tell "\n")
        (#(reduce (fn [gs [k v]]
                    (tell gs (str "  " (name k) " - " v "\n")))
                  %
                  (sort available-scenarios)))
        (tell "\nUsage: $scenario <name> or $scenario <room-id>\n"))
    ;; Load scenario
    (let [arg (first args)
          scenario-kw (if (clojure.string/starts-with? arg ":")
                        (keyword (subs arg 1))
                        (keyword arg))]
      (try
        (let [new-state (scenario scenario-kw)]
          (-> new-state
              (tell (str "Loaded scenario: " (name scenario-kw) "\n"))
              (tell (str "You are in: " (name (:here new-state)) "\n"))))
        (catch Exception e
          (tell game-state (str "Unknown scenario: " arg "\n"
                                "Use $scenario to list available scenarios.\n")))))))

(def subcommands
  "Subcommand definitions for help display."
  {"" "List available scenarios"
   "<name>" "Load a named scenario (e.g., loud-quiet, dam-full)"
   "<room>" "Start equipped at specified room (e.g., loud-room, dam-room)"})
