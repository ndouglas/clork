(ns clork.planner.actions
  "Action schema and extraction for GOAP planning.

   This module defines a unified action schema for all game actions and
   provides functions to extract actions from game state automatically.

   Action Schema:
   {:id          :action-id
    :type        :movement | :take | :puzzle | :combat | :deposit
    :preconditions
      {:here      :room-id        ; must be in this room (optional)
       :inventory #{:items}       ; must have these items
       :flags     #{:flags}}      ; these flags must be set
    :effects
      {:flags-set     #{:flags}   ; flags to set
       :flags-clear   #{:flags}   ; flags to clear
       :inventory-add #{:items}   ; items added to inventory
       :inventory-remove #{:items}} ; items removed from inventory
    :cost         1               ; estimated moves
    :reversible?  true            ; can action be undone?
    :commands     [\"cmd\" ...]}" ; actual game commands
  (:require [clojure.set :as set]))

;; =============================================================================
;; Exit Parsing
;; =============================================================================

(defn parse-exit
  "Parse an exit definition into a standardized format.

   Exit formats in Clork:
   - keyword: :other-room (direct bidirectional)
   - string: \"blocked message\" (permanently blocked)
   - {:to :room :if :flag} (conditional on flag)
   - {:to :room :door :door-id} (requires door open)
   - {:per :function} (computed exit, often one-way)

   Returns:
   {:to       :destination-room   ; or nil if blocked
    :requires {:flags #{...} :door :door-id :computed :fn-name}
    :one-way? boolean
    :blocked? boolean}"
  [exit-def direction]
  (cond
    ;; Simple keyword - direct connection
    (keyword? exit-def)
    {:to exit-def
     :requires nil
     :one-way? false
     :blocked? false}

    ;; String - blocked exit
    (string? exit-def)
    {:to nil
     :requires nil
     :one-way? false
     :blocked? true
     :message exit-def}

    ;; Map - conditional exit
    (map? exit-def)
    (cond
      ;; Computed exit via :per function
      (:per exit-def)
      (let [fn-name (:per exit-def)
            ;; Known one-way paths
            one-way-fns #{:maze-diodes}]
        {:to nil ; Computed at runtime
         :requires {:computed fn-name}
         :one-way? (contains? one-way-fns fn-name)
         :blocked? false
         :computed fn-name})

      ;; Flag-conditional exit
      (:if exit-def)
      {:to (:to exit-def)
       :requires {:flags #{(:if exit-def)}}
       :one-way? false
       :blocked? false}

      ;; Door-gated exit
      (:door exit-def)
      {:to (:to exit-def)
       :requires {:door (:door exit-def)}
       :one-way? false
       :blocked? false}

      ;; Unknown map format
      :else
      {:to (:to exit-def)
       :requires nil
       :one-way? false
       :blocked? false})

    :else
    {:to nil :requires nil :one-way? false :blocked? true}))

;; Known one-way path destinations (computed exits)
(def one-way-destinations
  "Map of one-way :per functions to their actual destinations.
   These are hardcoded because the destinations depend on current room."
  {:maze-diodes {:maze-2 :maze-4
                 :maze-7 :dead-end-1
                 :maze-9 :maze-11
                 :maze-12 :maze-5}})

;; =============================================================================
;; Movement Action Extraction
;; =============================================================================

(defn extract-room-exits
  "Extract all exits from a room definition.
   Returns sequence of parsed exits with direction."
  [room-def]
  (for [[direction exit-def] (:exits room-def {})]
    (assoc (parse-exit exit-def direction) :direction direction)))

(defn generate-movement-action
  "Generate a movement action for a single exit."
  [from-room direction parsed-exit]
  (let [to-room (:to parsed-exit)
        requires (:requires parsed-exit)]
    (when to-room
      {:id (keyword (str (name from-room) "->" (name to-room)))
       :type :movement
       :preconditions
       {:here from-room
        :inventory #{}
        :flags (or (get requires :flags) #{})}
       :effects
       {:flags-set #{}
        :flags-clear #{}
        :inventory-add #{}
        :inventory-remove #{}
        :new-location to-room}
       :cost 1
       :reversible? (not (:one-way? parsed-exit))
       :commands [(name direction)]
       :door (get requires :door)
       :computed (get requires :computed)})))

(defn extract-movement-actions
  "Extract all movement actions from game state.
   Returns map of action-id -> action."
  [game-state]
  (reduce-kv
   (fn [actions room-id room-def]
     (let [exits (extract-room-exits room-def)
           move-actions (keep #(generate-movement-action room-id (:direction %) %) exits)]
       (reduce (fn [m action] (assoc m (:id action) action))
               actions
               move-actions)))
   {}
   (:rooms game-state)))

;; =============================================================================
;; Take Action Extraction
;; =============================================================================

(defn takeable?
  "Check if an object can be taken (has :take flag)."
  [obj-def]
  (contains? (:flags obj-def #{}) :take))

(defn find-object-room
  "Find what room an object is in (recursively if in container)."
  [game-state obj-id]
  (let [location (get-in game-state [:objects obj-id :in])]
    (cond
      ;; In a room directly
      (contains? (:rooms game-state) location)
      location

      ;; In a container - find container's location
      (keyword? location)
      (find-object-room game-state location)

      ;; In player inventory
      (= location :adventurer)
      nil

      :else nil)))

(defn generate-take-action
  "Generate a take action for an object."
  [game-state obj-id obj-def]
  (let [room (find-object-room game-state obj-id)
        size (get obj-def :size 0)]
    (when room
      {:id (keyword (str "take-" (name obj-id)))
       :type :take
       :preconditions
       {:here room
        :inventory #{}
        :flags #{}}
       :effects
       {:flags-set #{}
        :flags-clear #{}
        :inventory-add #{obj-id}
        :inventory-remove #{}
        :removes-from-room obj-id}
       :cost 1
       :reversible? true
       :commands [(str "take " (name obj-id))]
       :object obj-id
       :weight size})))

(defn extract-take-actions
  "Extract take actions for all takeable objects.
   Returns map of action-id -> action."
  [game-state]
  (reduce-kv
   (fn [actions obj-id obj-def]
     (if (takeable? obj-def)
       (if-let [action (generate-take-action game-state obj-id obj-def)]
         (assoc actions (:id action) action)
         actions)
       actions))
   {}
   (:objects game-state)))

;; =============================================================================
;; Deposit Action Generation
;; =============================================================================

(def treasures
  "List of all treasures in Zork I with their value when deposited."
  #{:jeweled-egg :clockwork-canary :brass-bauble :gold-coffin :sceptre
    :ivory-torch :crystal-trident :jade-figurine :sapphire-bracelet
    :huge-diamond :bag-of-coins :crystal-skull :jewel-encrusted-trunk
    :gold-bar :emerald :painting :pot-of-gold :platinum-bar :scarab})

(defn generate-deposit-action
  "Generate an action to deposit a treasure in the trophy case."
  [treasure-id]
  {:id (keyword (str "deposit-" (name treasure-id)))
   :type :deposit
   :preconditions
   {:here :living-room
    :inventory #{treasure-id}
    :flags #{}}
   :effects
   {:flags-set #{}
    :flags-clear #{}
    :inventory-add #{}
    :inventory-remove #{treasure-id}
    :deposits treasure-id}
   :cost 2 ; open case + put item
   :reversible? true
   :commands ["open case" (str "put " (name treasure-id) " in case")]
   :treasure treasure-id})

(defn generate-deposit-actions
  "Generate deposit actions for all treasures.
   Returns map of action-id -> action."
  []
  (reduce (fn [m treasure-id]
            (let [action (generate-deposit-action treasure-id)]
              (assoc m (:id action) action)))
          {}
          treasures))

;; =============================================================================
;; Puzzle/Combat Actions (Manual Definitions)
;; =============================================================================

(def puzzle-actions
  "Manually defined puzzle and combat actions.
   These require specific knowledge about game mechanics."
  {:kill-troll
   {:id :kill-troll
    :type :combat
    :preconditions
    {:here :troll-room
     :inventory #{:sword}
     :flags #{}}
    :effects
    {:flags-set #{:troll-flag}
     :flags-clear #{}
     :inventory-add #{}
     :inventory-remove #{}}
    :cost 3
    :reversible? false
    :commands ["kill troll with sword"]}

   :scare-cyclops
   {:id :scare-cyclops
    :type :puzzle
    :preconditions
    {:here :cyclops-room
     :inventory #{}
     :flags #{:troll-flag}}
    :effects
    {:flags-set #{:magic-flag :cyclops-flag}
     :flags-clear #{}
     :inventory-add #{}
     :inventory-remove #{}}
    :cost 1
    :reversible? false
    :commands ["ulysses"]}

   :open-dam
   {:id :open-dam
    :type :puzzle
    :preconditions
    {:here :maintenance-room
     :inventory #{:wrench}
     :flags #{}}
    :effects
    {:flags-set #{:dam-opened}
     :flags-clear #{}
     :inventory-add #{}
     :inventory-remove #{}}
    :cost 1
    :reversible? true
    :commands ["turn bolt with wrench"]}

   :tie-rope
   {:id :tie-rope
    :type :puzzle
    :preconditions
    {:here :dome-room
     :inventory #{:rope}
     :flags #{}}
    :effects
    {:flags-set #{:dome-flag}
     :flags-clear #{}
     :inventory-add #{}
     :inventory-remove #{:rope}}
    :cost 1
    :reversible? false
    :commands ["tie rope to railing"]}

   :exorcism
   {:id :exorcism
    :type :puzzle
    :preconditions
    {:here :entrance-to-hades
     :inventory #{:bell :book :candles :matchbook}
     :flags #{}}
    :effects
    {:flags-set #{:lld-flag}
     :flags-clear #{}
     :inventory-add #{}
     :inventory-remove #{}}
    :cost 4
    :reversible? false
    :commands ["ring bell" "light match" "light candles with match" "read book"]}

   :wave-sceptre
   {:id :wave-sceptre
    :type :puzzle
    :preconditions
    {:here :end-of-rainbow
     :inventory #{:sceptre}
     :flags #{}}
    :effects
    {:flags-set #{:rainbow-flag}
     :flags-clear #{}
     :inventory-add #{:pot-of-gold}
     :inventory-remove #{}}
    :cost 1
    :reversible? false
    :commands ["wave sceptre"]}

   :enter-boat
   {:id :enter-boat
    :type :puzzle
    :preconditions
    {:here :dam-base
     :inventory #{}
     :flags #{}}
    :effects
    {:flags-set #{:in-boat}
     :flags-clear #{}
     :inventory-add #{}
     :inventory-remove #{}}
    :cost 1
    :reversible? true
    :commands ["enter boat"]}

   :wait-for-drain
   {:id :wait-for-drain
    :type :puzzle
    :preconditions
    {:inventory #{}
     :flags #{:dam-opened}}
    :effects
    {:flags-set #{:low-tide}
     :flags-clear #{}
     :inventory-add #{}
     :inventory-remove #{}}
    :cost 8
    :reversible? false
    :commands ["wait" "wait" "wait" "wait" "wait" "wait" "wait" "wait"]}

   :inflate-boat
   {:id :inflate-boat
    :type :puzzle
    :preconditions
    {:here :dam-base
     :inventory #{:hand-pump}
     :flags #{}}
    :effects
    {:flags-set #{:boat-inflated}
     :flags-clear #{}
     :inventory-add #{}
     :inventory-remove #{}}
    :cost 1
    :reversible? false
    :commands ["inflate boat with pump"]}

   :open-buoy
   {:id :open-buoy
    :type :puzzle
    :preconditions
    {:inventory #{:buoy}
     :flags #{}}
    :effects
    {:flags-set #{}
     :flags-clear #{}
     :inventory-add #{:emerald}
     :inventory-remove #{}}
    :cost 1
    :reversible? false
    :commands ["open buoy"]}

   ;; ==========================================================================
   ;; EGG PUZZLE (thief interaction)
   ;; ==========================================================================
   ;; The egg CANNOT be opened by the player - any attempt breaks it!
   ;; The ONLY way to open it safely is via the thief:
   ;; 1. Give egg to thief (or let him steal it)
   ;; 2. Thief takes egg to treasure room and deposits it
   ;; 3. When depositing, thief opens egg safely (sets :egg-solve flag, :open flag on egg)
   ;; 4. Kill thief, take the now-open egg
   ;; 5. Canary is inside the open egg
   ;;
   ;; Note: The thief is non-deterministic - he may not deposit immediately.
   ;; Must wait for him to actually deposit the egg.

   :give-egg-to-thief
   {:id :give-egg-to-thief
    :type :puzzle
    :preconditions
    {:here :treasure-room
     :inventory #{:jeweled-egg}
     :flags #{:cyclops-flag}}  ; Must reach treasure room
    :effects
    {:flags-set #{}  ; egg-solve happens when thief deposits, not immediately
     :flags-clear #{}
     :inventory-add #{}
     :inventory-remove #{:jeweled-egg}}
    :cost 1
    :reversible? false
    :commands ["give egg to thief"]
    :notes "Thief will eventually deposit egg, opening it safely."}

   ;; After thief has deposited (opened) the egg and been killed
   :take-canary-from-egg
   {:id :take-canary-from-egg
    :type :take
    :preconditions
    {:inventory #{:jeweled-egg}
     :flags #{:egg-solve}}  ; Thief must have deposited/opened the egg
    :effects
    {:flags-set #{}
     :flags-clear #{}
     :inventory-add #{:clockwork-canary}
     :inventory-remove #{}}
    :cost 1
    :reversible? false
    :commands ["take canary from egg"]}

   :dig-for-scarab
   {:id :dig-for-scarab
    :type :puzzle
    :preconditions
    {:here :sandy-cave
     :inventory #{:shovel}
     :flags #{}}
    :effects
    {:flags-set #{}
     :flags-clear #{}
     :inventory-add #{:scarab}
     :inventory-remove #{}}
    :cost 4  ; Multiple dig commands
    :reversible? false
    :commands ["dig sand" "dig sand" "dig sand" "dig sand"]}

   ;; ==========================================================================
   ;; THIEF COMBAT
   ;; ==========================================================================
   ;; Combat mechanics:
   ;; - Player fight-strength: 2 + (score/70), ranges from 2 (0 pts) to 7 (350 pts)
   ;; - Thief strength: 5
   ;; - At 0 points: strength 2 vs 5 = almost impossible to win
   ;; - At 210 points: strength 5 vs 5 = even match
   ;; - At 280+ points: strength 6+ vs 5 = advantage
   ;; - MUST have a weapon (sword or nasty knife)
   ;; - Nasty knife is thief's "best weapon" - gives player advantage
   ;;
   ;; Thief drops: chalice, stiletto, and any treasures he stole
   ;; Note: Thief does NOT open egg when killed - egg must be given to thief,
   ;; who will open it when he deposits it in treasure room (sets :egg-solve)

   :kill-thief-with-sword
   {:id :kill-thief-with-sword
    :type :combat
    :preconditions
    {:here :treasure-room
     :inventory #{:sword}
     :flags #{:cyclops-flag}}  ; Must reach treasure room
    :effects
    {:flags-set #{:thief-dead}
     :flags-clear #{}
     :inventory-add #{:chalice}  ; Chalice always dropped, stolen items vary
     :inventory-remove #{}}
    :cost 10  ; Combat takes many turns, sword is harder
    :reversible? false
    :commands ["kill thief with sword"]
    :notes "Requires high score (210+) for reliable success. Sword is less effective."}

   ;; Nasty knife is thief's weakness (his "best weapon" in ZIL terms)
   :kill-thief-with-knife
   {:id :kill-thief-with-knife
    :type :combat
    :preconditions
    {:here :treasure-room
     :inventory #{:nasty-knife}
     :flags #{:cyclops-flag}}
    :effects
    {:flags-set #{:thief-dead}
     :flags-clear #{}
     :inventory-add #{:chalice}
     :inventory-remove #{}}
    :cost 6  ; Faster with the knife (thief's weakness)
    :reversible? false
    :commands ["kill thief with knife"]
    :notes "Nasty knife gives combat advantage vs thief. Still needs decent score."}

   ;; Loud room puzzle:
   ;; - Room is deafening when dam gates open and water high
   ;; - Entering when deafening ejects you randomly
   ;; - Room is quiet when: loud-flag set OR (gates closed AND low-tide)
   ;; - Saying "echo" sets loud-flag and reveals gold bar
   ;; Safest approach: drain dam first (low-tide), then enter and say echo
   :say-echo
   {:id :say-echo
    :type :puzzle
    :preconditions
    {:here :loud-room
     :inventory #{}
     :flags #{:low-tide}}  ; Need low-tide to safely enter
    :effects
    {:flags-set #{:loud-flag}  ; Not loud-room-solved - use actual flag name
     :flags-clear #{}
     :inventory-add #{}
     :inventory-remove #{}}
    :cost 1
    :reversible? false
    :commands ["echo"]}

   :take-platinum-bar
   {:id :take-platinum-bar
    :type :take
    :preconditions
    {:here :loud-room
     :inventory #{}
     :flags #{:loud-flag}}  ; Echo puzzle must be solved first
    :effects
    {:flags-set #{}
     :flags-clear #{}
     :inventory-add #{:platinum-bar}
     :inventory-remove #{}}
    :cost 1
    :reversible? true
    :commands ["take bar"]}

   ;; ==========================================================================
   ;; COAL MINE PUZZLE (very complex multi-step puzzle!)
   ;; ==========================================================================
   ;; The coal mine puzzle requires:
   ;; 1. Garlic (for bat room protection)
   ;; 2. Brass lantern (only non-flame light, safe in gas room)
   ;; 3. Ivory torch (provides light at bottom of shaft - but is a flame!)
   ;; 4. Screwdriver (to turn machine switch)
   ;;
   ;; Sequence:
   ;; 1. Put torch + screwdriver in basket at shaft-room
   ;; 2. Lower basket (now lower-shaft is lit)
   ;; 3. Navigate through mine WITH LANTERN ONLY (torch would explode in gas room)
   ;; 4. Get coal from coal-mine-4
   ;; 5. Return to shaft-room via gas room (safe with lantern)
   ;; 6. Put coal in basket, lower it
   ;; 7. Navigate to ladder-bottom, then west
   ;; 8. DROP ALL to fit through narrow crack
   ;; 9. Now in lower-shaft (lit by torch in basket)
   ;; 10. Put coal in machine, close, turn switch with screwdriver, open, take diamond
   ;; 11. Put diamond in basket, raise it
   ;; 12. Go back through crack, pick up items
   ;; 13. Return to shaft-room, get diamond and torch from basket
   ;;
   ;; CRITICAL CONSTRAINTS:
   ;; - Gas room: torch/candles/match = instant death
   ;; - Bat room: need garlic or bat steals items
   ;; - Narrow crack: must be empty-handed
   ;; - Lower shaft: dark unless basket has torch

   :setup-basket
   {:id :setup-basket
    :type :puzzle
    :preconditions
    {:here :shaft-room
     :inventory #{:ivory-torch :screwdriver}
     :flags #{}}
    :effects
    {:flags-set #{:basket-has-light :basket-has-screwdriver}
     :flags-clear #{}
     :inventory-add #{}
     :inventory-remove #{:ivory-torch :screwdriver}}
    :cost 2
    :reversible? true
    :commands ["put torch in basket" "put screwdriver in basket"]}

   :lower-basket
   {:id :lower-basket
    :type :puzzle
    :preconditions
    {:here :shaft-room
     :inventory #{}
     :flags #{:basket-has-light}}  ; Need light source in basket for lower shaft
    :effects
    {:flags-set #{:basket-lowered :lower-shaft-lit}
     :flags-clear #{}
     :inventory-add #{}
     :inventory-remove #{}}
    :cost 1
    :reversible? true
    :commands ["lower basket"]}

   :put-coal-in-basket
   {:id :put-coal-in-basket
    :type :puzzle
    :preconditions
    {:here :shaft-room
     :inventory #{:coal}
     :flags #{}}
    :effects
    {:flags-set #{:basket-has-coal}
     :flags-clear #{}
     :inventory-add #{}
     :inventory-remove #{:coal}}
    :cost 1
    :reversible? false
    :commands ["put coal in basket"]}

   :raise-basket
   {:id :raise-basket
    :type :puzzle
    :preconditions
    {:here :shaft-room
     :inventory #{}
     :flags #{:basket-lowered}}
    :effects
    {:flags-set #{}
     :flags-clear #{:basket-lowered}
     :inventory-add #{}
     :inventory-remove #{}}
    :cost 1
    :reversible? true
    :commands ["raise basket"]}

   ;; The crack requires dropping everything
   :squeeze-through-crack
   {:id :squeeze-through-crack
    :type :puzzle
    :preconditions
    {:here :ladder-bottom
     :inventory #{}  ; Must be empty-handed!
     :flags #{:lower-shaft-lit}}  ; Need basket light at bottom
    :effects
    {:flags-set #{:at-lower-shaft}
     :flags-clear #{}
     :inventory-add #{}
     :inventory-remove #{}
     :new-location :lower-shaft}
    :cost 1
    :reversible? true
    :commands ["west"]}

   :get-diamond
   {:id :get-diamond
    :type :puzzle
    :preconditions
    {:here :lower-shaft
     :inventory #{}
     :flags #{:basket-has-coal :basket-has-screwdriver :basket-lowered}}
    :effects
    {:flags-set #{}
     :flags-clear #{:basket-has-coal}
     :inventory-add #{:huge-diamond}
     :inventory-remove #{}}
    :cost 6
    :reversible? false
    :commands ["take coal from basket" "take screwdriver from basket"
               "put coal in machine" "close lid" "turn switch with screwdriver"
               "open lid" "take diamond"]}

   :put-diamond-in-basket
   {:id :put-diamond-in-basket
    :type :puzzle
    :preconditions
    {:here :lower-shaft
     :inventory #{:huge-diamond}
     :flags #{}}
    :effects
    {:flags-set #{:basket-has-diamond}
     :flags-clear #{}
     :inventory-add #{}
     :inventory-remove #{:huge-diamond}}
    :cost 1
    :reversible? false
    :commands ["put diamond in basket"]}

   :retrieve-diamond-from-basket
   {:id :retrieve-diamond-from-basket
    :type :puzzle
    :preconditions
    {:here :shaft-room
     :inventory #{}
     :flags #{:basket-has-diamond}}
    :effects
    {:flags-set #{}
     :flags-clear #{:basket-has-diamond}
     :inventory-add #{:huge-diamond}
     :inventory-remove #{}}
    :cost 1
    :reversible? false
    :commands ["take diamond from basket"]}

   :retrieve-torch-from-basket
   {:id :retrieve-torch-from-basket
    :type :puzzle
    :preconditions
    {:here :shaft-room
     :inventory #{}
     :flags #{:basket-has-light}}
    :effects
    {:flags-set #{}
     :flags-clear #{:basket-has-light :lower-shaft-lit}
     :inventory-add #{:ivory-torch}
     :inventory-remove #{}}
    :cost 1
    :reversible? false
    :commands ["take torch from basket"]}})

;; =============================================================================
;; Action Registry
;; =============================================================================

(defn build-action-registry
  "Build complete action registry from game state.
   Combines auto-extracted and manual actions."
  [game-state]
  (merge
   (extract-movement-actions game-state)
   (extract-take-actions game-state)
   (generate-deposit-actions)
   puzzle-actions))

(defn actions-requiring-flag
  "Find all actions that require a specific flag."
  [registry flag]
  (filter #(contains? (get-in % [:preconditions :flags] #{}) flag)
          (vals registry)))

(defn actions-setting-flag
  "Find all actions that set a specific flag."
  [registry flag]
  (filter #(contains? (get-in % [:effects :flags-set] #{}) flag)
          (vals registry)))

(defn actions-at-location
  "Find all non-movement actions at a specific location."
  [registry room-id]
  (filter #(and (= (get-in % [:preconditions :here]) room-id)
                (not= (:type %) :movement))
          (vals registry)))

(defn movement-actions-from
  "Find all movement actions from a specific room."
  [registry room-id]
  (filter #(and (= (get-in % [:preconditions :here]) room-id)
                (= (:type %) :movement))
          (vals registry)))

;; =============================================================================
;; Action Validation
;; =============================================================================

(defn can-execute?
  "Check if an action can be executed given current state.
   State should have :here, :inventory, :flags keys."
  [action state]
  (let [preconditions (:preconditions action)]
    (and
     ;; Location check (if specified)
     (or (nil? (:here preconditions))
         (= (:here preconditions) (:here state)))
     ;; Inventory check
     (set/subset? (:inventory preconditions #{})
                  (:inventory state #{}))
     ;; Flags check
     (set/subset? (:flags preconditions #{})
                  (:flags state #{})))))

(defn apply-action-effects
  "Apply action effects to state.
   Returns updated state."
  [action state]
  (let [effects (:effects action)]
    (-> state
        (update :flags set/union (:flags-set effects #{}))
        (update :flags set/difference (:flags-clear effects #{}))
        (update :inventory set/union (:inventory-add effects #{}))
        (update :inventory set/difference (:inventory-remove effects #{}))
        (cond-> (:new-location effects) (assoc :here (:new-location effects))))))

;; =============================================================================
;; Debug Utilities
;; =============================================================================

(defn summarize-registry
  "Print summary of action registry."
  [registry]
  (let [by-type (group-by :type (vals registry))]
    (println "\n=== Action Registry Summary ===")
    (doseq [[type actions] (sort-by first by-type)]
      (println (str "  " (name type) ": " (count actions) " actions")))
    (println "  Total:" (count registry) "actions")))

(defn print-action
  "Pretty print an action."
  [action]
  (println "\n=== Action:" (:id action) "===")
  (println "  Type:" (:type action))
  (println "  Cost:" (:cost action))
  (println "  Preconditions:")
  (when (:here (:preconditions action))
    (println "    Location:" (:here (:preconditions action))))
  (when (seq (:inventory (:preconditions action)))
    (println "    Items:" (:inventory (:preconditions action))))
  (when (seq (:flags (:preconditions action)))
    (println "    Flags:" (:flags (:preconditions action))))
  (println "  Effects:")
  (when (seq (:flags-set (:effects action)))
    (println "    Sets:" (:flags-set (:effects action))))
  (when (seq (:inventory-add (:effects action)))
    (println "    Adds:" (:inventory-add (:effects action))))
  (println "  Commands:" (:commands action)))
