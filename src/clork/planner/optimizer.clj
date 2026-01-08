(ns clork.planner.optimizer
  "Route optimization for Clork speedrun plans.

   Converts abstract action plans into optimized command sequences:
   - Merges adjacent movement actions into efficient routes
   - Optimizes multi-destination routes (traveling salesman approach)
   - Handles inventory constraints (deposit trips)
   - Generates executable command sequences"
  (:require [clojure.set :as set]
            [clork.planner.actions :as actions]
            [clork.planner.constraints :as constraints]
            [clojure.data.priority-map :refer [priority-map]]))

;; =============================================================================
;; Region-Based Treasure Grouping
;; =============================================================================

(def treasure-regions
  "Treasures grouped by region for efficient collection.
   Collecting treasures in the same region together reduces backtracking."
  {:surface #{:jeweled-egg}
   :house #{:painting}
   :maze #{:bag-of-coins}
   :dam-area #{:emerald :platinum-bar :jewel-encrusted-trunk}
   :temple #{:ivory-torch :sceptre}
   :hades #{:crystal-skull}
   :thief-lair #{:jade-figurine :sapphire-bracelet :gold-coffin :clockwork-canary}
   :coal-mine #{:huge-diamond :scarab}
   :rainbow #{:pot-of-gold}
   :loud-room #{:gold-bar}
   :cyclops #{:brass-bauble}
   :atlantis #{:crystal-trident}})

(def region-order
  "Recommended order to visit regions (minimizes backtracking)."
  [:surface :house :maze :dam-area :temple :hades
   :coal-mine :loud-room :cyclops :thief-lair :atlantis :rainbow])

(defn treasures-in-region
  "Get set of treasures in a region."
  [region]
  (get treasure-regions region #{}))

(defn region-for-treasure
  "Find which region a treasure is in."
  [treasure-id]
  (first (for [[region treasures] treasure-regions
               :when (contains? treasures treasure-id)]
           region)))

;; =============================================================================
;; A* Pathfinding (copied from main planner for independence)
;; =============================================================================

(defn find-path
  "A* pathfinding from start to goal.
   Returns vector of rooms or nil if no path."
  [graph start goal]
  (if (= start goal)
    [start]
    (loop [open-set (priority-map start 0)
           came-from {}
           g-score {start 0}]
      (if (empty? open-set)
        nil
        (let [[current _] (first open-set)]
          (if (= current goal)
            (loop [path (list goal)
                   node goal]
              (if-let [prev (came-from node)]
                (recur (cons prev path) prev)
                (vec path)))
            (let [open-set (dissoc open-set current)
                  neighbors (get graph current #{})
                  tentative-g (inc (get g-score current))]
              (let [[new-open new-came-from new-g-score]
                    (reduce
                     (fn [[open cf gs] neighbor]
                       (if (< tentative-g (get gs neighbor Integer/MAX_VALUE))
                         [(assoc open neighbor tentative-g)
                          (assoc cf neighbor current)
                          (assoc gs neighbor tentative-g)]
                         [open cf gs]))
                     [open-set came-from g-score]
                     neighbors)]
                (recur new-open new-came-from new-g-score)))))))))

;; =============================================================================
;; Navigation Command Generation
;; =============================================================================

(defn build-nav-graph
  "Build navigation graph from game state.
   Includes both regular exits and known computed exits."
  [game-state]
  (reduce-kv
   (fn [graph room-id room-def]
     (let [exits (:exits room-def {})
           ;; Get destinations from regular exits
           regular-destinations
           (reduce-kv
            (fn [s dir exit]
              (let [target (actions/parse-exit exit dir)]
                (if (:to target)
                  (conj s (:to target))
                  s)))
            #{}
            exits)
           ;; Add destinations from known computed exits
           computed-destinations
           (reduce-kv
            (fn [s dir exit]
              (if (and (map? exit) (:per exit))
                (let [dest (get actions/computed-exit-destinations [room-id (:per exit)])]
                  (if dest (conj s dest) s))
                s))
            #{}
            exits)]
       (assoc graph room-id (set/union regular-destinations computed-destinations))))
   {}
   (:rooms game-state)))

(defn build-room-graph-with-directions
  "Build graph that tracks direction for each connection.
   Includes both regular exits and known computed exits."
  [game-state]
  (reduce-kv
   (fn [graph room-id room-def]
     (let [exits (:exits room-def {})
           ;; Get direction map from regular exits
           regular-dir-map
           (reduce-kv
            (fn [m dir exit]
              (let [parsed (actions/parse-exit exit dir)]
                (if (:to parsed)
                  (assoc m (:to parsed) dir)
                  m)))
            {}
            exits)
           ;; Add direction map from known computed exits
           computed-dir-map
           (reduce-kv
            (fn [m dir exit]
              (if (and (map? exit) (:per exit))
                (let [dest (get actions/computed-exit-destinations [room-id (:per exit)])]
                  (if dest (assoc m dest dir) m))
                m))
            {}
            exits)]
       (assoc graph room-id (merge regular-dir-map computed-dir-map))))
   {}
   (:rooms game-state)))

(defn path-to-directions
  "Convert a path of rooms to direction commands."
  [room-graph path]
  (when (and path (> (count path) 1))
    (loop [directions []
           remaining path]
      (if (< (count remaining) 2)
        directions
        (let [from (first remaining)
              to (second remaining)
              dir-map (get room-graph from {})
              dir (get dir-map to)]
          (if dir
            (recur (conj directions dir) (rest remaining))
            nil))))))

;; =============================================================================
;; Door/Setup Requirements for Navigation
;; =============================================================================

(def door-setup-commands
  "Commands required before traversing certain room transitions.
   Format: {[from-room to-room] [commands...]}"
  {[:behind-house :kitchen] ["open window"]
   [:living-room :cellar] ["move rug" "open trap door"]})

(def conditional-one-way-transitions
  "Transitions that are one-way UNTIL certain flags are set.
   Format: {[from-room to-room] {:unlocked-by #{flags...}}}"
  {[:living-room :cellar] {:unlocked-by #{:magic-flag :grating-unlocked}
                           :description "Trap door bars until alternate exit found"}})

(defn transition-is-one-way?
  "Check if a transition is currently one-way given achieved flags.
   Returns false if any unlocking flag has been achieved."
  ([from-room to-room]
   ;; Without flag info, assume worst case (one-way)
   (contains? conditional-one-way-transitions [from-room to-room]))
  ([from-room to-room achieved-flags]
   (if-let [constraint (get conditional-one-way-transitions [from-room to-room])]
     ;; One-way unless we have an unlocking flag
     (empty? (clojure.set/intersection (:unlocked-by constraint) achieved-flags))
     false)))

(def alternate-return-routes
  "When you can't return the way you came, use these routes.
   Format: {blocked-return-to alternative-route-via}"
  {:living-room {:via :strange-passage
                 :requires #{:magic-flag}
                 :from-rooms #{:cyclops-room}}})

(def underground-rooms
  "Rooms that are 'underground' - below the one-way trap door.
   Once you enter these, you cannot return via trap door."
  #{:cellar :troll-room :east-of-chasm :gallery :studio
    :east-west-passage :round-room :narrow-passage :mirror-room-south
    :mirror-room-north :winding-passage :mine-entrance :squeaky-room
    :bat-room :shaft-room :smelly-room :gas-room :coal-mine
    :maze-1 :maze-2 :maze-3 :maze-4 :maze-5 :maze-6 :maze-7 :maze-8
    :maze-9 :maze-10 :maze-11 :maze-12 :maze-13 :maze-14 :maze-15
    :dead-end :grating-room :cyclops-room :strange-passage :treasure-room
    :loud-room :deep-canyon :damp-cave :white-cliffs-beach-north
    :white-cliffs-beach-south :dam :dam-base :dam-lobby :maintenance-room
    :chasm-room :ns-passage :dome-room :torch-room :temple
    :egyptian-room :altar :cave :cave-north :twisting-passage
    :engravings-cave :north-south-crawlway :west-of-chasm})

(defn room-is-underground?
  "Check if a room is underground (below trap door)."
  [room]
  (contains? underground-rooms room))

(defn door-commands-for-transition
  "Get door/setup commands needed for a room transition."
  [from-room to-room]
  (get door-setup-commands [from-room to-room] []))

(defn generate-navigation
  "Generate navigation commands from current location to destination.
   Includes door-opening commands when needed.

   Optional :blocked-edges parameter excludes specific room transitions
   from pathfinding (e.g., #{[:cellar :living-room]} when trap door is barred)."
  [game-state current-room dest-room & {:keys [blocked-edges] :or {blocked-edges #{}}}]
  (let [base-graph (build-nav-graph game-state)
        ;; Remove blocked edges from graph
        graph (reduce (fn [g [from to]]
                        (if (contains? (get g from #{}) to)
                          (update g from disj to)
                          g))
                      base-graph
                      blocked-edges)
        room-graph (build-room-graph-with-directions game-state)
        path (find-path graph current-room dest-room)]
    (when path
      (let [;; Generate direction commands with door handling
            commands-with-doors
            (loop [remaining path
                   commands []]
              (if (< (count remaining) 2)
                commands
                (let [from (first remaining)
                      to (second remaining)
                      door-cmds (door-commands-for-transition from to)
                      dir-map (get room-graph from {})
                      dir (get dir-map to)]
                  (if dir
                    (recur (rest remaining)
                           (-> commands
                               (into door-cmds)
                               (conj (name dir))))
                    ;; No direction found - shouldn't happen
                    commands))))]
        {:path path
         :commands commands-with-doors
         :moves (count commands-with-doors)
         :has-one-way? (some (fn [[from to]]
                               (transition-is-one-way? from to))
                             (partition 2 1 path))}))))

;; =============================================================================
;; Route Optimization (Nearest Neighbor)
;; =============================================================================

(defn nearest-neighbor-route
  "Compute route using nearest neighbor heuristic.
   Returns destinations in optimized order."
  [game-state start-room destinations]
  (let [graph (build-nav-graph game-state)]
    (loop [current start-room
           remaining (set destinations)
           route []]
      (if (empty? remaining)
        route
        (let [distances (for [dest remaining
                              :let [path (find-path graph current dest)]
                              :when path]
                          {:dest dest :distance (dec (count path))})
              nearest (first (sort-by :distance distances))]
          (if nearest
            (recur (:dest nearest)
                   (disj remaining (:dest nearest))
                   (conj route (:dest nearest)))
            ;; Some destinations unreachable
            (into route remaining)))))))

;; =============================================================================
;; Plan to Commands Conversion
;; =============================================================================

(defn action-location
  "Get the location where an action must be performed."
  [action]
  (get-in action [:preconditions :here]))

(defn expand-action-to-commands
  "Expand an action to its command strings."
  [action]
  (or (:commands action) []))

;; =============================================================================
;; Combat Execution (handles RNG and retry logic)
;; =============================================================================

(defn expand-combat-action
  "Expand a combat action into an executable command block.

   Combat actions have a :combat spec with:
   - :action           - The attack command
   - :enemy            - Enemy keyword for state checking
   - :victory-flag     - Flag set when enemy dies
   - :expected-rounds  - [min max] typical range
   - :max-rounds       - Abort threshold for timeout
   - :retreat-dir      - Direction to flee for RNG reset
   - :retry-actions    - Commands to change RNG state before retry

   Returns a command block that can be interpreted by the executor:
   {:type :combat-loop
    :action \"attack X with Y\"
    :check-victory (fn [state] ...)
    :max-rounds N
    :on-timeout {:retreat \"dir\" :reset [\"cmd\"...] :retry true}}"
  [action]
  (if-let [combat (:combat action)]
    ;; Return structured combat block
    {:type :combat-loop
     :action (:action combat)
     :enemy (:enemy combat)
     :victory-flag (:victory-flag combat)
     :max-rounds (:max-rounds combat 15)
     :expected-rounds (:expected-rounds combat [2 5])
     :on-timeout {:retreat (:retreat-dir combat)
                  :reset (:retry-actions combat [])
                  :retry true}
     :score-requirement (:score-requirement combat 0)}
    ;; Fallback to simple commands
    {:type :simple
     :commands (expand-action-to-commands action)}))

(defn generate-combat-commands
  "Generate combat commands for transcript output.

   For simple output (non-interactive), generates expected number of attacks
   plus a few extra for variance. Real execution would use the loop construct.

   Options:
   - :mode :optimistic  - Use lower bound of expected rounds
   - :mode :pessimistic - Use upper bound + buffer
   - :mode :with-retry  - Include retreat/retry sequence"
  [combat-spec & {:keys [mode] :or {mode :pessimistic}}]
  (let [{:keys [action expected-rounds max-rounds on-timeout]} combat-spec
        [min-rounds max-expected] expected-rounds
        attack-count (case mode
                       :optimistic min-rounds
                       :pessimistic (+ max-expected 3)
                       :with-retry max-rounds)
        attacks (vec (repeat attack-count action))]
    (if (= mode :with-retry)
      ;; Include retry sequence after attacks
      (-> attacks
          (conj (:retreat on-timeout))
          (into (:reset on-timeout))
          (into (repeat 5 action)))  ; More attacks after RNG reset
      attacks)))

(defn action-is-combat?
  "Check if an action is a combat action with loop spec."
  [action]
  (and (= :combat (:type action))
       (some? (:combat action))))

(defn expand-action-for-transcript
  "Expand an action to commands suitable for transcript testing.
   Handles combat loops specially."
  [action & {:keys [combat-mode] :or {combat-mode :pessimistic}}]
  (if (action-is-combat? action)
    (let [combat-spec (expand-combat-action action)]
      (generate-combat-commands combat-spec :mode combat-mode))
    (expand-action-to-commands action)))

(defn action-sets-flag?
  "Check if an action sets a specific flag."
  [action flag]
  (contains? (get-in action [:effects :flags-set] #{}) flag))

(defn plan-to-command-sequence
  "Convert a plan to an executable command sequence.

   Parameters:
   - game-state: Current game state for navigation
   - plan: Sequence of actions
   - start-room: Starting location
   - opts: {:combat-mode :optimistic|:pessimistic|:with-retry}

   Tracks underground state to properly route around the barred trap door:
   - After going down through trap door, cellar->living-room is blocked
   - Until magic-flag or grating-unlocked is achieved

   Returns:
   {:commands [\"cmd\" ...]
    :total-moves n
    :by-action [{:action :id :commands [...]}]
    :combat-actions [{:action :id :spec {...}}]}"
  [game-state plan start-room & {:keys [combat-mode] :or {combat-mode :pessimistic}}]
  (loop [remaining plan
         current-room start-room
         all-commands []
         by-action []
         combat-actions []
         ;; Track underground state
         underground? false
         has-return-unlock? false]
    (if (empty? remaining)
      {:commands all-commands
       :total-moves (count all-commands)
       :by-action by-action
       :combat-actions combat-actions}
      (let [action (first remaining)
            dest-room (action-location action)

            ;; Calculate blocked edges based on current state
            ;; Trap door return is blocked when underground without unlock
            blocked-edges (if (and underground? (not has-return-unlock?))
                            #{[:cellar :living-room]}
                            #{})

            nav (when (and dest-room (not= dest-room current-room))
                  (generate-navigation game-state current-room dest-room
                                       :blocked-edges blocked-edges))
            nav-commands (or (:commands nav) [])

            ;; Use combat-aware expansion
            action-commands (expand-action-for-transcript action :combat-mode combat-mode)
            combined (into nav-commands action-commands)

            ;; Track combat actions separately
            combat-info (when (action-is-combat? action)
                          {:action (:id action)
                           :spec (expand-combat-action action)})

            ;; Update underground state
            ;; We become underground when navigation goes through trap door
            went-underground? (some #{"down"} nav-commands)
            new-underground? (or underground? went-underground?)

            ;; Update return unlock state
            ;; magic-flag or grating-unlocked allows return via trap door
            just-unlocked? (or (action-sets-flag? action :magic-flag)
                               (action-sets-flag? action :grating-unlocked))
            new-has-unlock? (or has-return-unlock? just-unlocked?)]

        (recur (rest remaining)
               (or dest-room current-room)
               (into all-commands combined)
               (conj by-action {:action (:id action)
                                :from current-room
                                :to dest-room
                                :nav-commands nav-commands
                                :action-commands action-commands
                                :is-combat? (action-is-combat? action)})
               (if combat-info
                 (conj combat-actions combat-info)
                 combat-actions)
               new-underground?
               new-has-unlock?)))))

;; =============================================================================
;; Deposit Trip Insertion
;; =============================================================================

(defn insert-deposit-trips
  "Insert deposit trips when inventory gets too full.

   Takes a plan and inserts deposit actions when carrying
   too many items."
  [game-state plan max-items]
  (loop [remaining plan
         current-inventory #{}
         result []
         current-room :west-of-house]
    (if (empty? remaining)
      result
      (let [action (first remaining)
            new-items (get-in action [:effects :inventory-add] #{})
            after-action (set/union current-inventory new-items)]

        (if (> (count after-action) max-items)
          ;; Need deposit trip
          (let [;; Create deposit actions for current inventory
                deposit-actions (map #(actions/generate-deposit-action %)
                                     current-inventory)]
            (recur remaining  ; Re-process current action after deposit
                   #{}        ; Inventory now empty
                   (into result deposit-actions)
                   :living-room))  ; After deposit we're at living room

          ;; Continue without deposit
          (recur (rest remaining)
                 after-action
                 (conj result action)
                 (or (get-in action [:effects :new-location])
                     (action-location action)
                     current-room)))))))

;; =============================================================================
;; Full Route Optimization
;; =============================================================================

(defn optimize-treasure-route
  "Optimize route for collecting multiple treasures.

   Returns:
   {:route [rooms to visit]
    :commands [command strings]
    :treasures [treasures in collection order]
    :total-moves n}"
  [game-state treasures-to-collect]
  (let [;; Get locations for each treasure
        treasure-locations (into {}
                                 (for [t treasures-to-collect]
                                   [t (actions/find-object-room game-state t)]))
        ;; Filter out treasures we can't locate
        locatable (filter #(treasure-locations %) treasures-to-collect)

        ;; Get unique destination rooms
        dest-rooms (set (vals treasure-locations))

        ;; Optimize route
        optimized-route (nearest-neighbor-route game-state :west-of-house dest-rooms)

        ;; Build commands
        graph (build-nav-graph game-state)
        room-graph (build-room-graph-with-directions game-state)]

    (loop [rooms optimized-route
           current :west-of-house
           all-commands []
           collected []]
      (if (empty? rooms)
        {:route optimized-route
         :commands all-commands
         :treasures collected
         :total-moves (count all-commands)}

        (let [dest (first rooms)
              nav (generate-navigation game-state current dest)
              nav-cmds (or (:commands nav) [])
              ;; Find treasures at this location
              treasures-here (filter #(= (treasure-locations %) dest) locatable)
              take-cmds (mapv #(str "take " (name %)) treasures-here)]
          (recur (rest rooms)
                 dest
                 (-> all-commands
                     (into nav-cmds)
                     (into take-cmds))
                 (into collected treasures-here)))))))

;; =============================================================================
;; Complete Speedrun Command Generation
;; =============================================================================

(defn generate-speedrun-commands
  "Generate complete speedrun command sequence.

   This is the main entry point for generating an optimized
   speedrun from a high-level plan."
  [game-state plan]
  (let [;; Convert plan to commands
        cmd-seq (plan-to-command-sequence game-state plan :west-of-house)

        ;; Calculate statistics
        by-type (group-by #(get-in % [:action] :unknown) (:by-action cmd-seq))]

    {:commands (:commands cmd-seq)
     :total-moves (:total-moves cmd-seq)
     :action-breakdown by-type}))

;; =============================================================================
;; Command Execution Simulation
;; =============================================================================

(defn simulate-commands
  "Simulate executing commands to validate a plan.
   Returns final state after simulation."
  [initial-state commands]
  ;; Simplified simulation - just tracks moves
  {:moves (count commands)
   :commands commands})

;; =============================================================================
;; Debug Utilities
;; =============================================================================

(defn print-route
  "Pretty print an optimized route."
  [{:keys [route commands treasures total-moves]}]
  (println "\n=== Optimized Route ===")
  (println "Rooms:" (count route))
  (println "Commands:" total-moves)
  (println "\nRoute:")
  (doseq [room route]
    (println "  ->" room))
  (println "\nTreasures collected:" (count treasures))
  (doseq [t treasures]
    (println "  -" (name t))))

(defn print-command-sequence
  "Print command sequence with action context."
  [{:keys [commands by-action total-moves]}]
  (println "\n=== Command Sequence (" total-moves "moves) ===")
  (doseq [{:keys [action from to nav-commands action-commands]} by-action]
    (when (or (seq nav-commands) (seq action-commands))
      (println (str "\n# " action))
      (when (and from to (not= from to))
        (println (str "  Navigate: " from " -> " to)))
      (when (seq nav-commands)
        (doseq [cmd nav-commands]
          (println (str "    " cmd))))
      (when (seq action-commands)
        (doseq [cmd action-commands]
          (println (str "    > " cmd)))))))

(defn compare-routes
  "Compare two routes by total moves."
  [route1 route2]
  (println "\n=== Route Comparison ===")
  (println "Route 1:" (:total-moves route1) "moves")
  (println "Route 2:" (:total-moves route2) "moves")
  (let [diff (- (:total-moves route1) (:total-moves route2))]
    (cond
      (pos? diff) (println (str "Route 2 is " diff " moves shorter"))
      (neg? diff) (println (str "Route 1 is " (- diff) " moves shorter"))
      :else (println "Routes are equal length"))))
