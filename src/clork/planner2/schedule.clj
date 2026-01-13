(ns clork.planner2.schedule
  "Timed Effect Scheduler for the Prep-Optimized Planner.

   Handles scheduling of timed effects (dam drain, exorcism windows)
   to overlap with productive work, maximizing turn efficiency.

   Key features:
   - Track active timed effects and their deadlines
   - Find productive actions that fit within time windows
   - Schedule dam drain to overlap with maze exploration
   - Warn about expiring effects (candles, lantern)

   See IMPLEMENTATION_PLAN.md for the full architecture."
  (:require [clork.planner2.prep :as prep]
            [clork.planner2.deps :as deps]
            [clork.planner2.route :as route]
            [clork.planner2.navigate :as nav]
            [clork.game-state :as gs]
            [clojure.set :as set]))

;;; ---------------------------------------------------------------------------
;;; TIMED EFFECT STATE
;;; ---------------------------------------------------------------------------

(defn make-timer-state
  "Create initial timer tracking state."
  []
  {:active-timers {}      ; timer-id -> {:started-turn N :duration M :effect E}
   :consumables {}        ; item-id -> {:remaining N :warned? bool}
   :current-turn 0})

(defn start-timer
  "Start a timed effect."
  [timer-state timer-id duration effect]
  (assoc-in timer-state [:active-timers timer-id]
            {:started-turn (:current-turn timer-state)
             :duration duration
             :effect effect}))

(defn advance-turn
  "Advance the turn counter and update all timers."
  [timer-state]
  (update timer-state :current-turn inc))

(defn timer-remaining
  "Get remaining turns for a timer."
  [timer-state timer-id]
  (when-let [timer (get-in timer-state [:active-timers timer-id])]
    (let [elapsed (- (:current-turn timer-state) (:started-turn timer))
          remaining (- (:duration timer) elapsed)]
      (max 0 remaining))))

(defn timer-complete?
  "Check if a timer has completed."
  [timer-state timer-id]
  (when-let [remaining (timer-remaining timer-state timer-id)]
    (zero? remaining)))

(defn active-timers
  "Get all active (not yet complete) timers."
  [timer-state]
  (for [[id _] (:active-timers timer-state)
        :when (not (timer-complete? timer-state id))]
    id))

;;; ---------------------------------------------------------------------------
;;; CONSUMABLE TRACKING
;;; ---------------------------------------------------------------------------

(defn set-consumable
  "Set remaining turns for a consumable item."
  [timer-state item remaining]
  (assoc-in timer-state [:consumables item] {:remaining remaining :warned? false}))

(defn use-consumable
  "Decrement a consumable by one turn of use."
  [timer-state item]
  (update-in timer-state [:consumables item :remaining] dec))

(defn consumable-remaining
  "Get remaining turns for a consumable."
  [timer-state item]
  (get-in timer-state [:consumables item :remaining] 0))

(defn consumable-low?
  "Check if a consumable is running low (< 20% remaining)."
  [timer-state item initial-amount]
  (let [remaining (consumable-remaining timer-state item)]
    (< remaining (* initial-amount 0.2))))

(defn mark-consumable-warned
  "Mark that we've warned about a low consumable."
  [timer-state item]
  (assoc-in timer-state [:consumables item :warned?] true))

;;; ---------------------------------------------------------------------------
;;; DAM PUZZLE SCHEDULING
;;; ---------------------------------------------------------------------------

(def dam-drain-turns 8)  ; Turns for reservoir to drain after opening gates

(defn schedule-dam-drain
  "Schedule dam drain to overlap with other work.

   The dam drain takes 8 turns. We want to:
   1. Open gates
   2. Do 8+ turns of productive work nearby
   3. Return to reservoir area when drained

   Returns a schedule of actions to perform during drain."
  [game-state route-plan]
  (let [;; Find treasures that can be collected during drain
        ;; These should be in areas accessible without low-tide
        non-tide-treasures [:bag-of-coins :platinum-bar :painting]
        drain-work (filter #(contains? (set non-tide-treasures) %)
                           (:treasures route-plan))]
    {:trigger-action {:verb :turn :direct-object :bolt :indirect-object :wrench}
     :trigger-location :dam-room
     :wait-time dam-drain-turns
     :work-during-wait drain-work
     :resume-after [:sapphire-bracelet :sapphire :jade-figurine]}))

(defn turns-for-work
  "Estimate turns needed for a set of work items."
  [game-state work-items start-location]
  ;; Rough estimate: 2 turns per item (travel + action)
  ;; TODO: Use actual distances
  (* (count work-items) 5))

(defn can-fit-in-window?
  "Check if work can be completed within a time window."
  [game-state work-items start-location window-turns]
  (<= (turns-for-work game-state work-items start-location) window-turns))

;;; ---------------------------------------------------------------------------
;;; EXORCISM SCHEDULING
;;; ---------------------------------------------------------------------------

(def exorcism-bell-window 6)   ; Turns after ringing bell to light candles
(def exorcism-candle-window 3) ; Turns after lighting candles to read book

(defn schedule-exorcism
  "Schedule the exorcism sequence.

   The exorcism has tight timing:
   1. Ring bell (starts 6-turn window)
   2. Pick up and light candles (must complete in 6 turns)
   3. Read book (must complete in 3 turns after candles)

   This cannot overlap with other work - must be done atomically."
  []
  {:phase-1 {:action {:verb :ring :direct-object :bell}
             :location :entrance-to-hades
             :window exorcism-bell-window}
   :phase-2 {:action {:verb :light :direct-object :candles :indirect-object :matches}
             :location :entrance-to-hades
             :window exorcism-candle-window}
   :phase-3 {:action {:verb :read :direct-object :book}
             :location :entrance-to-hades}
   :required-items #{:bell :candles :matches :book}
   :interruptible? false})

;;; ---------------------------------------------------------------------------
;;; PRODUCTIVE WORK FINDER
;;; ---------------------------------------------------------------------------

(defn nearby-treasures
  "Find treasures that are near a location."
  [game-state location max-distance]
  (let [graph (nav/build-room-graph game-state)
        distances (:dist (route/floyd-warshall graph))]
    (for [[treasure loc] route/treasure-locations
          :let [dist (route/distance distances location loc)]
          :when (and (< dist Integer/MAX_VALUE)
                     (<= dist max-distance))]
      {:treasure treasure
       :location loc
       :distance dist})))

(defn work-for-window
  "Find productive work that can be done within a time window.

   Returns a list of actions that fit within the window,
   ordered by efficiency (value / time)."
  [game-state current-location window-turns already-collected]
  (let [;; Find nearby treasures
        candidates (nearby-treasures game-state current-location (quot window-turns 2))
        ;; Filter out already collected
        available (remove #(contains? already-collected (:treasure %)) candidates)
        ;; Sort by distance (efficiency proxy)
        sorted (sort-by :distance available)]
    ;; Greedily select treasures that fit
    (loop [remaining sorted
           selected []
           turns-used 0]
      (if (empty? remaining)
        selected
        (let [candidate (first remaining)
              ;; Estimate turns: there + action + back
              turns-needed (+ (* 2 (:distance candidate)) 2)]
          (if (<= (+ turns-used turns-needed) window-turns)
            (recur (rest remaining)
                   (conj selected (:treasure candidate))
                   (+ turns-used turns-needed))
            (recur (rest remaining) selected turns-used)))))))

;;; ---------------------------------------------------------------------------
;;; OPPORTUNISTIC COLLECTION
;;; ---------------------------------------------------------------------------
;;; When navigating between locations, check if we pass through rooms that
;;; contain treasures we want. If so, collect them along the way to avoid
;;; leaving them for the thief to steal.

(defn treasures-on-path
  "Find treasures that are located along a navigation path.

   Parameters:
     game-state - Current game state
     path - Sequence of room-ids we'll traverse
     target-treasures - Set of treasure-ids we want to collect

   Returns map of {room-id -> #{treasure-ids}} for treasures on the path.
   Multiple treasures at the same location are grouped together."
  [game-state path target-treasures]
  (let [path-set (set path)
        treasure-locs (for [t target-treasures
                           :let [loc (route/treasure-location t)]
                           :when (contains? path-set loc)]
                       [loc t])]
    ;; Group by room, since multiple treasures can be at the same location
    (reduce (fn [m [loc t]]
              (update m loc (fnil conj #{}) t))
            {}
            treasure-locs)))

(defn calculate-route-path
  "Calculate the full navigation path between two locations.
   Returns vector of room-ids or nil if unreachable."
  [game-state from-room to-room]
  (when (and from-room to-room (not= from-room to-room))
    (let [graph (nav/build-room-graph game-state)]
      (nav/find-path graph from-room to-room))))

(defn prep-sets-flag
  "Get the flag that a prep action will set, or nil."
  [prep-entry]
  (let [id (:id prep-entry)]
    ;; Common prep IDs that correspond to flags
    (case id
      :troll-flag :troll-flag
      :cyclops-flag :cyclops-flag
      :dome-flag :dome-flag
      :gate-flag :gate-flag
      :lld-flag :lld-flag
      :magic-flag :magic-flag
      :rug-moved :rug-moved
      :trap-door-open :trap-door-open
      nil)))

(defn calculate-route-path-with-flags
  "Calculate the full navigation path between two locations,
   using a game-state that has the given flags set."
  [game-state from-room to-room flags]
  (when (and from-room to-room (not= from-room to-room))
    (let [;; Create a modified game-state with flags set
          modified-state (reduce (fn [gs flag]
                                   (assoc gs flag true))
                                 game-state
                                 flags)
          graph (nav/build-room-graph modified-state)]
      (nav/find-path graph from-room to-room))))

(defn inject-opportunistic-collections
  "Post-process schedule to collect treasures when passing through their rooms.

   This prevents the thief from stealing treasures in rooms we visit but
   don't collect from immediately.

   Strategy:
   1. Extract all scheduled treasure collections
   2. Scan schedule for preps that set flags (for path calculation)
   3. Process schedule entries, tracking navigation paths with accumulated flags
   4. When we pass through a room with a scheduled treasure, collect it then
   5. Remove duplicate collections (treasure already collected opportunistically)

   Returns modified schedule with reordered collections."
  [game-state schedule treasures]
  (let [;; Find all :collect entries in schedule and their locations
        collect-entries (->> schedule
                             (filter #(= :collect (:type %)))
                             (map (fn [e] [(:treasure e) e]))
                             (into {}))
        pending-treasures (atom (set (keys collect-entries)))]

    ;; Process schedule entries and inject collections at optimal points
    ;; Track flags set by preps so we can compute paths accurately
    (loop [result []
           remaining schedule
           current-location :west-of-house
           active-flags #{}]  ; Flags set by preps so far
      (if (empty? remaining)
        ;; Add any remaining uncollected treasures at the end
        (let [uncollected @pending-treasures
              remaining-entries (map #(get collect-entries %) uncollected)]
          (into result remaining-entries))

        (let [entry (first remaining)
              entry-type (:type entry)

              ;; Track flags set by preps
              new-flag (when (= :prep entry-type)
                         (prep-sets-flag entry))
              updated-flags (if new-flag
                              (conj active-flags new-flag)
                              active-flags)

              ;; Skip existing :collect entries - we'll add them opportunistically
              is-collect? (= :collect entry-type)]

          (if is-collect?
            ;; Handle :collect entries
            (cond
              ;; Fixed entries (above-ground) - keep in place, mark as collected
              (:fixed? entry)
              (do
                (swap! pending-treasures disj (:treasure entry))
                (let [entry-location (:location entry)]
                  (recur (conj result entry) (rest remaining)
                         (or entry-location current-location) updated-flags)))

              ;; Already collected opportunistically - skip this duplicate
              (not (contains? @pending-treasures (:treasure entry)))
              (recur result (rest remaining) current-location updated-flags)

              ;; Not collected yet - skip for now, might collect it later opportunistically
              :else
              (recur result (rest remaining) current-location updated-flags))

            ;; Non-collect entry: check if we pass through any treasure rooms
            (let [entry-location (or (:location entry) (:to entry))

                  ;; Calculate path using flags that will be set by this point
                  path (when (and entry-location (not= current-location entry-location))
                         (calculate-route-path-with-flags
                          game-state current-location entry-location updated-flags))

                  ;; Find pending treasures along the path
                  path-treasures (when (and path (seq @pending-treasures))
                                   (treasures-on-path game-state path @pending-treasures))

                  ;; Create collection entries for treasures we pass through
                  ;; Order them by their position in the path
                  ;; path-treasures is {room -> #{treasures}}, so flatten to entries
                  ;; IMPORTANT: Only opportunistically collect treasures that:
                  ;; 1. Are high-priority (vulnerable) - not safe treasures
                  ;; 2. Have all their prep requirements satisfied
                  ;; This prevents collecting platinum-bar before loud-flag, etc.
                  opportunistic-entries
                  (when (seq path-treasures)
                    (let [path-index (zipmap path (range))
                          ;; Check if all preps for a treasure are in active-flags
                          preps-satisfied?
                          (fn [treasure]
                            (let [required-preps (deps/preps-for-treasure treasure)]
                              (every? #(contains? updated-flags %) required-preps)))]
                      (->> path-treasures
                           ;; Sort rooms by path order
                           (sort-by (fn [[room _]] (get path-index room 999)))
                           ;; Expand each room's treasures into entries
                           (mapcat (fn [[room treasures]]
                                     (map (fn [treasure]
                                            (-> (get collect-entries treasure)
                                                (assoc :opportunistic? true)))
                                          treasures)))
                           ;; Filter out safe treasures - don't collect them early
                           ;; This preserves vulnerability-based ordering
                           (remove :safe?)
                           ;; Filter out treasures whose preps aren't done yet
                           (filter #(preps-satisfied? (:treasure %))))))

                  ;; Track which treasures we're collecting
                  newly-collected (set (map :treasure opportunistic-entries))]

              ;; Update pending treasures
              (swap! pending-treasures #(set/difference % newly-collected))

              ;; Update location after this entry
              (let [next-location (or entry-location current-location)]
                (recur (into result (concat opportunistic-entries [entry]))
                       (rest remaining)
                       next-location
                       updated-flags)))))))))

(defn optimize-collection-order
  "Reorder treasure collections to grab treasures when passing through.

   This is the main entry point for opportunistic collection optimization.
   It wraps inject-opportunistic-collections, removes duplicate collections,
   and ensures deposit-all entries come AFTER all collections."
  [game-state schedule treasures]
  (let [optimized (inject-opportunistic-collections game-state schedule treasures)
        ;; Remove any duplicate collections (scheduled treasure that was also opportunistic)
        seen (atom #{})
        deduped (filter (fn [entry]
                         (if (= :collect (:type entry))
                           (let [t (:treasure entry)]
                             (if (contains? @seen t)
                               false
                               (do (swap! seen conj t) true)))
                           true))
                       optimized)
        ;; Separate deposit-all entries and move them to the end
        ;; (deposit should happen after all collections, not in the middle)
        deposit-entries (filter #(= :deposit-all (:type %)) deduped)
        non-deposit (remove #(= :deposit-all (:type %)) deduped)]
    (vec (concat non-deposit deposit-entries))))

;;; ---------------------------------------------------------------------------
;;; INVENTORY MANAGEMENT
;;; ---------------------------------------------------------------------------
;;; Track inventory usage and insert deposit trips when approaching the limit.
;;; This prevents "You are carrying too many things" errors during execution.

(def inventory-limit
  "Maximum number of items the player can carry.
   In Zork, this is typically 7 items by count (not weight)."
  7)

(def essential-items
  "Items we must keep in inventory throughout the speedrun.
   These count against our carrying capacity."
  #{:brass-lantern :sword})

(def inventory-buffer
  "Keep this many slots free for essential items and contingencies."
  2)

(defn effective-capacity
  "Get the effective treasure capacity, accounting for essential items."
  []
  (- inventory-limit (count essential-items) inventory-buffer))

(defn entry-inventory-change
  "Get the inventory change for a schedule entry.
   Returns positive number for items acquired, negative for items dropped."
  [entry]
  (case (:type entry)
    :collect 1      ; Picking up a treasure
    :prep (case (:id entry)
            :have-lantern 1
            :have-sword 1
            0)
    :deposit-all 0  ; Handled specially - resets to essentials
    :deposit (- (count (:treasures entry [])))
    0))

(defn inject-deposit-trips
  "Post-process schedule to insert deposit trips when inventory is full.

   When the player's inventory approaches the limit, insert a trip to
   the living room to deposit treasures before continuing.

   Returns modified schedule with deposit trips added."
  [game-state schedule]
  (let [capacity (effective-capacity)]
    (loop [result []
           remaining schedule
           inventory 0]  ; Current treasure count (excluding essentials)
      (if (empty? remaining)
        result
        (let [entry (first remaining)
              change (entry-inventory-change entry)
              new-inventory (+ inventory change)]

          (cond
            ;; deposit-all resets inventory to 0
            (= :deposit-all (:type entry))
            (recur (conj result entry) (rest remaining) 0)

            ;; Would exceed capacity - need deposit trip first
            (and (pos? change) (> new-inventory capacity))
            (let [deposit-entry {:type :deposit-run
                                 :location :living-room
                                 :reason :inventory-full
                                 :treasures-held inventory}]
              ;; Insert deposit, then process this entry with fresh inventory
              (recur (conj result deposit-entry entry)
                     (rest remaining)
                     change))  ; After deposit: 0 + this item's change

            ;; Normal case - accumulate inventory
            :else
            (recur (conj result entry) (rest remaining) new-inventory)))))))

(defn estimate-deposit-trips
  "Estimate how many deposit trips will be needed for a set of treasures.
   Useful for planning and turn estimation."
  [num-treasures]
  (let [cap (effective-capacity)]
    (if (<= num-treasures cap)
      0
      (int (Math/ceil (/ (- num-treasures cap) cap))))))

;;; ---------------------------------------------------------------------------
;;; ABOVE-GROUND / UNDERGROUND TREASURE CLASSIFICATION
;;; ---------------------------------------------------------------------------
;;; The trap door is one-way: once underground, you can't return to above-ground
;;; areas (except via special exits like prayer teleport or the grating).
;;; Therefore, we must collect above-ground treasures BEFORE going underground.

(def above-ground-rooms
  "Rooms accessible from the surface before going through the trap door.
   These areas include the forest, house exterior, and inside the house
   (before descending through the trap door)."
  #{;; House exterior
    :west-of-house :north-of-house :south-of-house :behind-house
    ;; Inside house (ground floor)
    :kitchen :living-room :attic
    ;; Forest and clearings
    :forest-1 :forest-2 :forest-3 :forest-path
    :clearing :north-clearing :east-clearing :canyon-view
    ;; Tree
    :up-a-tree
    ;; River bank (surface)
    :stone-barrow})

(defn above-ground-treasure?
  "Check if a treasure is located in an above-ground area.
   These treasures should be collected before going underground."
  [treasure]
  (let [loc (route/treasure-location treasure)]
    (contains? above-ground-rooms loc)))

(defn classify-treasures
  "Separate treasures into above-ground and underground collections.
   Returns {:above-ground [...] :underground [...]}."
  [treasures]
  (let [{above true underground false}
        (group-by above-ground-treasure? treasures)]
    {:above-ground (vec (or above []))
     :underground (vec (or underground []))}))

;;; ---------------------------------------------------------------------------
;;; THIEF-AWARE TREASURE PRIORITIZATION
;;; ---------------------------------------------------------------------------
;;; The thief can steal treasures from rooms the player has visited (TOUCHBIT set)
;;; but ONLY from non-sacred rooms. Sacred rooms are protected.
;;; We prioritize collecting treasures from vulnerable (non-sacred) rooms first.

(defn treasure-in-sacred-room?
  "Check if a treasure is located in a sacred room.
   Treasures in sacred rooms are protected from thief theft."
  [game-state treasure]
  (let [loc (route/treasure-location treasure)]
    (when loc
      (gs/set-thing-flag? game-state loc :sacred))))

(defn treasure-vulnerability
  "Classify a treasure by its vulnerability to theft.
   Returns :safe (sacred room), :vulnerable (non-sacred), or :unknown.

   Vulnerabilities:
   - :safe - In sacred room, thief cannot steal
   - :vulnerable - In non-sacred room, thief can steal once visited
   - :above-ground - Above ground, collected before going underground (special case)"
  [game-state treasure]
  (let [loc (route/treasure-location treasure)]
    (cond
      (nil? loc) :unknown
      (contains? above-ground-rooms loc) :above-ground
      (gs/set-thing-flag? game-state loc :sacred) :safe
      :else :vulnerable)))

(defn prioritize-treasures-by-vulnerability
  "Sort treasures to collect vulnerable ones first.

   Priority order:
   1. Above-ground treasures (must collect before going underground)
   2. Vulnerable treasures (non-sacred rooms, thief can steal)
   3. Safe treasures (sacred rooms, thief cannot steal)

   Within each category, maintain the original route-optimized order."
  [game-state treasures]
  (let [vuln-map (group-by #(treasure-vulnerability game-state %) treasures)]
    (vec (concat
          ;; Above-ground always first (constraint, not optimization)
          (get vuln-map :above-ground [])
          ;; Then vulnerable treasures (thief risk)
          (get vuln-map :vulnerable [])
          ;; Then safe treasures (no thief risk)
          (get vuln-map :safe [])
          ;; Unknown at the end
          (get vuln-map :unknown [])))))

(defn estimate-thief-exposure
  "Estimate how many turns treasures are exposed to theft risk.
   Higher exposure = higher priority to collect.

   Returns map of treasure -> exposure-turns estimate."
  [game-state schedule treasures]
  ;; Simple estimate: turns from start until treasure is collected
  (let [collect-indices (into {}
                              (keep-indexed
                               (fn [idx entry]
                                 (when (= :collect (:type entry))
                                   [(:treasure entry) idx]))
                               schedule))]
    (into {}
          (for [t treasures
                :let [vuln (treasure-vulnerability game-state t)
                      idx (get collect-indices t 999)
                      ;; Vulnerable treasures have higher exposure
                      base-exposure (case vuln
                                      :vulnerable idx
                                      :safe 0
                                      :above-ground 0  ; Collected before underground
                                      idx)]]
            [t base-exposure]))))

(defn analyze-schedule-thief-risk
  "Analyze a schedule for thief-related risks.

   Returns:
   {:vulnerable-treasures [...] - treasures in non-sacred rooms
    :safe-treasures [...] - treasures in sacred rooms
    :total-exposure N - estimated turns of exposure
    :recommendations [...] - suggestions for improvement}"
  [game-state schedule treasures]
  (let [vuln-map (group-by #(treasure-vulnerability game-state %) treasures)
        vulnerable (:vulnerable vuln-map [])
        safe (:safe vuln-map [])
        exposure (estimate-thief-exposure game-state schedule treasures)
        total-exposure (reduce + 0 (vals exposure))]
    {:vulnerable-treasures (vec vulnerable)
     :safe-treasures (vec safe)
     :total-exposure total-exposure
     :exposure-by-treasure exposure
     :recommendations
     (cond-> []
       (> (count vulnerable) 3)
       (conj "Many vulnerable treasures - consider deposit trips to reduce exposure")

       (and (seq vulnerable) (> total-exposure 50))
       (conj "High thief exposure - prioritize faster collection of vulnerable items")

       (> (count safe) (count vulnerable))
       (conj "Most treasures are safe - thief risk is low"))}))

(defn classify-and-prioritize-treasures
  "Separate treasures into above-ground and underground, then prioritize
   underground treasures by thief vulnerability.

   This is the main entry point for thief-aware treasure ordering.

   Returns {:above-ground [...] :underground [...]}
   where underground treasures are sorted with vulnerable (non-sacred) first."
  [game-state treasures]
  (let [{:keys [above-ground underground]} (classify-treasures treasures)
        ;; Sort underground by vulnerability (vulnerable before safe)
        sorted-underground (if (seq underground)
                            (let [vuln-sorted
                                  (sort-by #(case (treasure-vulnerability game-state %)
                                              :vulnerable 0
                                              :safe 1
                                              :unknown 2
                                              1)
                                           underground)]
                              (vec vuln-sorted))
                            [])]
    {:above-ground above-ground
     :underground sorted-underground}))

;;; ---------------------------------------------------------------------------
;;; SCHEDULE GENERATION
;;; ---------------------------------------------------------------------------

(def above-ground-preps
  "Preps required for accessing above-ground treasures.
   These are done before going underground."
  [{:type :prep :id :kitchen-window-open
    :location :behind-house
    :action {:verb :open :direct-object :kitchen-window}}])

(def underground-entry-preps
  "Preps required for entering underground areas.
   These include getting essential items (sword, lantern) and opening the trap door."
  [{:type :prep :id :rug-moved
    :location :living-room
    :action {:verb :move :direct-object :rug}}
   {:type :prep :id :trap-door-open
    :location :living-room
    :action {:verb :open :direct-object :trap-door}}
   {:type :prep :id :have-lantern
    :location :living-room
    :action {:verb :take :direct-object :brass-lantern}}
   {:type :prep :id :lantern-on
    :location :living-room
    :action {:verb :lamp-on :direct-object :brass-lantern}}
   {:type :prep :id :have-sword
    :location :living-room
    :action {:verb :take :direct-object :sword}}])

(defn generate-schedule
  "Generate a full schedule that interleaves preps, treasures, and waits.

   IMPORTANT: The schedule respects the above-ground / underground constraint.
   The trap door is one-way (closes behind you), so we MUST:
   1. Collect all above-ground treasures FIRST
   2. Then enter underground and collect underground treasures
   3. Exit via grating or prayer teleport (not back through trap door)

   Returns a sequence of schedule entries:
   [{:type :move :to :room}
    {:type :prep :id :prep-id :action {...}}
    {:type :wait :turns 8 :reason \"dam drain\"}
    {:type :work :treasures [...]}
    {:type :collect :treasure :treasure-id}
    {:type :deposit :treasures [...]}]

   The schedule is post-processed to inject opportunistic collections -
   grabbing treasures when passing through rooms that contain them,
   preventing the thief from stealing them."
  [game-state treasures]
  (let [;; Classify treasures by location (above/underground) with thief-aware prioritization
        ;; Underground treasures are sorted to collect vulnerable ones first
        {:keys [above-ground underground]} (classify-and-prioritize-treasures game-state treasures)

        ;; Get route plan for underground treasures (for prep extraction)
        route-plan (when (seq underground)
                     (route/plan-full-route game-state underground))

        needs-dam? (some #{:sapphire-bracelet :sapphire :jade-figurine} treasures)
        needs-exorcism? (some #{:crystal-skull} treasures)
        needs-loud-room? (some #{:platinum-bar} treasures)
        needs-rainbow? (some #{:pot-of-gold} treasures)
        needs-underground? (seq underground)

        ;; Build base schedule with proper sequencing
        base-schedule
        (vec
         (concat
          ;; Phase 1: Above-ground preps (open kitchen window for house access)
          above-ground-preps

          ;; Phase 2: Collect ABOVE-GROUND treasures FIRST (before trap door)
          ;; This is critical because trap door is one-way!
          ;; Mark these as :fixed to prevent opportunistic reordering
          (when (seq above-ground)
            (for [t above-ground]
              {:type :collect :treasure t :location (route/treasure-location t)
               :fixed? true}))

          ;; Phase 2b: Deposit above-ground treasures (conditionally)
          ;; IMPORTANT: For full speedruns that include the clockwork canary, we WANT
          ;; the thief to steal the egg because he opens it, revealing the canary inside.
          ;; When we kill the thief, he drops both egg and canary.
          ;; Only deposit immediately if we're NOT planning to kill the thief.
          (when (and (seq above-ground)
                     (not (some #{:clockwork-canary :trunk} treasures)))
            [{:type :deposit-run
              :location :living-room
              :reason :thief-protection}])

          ;; Phase 3: Underground entry preps (only if we have underground treasures)
          (when needs-underground?
            underground-entry-preps)

          ;; Phase 4: Troll-flag (must be done first for underground access)
          (when needs-underground?
            (for [action route-plan
                  :when (and (= :prep (:type action))
                             (= :troll-flag (:id action)))]
              action))

          ;; Phase 4b: Loud room puzzle (IMMEDIATELY after troll)
          ;; CRITICAL: Must do this early before thief can steal platinum-bar!
          ;; Only requires troll-flag to access, so we do it before other preps.
          (when needs-loud-room?
            [{:type :atomic-sequence
              :name "loud-room"
              :steps [{:type :prep :id :loud-flag
                       :action {:verb :echo}}]
              :location :loud-room
              :collect-after :platinum-bar}])

          ;; Phase 4c: Other underground treasure preps (cyclops, dome, etc.)
          ;; Excludes:
          ;; - Entry preps (handled in Phase 3)
          ;; - Dam preps (handled in Phase 5)
          ;; - Loud room prep (handled in Phase 4b)
          ;; - Dome-flag (handled in Phase 4d - must collect torch-room treasures immediately)
          ;; - Rainbow prep (handled in Phase 6b - needs sceptre which requires dome-flag)
          ;; - Exorcism preps (handled in Phase 8 atomic-sequence)
          ;; - Troll flag (handled in Phase 4)
          (when needs-underground?
            (for [action route-plan
                  :when (and (= :prep (:type action))
                             (not (#{;; Entry preps
                                     :kitchen-window-open :rug-moved :trap-door-open
                                     ;; Dam preps
                                     :gates-open :low-tide
                                     ;; Loud room prep (handled above)
                                     :loud-flag
                                     ;; Dome-flag (handled in Phase 4d with immediate torch collection)
                                     :dome-flag
                                     ;; Coffin-cure requires south-temple which needs dome-flag
                                     :coffin-cure
                                     ;; Rainbow prep (needs sceptre from torch-room which needs dome-flag)
                                     :rainbow-flag
                                     ;; Exorcism preps (handled in atomic-sequence)
                                     :bell-rang :candles-lit :lld-flag
                                     ;; Troll flag (handled above)
                                     :troll-flag} (:id action))))]
              action))

          ;; Phase 4d: Dome puzzle + torch-room treasures (CRITICAL: collect immediately!)
          ;; The path to egypt-room and other destinations goes THROUGH torch-room.
          ;; If we set dome-flag and then navigate elsewhere, we visit torch-room
          ;; which sets TOUCHBIT, allowing the thief to steal ivory-torch and sceptre.
          ;; Solution: immediately after dome-flag, collect torch-room treasures.
          ;; NOTE: dome-flag is also needed for gold-coffin (coffin-cure requires south-temple access)
          (let [needs-dome-treasures? (some #{:ivory-torch :sceptre} treasures)
                needs-dome-for-coffin? (some #{:gold-coffin} treasures)
                needs-dome? (or needs-dome-treasures? needs-dome-for-coffin?)]
            (when needs-dome?
              (concat
               ;; Always do dome-flag prep
               [{:type :atomic-sequence
                 :name "dome-and-torch"
                 :steps [{:type :prep :id :dome-flag
                          :action {:verb :tie :direct-object :rope :indirect-object :railing}}]
                 :location :dome-room
                 ;; Only collect ivory-torch if it's in the treasure list
                 :collect-after (when needs-dome-treasures? :ivory-torch)}]
               ;; Sceptre is at torch-room too - collect immediately after ivory-torch
               (when (some #{:sceptre} treasures)
                 [{:type :collect :treasure :sceptre :location :torch-room
                   :fixed? true :priority :immediate}]))))

          ;; Phase 4e: Preps that require dome-flag access (south-temple, egypt-room, etc.)
          ;; These must come AFTER dome-flag is set in Phase 4d.
          (let [needs-coffin? (some #{:gold-coffin} treasures)]
            (when needs-coffin?
              [{:type :prep :id :coffin-cure
                :location :south-temple
                :action {:verb :pray}}]))

          ;; Phase 5: Dam puzzle with overlapped work
          (when needs-dam?
            (let [dam-schedule (schedule-dam-drain game-state {:treasures treasures})]
              [{:type :move :to :maintenance-room}
               {:type :prep :id :gate-flag
                :action {:verb :push :direct-object :yellow-button}}
               {:type :move :to :dam-room}
               {:type :prep :id :gates-open
                :action (:trigger-action dam-schedule)}
               {:type :parallel-work
                :duration dam-drain-turns
                :treasures (:work-during-wait dam-schedule)}]))

          ;; Phase 6: Underground treasures (non-dam, non-exorcism, non-loud-room, non-rainbow, non-torch)
          ;; Uses vulnerability-prioritized underground list, NOT route-plan order
          ;; This ensures vulnerable treasures (non-sacred rooms) are collected
          ;; before safe ones, reducing thief theft risk.
          ;; All entries are marked :fixed? to prevent opportunistic reordering.
          (when needs-underground?
            (for [t underground
                  :when (not (contains? #{:sapphire-bracelet :sapphire
                                          :jade-figurine :crystal-skull
                                          :platinum-bar   ; Handled in loud-room atomic sequence
                                          :ivory-torch    ; Handled in dome-and-torch atomic sequence
                                          :sceptre        ; Handled in dome-and-torch atomic sequence
                                          :pot-of-gold}   ; Handled in rainbow atomic sequence
                                        t))
                  :let [vuln (treasure-vulnerability game-state t)]]
              {:type :collect
               :treasure t
               :location (route/treasure-location t)
               :fixed? true  ; Prevent opportunistic reordering
               :safe? (= :safe vuln)}))

          ;; Phase 6b: Rainbow puzzle (after collecting sceptre in Phase 4d)
          ;; CRITICAL: Must collect sceptre BEFORE we can wave it at the rainbow.
          ;; Sceptre is collected in Phase 4d (dome-and-torch atomic sequence),
          ;; so this can happen after Phase 4d completes.
          (when needs-rainbow?
            [{:type :atomic-sequence
              :name "rainbow"
              :steps [{:type :prep :id :rainbow-flag
                       :action {:verb :wave :direct-object :sceptre}}]
              :location :end-of-rainbow
              :collect-after :pot-of-gold}])

          ;; Phase 7: Dam-dependent treasures (after drain)
          (when needs-dam?
            (for [t [:sapphire-bracelet :sapphire :jade-figurine]
                  :when (some #{t} treasures)]
              {:type :collect :treasure t :location (route/treasure-location t)}))

          ;; Phase 7b: Exorcism item collection (before exorcism sequence)
          ;; The exorcism requires: brass-bell, candles, matchbook, black-book
          ;; These must be collected before the exorcism atomic sequence runs.
          (when needs-exorcism?
            [{:type :prep :id :have-brass-bell
              :location :north-temple
              :action {:verb :take :direct-object :brass-bell}}
             {:type :prep :id :have-candles
              :location :south-temple
              :action {:verb :take :direct-object :candles}}
             {:type :prep :id :have-black-book
              :location :south-temple
              :action {:verb :take :direct-object :black-book}}
             {:type :prep :id :have-matchbook
              :location :dam-lobby
              :action {:verb :take :direct-object :matchbook}}])

          ;; Phase 8: Exorcism (atomic sequence)
          ;; IMPORTANT: When bell is rung, candles drop and extinguish!
          ;; Sequence must: ring bell -> take candles -> light candles -> read book
          ;; NOTE: Use :lamp-on verb (not :light) for candles
          (when needs-exorcism?
            (let [ex (schedule-exorcism)]
              [{:type :atomic-sequence
                :name "exorcism"
                :steps [{:type :prep :id :bell-rang :action {:verb :ring :direct-object :brass-bell}}
                        {:type :prep :id :candles-recovered :action {:verb :take :direct-object :candles}}
                        {:type :prep :id :candles-lit :action {:verb :lamp-on :direct-object :candles :indirect-object :matchbook}}
                        {:type :prep :id :lld-flag :action {:verb :read :direct-object :black-book}}]
                :location :entrance-to-hades}
               {:type :collect :treasure :crystal-skull
                :location :land-of-living-dead}]))

          ;; Phase 9: Final deposit run
          [{:type :deposit-all :location :living-room}]))]

    ;; Post-process schedule:
    ;; 1. Inject opportunistic collections (grab treasures when passing through)
    ;; 2. Inject deposit trips when inventory is full
    (-> base-schedule
        (as-> s (optimize-collection-order game-state s treasures))
        (as-> s (inject-deposit-trips game-state s)))))

;;; ---------------------------------------------------------------------------
;;; SCHEDULE ANALYSIS
;;; ---------------------------------------------------------------------------

(defn schedule-turns
  "Estimate total turns for a schedule."
  [schedule]
  (reduce
   (fn [total entry]
     (+ total
        (case (:type entry)
          :move 5  ; Average movement
          :prep 2  ; Action turns
          :wait (:turns entry)
          :parallel-work (:duration entry)
          :collect 2  ; Get treasure
          :deposit-all 20  ; Multiple deposits
          :atomic-sequence 10  ; Exorcism
          1)))
   0
   schedule))

(defn schedule-efficiency
  "Calculate efficiency of a schedule (treasures per turn)."
  [schedule treasures]
  (let [turns (schedule-turns schedule)
        treasure-count (count treasures)]
    (if (zero? turns)
      0
      (/ treasure-count turns))))

(defn print-schedule
  "Print a human-readable schedule."
  [schedule]
  (println "=== Speedrun Schedule ===")
  (println "Estimated turns:" (schedule-turns schedule))
  (println)
  (doseq [[idx entry] (map-indexed vector schedule)]
    (println (str (inc idx) ". "
                  (case (:type entry)
                    :move (str "Move to " (name (:to entry)))
                    :prep (str "Prep: " (name (:id entry)))
                    :wait (str "Wait " (:turns entry) " turns (" (:reason entry) ")")
                    :parallel-work (str "During wait, collect: "
                                        (vec (map name (:treasures entry))))
                    :collect (str "Collect " (name (:treasure entry))
                                  " at " (name (:location entry)))
                    :deposit-all "Return to living room and deposit all"
                    :atomic-sequence (str "Execute " (:name entry) " sequence")
                    (str entry))))))

;;; ---------------------------------------------------------------------------
;;; WARNINGS
;;; ---------------------------------------------------------------------------

(defn check-consumable-warnings
  "Check if any consumables need warnings."
  [timer-state]
  (let [warnings []]
    (cond-> warnings
      ;; Only warn if candles are tracked and running low
      (and (contains? (:consumables timer-state) :candles)
           (< (consumable-remaining timer-state :candles) 10)
           (not (get-in timer-state [:consumables :candles :warned?])))
      (conj {:item :candles :remaining (consumable-remaining timer-state :candles)
             :message "Candles running low! Plan exorcism soon."})

      ;; Only warn if lantern is tracked and running low
      (and (contains? (:consumables timer-state) :brass-lantern)
           (< (consumable-remaining timer-state :brass-lantern) 30)
           (not (get-in timer-state [:consumables :brass-lantern :warned?])))
      (conj {:item :brass-lantern :remaining (consumable-remaining timer-state :brass-lantern)
             :message "Lantern running low! Finish underground exploration."}))))

(defn check-timer-warnings
  "Check if any timers are about to expire."
  [timer-state]
  (for [id (active-timers timer-state)
        :let [remaining (timer-remaining timer-state id)]
        :when (and remaining (< remaining 3))]
    {:timer id :remaining remaining
     :message (str "Timer " (name id) " expires in " remaining " turns!")}))
