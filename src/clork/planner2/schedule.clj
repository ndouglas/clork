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
            [clork.planner2.route :as route]
            [clork.planner2.navigate :as nav]))

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
;;; SCHEDULE GENERATION
;;; ---------------------------------------------------------------------------

(defn generate-schedule
  "Generate a full schedule that interleaves preps, treasures, and waits.

   Returns a sequence of schedule entries:
   [{:type :move :to :room}
    {:type :prep :id :prep-id :action {...}}
    {:type :wait :turns 8 :reason \"dam drain\"}
    {:type :work :treasures [...]}
    {:type :collect :treasure :treasure-id}
    {:type :deposit :treasures [...]}]"
  [game-state treasures]
  (let [route-plan (route/plan-full-route game-state treasures)
        needs-dam? (some #{:sapphire-bracelet :sapphire :jade-figurine}
                         treasures)
        needs-exorcism? (some #{:crystal-skull} treasures)]

    ;; Build schedule
    (concat
     ;; Initial preps (non-timed)
     (for [action route-plan
           :when (and (= :prep (:type action))
                      (not (#{:gates-open :low-tide} (:id action))))]
       action)

     ;; Dam puzzle with overlapped work
     (when needs-dam?
       (let [dam-schedule (schedule-dam-drain game-state
                                               {:treasures treasures})]
         [{:type :move :to :maintenance-room}
          {:type :prep :id :gate-flag
           :action {:verb :push :direct-object :yellow-button}}
          {:type :move :to :dam-room}
          {:type :prep :id :gates-open
           :action (:trigger-action dam-schedule)}
          {:type :parallel-work
           :duration dam-drain-turns
           :treasures (:work-during-wait dam-schedule)}]))

     ;; Non-dam treasures
     (for [action route-plan
           :when (and (= :treasure (:type action))
                      (not (contains? #{:sapphire-bracelet :sapphire
                                        :jade-figurine :crystal-skull}
                                      (:id action))))]
       {:type :collect :treasure (:id action) :location (:location action)})

     ;; Dam-dependent treasures (after drain)
     (when needs-dam?
       (for [t [:sapphire-bracelet :sapphire :jade-figurine]
             :when (some #{t} treasures)]
         {:type :collect :treasure t :location (route/treasure-location t)}))

     ;; Exorcism (atomic sequence)
     (when needs-exorcism?
       (let [ex (schedule-exorcism)]
         [{:type :atomic-sequence
           :name "exorcism"
           :steps [{:type :prep :id :bell-rang :action (:action (:phase-1 ex))}
                   {:type :prep :id :candles-lit :action (:action (:phase-2 ex))}
                   {:type :prep :id :lld-flag :action (:action (:phase-3 ex))}]
           :location :entrance-to-hades}
          {:type :collect :treasure :crystal-skull
           :location :land-of-living-dead}]))

     ;; Final deposit run
     [{:type :deposit-all :location :living-room}])))

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
      (and (< (consumable-remaining timer-state :candles) 10)
           (not (get-in timer-state [:consumables :candles :warned?])))
      (conj {:item :candles :remaining (consumable-remaining timer-state :candles)
             :message "Candles running low! Plan exorcism soon."})

      (and (< (consumable-remaining timer-state :brass-lantern) 30)
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
