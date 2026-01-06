(ns clork.daemon
  "Daemon system - ZIL-equivalent QUEUE/INT/CLOCKER.

   Daemons are background processes that run each turn. They handle:
   - NPCs (thief movement, troll behavior)
   - Timed events (lantern countdown, candles burning)
   - Environmental effects (water rising, cave-ins)

   ZIL Reference: gclock.zil

   Tick semantics (match ZIL):
   - tick = -1: Run every turn (continuous daemon)
   - tick = 0:  Run this turn, then disable
   - tick > 0:  Countdown; run when reaches 0, then disable

   State structure:
   {:daemons {:daemon-id {:enabled true
                          :tick -1
                          :data {}}}
    :daemon-queue []       ;; Ordered list of daemon-ids to process
    :daemon-history []}    ;; Execution log for debugging"
  (:require [clork.debug.trace :as trace]))

;;; ---------------------------------------------------------------------------
;;; DAEMON STATE ACCESS
;;; ---------------------------------------------------------------------------

(defn get-daemon
  "Get a daemon entry by id."
  [game-state daemon-id]
  (get-in game-state [:daemons daemon-id]))

(defn daemon-enabled?
  "Check if a daemon is enabled."
  [game-state daemon-id]
  (get-in game-state [:daemons daemon-id :enabled] false))

(defn get-daemon-tick
  "Get a daemon's current tick value."
  [game-state daemon-id]
  (get-in game-state [:daemons daemon-id :tick] 0))

(defn get-all-daemons
  "Get all daemon ids."
  [game-state]
  (keys (:daemons game-state)))

(defn get-enabled-daemons
  "Get all enabled daemon ids."
  [game-state]
  (filter #(daemon-enabled? game-state %) (get-all-daemons game-state)))

;;; ---------------------------------------------------------------------------
;;; DAEMON REGISTRATION
;;; ---------------------------------------------------------------------------

(defn register-daemon
  "Register a daemon with its handler function.

   daemon-id: keyword identifying the daemon (e.g., :i-thief)
   handler-fn: function (fn [game-state] ...) returning updated game-state
   opts: map of options:
     :tick    - initial tick value (-1 = continuous, 0 = run once, >0 = countdown)
     :enabled - whether daemon starts enabled (default true)
     :data    - daemon-specific data map"
  [game-state daemon-id handler-fn & {:keys [tick enabled data]
                                      :or {tick -1 enabled true data {}}}]
  (assoc-in game-state [:daemons daemon-id]
            {:handler handler-fn
             :enabled enabled
             :tick tick
             :data data}))

(defn unregister-daemon
  "Remove a daemon entirely."
  [game-state daemon-id]
  (update game-state :daemons dissoc daemon-id))

;;; ---------------------------------------------------------------------------
;;; DAEMON CONTROL (ZIL: QUEUE, ENABLE, DISABLE)
;;; ---------------------------------------------------------------------------

(defn queue
  "Queue a daemon to run after specified ticks.

   ZIL: <QUEUE daemon-routine ticks>

   tick = -1: Run every turn (continuous)
   tick = 0:  Run this turn, then disable
   tick > 0:  Countdown to run"
  [game-state daemon-id tick]
  (-> game-state
      (assoc-in [:daemons daemon-id :tick] tick)
      (assoc-in [:daemons daemon-id :enabled] true)))

(defn enable
  "Enable a daemon.

   ZIL: <ENABLE <INT daemon>>"
  [game-state daemon-id]
  (assoc-in game-state [:daemons daemon-id :enabled] true))

(defn disable
  "Disable a daemon.

   ZIL: <DISABLE <INT daemon>>"
  [game-state daemon-id]
  (assoc-in game-state [:daemons daemon-id :enabled] false))

(defn int-routine
  "Get or create interrupt entry for daemon.

   ZIL: <INT daemon>

   Returns [game-state daemon-entry]"
  [game-state daemon-id]
  (if (get-daemon game-state daemon-id)
    [game-state (get-daemon game-state daemon-id)]
    ;; Create empty entry if doesn't exist
    (let [gs (assoc-in game-state [:daemons daemon-id]
                       {:handler nil :enabled false :tick 0 :data {}})]
      [gs (get-daemon gs daemon-id)])))

;;; ---------------------------------------------------------------------------
;;; DAEMON DATA ACCESS
;;; ---------------------------------------------------------------------------

(defn get-daemon-data
  "Get daemon-specific data."
  [game-state daemon-id key]
  (get-in game-state [:daemons daemon-id :data key]))

(defn set-daemon-data
  "Set daemon-specific data."
  [game-state daemon-id key value]
  (assoc-in game-state [:daemons daemon-id :data key] value))

(defn update-daemon-data
  "Update daemon-specific data with a function."
  [game-state daemon-id key f & args]
  (apply update-in game-state [:daemons daemon-id :data key] f args))

;;; ---------------------------------------------------------------------------
;;; CLOCKER (Main daemon processor)
;;; ---------------------------------------------------------------------------

(defn- should-run?
  "Check if daemon should run this turn."
  [game-state daemon-id]
  (let [daemon (get-daemon game-state daemon-id)]
    (and (:enabled daemon)
         (:handler daemon)
         (let [tick (:tick daemon 0)]
           (or (= tick -1)    ; continuous
               (<= tick 0)))))) ; countdown reached 0

(defn- decrement-tick
  "Decrement a daemon's tick counter if it's a countdown."
  [game-state daemon-id]
  (let [tick (get-daemon-tick game-state daemon-id)]
    (if (> tick 0)
      (assoc-in game-state [:daemons daemon-id :tick] (dec tick))
      game-state)))

(defn- post-run-update
  "Update daemon state after running.
   If tick was 0, disable the daemon."
  [game-state daemon-id]
  (let [tick (get-daemon-tick game-state daemon-id)]
    (if (= tick 0)
      (disable game-state daemon-id)
      game-state)))

(defn- record-execution
  "Record daemon execution for debugging."
  [game-state daemon-id result-type]
  (update game-state :daemon-history conj
          {:turn (:moves game-state 0)
           :daemon daemon-id
           :event result-type
           :timestamp (System/currentTimeMillis)}))

(defn- run-daemon
  "Run a single daemon and return updated game-state."
  [game-state daemon-id]
  (let [daemon (get-daemon game-state daemon-id)
        handler (:handler daemon)]
    (if handler
      (try
        (let [;; Trace daemon execution if enabled
              gs (trace/trace-daemon game-state daemon-id "executing")
              ;; Run the handler
              gs (handler gs)
              ;; Record execution
              gs (record-execution gs daemon-id :executed)
              ;; Post-run updates
              gs (post-run-update gs daemon-id)]
          gs)
        (catch Exception e
          (-> game-state
              (record-execution daemon-id :error)
              (trace/trace-daemon daemon-id (str "ERROR: " (.getMessage e))))))
      game-state)))

;; Daemon processing order - matches ZIL C-TABLE order.
;; I-SWORD runs before I-FIGHT so sword glow message appears before combat.
(def daemon-order
  "Priority order for daemon processing. Lower numbers run first.
   Daemons not in this list run after all listed daemons."
  {:i-sword 10
   :i-fight 20
   :i-thief 30
   :i-cyclops 40
   :i-lantern 100
   :i-candles 110
   :i-match 120
   :i-rfill 200
   :i-rempty 210
   :i-maint-room 220})

(defn- daemon-priority
  "Get priority for a daemon. Lower = runs first. Default 999."
  [daemon-id]
  (get daemon-order daemon-id 999))

(defn call-room-m-beg
  "Call the current room's action with :m-beg.

   ZIL: Called at the beginning of each turn to allow rooms to set up
   state needed for the turn (e.g., south-temple sets coffin-cure flag)."
  [game-state]
  (let [here (:here game-state)
        rooms (:rooms game-state)
        room (get rooms here)
        room-action (:action room)]
    (if room-action
      (let [result (room-action game-state :m-beg)]
        ;; If room returns use-default marker, strip it and return
        (if (get result :clork.game-state/use-default)
          (dissoc result :clork.game-state/use-default)
          result))
      game-state)))

(defn- call-room-m-end
  "Call the current room's action with :m-end.

   ZIL: <SET V <APPLY <GETP <LOC ,WINNER> ,P?ACTION> ,M-END>>
   This is called at the end of each turn to allow rooms to perform
   end-of-turn processing (e.g., loud room ejecting player)."
  [game-state]
  (let [here (:here game-state)
        rooms (:rooms game-state)
        room (get rooms here)
        room-action (:action room)]
    (if room-action
      (room-action game-state :m-end)
      game-state)))

(defn clocker
  "Process room end-of-turn actions and all active daemons.

   ZIL: CLOCKER routine in gclock.zil

   Called after each command. Processes in order:
   1. Call room action with :m-end (ZIL M-END processing - before daemons)
   2. Decrement tick counters for countdown daemons
   3. Run daemons that are ready (tick <= 0 or tick = -1)
   4. Disable daemons that just ran with tick = 0"
  [game-state]
  ;; Call room's M-END action first (ZIL: M-END before CLOCKER)
  (let [gs (call-room-m-end game-state)
        daemon-ids (sort-by daemon-priority (get-all-daemons gs))]
    (reduce
     (fn [gs daemon-id]
       (if (daemon-enabled? gs daemon-id)
         (let [;; Decrement countdown ticks
               gs (decrement-tick gs daemon-id)]
           ;; Run if ready
           (if (should-run? gs daemon-id)
             (run-daemon gs daemon-id)
             gs))
         gs))
     gs
     daemon-ids)))

;;; ---------------------------------------------------------------------------
;;; DAEMON HISTORY ACCESS
;;; ---------------------------------------------------------------------------

(defn get-daemon-history
  "Get execution history for a daemon, or all if daemon-id is nil."
  [game-state & [daemon-id n]]
  (let [history (or (:daemon-history game-state) [])
        filtered (if daemon-id
                   (filter #(= daemon-id (:daemon %)) history)
                   history)]
    (take-last (or n 20) filtered)))

(defn clear-daemon-history
  "Clear daemon execution history."
  [game-state]
  (assoc game-state :daemon-history []))
