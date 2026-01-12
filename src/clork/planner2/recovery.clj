(ns clork.planner2.recovery
  "Recovery strategies for the speedrun planner.

   When things go wrong, this module provides strategies to recover:
   - Rerouting when path is blocked (e.g., thief in the way)
   - Item recovery when stolen (from thief's bag after killing)
   - Weapon recovery when dropped
   - Alternative approaches when primary path fails

   NOTE: Player death is NOT recoverable - it ends the run.
   We focus on prevention and avoidance strategies."
  (:require [clork.planner2.observe :as obs]
            [clork.planner2.thief :as thief]
            [clork.planner2.navigate :as nav]
            [clork.planner2.verify :as verify]
            [clork.planner2.combat :as combat]
            [clork.game-state :as gs]))

;;; ---------------------------------------------------------------------------
;;; RECOVERY STRATEGY TYPES
;;; ---------------------------------------------------------------------------

(defrecord RecoveryStrategy
  [type          ; :reroute, :item-recovery, :weapon-pickup, :wait, :engage
   priority      ; Lower = try first
   preconditions ; Map of required conditions
   actions       ; Vector of actions to execute
   description]) ; Human-readable description

(defn make-strategy
  "Create a recovery strategy."
  [type priority preconditions actions description]
  (->RecoveryStrategy type priority preconditions actions description))

;;; ---------------------------------------------------------------------------
;;; ROUTE BLOCKING DETECTION
;;; ---------------------------------------------------------------------------

(defn route-blocked?
  "Check if a route is blocked by an obstacle.
   Returns {:blocked? bool :blocker :thief/:troll/nil :location room-id}."
  [game-state route]
  (let [thief-loc (thief/thief-location game-state)
        thief-alive? (not (thief/thief-dead? game-state))
        troll-loc (get-in game-state [:objects :troll :in])
        troll-alive? (not (obs/troll-dead? game-state))
        ;; Check each room on route
        thief-blocking? (and thief-alive?
                             (some #{thief-loc} route))
        troll-blocking? (and troll-alive?
                             (= troll-loc :troll-room)
                             (some #{:troll-room} route))]
    (cond
      troll-blocking?
      {:blocked? true :blocker :troll :location :troll-room}

      thief-blocking?
      {:blocked? true :blocker :thief :location thief-loc}

      :else
      {:blocked? false :blocker nil :location nil})))

;;; ---------------------------------------------------------------------------
;;; REROUTING STRATEGIES
;;; ---------------------------------------------------------------------------

(defn find-alternate-route
  "Find an alternate route that avoids a blocked room.
   Returns new route or nil if no alternative exists."
  [game-state from to blocked-room]
  (let [;; Build a modified graph excluding the blocked room
        graph (nav/build-room-graph game-state)
        ;; Remove blocked room from graph
        filtered-graph (dissoc graph blocked-room)
        ;; Also remove blocked room from all adjacency sets
        cleaned-graph (into {}
                            (for [[room exits] filtered-graph]
                              [room (disj exits blocked-room)]))]
    ;; Find path in cleaned graph
    (nav/find-path cleaned-graph from to)))

(defn reroute-around-thief
  "Create a rerouting strategy to avoid the thief.
   Returns RecoveryStrategy or nil if no route exists."
  [game-state current-destination]
  (let [here (:here game-state)
        thief-loc (thief/thief-location game-state)
        alternate (find-alternate-route game-state here current-destination thief-loc)]
    (when alternate
      (make-strategy
       :reroute
       10  ; High priority (try first)
       {:thief-alive true :thief-blocking true}
       [{:type :navigate :route alternate}]
       (str "Reroute around thief at " (name thief-loc))))))

(defn reroute-around-troll
  "Create a rerouting strategy to avoid the troll.
   Returns RecoveryStrategy or nil if no route exists."
  [game-state current-destination]
  (let [here (:here game-state)
        alternate (find-alternate-route game-state here current-destination :troll-room)]
    (when alternate
      (make-strategy
       :reroute
       10
       {:troll-alive true :needs-troll-room true}
       [{:type :navigate :route alternate}]
       "Reroute around troll room"))))

;;; ---------------------------------------------------------------------------
;;; ITEM RECOVERY STRATEGIES
;;; ---------------------------------------------------------------------------

(defn recover-from-thief-bag
  "Create a strategy to recover items from dead thief's bag.
   Only valid after thief has been killed."
  [game-state stolen-items]
  (when (thief/thief-dead? game-state)
    (let [bag-contents (thief/thief-bag-contents game-state)
          recoverable (filter (set bag-contents) stolen-items)]
      (when (seq recoverable)
        (make-strategy
         :item-recovery
         30
         {:thief-dead true :items-in-bag true}
         (vec (for [item recoverable]
                {:type :take :item item}))
         (str "Recover " (count recoverable) " items from thief's bag"))))))

(defn plan-thief-kill-for-recovery
  "Create a strategy to kill thief and recover stolen items.
   Should only be used when we're strong enough."
  [game-state stolen-items]
  (let [avoidance (thief/should-avoid-thief? game-state)]
    (when-not (:avoid? avoidance)
      (make-strategy
       :engage
       50  ; Lower priority than rerouting
       {:player-strong true :items-stolen true}
       [{:type :combat :enemy :thief}
        {:type :recover-from-bag}]
       "Kill thief to recover stolen items"))))

;;; ---------------------------------------------------------------------------
;;; WEAPON RECOVERY STRATEGIES
;;; ---------------------------------------------------------------------------

(defn recover-dropped-weapon
  "Create a strategy to pick up a dropped weapon.
   Returns RecoveryStrategy or nil."
  [game-state]
  (let [here (:here game-state)
        winner (:winner game-state)
        weapon-in-room? (fn [w]
                          (and (= (get-in game-state [:objects w :in]) here)
                               (gs/set-thing-flag? game-state w :weapon)))
        ;; Check for weapons in current room
        available-weapons (filter weapon-in-room? [:sword :knife :rusty-knife :stiletto :axe])]
    (when (seq available-weapons)
      (make-strategy
       :weapon-pickup
       5  ; Very high priority
       {:weapon-in-room true :player-unarmed true}
       [{:type :take :item (first available-weapons)}]
       (str "Pick up " (name (first available-weapons)))))))

;;; ---------------------------------------------------------------------------
;;; RECOVERY STRATEGY SELECTION
;;; ---------------------------------------------------------------------------

(defn applicable-strategies
  "Get all applicable recovery strategies for current situation.
   Returns sorted vector of strategies (by priority)."
  [game-state situation]
  (let [strategies
        (case (:type situation)
          :route-blocked
          (let [{:keys [blocker destination]} situation]
            (cond-> []
              (= blocker :thief)
              (conj (reroute-around-thief game-state destination))

              (= blocker :troll)
              (conj (reroute-around-troll game-state destination))))

          :items-stolen
          (let [{:keys [stolen-items]} situation]
            (cond-> []
              (thief/thief-dead? game-state)
              (conj (recover-from-thief-bag game-state stolen-items))

              (not (thief/thief-dead? game-state))
              (conj (plan-thief-kill-for-recovery game-state stolen-items))))

          :weapon-lost
          [(recover-dropped-weapon game-state)]

          ;; Default: no strategies
          [])]
    (->> strategies
         (remove nil?)
         (sort-by :priority)
         vec)))

(defn best-strategy
  "Get the best applicable recovery strategy.
   Returns RecoveryStrategy or nil."
  [game-state situation]
  (first (applicable-strategies game-state situation)))

;;; ---------------------------------------------------------------------------
;;; RECOVERY EXECUTION
;;; ---------------------------------------------------------------------------

(defrecord RecoveryResult
  [success?     ; Boolean - recovery succeeded
   strategy     ; The strategy that was attempted
   game-state   ; State after recovery attempt
   events])     ; Log of what happened

(defn make-recovery-result
  "Create a recovery result."
  [success? strategy game-state events]
  (->RecoveryResult success? strategy game-state (vec events)))

(defn execute-recovery
  "Execute a recovery strategy.

   Parameters:
   - game-state: Current state
   - strategy: RecoveryStrategy to execute
   - execute-fn: Function (gs command) -> gs

   Returns RecoveryResult."
  [game-state strategy execute-fn]
  (if (nil? strategy)
    (make-recovery-result false nil game-state ["No recovery strategy available"])

    (let [events [(str "Attempting recovery: " (:description strategy))]]
      ;; Execute each action in the strategy
      (loop [gs game-state
             actions (:actions strategy)
             ev events]
        (if (empty? actions)
          (make-recovery-result true strategy gs
                                (conj ev "Recovery completed"))
          (let [action (first actions)
                cmd (case (:type action)
                      :navigate nil  ; Special case - would need full navigation
                      :take (str "take " (name (:item action)))
                      :combat (str "attack " (name (:enemy action)))
                      nil)
                new-gs (if cmd
                         (execute-fn gs cmd)
                         gs)
                new-ev (conj ev (str "Executed: " (or cmd (pr-str action))))]
            ;; Check if action succeeded (simplified)
            (recur new-gs (rest actions) new-ev)))))))

;;; ---------------------------------------------------------------------------
;;; SITUATION DETECTION
;;; ---------------------------------------------------------------------------

(defn detect-situation
  "Analyze game state and identify any situation requiring recovery.
   Returns map with :type and situation-specific keys, or nil if no recovery needed."
  [pre-state post-state planned-destination]
  (let [winner (:winner post-state)
        ;; Check for weapon loss
        pre-weapon (some (fn [w]
                           (when (= (get-in pre-state [:objects w :in]) winner) w))
                         [:sword :knife :stiletto :axe])
        post-weapon (some (fn [w]
                            (when (= (get-in post-state [:objects w :in]) winner) w))
                          [:sword :knife :stiletto :axe])
        weapon-lost? (and pre-weapon (not post-weapon))

        ;; Check for theft
        theft (thief/detect-theft pre-state post-state)

        ;; Check for route blockage
        route-to-dest (when planned-destination
                        (let [here (:here post-state)]
                          (when (not= here planned-destination)
                            [here planned-destination])))  ; Simplified route
        blockage (when route-to-dest
                   (route-blocked? post-state route-to-dest))]

    (cond
      weapon-lost?
      {:type :weapon-lost
       :lost-weapon pre-weapon
       :weapon-location (get-in post-state [:objects pre-weapon :in])}

      theft
      {:type :items-stolen
       :stolen-items (:stolen theft)
       :thief-location (:thief-location theft)}

      (and blockage (:blocked? blockage))
      {:type :route-blocked
       :blocker (:blocker blockage)
       :location (:location blockage)
       :destination planned-destination}

      :else nil)))

;;; ---------------------------------------------------------------------------
;;; FORMAT OUTPUT
;;; ---------------------------------------------------------------------------

(defn format-strategy
  "Format a recovery strategy for display."
  [strategy]
  (str "Recovery Strategy: " (:description strategy)
       "\n  Type: " (name (:type strategy))
       "\n  Priority: " (:priority strategy)
       "\n  Actions: " (count (:actions strategy))))

(defn format-result
  "Format a recovery result for display."
  [result]
  (str "=== Recovery Result ===\n"
       "Success: " (:success? result) "\n"
       "Strategy: " (when (:strategy result)
                      (:description (:strategy result))) "\n"
       "Events:\n"
       (clojure.string/join "\n" (map #(str "  " %) (:events result)))))

(defn print-result
  "Print a recovery result."
  [result]
  (println (format-result result)))
