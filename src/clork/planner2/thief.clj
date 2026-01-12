(ns clork.planner2.thief
  "Thief monitoring and avoidance for the speedrun planner.

   The thief is a complex NPC that:
   - Wanders randomly between non-sacred rooms
   - Steals treasures from the player or rooms
   - Can engage in combat with the player
   - Has a lair (treasure-room) where stolen items go

   For speedrun planning, we need to:
   - Track thief location relative to planned route
   - Detect theft events (items disappearing)
   - Recommend rerouting to avoid early-game encounters
   - Plan for thief combat when necessary (late game)"
  (:require [clork.planner2.observe :as obs]
            [clork.game-state :as gs]
            [clojure.set :as set]))

;;; ---------------------------------------------------------------------------
;;; THIEF STATE OBSERVATION
;;; ---------------------------------------------------------------------------

(defn thief-location
  "Get the thief's current location."
  [game-state]
  (get-in game-state [:objects :thief :in]))

(defn thief-visible?
  "Check if thief is currently visible."
  [game-state]
  (not (gs/set-thing-flag? game-state :thief :invisible)))

(defn thief-in-room?
  "Check if thief is in a specific room."
  [game-state room-id]
  (= (thief-location game-state) room-id))

(defn thief-here?
  "Check if thief is in the current room."
  [game-state]
  (thief-in-room? game-state (:here game-state)))

(defn thief-dead?
  "Check if thief has been killed."
  [game-state]
  (obs/thief-dead? game-state))

(defn thief-strength
  "Get the thief's current combat strength."
  [game-state]
  (get-in game-state [:objects :thief :strength] 5))

;;; ---------------------------------------------------------------------------
;;; THIEF BAG CONTENTS
;;; ---------------------------------------------------------------------------

(defn thief-bag-contents
  "Get items in thief's bag (excluding stiletto and bag itself)."
  [game-state]
  (let [contents (gs/get-contents game-state :thief)]
    (remove #{:stiletto :large-bag} contents)))

(defn thief-has-item?
  "Check if thief has stolen a specific item."
  [game-state item-id]
  (some #{item-id} (thief-bag-contents game-state)))

(defn thief-bag-value
  "Calculate total value of items in thief's bag."
  [game-state]
  (reduce
   (fn [sum item-id]
     (+ sum (get-in game-state [:objects item-id :tvalue] 0)))
   0
   (thief-bag-contents game-state)))

;;; ---------------------------------------------------------------------------
;;; THEFT DETECTION
;;; ---------------------------------------------------------------------------

(defn detect-theft
  "Compare two game states and detect if items were stolen.
   Returns {:stolen [...] :source :player/:room} or nil if no theft."
  [pre-state post-state]
  (let [winner (:winner post-state)
        ;; Items player had before (get-contents returns IDs directly)
        pre-inventory (set (gs/get-contents pre-state winner))
        ;; Items player has after
        post-inventory (set (gs/get-contents post-state winner))
        ;; Items missing from inventory
        missing-from-player (set/difference pre-inventory post-inventory)
        ;; Check if they went to thief
        to-thief (filter #(= :thief (get-in post-state [:objects % :in]))
                         missing-from-player)]
    (when (seq to-thief)
      {:stolen (vec to-thief)
       :source :player
       :thief-location (thief-location post-state)})))

(defn detect-room-theft
  "Check if items were stolen from a specific room between states."
  [pre-state post-state room-id]
  (let [pre-room-contents (set (gs/get-contents pre-state room-id))
        post-room-contents (set (gs/get-contents post-state room-id))
        missing (set/difference pre-room-contents post-room-contents)
        to-thief (filter #(= :thief (get-in post-state [:objects % :in])) missing)]
    (when (seq to-thief)
      {:stolen (vec to-thief)
       :source room-id
       :thief-location (thief-location post-state)})))

;;; ---------------------------------------------------------------------------
;;; ROUTE INTERFERENCE DETECTION
;;; ---------------------------------------------------------------------------

(defn rooms-thief-avoids
  "Rooms the thief generally doesn't go to (sacred rooms)."
  [game-state]
  (->> (:rooms game-state)
       (filter (fn [[room-id room]]
                 (gs/set-thing-flag? game-state room-id :sacred)))
       (map first)
       set))

(defn thief-on-route?
  "Check if thief is on a planned route.
   route is a sequence of room-ids."
  [game-state route]
  (let [thief-loc (thief-location game-state)]
    (some #{thief-loc} route)))

(defn route-thief-risk
  "Estimate risk of encountering thief on a route.
   Returns {:risk :low/:medium/:high, :thief-rooms [...], :recommendations [...]}."
  [game-state route]
  (let [thief-loc (thief-location game-state)
        thief-alive? (not (thief-dead? game-state))
        thief-on-path? (and thief-alive? (some #{thief-loc} route))
        sacred-rooms (rooms-thief-avoids game-state)
        non-sacred-on-route (remove sacred-rooms route)
        ;; Score is on items we carry that thief wants
        has-treasures? (some (fn [item-id]
                               (pos? (get-in game-state [:objects item-id :tvalue] 0)))
                             (obs/inventory game-state))]
    {:risk (cond
             (not thief-alive?) :none
             (and thief-on-path? has-treasures?) :high
             thief-on-path? :medium
             (and (seq non-sacred-on-route) has-treasures?) :medium
             :else :low)
     :thief-location thief-loc
     :thief-on-route? thief-on-path?
     :vulnerable-rooms (vec non-sacred-on-route)
     :recommendations (cond-> []
                        thief-on-path?
                        (conj "Thief is directly on route - consider alternative path")

                        (and thief-alive? has-treasures?)
                        (conj "Carrying treasures - deposit soon to reduce theft risk")

                        (and thief-alive? (not (thief-dead? game-state)))
                        (conj "Kill thief before final treasure run to secure items"))}))

;;; ---------------------------------------------------------------------------
;;; AVOIDANCE STRATEGIES
;;; ---------------------------------------------------------------------------

(defn should-avoid-thief?
  "Determine if we should avoid the thief based on game state.
   Early game: avoid (low strength, can't kill reliably)
   Late game: engage (high strength, need stolen items back)"
  [game-state]
  (let [score (obs/score game-state)
        ;; Player strength increases with score
        ;; At 0 pts: strength 2, at 350 pts: strength 7
        estimated-strength (+ 2 (min 5 (int (/ score 70))))
        thief-bag-val (thief-bag-value game-state)]
    (cond
      ;; Thief already dead
      (thief-dead? game-state)
      {:avoid? false :reason "Thief is dead"}

      ;; Thief has high-value items - might need to kill
      (>= thief-bag-val 50)
      {:avoid? false :reason (str "Thief has " thief-bag-val " points of treasure")}

      ;; High strength - can kill safely
      (>= estimated-strength 5)
      {:avoid? false :reason (str "Player strong enough (est. " estimated-strength ")")}

      ;; Low strength - avoid
      :else
      {:avoid? true
       :reason (str "Too weak (est. " estimated-strength "), avoid thief")})))

(defn suggest-thief-avoidance
  "Given a planned route and current state, suggest how to handle thief.
   Returns {:strategy :avoid/:engage/:ignore, :alternate-route [...] | nil, ...}."
  [game-state planned-route]
  (let [risk (route-thief-risk game-state planned-route)
        avoid-info (should-avoid-thief? game-state)]
    (cond
      ;; Thief dead - ignore
      (thief-dead? game-state)
      {:strategy :ignore
       :reason "Thief is dead"
       :alternate-route nil}

      ;; Not on route - proceed normally
      (not (:thief-on-route? risk))
      {:strategy :ignore
       :reason "Thief not on planned route"
       :alternate-route nil}

      ;; Should avoid - try alternate route
      (:avoid? avoid-info)
      {:strategy :avoid
       :reason (:reason avoid-info)
       :thief-location (:thief-location risk)
       :alternate-route nil}  ; TODO: implement pathfinding around thief

      ;; Strong enough - engage
      :else
      {:strategy :engage
       :reason (:reason avoid-info)
       :thief-location (:thief-location risk)})))

;;; ---------------------------------------------------------------------------
;;; THIEF MONITORING STATE
;;; ---------------------------------------------------------------------------

(defrecord ThiefMonitor
  [last-location       ; Last known thief location
   location-history    ; Vector of recent locations
   thefts-detected     ; Count of detected thefts
   items-stolen        ; Set of items known stolen
   encounters])        ; Vector of {:room :turn :outcome}

(defn make-thief-monitor
  "Create a new thief monitor."
  []
  (->ThiefMonitor nil [] 0 #{} []))

(defn update-thief-monitor
  "Update thief monitor with new game state.
   Call this after each action to track thief."
  [monitor game-state]
  (let [current-loc (thief-location game-state)
        history-limit 20]
    (-> monitor
        (assoc :last-location current-loc)
        (update :location-history
                (fn [h]
                  (let [new-h (conj h current-loc)]
                    (if (> (count new-h) history-limit)
                      (subvec new-h (- (count new-h) history-limit))
                      new-h)))))))

(defn record-theft
  "Record a theft event in the monitor."
  [monitor stolen-items]
  (-> monitor
      (update :thefts-detected inc)
      (update :items-stolen into stolen-items)))

(defn record-encounter
  "Record a thief encounter."
  [monitor room turn outcome]
  (update monitor :encounters conj
          {:room room :turn turn :outcome outcome}))

;;; ---------------------------------------------------------------------------
;;; FORMAT OUTPUT
;;; ---------------------------------------------------------------------------

(defn format-thief-status
  "Format thief status for display."
  [game-state]
  (let [loc (thief-location game-state)
        dead? (thief-dead? game-state)
        visible? (thief-visible? game-state)
        bag (thief-bag-contents game-state)
        bag-val (thief-bag-value game-state)]
    (str "=== Thief Status ===\n"
         "Location: " (if loc (name loc) "unknown") "\n"
         "Status: " (cond dead? "DEAD" visible? "visible" :else "hidden") "\n"
         "Strength: " (when-not dead? (thief-strength game-state)) "\n"
         "Stolen items: " (if (seq bag)
                           (str (count bag) " items (value: " bag-val ")")
                           "none") "\n"
         (when (seq bag)
           (str "  " (clojure.string/join ", " (map name bag)) "\n")))))

(defn print-thief-status
  "Print thief status to stdout."
  [game-state]
  (println (format-thief-status game-state)))
