(ns clork.planner2.goals
  "Goal system for the reactive planner.

   Goals are maps with a :type key and type-specific data.
   Each goal type has:
   - Satisfaction predicate: Is the goal achieved?
   - Decomposition: What sub-goals are needed?
   - Action selection: What action makes progress?"
  (:require [clork.planner2.observe :as obs]))

;;; ---------------------------------------------------------------------------
;;; GOAL DEFINITIONS
;;; ---------------------------------------------------------------------------
;;;
;;; Goal format: {:type <keyword> ...type-specific-keys...}
;;;
;;; Goal types:
;;; - :at-room {:room <room-id>}
;;; - :have-item {:item <item-id>}
;;; - :item-visible {:item <item-id>}
;;; - :flag-set {:flag <flag-keyword>}
;;; - :item-deposited {:item <treasure-id>}
;;; - :kill-enemy {:enemy <enemy-id>}
;;; - :container-open {:container <container-id>}
;;; - :lantern-on {}
;;; - :all-treasures-deposited {}
;;; - :win {}

;;; ---------------------------------------------------------------------------
;;; GOAL CONSTRUCTORS (for readability)
;;; ---------------------------------------------------------------------------

(defn at-room
  "Create a goal to be at a specific room."
  [room-id]
  {:type :at-room :room room-id})

(defn have-item
  "Create a goal to have an item in inventory."
  [item-id]
  {:type :have-item :item item-id})

(defn item-visible
  "Create a goal to have an item visible (in room or inventory)."
  [item-id]
  {:type :item-visible :item item-id})

(defn flag-set
  "Create a goal to have a flag set."
  [flag]
  {:type :flag-set :flag flag})

(defn item-deposited
  "Create a goal to have a treasure deposited in trophy case."
  [treasure-id]
  {:type :item-deposited :item treasure-id})

(defn kill-enemy
  "Create a goal to kill an enemy."
  [enemy-id]
  {:type :kill-enemy :enemy enemy-id})

(defn container-open
  "Create a goal to open a container."
  [container-id]
  {:type :container-open :container container-id})

(defn lantern-on
  "Create a goal to have the lantern on."
  []
  {:type :lantern-on})

(defn all-treasures-deposited
  "Create a goal to deposit all treasures."
  []
  {:type :all-treasures-deposited})

(defn win
  "Create a goal to win the game."
  []
  {:type :win})

;;; ---------------------------------------------------------------------------
;;; TREASURE LIST
;;; ---------------------------------------------------------------------------

(def treasures
  "All treasures that must be deposited to win."
  #{:egg :painting :bag-of-coins :jeweled-scarab :huge-diamond
    :large-emerald :sapphire-bracelet :jade-figurine :ivory-torch
    :crystal-trident :pot-of-gold :gold-coffin :sceptre
    :crystal-skull :jewel-encrusted-trunk :brass-bauble
    :silver-chalice :clockwork-canary :platinum-bar})

;;; ---------------------------------------------------------------------------
;;; GOAL SATISFACTION
;;; ---------------------------------------------------------------------------

(defmulti goal-satisfied?
  "Check if a goal is satisfied in the current game state.
   Returns true if goal is achieved, false otherwise."
  (fn [_game-state goal] (:type goal)))

(defmethod goal-satisfied? :at-room
  [game-state {:keys [room]}]
  (obs/at-room? game-state room))

(defmethod goal-satisfied? :have-item
  [game-state {:keys [item]}]
  (obs/has-item? game-state item))

(defmethod goal-satisfied? :item-visible
  [game-state {:keys [item]}]
  (or (obs/has-item? game-state item)
      (obs/object-visible? game-state item)))

(defmethod goal-satisfied? :flag-set
  [game-state {:keys [flag]}]
  (obs/flag-set? game-state flag))

(defmethod goal-satisfied? :item-deposited
  [game-state {:keys [item]}]
  (obs/item-deposited? game-state item))

(defmethod goal-satisfied? :kill-enemy
  [game-state {:keys [enemy]}]
  (case enemy
    :troll (obs/troll-dead? game-state)
    :cyclops (obs/cyclops-gone? game-state)
    :thief (obs/thief-dead? game-state)
    false))

(defmethod goal-satisfied? :container-open
  [game-state {:keys [container]}]
  (obs/object-open? game-state container))

(defmethod goal-satisfied? :lantern-on
  [game-state _goal]
  (obs/lantern-on? game-state))

(defmethod goal-satisfied? :all-treasures-deposited
  [game-state _goal]
  (every? #(obs/item-deposited? game-state %) treasures))

(defmethod goal-satisfied? :win
  [game-state _goal]
  (obs/game-finished? game-state))

(defmethod goal-satisfied? :default
  [_game-state goal]
  (throw (ex-info (str "Unknown goal type: " (:type goal)) {:goal goal})))

;;; ---------------------------------------------------------------------------
;;; GOAL DECOMPOSITION
;;; ---------------------------------------------------------------------------
;;;
;;; When a goal cannot be directly achieved with a single action,
;;; decompose it into sub-goals that need to be achieved first.

(defmulti decompose-goal
  "Decompose a goal into sub-goals.
   Returns a vector of sub-goals (to be pushed onto goal stack in reverse order),
   or nil if goal cannot be decomposed (need to find direct action)."
  (fn [_game-state goal] (:type goal)))

;; at-room: Find path and decompose into movement sub-goals
;; This is handled specially by the navigation system, not here
(defmethod decompose-goal :at-room
  [_game-state _goal]
  nil)  ; Navigation handles this

;; have-item: Need to go to item location and take it
(defmethod decompose-goal :have-item
  [game-state {:keys [item]}]
  (let [item-room (obs/find-object-room game-state item)]
    (cond
      ;; Already have it
      (obs/has-item? game-state item)
      nil

      ;; Item is visible - just need to take it
      (obs/object-visible? game-state item)
      nil

      ;; Item is in a room - go there first
      item-room
      [(at-room item-room)]

      ;; Item is in limbo or inaccessible - special handling needed
      :else
      nil)))

;; item-deposited: Need to have item, be at living room, open case, put in
(defmethod decompose-goal :item-deposited
  [game-state {:keys [item]}]
  (cond
    ;; Already deposited
    (obs/item-deposited? game-state item)
    nil

    ;; Don't have the item - get it first
    (not (obs/has-item? game-state item))
    [(have-item item)]

    ;; Not at living room - go there
    (not (obs/at-room? game-state :living-room))
    [(at-room :living-room)]

    ;; Case not open - open it
    (not (obs/object-open? game-state :trophy-case))
    [(container-open :trophy-case)]

    ;; Ready to deposit (action selection handles the actual PUT)
    :else
    nil))

;; kill-enemy: Need to be at enemy location with weapon
(defmethod decompose-goal :kill-enemy
  [game-state {:keys [enemy]}]
  (let [weapon (case enemy
                 :troll :sword
                 :thief :nasty-knife  ; or :sword
                 :sword)
        enemy-room (case enemy
                     :troll :troll-room
                     :cyclops :cyclops-room
                     :thief nil  ; Thief moves around
                     nil)]
    (cond
      ;; Enemy already dead
      (goal-satisfied? game-state {:type :kill-enemy :enemy enemy})
      nil

      ;; Don't have a weapon
      (and (not (obs/has-item? game-state :sword))
           (not (obs/has-item? game-state :nasty-knife)))
      [(have-item :sword)]

      ;; Need light for underground areas
      (and (not (obs/lantern-on? game-state))
           (not (obs/lit? game-state)))
      [(lantern-on)]

      ;; Not at enemy location (for stationary enemies)
      (and enemy-room (not (obs/at-room? game-state enemy-room)))
      [(at-room enemy-room)]

      ;; Ready to fight
      :else
      nil)))

;; container-open: Need to be at container location
(defmethod decompose-goal :container-open
  [game-state {:keys [container]}]
  (cond
    ;; Already open
    (obs/object-open? game-state container)
    nil

    ;; Container is visible - just open it
    (obs/object-visible? game-state container)
    nil

    ;; Container is in inventory - just open it
    (obs/has-item? game-state container)
    nil

    ;; Need to find and go to container
    :else
    (let [container-room (obs/find-object-room game-state container)]
      (when container-room
        [(at-room container-room)]))))

;; lantern-on: Need lantern first
(defmethod decompose-goal :lantern-on
  [game-state _goal]
  (cond
    ;; Already on
    (obs/lantern-on? game-state)
    nil

    ;; Don't have lantern
    (not (obs/has-item? game-state :brass-lantern))
    [(have-item :brass-lantern)]

    ;; Have lantern, just need to turn it on
    :else
    nil))

;; all-treasures-deposited: Deposit each treasure
(defmethod decompose-goal :all-treasures-deposited
  [game-state _goal]
  (let [undeposited (remove #(obs/item-deposited? game-state %) treasures)]
    (when (seq undeposited)
      ;; Return first undeposited treasure as sub-goal
      [(item-deposited (first undeposited))])))

;; win: Need all treasures deposited, then enter barrow
(defmethod decompose-goal :win
  [game-state _goal]
  (cond
    ;; Already won
    (obs/game-finished? game-state)
    nil

    ;; Not all treasures deposited
    (not (every? #(obs/item-deposited? game-state %) treasures))
    [(all-treasures-deposited)]

    ;; Not at barrow entrance
    (not (obs/at-room? game-state :west-of-barrow))
    [(at-room :west-of-barrow)]

    ;; Ready to enter barrow
    :else
    nil))

(defmethod decompose-goal :default
  [_game-state goal]
  (throw (ex-info (str "Unknown goal type for decomposition: " (:type goal)) {:goal goal})))

;;; ---------------------------------------------------------------------------
;;; GOAL PRIORITY
;;; ---------------------------------------------------------------------------

(defn goal-priority
  "Get the priority of a goal (lower = more urgent).
   Used for ordering goals when multiple are pending."
  [goal]
  (case (:type goal)
    :lantern-on 0      ; Safety first
    :at-room 1         ; Navigation
    :container-open 2  ; Access
    :have-item 3       ; Acquisition
    :kill-enemy 4      ; Combat
    :item-deposited 5  ; Deposit
    :flag-set 6        ; Puzzles
    :all-treasures-deposited 7
    :win 8
    99))

;;; ---------------------------------------------------------------------------
;;; GOAL DESCRIPTION (for debugging)
;;; ---------------------------------------------------------------------------

(defn goal->string
  "Convert a goal to a human-readable string."
  [goal]
  (case (:type goal)
    :at-room (str "Go to " (name (:room goal)))
    :have-item (str "Get " (name (:item goal)))
    :item-visible (str "Find " (name (:item goal)))
    :flag-set (str "Achieve " (name (:flag goal)))
    :item-deposited (str "Deposit " (name (:item goal)))
    :kill-enemy (str "Kill " (name (:enemy goal)))
    :container-open (str "Open " (name (:container goal)))
    :lantern-on "Turn on lantern"
    :all-treasures-deposited "Deposit all treasures"
    :win "Win the game"
    (str "Unknown goal: " goal)))

(defn print-goal-stack
  "Print the current goal stack."
  [goal-stack]
  (println "\n=== Goal Stack ===")
  (if (empty? goal-stack)
    (println "  (empty)")
    (doseq [[idx goal] (map-indexed vector goal-stack)]
      (println (str "  " idx ". " (goal->string goal))))))
