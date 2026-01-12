(ns clork.ml
  "ML Training API - Provides structured access to game state for machine learning agents.

   Instead of parsing text commands, agents can:
   1. Query valid actions at each state
   2. Execute actions as structured data
   3. Receive structured state + metrics back

   This eliminates the need for agents to generate valid English text."
  (:require [clork.game-state :as gs]
            [clork.verb-defs :as verb-defs]
            [clork.utils :as utils]
            [clork.daemon :as daemon]
            [clojure.data.json :as json])
  (:import [java.io BufferedReader]))

;;; ---------------------------------------------------------------------------
;;; VISIBILITY HELPERS
;;; ---------------------------------------------------------------------------

(defn- container-contents-visible?
  "Returns true if the contents of a container are visible.
   Contents are visible if container is :open, :trans (transparent), or :surface."
  [game-state container-id]
  (or (gs/set-thing-flag? game-state container-id :open)
      (gs/set-thing-flag? game-state container-id :trans)
      (gs/set-thing-flag? game-state container-id :surface)))

(defn- get-visible-objects-in
  "Recursively get all visible objects inside a container/room.
   Returns a flat list of {:id :container :depth} maps."
  ([game-state container-id]
   (get-visible-objects-in game-state container-id 0 nil))
  ([game-state container-id depth parent-id]
   (let [direct-contents (gs/get-contents game-state container-id)]
     (reduce
      (fn [acc obj-id]
        (if (gs/set-thing-flag? game-state obj-id :invisible)
          acc  ; Skip invisible objects
          (let [entry {:id obj-id
                       :container parent-id
                       :depth depth}
                ;; Check if this object is a container with visible contents
                is-container? (gs/set-thing-flag? game-state obj-id :cont)
                contents-visible? (and is-container?
                                       (container-contents-visible? game-state obj-id))
                nested (if contents-visible?
                         (get-visible-objects-in game-state obj-id (inc depth) obj-id)
                         [])]
            (into (conj acc entry) nested))))
      []
      direct-contents))))

(defn- get-global-objects-in-room
  "Get global objects (local-globals) visible from the current room.
   Returns a vector of maps with :id, :container nil, :depth 0."
  [game-state]
  (let [here (:here game-state)
        room (gs/get-thing game-state here)
        globals (or (:globals room) #{})]
    ;; Find all objects with :in :local-globals that are in this room's globals
    (->> (:objects game-state)
         vals
         (filter #(and (= (:in %) :local-globals)
                       (contains? globals (:id %))))
         (map (fn [obj] {:id (:id obj) :container nil :depth 0}))
         vec)))

(defn get-visible-objects
  "Get all objects visible in the current room, including nested containers
   and global objects (local-globals) visible from this room.
   Returns a vector of maps with :id, :container (parent), and :depth."
  [game-state]
  (let [here (:here game-state)
        room-objects (get-visible-objects-in game-state here 0 nil)
        global-objects (get-global-objects-in-room game-state)]
    (into room-objects global-objects)))

(defn get-inventory
  "Get all objects in player's inventory, including nested containers.
   Returns a vector of maps with :id, :container (parent), and :depth."
  [game-state]
  (let [winner (:winner game-state)]
    (get-visible-objects-in game-state winner 0 nil)))

;;; ---------------------------------------------------------------------------
;;; EXIT HELPERS
;;; ---------------------------------------------------------------------------

(defn- exit-available?
  "Check if an exit is currently traversable.
   Returns the destination room-id if available, nil otherwise."
  [game-state exit-data]
  (cond
    ;; Simple exit: keyword destination
    (keyword? exit-data)
    exit-data

    ;; Blocked exit: string message
    (string? exit-data)
    nil

    ;; Conditional exit: {:to room :if flag} or {:to room :door door-id}
    (map? exit-data)
    (let [{:keys [to if door]} exit-data]
      (cond
        ;; Flag condition: {:to :stone-barrow :if :won}
        (and if (not (get game-state if)))
        nil

        ;; Door condition: {:to :behind-house :door :kitchen-window}
        (and door (not (gs/set-thing-flag? game-state door :open)))
        nil

        ;; Condition met
        :else to))

    :else nil))

(defn get-available-exits
  "Get all available exits from the current room.
   Returns a map of direction -> destination room-id."
  [game-state]
  (let [here (gs/get-here game-state)
        exits (:exits here {})]
    (reduce-kv
     (fn [acc dir exit-data]
       (if-let [dest (exit-available? game-state exit-data)]
         (assoc acc dir dest)
         acc))
     {}
     exits)))

;;; ---------------------------------------------------------------------------
;;; OBJECT FLAG QUERIES
;;; ---------------------------------------------------------------------------

(defn object-flags
  "Get the relevant flags for an object as a set of keywords.
   Only includes flags that affect valid actions."
  [game-state obj-id]
  (let [check-flag (fn [flag] (when (gs/set-thing-flag? game-state obj-id flag) flag))]
    (into #{}
          (filter some?
                  [(check-flag :take)
                   (check-flag :trytake)
                   (check-flag :read)
                   (check-flag :light)
                   (check-flag :on)
                   (check-flag :cont)
                   (check-flag :open)
                   (check-flag :door)
                   (check-flag :locked)
                   (check-flag :surface)
                   (check-flag :trans)
                   (check-flag :vehicle)
                   (check-flag :climb)
                   (check-flag :drink)
                   (check-flag :food)
                   (check-flag :burn)
                   (check-flag :weapon)
                   (check-flag :tool)]))))

;;; ---------------------------------------------------------------------------
;;; VALID ACTIONS
;;; ---------------------------------------------------------------------------

(defn- object-info
  "Build info map for an object."
  [game-state obj-id]
  (let [obj (gs/get-thing game-state obj-id)
        flags (object-flags game-state obj-id)]
    {:id obj-id
     :name (gs/thing-name game-state obj-id)
     :flags flags
     :readable? (and (contains? flags :read)
                     (some? (:text obj)))
     :has-text? (some? (:text obj))}))

(defn- valid-verbs-for-object
  "Determine which verbs can be applied to an object based on its flags."
  [game-state obj-id in-inventory?]
  (let [flags (object-flags game-state obj-id)
        obj (gs/get-thing game-state obj-id)]
    (cond-> []
      ;; Can always examine visible objects
      true
      (conj :examine)

      ;; Taking: need :take flag, and not already in inventory
      (and (or (contains? flags :take)
               (contains? flags :trytake))
           (not in-inventory?))
      (conj :take)

      ;; Dropping: must be in inventory
      in-inventory?
      (conj :drop)

      ;; Reading: needs :read flag and :text content
      (and (contains? flags :read)
           (:text obj))
      (conj :read)

      ;; Opening: containers or doors that aren't already open
      (and (or (contains? flags :cont)
               (contains? flags :door))
           (not (contains? flags :open)))
      (conj :open)

      ;; Closing: containers or doors that are open
      (and (or (contains? flags :cont)
               (contains? flags :door))
           (contains? flags :open))
      (conj :close)

      ;; Light sources
      (and (contains? flags :light)
           (not (contains? flags :on)))
      (conj :lamp-on)

      (and (contains? flags :light)
           (contains? flags :on))
      (conj :lamp-off)

      ;; Looking inside containers
      (contains? flags :cont)
      (conj :look-inside)

      ;; Moving objects in the room (not inventory)
      (not in-inventory?)
      (conj :move)

      ;; Climbing climbable objects
      (contains? flags :climb)
      (conj :climb)

      ;; Entering/boarding vehicles
      (contains? flags :vehicle)
      (conj :enter))))

(defn- valid-two-object-verbs
  "Determine valid two-object actions (PUT X IN Y, etc).
   Returns list of {:verb :obj1 :prep :obj2} maps."
  [game-state inventory-ids room-obj-ids]
  (let [all-containers (filter
                        (fn [obj-id]
                          (or (gs/set-thing-flag? game-state obj-id :cont)
                              (gs/set-thing-flag? game-state obj-id :surface)))
                        (concat inventory-ids room-obj-ids))]
    ;; For each inventory item, can put in/on each container
    (for [held-obj inventory-ids
          container all-containers
          :when (not= held-obj container)
          :let [is-surface? (gs/set-thing-flag? game-state container :surface)
                prep (if is-surface? :on :in)]]
      {:verb :put
       :direct-object held-obj
       :prep prep
       :indirect-object container})))

(defn valid-actions
  "Compute all valid actions from the current game state.

   Returns a map with:
   - :meta-verbs     - Always-available verbs (look, inventory, score, etc.)
   - :movement       - Available directions to move
   - :object-actions - Map of object-id -> [applicable verbs]
   - :two-object     - List of valid two-object commands
   - :special-mode   - If non-nil, indicates a special input mode is active

   Special modes (like :loud-room-mode) restrict available actions."
  [game-state]
  ;; Check for special input modes that restrict available actions
  (if (:loud-room-mode game-state)
    ;; Loud room special mode - only limited actions available
    ;; ZIL: LOUD-ROOM-FCN lines 1706-1741
    {:meta-verbs [:look :quit :save :restore]
     :movement {:exits {:west :round-room
                        :east :damp-cave
                        :up :deep-canyon}
                :directions [:west :east :up]}
     :room-objects []
     :inventory []
     :object-actions {}
     :two-object-actions []
     :lit? true
     :special-mode :loud-room
     :special-actions [:echo]  ; The puzzle solution
     :special-mode-hint "The room is too loud. Say 'echo' or leave via west/east/up."}

    ;; Normal mode - compute all valid actions
    (let [;; Check if room is lit (affects what we can do)
          lit? (or (:lit game-state)
                   (gs/set-here-flag? game-state :lit))

          ;; Get visible objects and inventory
          room-objects (get-visible-objects game-state)
          inventory (get-inventory game-state)

          room-obj-ids (map :id room-objects)
          inventory-ids (map :id inventory)

          ;; Available exits
          exits (get-available-exits game-state)

          ;; Meta verbs (always available)
          meta-verbs [:look :inventory :score :diagnose :verbose :brief
                      :super-brief :quit :save :restore :restart]

          ;; Object-specific actions
          object-actions
          (if lit?
            (merge
             ;; Room objects
             (into {}
                   (for [{:keys [id]} room-objects]
                     [id {:info (object-info game-state id)
                          :verbs (valid-verbs-for-object game-state id false)}]))
             ;; Inventory objects
             (into {}
                   (for [{:keys [id]} inventory]
                     [id {:info (object-info game-state id)
                          :verbs (valid-verbs-for-object game-state id true)}])))
            ;; In darkness, can only interact with light sources
            (let [light-sources (filter
                                 #(gs/set-thing-flag? game-state (:id %) :light)
                                 (concat room-objects inventory))]
              (into {}
                    (for [{:keys [id]} light-sources]
                      (let [in-inv? (some #(= id (:id %)) inventory)]
                        [id {:info (object-info game-state id)
                             :verbs (valid-verbs-for-object game-state id in-inv?)}])))))

          ;; Two-object actions (PUT X IN Y, etc.)
          two-object-actions
          (when lit?
            (valid-two-object-verbs game-state inventory-ids room-obj-ids))]

      {:meta-verbs meta-verbs
       :movement {:exits exits
                  :directions (vec (keys exits))}
       :room-objects room-objects
       :inventory inventory
       :object-actions object-actions
       :two-object-actions (vec two-object-actions)
       :lit? lit?
       :special-mode nil})))

;;; ---------------------------------------------------------------------------
;;; STATE SNAPSHOT
;;; ---------------------------------------------------------------------------

(defn- game-over?
  "Check if the game is in a terminal state.

   Game over conditions:
   - Player has quit (:quit)
   - Player has finished (entered barrow after winning)
   - Player has died permanently (3+ deaths)
   - Player is in 'dead' state (zombified in Hades)"
  [game-state]
  (or (:quit game-state)
      (:finished game-state)
      (>= (:deaths game-state 0) 3)
      (:dead game-state)))

(defn- game-over-reason
  "Return the reason for game over, or nil if game is not over."
  [game-state]
  (cond
    (:finished game-state) :won  ; Entered barrow - true victory
    (:quit game-state) :quit
    (>= (:deaths game-state 0) 3) :permanent-death
    (:dead game-state) :dead-but-playing  ; Hades state - can still play but dead
    :else nil))

(defn state-snapshot
  "Get a complete snapshot of game state for ML agent.

   Returns a map suitable for JSON serialization with:
   - Game metrics (score, moves, deaths)
   - Current location info
   - Visible objects and inventory
   - Valid actions
   - Last message hash (for novelty detection)"
  [game-state & {:keys [message] :or {message ""}}]
  (let [here (gs/get-here game-state)
        va (valid-actions game-state)
        ;; Hash the message for novelty detection
        message-hash (when (seq message)
                       (hash message))
        ;; Detect game-over state
        is-game-over (game-over? game-state)
        over-reason (game-over-reason game-state)]
    {:score (:score game-state 0)
     :max-score (:score-max game-state 350)
     :moves (:moves game-state 0)
     :deaths (:deaths game-state 0)
     :game-over is-game-over
     :game-over-reason over-reason

     :room {:id (:here game-state)
            :name (:desc here)
            :visited? (gs/set-here-flag? game-state :touch)}

     :lit? (:lit? va)
     :message message
     :message-hash message-hash

     :valid-actions va}))

;;; ---------------------------------------------------------------------------
;;; ACTION EXECUTION
;;; ---------------------------------------------------------------------------

(defn- loud-room-action-valid?
  "Check if an action is valid in loud-room special mode."
  [action]
  (let [{:keys [verb direction]} action]
    (or
     ;; Meta verbs
     (#{:look :quit :save :restore} verb)
     ;; Echo (the puzzle solution)
     (= verb :echo)
     ;; Movement to valid exits only
     (and (= verb :go)
          (#{:west :east :up :w :e :u} direction)))))

(defn- loud-room-execute
  "Execute an action in loud-room special mode.
   Returns {:game-state :message :snapshot} like execute-action."
  [game-state action]
  (let [{:keys [verb direction]} action]
    (cond
      ;; Echo - solve the puzzle
      (= verb :echo)
      (let [v-echo (requiring-resolve 'clork.loud-room/v-echo)
            output-buffer (atom [])
            gs-with-output (assoc game-state :output-buffer output-buffer)
            result-gs (v-echo gs-with-output)
            result-gs (dissoc result-gs :loud-room-mode :output-buffer)
            message (apply str @output-buffer)]
        {:game-state result-gs
         :message message
         :snapshot (state-snapshot result-gs :message message)})

      ;; Movement - go to specific rooms
      (= verb :go)
      (let [dest (case direction
                   (:west :w) :round-room
                   (:east :e) :damp-cave
                   (:up :u) :deep-canyon
                   nil)
            v-first-look (requiring-resolve 'clork.verbs-look/v-first-look)]
        (if dest
          (let [output-buffer (atom [])
                gs-with-output (assoc game-state :output-buffer output-buffer)
                result-gs (-> gs-with-output
                              (assoc :here dest)
                              (dissoc :loud-room-mode)
                              (v-first-look))
                result-gs (dissoc result-gs :output-buffer)
                message (apply str @output-buffer)]
            {:game-state result-gs
             :message message
             :snapshot (state-snapshot result-gs :message message)})
          ;; Invalid direction
          {:game-state game-state
           :message "You can't go that way in all this noise."
           :snapshot (state-snapshot game-state :message "You can't go that way in all this noise.")}))

      ;; Look - show room description
      (= verb :look)
      (let [loud-room-action (requiring-resolve 'clork.loud-room/loud-room-action)
            output-buffer (atom [])
            gs-with-output (assoc game-state :output-buffer output-buffer)
            result-gs (loud-room-action gs-with-output :look)
            result-gs (dissoc result-gs :output-buffer)
            message (apply str @output-buffer)]
        {:game-state result-gs
         :message message
         :snapshot (state-snapshot result-gs :message message)})

      ;; Meta verbs that pass through
      (#{:quit :save :restore} verb)
      (let [output-buffer (atom [])
            gs-with-output (assoc game-state :output-buffer output-buffer)
            parser-state {:prsa verb}
            gs-with-parser (assoc gs-with-output :parser
                                  (merge (:parser gs-with-output) parser-state))
            result-gs (verb-defs/perform gs-with-parser)
            result-gs (dissoc result-gs :output-buffer)
            message (apply str @output-buffer)]
        {:game-state result-gs
         :message message
         :snapshot (state-snapshot result-gs :message message)})

      ;; Invalid action - echo it back
      :else
      (let [verb-str (name verb)
            message (str verb-str " " verb-str " ...")]
        {:game-state game-state
         :message message
         :snapshot (state-snapshot game-state :message message)}))))

(defn execute-action
  "Execute a structured action and return new state + response.

   Action format:
   - {:verb :look}                                    ; no-argument verbs
   - {:verb :go :direction :north}                    ; movement
   - {:verb :take :direct-object :lamp}               ; single object
   - {:verb :put :direct-object :egg :prep :in :indirect-object :nest}

   Returns {:game-state new-state :message output-text :snapshot state-info}

   Note: Special input modes (like :loud-room-mode) restrict available actions.
   Invalid actions in special modes will be rejected with appropriate messages."
  [game-state action]
  ;; Check for special input modes
  (if (:loud-room-mode game-state)
    ;; Loud room special mode - limited actions only
    (loud-room-execute game-state action)

    ;; Normal execution
    (let [{:keys [verb direction direct-object indirect-object]} action

          ;; Convert to parser state format
          parser-state (cond-> {:prsa verb}
                         direct-object
                         (assoc :prso [direct-object])

                         indirect-object
                         (assoc :prsi indirect-object))

          ;; Special handling for movement
          final-parser-state
          (if (= verb :go)
            (assoc parser-state :prsa :walk :prso [direction])
            parser-state)

          ;; Set up game state with parser info
          ;; Important: We replace :prsa/:prso/:prsi rather than merge to avoid
          ;; stale values from previous actions affecting current action
          gs-with-parser (assoc game-state :parser
                                (-> (:parser game-state)
                                    (dissoc :prsa :prso :prsi)
                                    (merge final-parser-state)))

          ;; Capture output via atom
          output-buffer (atom [])
          gs-with-output (assoc gs-with-parser :output-buffer output-buffer)

          ;; Execute the action
          result-gs (verb-defs/perform gs-with-output)

          ;; Run daemons (combat, sword glow, etc.)
          ;; Output buffer stays attached so daemon messages are captured
          result-gs (daemon/clocker result-gs)

          ;; Get captured output from atom
          message (apply str @output-buffer)

          ;; Clean up and increment moves
          final-gs (-> result-gs
                       (dissoc :output-buffer)
                       (update :moves (fnil inc 0)))]

      {:game-state final-gs
       :message message
       :snapshot (state-snapshot final-gs :message message)})))

;;; ---------------------------------------------------------------------------
;;; CONVENIENCE: FLAT ACTION LIST
;;; ---------------------------------------------------------------------------

(defn action-list
  "Get a flat list of all valid actions as executable maps.

   Each entry is a complete action map that can be passed to execute-action.
   Useful for agents that want to sample from available actions."
  [game-state]
  (let [va (valid-actions game-state)
        {:keys [meta-verbs movement object-actions two-object-actions special-actions]} va]
    (vec
     (concat
      ;; Meta verbs
      (for [v meta-verbs]
        {:verb v})

      ;; Movement
      (for [dir (:directions movement)]
        {:verb :go :direction dir})

      ;; Special actions (e.g., :echo in loud room)
      (for [v (or special-actions [])]
        {:verb v})

      ;; Single-object actions
      (for [[obj-id {:keys [verbs]}] object-actions
            v verbs]
        {:verb v :direct-object obj-id})

      ;; Two-object actions
      (for [{:keys [verb direct-object prep indirect-object]} two-object-actions]
        {:verb verb
         :direct-object direct-object
         :prep prep
         :indirect-object indirect-object})))))

(defn action-count
  "Get the number of valid actions from current state.
   Useful for understanding action space size."
  [game-state]
  (count (action-list game-state)))

;;; ---------------------------------------------------------------------------
;;; JSON SERIALIZATION HELPERS
;;; ---------------------------------------------------------------------------

(defn- keyword->string
  "Convert keywords to strings for JSON serialization."
  [x]
  (cond
    (keyword? x) (name x)
    (map? x) (into {} (map (fn [[k v]] [(keyword->string k) (keyword->string v)]) x))
    (coll? x) (into (empty x) (map keyword->string x))
    (set? x) (vec (map keyword->string x))
    :else x))

(defn snapshot->json
  "Convert a state snapshot to JSON string."
  [snapshot]
  (json/write-str (keyword->string snapshot)))

(defn action<-json
  "Parse a JSON action string to Clojure map with keywords."
  [json-str]
  (let [parsed (json/read-str json-str)]
    (into {}
          (map (fn [[k v]]
                 [(keyword k)
                  (if (string? v) (keyword v) v)])
               parsed))))

;;; ---------------------------------------------------------------------------
;;; SESSION TRACKING (for reward shaping)
;;; ---------------------------------------------------------------------------
;;;
;;; A "session" tracks cumulative state across multiple actions for an ML agent.
;;; This enables reward shaping based on novelty (new rooms, new messages, etc.)

(defn initial-session
  "Create a fresh session state for tracking cumulative metrics.

   Session tracks:
   - :rooms-visited      - Set of room IDs visited this session
   - :messages-seen      - Set of message hashes seen (for novelty)
   - :objects-taken      - Objects ever picked up
   - :objects-examined   - Objects ever examined
   - :containers-opened  - Containers/doors ever opened
   - :max-score          - Highest score achieved
   - :total-deaths       - Cumulative death count
   - :total-moves        - Total moves taken
   - :start-time         - Session start timestamp"
  []
  {:rooms-visited #{}
   :messages-seen #{}
   :objects-taken #{}
   :objects-examined #{}
   :containers-opened #{}
   :puzzles-solved #{}
   :max-score 0
   :total-deaths 0
   :total-moves 0
   :valid-actions 0
   :invalid-actions 0
   :start-time (System/currentTimeMillis)})

(defn- extract-action-type
  "Extract the general action type from an action for tracking."
  [action]
  (let [verb (:verb action)]
    (cond
      (#{:take :get :grab} verb) :take
      (#{:examine :x :look-inside} verb) :examine
      (#{:open} verb) :open
      (#{:go :walk :north :south :east :west} verb) :move
      :else verb)))

(defn- compute-reward-signals
  "Compute reward signals by comparing before/after states and session.

   Returns a map of individual reward signals:
   - :score-delta       - Change in game score (direct from game)
   - :novel-room?       - True if this room wasn't visited before
   - :novel-message?    - True if message hash is new
   - :inventory-delta   - Change in inventory item count
   - :death?            - True if player died
   - :container-opened? - True if a new container was opened
   - :object-taken?     - True if a new object was acquired
   - :valid-action?     - True if action produced meaningful output"
  [before-gs after-gs session action message]
  (let [before-score (:score before-gs 0)
        after-score (:score after-gs 0)
        before-deaths (:deaths before-gs 0)
        after-deaths (:deaths after-gs 0)
        before-room (:here before-gs)
        after-room (:here after-gs)
        before-inv-count (count (get-inventory before-gs))
        after-inv-count (count (get-inventory after-gs))

        ;; Message novelty
        msg-hash (when (seq message) (hash message))
        novel-message? (and msg-hash
                            (not (contains? (:messages-seen session) msg-hash)))

        ;; Room novelty
        novel-room? (and (not= before-room after-room)
                         (not (contains? (:rooms-visited session) after-room)))

        ;; Action-specific novelty
        action-type (extract-action-type action)
        direct-obj (:direct-object action)

        object-taken? (and (= action-type :take)
                           direct-obj
                           (> after-inv-count before-inv-count)
                           (not (contains? (:objects-taken session) direct-obj)))

        container-opened? (and (= action-type :open)
                               direct-obj
                               (not (contains? (:containers-opened session) direct-obj))
                               ;; Check if it's actually open now
                               (gs/set-thing-flag? after-gs direct-obj :open))

        ;; Valid action detection (message isn't an error/nothing happened)
        valid-action? (and (seq message)
                           (not (re-find #"(?i)don't understand|can't do that|not here" message)))]

    {:score-delta (- after-score before-score)
     :novel-room? novel-room?
     :novel-message? novel-message?
     :room-changed? (not= before-room after-room)
     :inventory-delta (- after-inv-count before-inv-count)
     :death? (> after-deaths before-deaths)
     :object-taken? object-taken?
     :container-opened? container-opened?
     :valid-action? valid-action?
     :message-hash msg-hash}))

(defn- compute-composite-reward
  "Compute a weighted composite reward from individual signals.

   Default weights (can be overridden):
   - score-delta:      1.0  (direct game score)
   - novel-room:       5.0  (exploration bonus)
   - novel-message:    0.5  (variety bonus)
   - object-taken:     2.0  (acquisition bonus)
   - container-opened: 1.5  (discovery bonus)
   - death:          -10.0  (death penalty)
   - invalid-action:  -0.1  (discourage spam)"
  [signals & {:keys [weights]
              :or {weights {:score-delta 1.0
                            :novel-room 5.0
                            :novel-message 0.5
                            :object-taken 2.0
                            :container-opened 1.5
                            :death -10.0
                            :invalid-action -0.1}}}]
  (let [{:keys [score-delta novel-room? novel-message? object-taken?
                container-opened? death? valid-action?]} signals]
    (+ (* score-delta (:score-delta weights))
       (if novel-room? (:novel-room weights) 0)
       (if novel-message? (:novel-message weights) 0)
       (if object-taken? (:object-taken weights) 0)
       (if container-opened? (:container-opened weights) 0)
       (if death? (:death weights) 0)
       (if (not valid-action?) (:invalid-action weights) 0))))

(defn update-session
  "Update session state based on action results and reward signals.
   Returns updated session."
  [session after-gs signals action]
  (let [{:keys [novel-room? novel-message? object-taken? container-opened?
                death? valid-action? message-hash]} signals
        current-room (:here after-gs)
        direct-obj (:direct-object action)]
    (cond-> session
      ;; Always increment moves
      true
      (update :total-moves inc)

      ;; Track room visits
      novel-room?
      (update :rooms-visited conj current-room)

      ;; Track message novelty
      (and novel-message? message-hash)
      (update :messages-seen conj message-hash)

      ;; Track object acquisition
      (and object-taken? direct-obj)
      (update :objects-taken conj direct-obj)

      ;; Track container opens
      (and container-opened? direct-obj)
      (update :containers-opened conj direct-obj)

      ;; Track examinations
      (and (= (extract-action-type action) :examine) direct-obj)
      (update :objects-examined conj direct-obj)

      ;; Track max score
      (> (:score after-gs 0) (:max-score session 0))
      (assoc :max-score (:score after-gs 0))

      ;; Track deaths
      death?
      (update :total-deaths inc)

      ;; Track valid/invalid actions
      valid-action?
      (update :valid-actions inc)

      (not valid-action?)
      (update :invalid-actions inc))))

(defn session-stats
  "Get summary statistics from a session."
  [session]
  (let [elapsed-ms (- (System/currentTimeMillis) (:start-time session))
        total-actions (+ (:valid-actions session) (:invalid-actions session))]
    {:rooms-discovered (count (:rooms-visited session))
     :unique-messages (count (:messages-seen session))
     :objects-collected (count (:objects-taken session))
     :containers-opened (count (:containers-opened session))
     :max-score (:max-score session)
     :total-deaths (:total-deaths session)
     :total-moves (:total-moves session)
     :valid-action-rate (if (pos? total-actions)
                          (/ (:valid-actions session) total-actions)
                          0.0)
     :elapsed-seconds (/ elapsed-ms 1000.0)}))

;;; ---------------------------------------------------------------------------
;;; ENHANCED ACTION EXECUTION (with rewards)
;;; ---------------------------------------------------------------------------

(defn execute-action-with-rewards
  "Execute an action and compute reward signals.

   Like execute-action, but also takes a session and returns reward information.

   Returns:
   {:game-state    - New game state after action
    :message       - Text output from action
    :snapshot      - State snapshot for ML agent
    :rewards       - Map of individual reward signals
    :composite     - Single composite reward value
    :session       - Updated session state}"
  [before-gs session action & {:keys [reward-weights]}]
  (let [;; Execute the action (before-gs is the state BEFORE the action)
        {:keys [game-state message snapshot]} (execute-action before-gs action)

        ;; Compute rewards comparing before and after
        signals (compute-reward-signals before-gs game-state session action message)

        ;; Compute composite reward
        composite (if reward-weights
                    (compute-composite-reward signals :weights reward-weights)
                    (compute-composite-reward signals))

        ;; Update session
        new-session (update-session session game-state signals action)]

    {:game-state game-state
     :message message
     :snapshot snapshot
     :rewards signals
     :composite-reward composite
     :session new-session}))

;;; ---------------------------------------------------------------------------
;;; REWARD-AWARE JSON-LINES MODE
;;; ---------------------------------------------------------------------------

(defn json-line-mode-with-rewards
  "JSON-lines mode with reward tracking for RL training.

   Like json-line-mode, but includes reward signals in each response.

   Output format (after each action):
   {
     \"score\": 10,
     \"rewards\": {
       \"score_delta\": 10,
       \"novel_room\": true,
       \"composite\": 15.5
     },
     \"session\": {
       \"rooms_discovered\": 5,
       \"total_moves\": 12
     },
     \"valid_actions\": [...],
     ...
   }

   Special actions:
   - {\"verb\": \"quit\"} - Exit and return final stats
   - {\"verb\": \"reset\"} - Restart game (keeps session stats for comparison)
   - {\"verb\": \"stats\"} - Return session statistics without taking action"
  [init-fn & {:keys [reward-weights]}]
  (let [reader (BufferedReader. *in*)]
    (loop [game-state (init-fn)
           session (-> (initial-session)
                       ;; Add starting room to visited
                       (update :rooms-visited conj (:here game-state)))
           last-message ""
           last-rewards nil]

      ;; Build output with rewards
      (let [snapshot (state-snapshot game-state :message last-message)
            output (cond-> snapshot
                     last-rewards
                     (assoc :rewards last-rewards
                            :composite-reward (if reward-weights
                                                (compute-composite-reward last-rewards :weights reward-weights)
                                                (compute-composite-reward last-rewards)))
                     true
                     (assoc :session-stats (session-stats session)))]
        (println (snapshot->json output))
        (flush))

      ;; Read action from stdin
      (if-let [line (.readLine reader)]
        (let [action (try
                       (action<-json line)
                       (catch Exception e
                         {:verb :invalid :error (.getMessage e)}))]
          (cond
            ;; Invalid JSON
            (= (:verb action) :invalid)
            (do
              (println (json/write-str {"error" (:error action)}))
              (flush)
              (recur game-state session "" nil))

            ;; Quit - return final session stats
            (= (:verb action) :quit)
            (do
              (println (json/write-str {"final_stats" (keyword->string (session-stats session))}))
              (flush)
              game-state)

            ;; Stats - return session stats without action
            (= (:verb action) :stats)
            (do
              (println (json/write-str {"session_stats" (keyword->string (session-stats session))}))
              (flush)
              (recur game-state session last-message last-rewards))

            ;; Reset - restart game but preserve session for comparison
            (= (:verb action) :reset)
            (let [new-gs (init-fn)
                  ;; Optionally reset session or keep for episode comparison
                  new-session (-> (initial-session)
                                  (update :rooms-visited conj (:here new-gs)))]
              (recur new-gs new-session "" nil))

            ;; Execute action with rewards
            :else
            (let [result (execute-action-with-rewards
                          game-state session action
                          :reward-weights reward-weights)]
              (recur (:game-state result)
                     (:session result)
                     (:message result)
                     (:rewards result)))))

        ;; EOF - return final stats
        (do
          (println (json/write-str {"final_stats" (keyword->string (session-stats session))}))
          (flush)
          game-state)))))

;;; ---------------------------------------------------------------------------
;;; JSON-LINES MODE (for subprocess communication)
;;; ---------------------------------------------------------------------------

(defn json-line-mode
  "Run in JSON-lines mode for ML agent communication.

   Protocol:
   - On startup and after each action: send state snapshot as JSON line on stdout
   - Read action as JSON line from stdin
   - Special actions:
     - {:verb :quit} or {\"verb\": \"quit\"} - exit
     - {:verb :reset} or {\"verb\": \"reset\"} - restart game

   The init-fn should return a fresh, initialized game state.
   Returns the final game state when quit."
  [init-fn]
  (let [reader (BufferedReader. *in*)]
    (loop [game-state (init-fn)
           last-message ""]
      ;; Output current state as JSON line (with message from last action)
      (println (snapshot->json (state-snapshot game-state :message last-message)))
      (flush)

      ;; Read action from stdin
      (if-let [line (.readLine reader)]
        (let [action (try
                       (action<-json line)
                       (catch Exception e
                         {:verb :invalid :error (.getMessage e)}))]
          (cond
            ;; Invalid JSON
            (= (:verb action) :invalid)
            (do
              (println (json/write-str {"error" (:error action)}))
              (flush)
              (recur game-state ""))

            ;; Quit
            (= (:verb action) :quit)
            game-state

            ;; Reset
            (= (:verb action) :reset)
            (recur (init-fn) "")

            ;; Execute action
            :else
            (let [{:keys [game-state message]} (execute-action game-state action)]
              (recur game-state message))))

        ;; EOF - exit
        game-state))))
