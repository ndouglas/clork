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

(defn get-visible-objects
  "Get all objects visible in the current room, including nested containers.
   Returns a vector of maps with :id, :container (parent), and :depth."
  [game-state]
  (let [here (:here game-state)]
    (get-visible-objects-in game-state here 0 nil)))

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
   - :two-object     - List of valid two-object commands"
  [game-state]
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
     :lit? lit?}))

;;; ---------------------------------------------------------------------------
;;; STATE SNAPSHOT
;;; ---------------------------------------------------------------------------

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
                       (hash message))]
    {:score (:score game-state 0)
     :max-score (:score-max game-state 350)
     :moves (:moves game-state 0)
     :deaths (:deaths game-state 0)
     :game-over false  ; TODO: detect game over state

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

(defn execute-action
  "Execute a structured action and return new state + response.

   Action format:
   - {:verb :look}                                    ; no-argument verbs
   - {:verb :go :direction :north}                    ; movement
   - {:verb :take :direct-object :lamp}               ; single object
   - {:verb :put :direct-object :egg :prep :in :indirect-object :nest}

   Returns {:game-state new-state :message output-text :snapshot state-info}"
  [game-state action]
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
        gs-with-parser (assoc game-state :parser
                              (merge (:parser game-state) final-parser-state))

        ;; Capture output via atom
        output-buffer (atom [])
        gs-with-output (assoc gs-with-parser :output-buffer output-buffer)

        ;; Execute the action
        result-gs (verb-defs/perform gs-with-output)

        ;; Get captured output from atom
        message (apply str @output-buffer)

        ;; Clean up and increment moves
        final-gs (-> result-gs
                     (dissoc :output-buffer)
                     (update :moves (fnil inc 0)))]

    {:game-state final-gs
     :message message
     :snapshot (state-snapshot final-gs :message message)}))

;;; ---------------------------------------------------------------------------
;;; CONVENIENCE: FLAT ACTION LIST
;;; ---------------------------------------------------------------------------

(defn action-list
  "Get a flat list of all valid actions as executable maps.

   Each entry is a complete action map that can be passed to execute-action.
   Useful for agents that want to sample from available actions."
  [game-state]
  (let [va (valid-actions game-state)
        {:keys [meta-verbs movement object-actions two-object-actions]} va]
    (vec
     (concat
      ;; Meta verbs
      (for [v meta-verbs]
        {:verb v})

      ;; Movement
      (for [dir (:directions movement)]
        {:verb :go :direction dir})

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
