(ns clork.intel.transition
  "Stage 1: Instrumented transition function with change tracking.

   Provides `step` - a wrapper around ml/execute-action that captures
   exactly what changed between game states. This is the foundation for
   all planner infrastructure.

   Uses a HYBRID approach:
   - EVENT SOURCING for: flags, score, daemons, location
     (these go through centralized functions that record changes)
   - STATE DIFFING for: object locations (:in field)
     (modified inline throughout the codebase)"
  (:require [clork.ml :as ml]
            [clork.game-state :as gs]
            [clojure.set :as set]
            [clojure.string :as str]))

;;; ---------------------------------------------------------------------------
;;; OBJECT LOCATION DIFFING
;;; ---------------------------------------------------------------------------
;;; Object :in field is modified inline throughout the codebase, so we still
;;; need to diff this. Other changes are captured via event sourcing.

(defn- extract-object-locations
  "Extract location of all objects. Returns map of object-id -> location."
  [state]
  (into {}
        (for [[obj-id obj] (:objects state)]
          [obj-id (:in obj)])))

(defn- diff-object-locations
  "Return map of object-id -> {:from loc :to loc} for moved objects."
  [before after]
  (let [before-locs (extract-object-locations before)
        after-locs (extract-object-locations after)]
    (into {}
          (for [obj-id (set/union (set (keys before-locs)) (set (keys after-locs)))
                :let [bloc (get before-locs obj-id)
                      aloc (get after-locs obj-id)]
                :when (not= bloc aloc)]
            [obj-id {:from bloc :to aloc}]))))

;;; ---------------------------------------------------------------------------
;;; MOVES DIFFING
;;; ---------------------------------------------------------------------------
;;; Move counter is incremented inline, so we diff it too.

(defn- diff-moves [before after]
  (let [before-moves (:moves before 0)
        after-moves (:moves after 0)]
    (when (not= before-moves after-moves)
      {:from before-moves :to after-moves})))

;;; ---------------------------------------------------------------------------
;;; CONVERT RECORDED CHANGES TO DIFF FORMAT
;;; ---------------------------------------------------------------------------

(defn- process-recorded-changes
  "Convert recorded changes from event sourcing into diff format.

   Takes the :changes vector from game-state and returns:
   {:location-changed {:from x :to y} or nil
    :score-changed {:from x :to y} or nil
    :flags-set #{...}
    :flags-cleared #{...}
    :daemons-started #{...}
    :daemons-stopped #{}
    :objects-moved {obj-id {:from loc :to loc :reason kw} ...}}"
  [changes]
  (reduce
    (fn [acc change]
      (case (:type change)
        :location-changed
        (assoc acc :location-changed {:from (:from change) :to (:to change)})

        :score-changed
        (assoc acc :score-changed {:from (:from change) :to (:to change)})

        :flag-set
        (update acc :flags-set conj
                (if (= :game (:entity change))
                  (:flag change)
                  [(:entity change) (:flag change)]))

        :flag-cleared
        (update acc :flags-cleared conj
                (if (= :game (:entity change))
                  (:flag change)
                  [(:entity change) (:flag change)]))

        ;; Game-level flags (not entity flags)
        :game-flag-set
        (update acc :flags-set conj (:flag change))

        :game-flag-cleared
        (update acc :flags-cleared conj (:flag change))

        :daemon-started
        (update acc :daemons-started conj (:daemon change))

        :daemon-stopped
        (update acc :daemons-stopped conj (:daemon change))

        ;; Object movement with optional reason
        :object-moved
        (assoc-in acc [:objects-moved (:object change)]
                  (cond-> {:from (:from change) :to (:to change)}
                    (:reason change) (assoc :reason (:reason change))))

        ;; Unknown change type - ignore
        acc))
    {:location-changed nil
     :score-changed nil
     :flags-set #{}
     :flags-cleared #{}
     :daemons-started #{}
     :daemons-stopped #{}
     :objects-moved {}}
    changes))

;;; ---------------------------------------------------------------------------
;;; FALLBACK DIFFING (for backward compatibility)
;;; ---------------------------------------------------------------------------
;;; When no recorded changes are available, we fall back to state diffing.
;;; This is less reliable but ensures backward compatibility.

(def ^:private game-flags
  "Top-level game state keys that represent meaningful game flags.
   NOTE: :lit is excluded because it's derived/computed state that gets
   recalculated on every move, not an intentional game state flag."
  #{:troll-flag :cyclops-flag :magic-flag :low-tide :gates-open :gate-flag
    :loud-flag :won :finished :xb :xc :lld-flag :deflate :inflate
    :rug-moved :trap-door-open :dome-flag :rainbow-flag :endgame
    :coffin-cure :match-lit :candle-lit :buoy-flag :echo-flag
    :dead :lucky :water-level})

(def ^:private entity-structural-keys
  "Keys on objects/rooms that are NOT runtime flag overrides."
  #{:id :in :synonym :adjective :desc :ldesc :fdesc :flags :capacity :size
    :order :inv-seq :text :action :exits :globals :value :pseudo :strength
    :handler :enabled :tick :data :tvalue :fval :owner :changes})

(def ^:private known-flag-keys
  "Known runtime flag override keys."
  #{:on :open :locked :touch :lit :worn :raised :tied :inflated :deflated
    :fighting :staggered :unconscious :dead :invisible :ndesc :vehicle
    :burned-out :trytake :flame :sacred :maze :rwater :surface :trans
    :weapon :actor :fight :tool :cont :light :take :door :read :climb
    :turnable :turnflag})

(defn- diff-location-fallback [before after]
  (let [b (:here before)
        a (:here after)]
    (when (not= b a)
      {:from b :to a})))

(defn- diff-score-fallback [before after]
  (let [b (:score before 0)
        a (:score after 0)]
    (when (not= b a)
      {:from b :to a})))

(defn- extract-game-flags [state]
  (into {}
        (for [k game-flags
              :let [v (get state k)]
              :when (some? v)]
          [k (boolean v)])))

(defn- diff-game-flags-fallback [before after]
  (let [before-flags (extract-game-flags before)
        after-flags (extract-game-flags after)
        all-keys (set/union (set (keys before-flags)) (set (keys after-flags)))
        set-flags (into #{}
                        (for [k all-keys
                              :let [bv (get before-flags k false)
                                    av (get after-flags k false)]
                              :when (and (not bv) av)]
                          k))
        cleared-flags (into #{}
                            (for [k all-keys
                                  :let [bv (get before-flags k false)
                                        av (get after-flags k false)]
                                  :when (and bv (not av))]
                              k))]
    {:set set-flags :cleared cleared-flags}))

(defn- flag-key? [k v]
  (and (keyword? k)
       (not (contains? entity-structural-keys k))
       (or (contains? known-flag-keys k)
           (boolean? v))))

(defn- extract-entity-flags
  "Extract runtime flag overrides from an entity.
   NOTE: This only extracts RUNTIME keys, not static :flags set.
   For semantic flag state, use gs/flag? which checks both layers."
  [entity]
  (into {}
        (for [[k v] entity
              :when (flag-key? k v)]
          [k (boolean v)])))

(defn- get-semantic-flag-state
  "Get the semantic flag state using the two-layer system.
   This properly checks both static :flags set AND runtime overrides."
  [state entity-type entity-id flag]
  (gs/flag? state entity-type entity-id flag))

(defn- extract-all-entity-flags-semantic
  "Extract semantic flag state for all entities using the two-layer system.
   This is more accurate than extract-all-entity-flags because it checks
   both the static :flags set AND runtime overrides."
  [state]
  (let [object-flags (for [[obj-id obj] (:objects state)
                           flag known-flag-keys]
                       [[:objects obj-id flag]
                        (get-semantic-flag-state state :objects obj-id flag)])
        room-flags (for [[room-id room] (:rooms state)
                         flag known-flag-keys]
                     [[:rooms room-id flag]
                      (get-semantic-flag-state state :rooms room-id flag)])]
    (into {} (concat object-flags room-flags))))

(defn- extract-all-entity-flags [state]
  ;; Use semantic extraction that properly handles two-layer flag system
  (extract-all-entity-flags-semantic state))

(defn- diff-entity-flags-fallback [before after]
  (let [before-flags (extract-all-entity-flags before)
        after-flags (extract-all-entity-flags after)
        all-keys (set/union (set (keys before-flags)) (set (keys after-flags)))
        set-flags (into #{}
                        (for [k all-keys
                              :let [bv (get before-flags k false)
                                    av (get after-flags k false)]
                              :when (and (not bv) av)]
                          (let [[_ entity-id flag] k]
                            [entity-id flag])))
        cleared-flags (into #{}
                            (for [k all-keys
                                  :let [bv (get before-flags k false)
                                        av (get after-flags k false)]
                                  :when (and bv (not av))]
                              (let [[_ entity-id flag] k]
                                [entity-id flag])))]
    {:set set-flags :cleared cleared-flags}))

(defn- extract-daemon-states [state]
  (into {}
        (for [[daemon-id daemon] (:daemons state)]
          [daemon-id (boolean (:enabled daemon))])))

(defn- diff-daemons-fallback [before after]
  (let [before-states (extract-daemon-states before)
        after-states (extract-daemon-states after)
        all-ids (set/union (set (keys before-states)) (set (keys after-states)))
        started (into #{}
                      (for [id all-ids
                            :let [bv (get before-states id false)
                                  av (get after-states id false)]
                            :when (and (not bv) av)]
                        id))
        stopped (into #{}
                      (for [id all-ids
                            :let [bv (get before-states id false)
                                  av (get after-states id false)]
                            :when (and bv (not av))]
                        id))]
    {:started started :stopped stopped}))

;;; ---------------------------------------------------------------------------
;;; MAIN DIFF FUNCTION
;;; ---------------------------------------------------------------------------

(defn compute-state-diff
  "Compute diff between two game states.

   Uses a hybrid approach:
   - If recorded changes exist (from event sourcing), use those (more reliable)
   - Falls back to state diffing when no recorded changes available
   - For object movements: merges recorded (with :reason) and diffed (legacy)

   Returns a map with:
   - :location-changed - {:from room :to room} or nil
   - :score-changed - {:from n :to n} or nil
   - :moves-changed - {:from n :to n} or nil
   - :objects-moved - map of obj-id -> {:from loc :to loc :reason kw}
   - :flags-set - set of flags (game-level or [entity flag] pairs)
   - :flags-cleared - set of flags
   - :daemons-started - set of daemon ids
   - :daemons-stopped - set of daemon ids"
  [before after]
  (let [recorded-changes (gs/get-changes after)
        ;; Always diff object locations (catches untracked movements)
        diffed-objects (diff-object-locations before after)
        moves-changed (diff-moves before after)]
    (if (seq recorded-changes)
      ;; Use recorded changes (more reliable, has causality)
      (let [from-events (process-recorded-changes recorded-changes)
            recorded-objects (:objects-moved from-events)
            ;; Merge: recorded takes precedence (has :reason), diffed fills gaps
            merged-objects (merge diffed-objects recorded-objects)]
        {:location-changed (:location-changed from-events)
         :score-changed (:score-changed from-events)
         :moves-changed moves-changed
         :objects-moved merged-objects
         :flags-set (:flags-set from-events)
         :flags-cleared (:flags-cleared from-events)
         :daemons-started (:daemons-started from-events)
         :daemons-stopped (:daemons-stopped from-events)})
      ;; Fallback to diffing (backward compatibility)
      (let [game-flag-diff (diff-game-flags-fallback before after)
            entity-flag-diff (diff-entity-flags-fallback before after)
            daemon-diff (diff-daemons-fallback before after)]
        {:location-changed (diff-location-fallback before after)
         :score-changed (diff-score-fallback before after)
         :moves-changed moves-changed
         :objects-moved diffed-objects
         :flags-set (set/union (:set game-flag-diff) (:set entity-flag-diff))
         :flags-cleared (set/union (:cleared game-flag-diff) (:cleared entity-flag-diff))
         :daemons-started (:started daemon-diff)
         :daemons-stopped (:stopped daemon-diff)}))))

;;; ---------------------------------------------------------------------------
;;; STEP FUNCTION
;;; ---------------------------------------------------------------------------

(defn step
  "Execute an action and return the result with change tracking.

   Uses HYBRID approach:
   - Event sourcing for flags, score, daemons, location
   - State diffing for object locations (:in field)

   Takes a game-state and action map (e.g., {:verb :take :direct-object :lamp}).
   Returns a map with:
   - :game-state - the new game state after execution
   - :message - text output from the action
   - :diff - computed changes (see compute-state-diff)
   - :changes - raw change events (for causality tracking)
   - :before - the game state before execution (for debugging)
   - :action - the action that was executed"
  [game-state action]
  (let [;; Clear any previous changes before executing
        clean-state (gs/clear-changes game-state)
        ;; Execute the action
        result (ml/execute-action clean-state action)
        after (:game-state result)
        ;; Get the raw recorded changes (for causality)
        raw-changes (gs/get-changes after)
        ;; Convert to diff format + add diffed object locations
        diff (compute-state-diff clean-state after)]
    (assoc result
           :diff diff
           :changes raw-changes
           :before game-state
           :action action)))

;;; ---------------------------------------------------------------------------
;;; HUMAN READABLE OUTPUT
;;; ---------------------------------------------------------------------------

(defn- format-flag [flag]
  "Format a flag for display. Handles both keywords and [obj flag] vectors."
  (if (vector? flag)
    (str (name (first flag)) ":" (name (second flag)))
    (name flag)))

(defn diff-summary
  "Return a human-readable summary of a state diff."
  [diff]
  (let [parts (cond-> []
                (:location-changed diff)
                (conj (str "Moved: " (name (:from (:location-changed diff)))
                           " -> " (name (:to (:location-changed diff)))))

                (:score-changed diff)
                (conj (str "Score: " (:from (:score-changed diff))
                           " -> " (:to (:score-changed diff))))

                (:moves-changed diff)
                (conj (str "Moves: " (:from (:moves-changed diff))
                           " -> " (:to (:moves-changed diff))))

                (seq (:objects-moved diff))
                (conj (str "Objects moved: "
                           (str/join ", "
                                     (for [[obj-id {:keys [from to]}] (:objects-moved diff)]
                                       (str (name obj-id) " ("
                                            (if from (name from) "nil") " -> "
                                            (if to (name to) "nil") ")")))))

                (seq (:flags-set diff))
                (conj (str "Flags set: "
                           (str/join ", " (map format-flag (:flags-set diff)))))

                (seq (:flags-cleared diff))
                (conj (str "Flags cleared: "
                           (str/join ", " (map format-flag (:flags-cleared diff)))))

                (seq (:daemons-started diff))
                (conj (str "Daemons started: "
                           (str/join ", " (map name (:daemons-started diff)))))

                (seq (:daemons-stopped diff))
                (conj (str "Daemons stopped: "
                           (str/join ", " (map name (:daemons-stopped diff))))))]
    (if (empty? parts)
      "No changes."
      (str/join "\n" parts))))

;;; ---------------------------------------------------------------------------
;;; CONVENIENCE FUNCTIONS
;;; ---------------------------------------------------------------------------

(defn step-verbose
  "Like step, but prints the diff summary to stdout."
  [game-state action]
  (let [result (step game-state action)]
    (println "Action:" action)
    (println (diff-summary (:diff result)))
    (println "---")
    result))

(defn changed?
  "Quick check if any meaningful changes occurred."
  [diff]
  (or (:location-changed diff)
      (:score-changed diff)
      (seq (:objects-moved diff))
      (seq (:flags-set diff))
      (seq (:flags-cleared diff))
      (seq (:daemons-started diff))
      (seq (:daemons-stopped diff))))

(defn get-raw-changes
  "Get the raw change events from a step result.
   Useful for causality tracking - know exactly what caused each change."
  [step-result]
  (:changes step-result))

;;; ---------------------------------------------------------------------------
;;; UNTRACKED CHANGE DETECTION
;;; ---------------------------------------------------------------------------
;;; Compares event-sourced changes against state diffing to find "backdoor"
;;; modifications that bypass the tracked functions.

(defn- recorded-flags-set
  "Extract set of flags that were recorded as set."
  [changes]
  (into #{}
        (for [c changes :when (= :flag-set (:type c))]
          (if (= :game (:entity c))
            (:flag c)
            [(:entity c) (:flag c)]))))

(defn- recorded-flags-cleared
  "Extract set of flags that were recorded as cleared."
  [changes]
  (into #{}
        (for [c changes :when (= :flag-cleared (:type c))]
          (if (= :game (:entity c))
            (:flag c)
            [(:entity c) (:flag c)]))))

(defn- recorded-daemons-started
  "Extract set of daemons that were recorded as started."
  [changes]
  (into #{} (for [c changes :when (= :daemon-started (:type c))] (:daemon c))))

(defn- recorded-daemons-stopped
  "Extract set of daemons that were recorded as stopped."
  [changes]
  (into #{} (for [c changes :when (= :daemon-stopped (:type c))] (:daemon c))))

(defn- recorded-location-change
  "Extract location change from recorded changes."
  [changes]
  (some #(when (= :location-changed (:type %))
           {:from (:from %) :to (:to %)})
        changes))

(defn- recorded-score-change
  "Extract score change from recorded changes."
  [changes]
  (some #(when (= :score-changed (:type %))
           {:from (:from %) :to (:to %)})
        changes))

(defn detect-untracked-changes
  "Compare event-sourced changes against state diffing to find untracked modifications.

   Returns a map of untracked changes by category:
   {:location-changed  - location changed but not recorded
    :score-changed     - score changed but not recorded
    :flags-set         - set of flags set without recording
    :flags-cleared     - set of flags cleared without recording
    :daemons-started   - set of daemons started without recording
    :daemons-stopped   - set of daemons stopped without recording
    :objects-moved     - (always present, object :in is intentionally untracked)}

   Empty sets/nils indicate all changes were properly tracked."
  [before after]
  (let [recorded (gs/get-changes after)

        ;; What was recorded via event sourcing
        rec-flags-set (recorded-flags-set recorded)
        rec-flags-cleared (recorded-flags-cleared recorded)
        rec-daemons-started (recorded-daemons-started recorded)
        rec-daemons-stopped (recorded-daemons-stopped recorded)
        rec-location (recorded-location-change recorded)
        rec-score (recorded-score-change recorded)

        ;; What diffing detected (fallback approach)
        game-flag-diff (diff-game-flags-fallback before after)
        entity-flag-diff (diff-entity-flags-fallback before after)
        daemon-diff (diff-daemons-fallback before after)
        diff-location (diff-location-fallback before after)
        diff-score (diff-score-fallback before after)

        ;; Combine all diffed flags
        diff-flags-set (set/union (:set game-flag-diff) (:set entity-flag-diff))
        diff-flags-cleared (set/union (:cleared game-flag-diff) (:cleared entity-flag-diff))

        ;; Find what diffing detected but event sourcing missed
        untracked-flags-set (set/difference diff-flags-set rec-flags-set)
        untracked-flags-cleared (set/difference diff-flags-cleared rec-flags-cleared)
        untracked-daemons-started (set/difference (:started daemon-diff) rec-daemons-started)
        untracked-daemons-stopped (set/difference (:stopped daemon-diff) rec-daemons-stopped)
        untracked-location (when (and diff-location (not= diff-location rec-location))
                             diff-location)
        untracked-score (when (and diff-score (not= diff-score rec-score))
                          diff-score)]

    {:location-changed untracked-location
     :score-changed untracked-score
     :flags-set untracked-flags-set
     :flags-cleared untracked-flags-cleared
     :daemons-started untracked-daemons-started
     :daemons-stopped untracked-daemons-stopped
     ;; Objects are intentionally untracked (modified inline everywhere)
     :objects-moved (diff-object-locations before after)}))

(defn has-untracked-changes?
  "Check if any untracked changes were detected (excluding objects-moved)."
  [untracked]
  (or (:location-changed untracked)
      (:score-changed untracked)
      (seq (:flags-set untracked))
      (seq (:flags-cleared untracked))
      (seq (:daemons-started untracked))
      (seq (:daemons-stopped untracked))))

(defn untracked-summary
  "Return a human-readable summary of untracked changes."
  [untracked]
  (let [parts (cond-> []
                (:location-changed untracked)
                (conj (str "UNTRACKED location: "
                           (name (:from (:location-changed untracked)))
                           " -> " (name (:to (:location-changed untracked)))))

                (:score-changed untracked)
                (conj (str "UNTRACKED score: "
                           (:from (:score-changed untracked))
                           " -> " (:to (:score-changed untracked))))

                (seq (:flags-set untracked))
                (conj (str "UNTRACKED flags set: "
                           (str/join ", " (map format-flag (:flags-set untracked)))))

                (seq (:flags-cleared untracked))
                (conj (str "UNTRACKED flags cleared: "
                           (str/join ", " (map format-flag (:flags-cleared untracked)))))

                (seq (:daemons-started untracked))
                (conj (str "UNTRACKED daemons started: "
                           (str/join ", " (map name (:daemons-started untracked)))))

                (seq (:daemons-stopped untracked))
                (conj (str "UNTRACKED daemons stopped: "
                           (str/join ", " (map name (:daemons-stopped untracked))))))]
    (if (empty? parts)
      "All changes properly tracked."
      (str/join "\n" parts))))

(defn step-with-tracking-validation
  "Like step, but also detects and reports untracked changes.

   Returns the normal step result with an additional :untracked key
   containing any changes that bypassed event sourcing."
  [game-state action]
  (let [clean-state (gs/clear-changes game-state)
        result (ml/execute-action clean-state action)
        after (:game-state result)
        untracked (detect-untracked-changes clean-state after)]
    (assoc (step game-state action)
           :untracked untracked)))
