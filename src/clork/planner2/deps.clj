(ns clork.planner2.deps
  "Dependency Graph for the Prep-Optimized Planner.

   This module builds and queries the dependency graph that shows
   what prep actions require what other prep actions, items, or flags.

   Key queries:
   - What preps do I need for treasure X?
   - What order should I do preps in?
   - Are there any cycles or impossible configurations?

   See IMPLEMENTATION_PLAN.md for the full architecture."
  (:require [clork.planner2.prep :as prep]
            [clojure.set :as set]))

;;; ---------------------------------------------------------------------------
;;; PREP → PREP DEPENDENCIES
;;; ---------------------------------------------------------------------------

(defn prep-depends-on-prep
  "Determine which prep actions a given prep depends on.
   Returns a set of prep-ids that must be completed before this prep."
  [prep-id]
  (let [requires (prep/prep-requires prep-id)]
    ;; Check each requirement to see if it's a flag that comes from a prep
    (set
     (for [req requires
           :when (contains? prep/prep-actions req)]
       req))))

(defn build-prep-dependency-graph
  "Build a map of prep-id -> set of prep-ids it depends on."
  []
  (into {}
        (for [prep-id (prep/all-preps)]
          [prep-id (prep-depends-on-prep prep-id)])))

(def prep-dependency-graph
  "Cached prep dependency graph."
  (delay (build-prep-dependency-graph)))

;;; ---------------------------------------------------------------------------
;;; ITEM → PREP DEPENDENCIES
;;; ---------------------------------------------------------------------------

(defn item-requires-prep
  "Determine which prep action(s) are needed to obtain an item.
   Returns a set of prep-ids."
  [item]
  (if-let [info (get prep/prep-item-locations item)]
    (let [reqs (:requires info #{})]
      ;; Filter to just the prep flags
      (set (filter #(contains? prep/prep-actions %) reqs)))
    #{}))

(defn build-item-dependency-graph
  "Build a map of item -> set of prep-ids needed to obtain it."
  []
  (into {}
        (for [[item _] prep/prep-item-locations]
          [item (item-requires-prep item)])))

(def item-dependency-graph
  "Cached item dependency graph."
  (delay (build-item-dependency-graph)))

;;; ---------------------------------------------------------------------------
;;; TREASURE → PREP DEPENDENCIES
;;; ---------------------------------------------------------------------------

(defn treasure-requires-preps
  "Get all prep actions required to obtain and deposit a treasure.
   This includes transitive dependencies."
  [treasure]
  (get-in prep/treasure-prep-requirements [treasure :required] #{}))

(defn all-treasures
  "Get all treasure IDs."
  []
  (keys prep/treasure-prep-requirements))

;;; ---------------------------------------------------------------------------
;;; TRANSITIVE DEPENDENCY RESOLUTION
;;; ---------------------------------------------------------------------------

(defn transitive-prep-deps
  "Get all prep actions needed for a given prep, including transitive deps.
   Returns preps in dependency order (dependencies first)."
  [prep-id]
  (loop [stack [prep-id]      ; Stack of nodes to visit
         visited #{}          ; Nodes we've fully visited
         visiting #{}         ; Nodes currently being visited (on stack)
         result []]
    (if (empty? stack)
      result
      (let [current (peek stack)]
        (cond
          ;; Already fully processed
          (visited current)
          (recur (pop stack) visited visiting result)

          ;; First time seeing this node
          (not (visiting current))
          (let [deps (prep-depends-on-prep current)
                unvisited-deps (remove visited deps)]
            ;; Push deps onto stack, mark current as visiting
            (recur (into (pop stack) (cons current unvisited-deps))
                   visited
                   (conj visiting current)
                   result))

          ;; All deps are done, finalize this node
          :else
          (recur (pop stack)
                 (conj visited current)
                 (disj visiting current)
                 (conj result current)))))))

(defn transitive-treasure-deps
  "Get all prep actions needed for a treasure, including transitive deps.
   Returns preps in dependency order (dependencies first)."
  [treasure]
  (let [direct-preps (treasure-requires-preps treasure)]
    (vec (distinct (mapcat transitive-prep-deps direct-preps)))))

(defn all-preps-for-treasures
  "Get all prep actions needed for a set of treasures.
   Returns preps in a valid execution order."
  [treasures]
  (let [all-deps (mapcat transitive-treasure-deps treasures)]
    (vec (distinct all-deps))))

;;; ---------------------------------------------------------------------------
;;; TOPOLOGICAL SORT
;;; ---------------------------------------------------------------------------

(defn topological-sort
  "Sort prep actions in dependency order using Kahn's algorithm.
   Returns nil if there's a cycle."
  [prep-ids]
  (let [;; Build adjacency list for just the specified preps
        ;; edges[p] = set of preps that p depends on
        preps (set prep-ids)
        deps-of (into {}
                      (for [p preps]
                        [p (set/intersection (prep-depends-on-prep p) preps)]))
        ;; Count dependencies (number of preps each prep depends on)
        dep-count (into {} (for [[p deps] deps-of] [p (count deps)]))]
    (loop [result []
           remaining preps
           counts dep-count]
      (if (empty? remaining)
        result
        ;; Find nodes with no unprocessed dependencies
        (let [ready (filter #(zero? (get counts % 0)) remaining)]
          (if (empty? ready)
            nil  ; Cycle detected!
            (let [node (first ready)
                  new-remaining (disj remaining node)
                  ;; Decrease count for nodes that depend on this node
                  dependents (for [p new-remaining
                                   :when (contains? (get deps-of p) node)]
                               p)
                  new-counts (reduce #(update %1 %2 dec) counts dependents)]
              (recur (conj result node)
                     new-remaining
                     new-counts))))))))

;;; ---------------------------------------------------------------------------
;;; CYCLE DETECTION
;;; ---------------------------------------------------------------------------

(defn find-cycle
  "Find a cycle in the dependency graph, if one exists.
   Returns the cycle as a vector, or nil if no cycle."
  []
  (let [graph @prep-dependency-graph]
    (loop [to-visit (set (keys graph))
           visiting #{}
           visited #{}
           path []]
      (cond
        ;; Done visiting all nodes
        (and (empty? to-visit) (empty? visiting))
        nil

        ;; Need to pick a new starting node
        (empty? visiting)
        (let [start (first to-visit)]
          (recur (disj to-visit start)
                 #{start}
                 visited
                 [start]))

        ;; Continue DFS
        :else
        (let [current (peek path)
              deps (get graph current #{})
              unvisited-deps (set/difference deps visited)]
          (if-let [cycle-node (first (set/intersection deps visiting))]
            ;; Found a cycle!
            (let [cycle-start (first (keep-indexed #(when (= %2 cycle-node) %1) path))]
              (subvec path cycle-start))
            (if-let [next-node (first unvisited-deps)]
              ;; Visit next dependency
              (recur to-visit
                     (conj visiting next-node)
                     visited
                     (conj path next-node))
              ;; Done with this node
              (recur to-visit
                     (disj visiting current)
                     (conj visited current)
                     (if (empty? (rest path))
                       []
                       (pop path))))))))))

(defn valid-dependency-graph?
  "Check if the dependency graph is valid (no cycles)."
  []
  (nil? (find-cycle)))

;;; ---------------------------------------------------------------------------
;;; QUERY FUNCTIONS
;;; ---------------------------------------------------------------------------

(defn preps-for-treasure
  "What prep actions do I need to get treasure X?
   Returns a vector of prep-ids in execution order."
  [treasure]
  (let [deps (transitive-treasure-deps treasure)]
    (or (topological-sort deps) deps)))

(defn preps-for-treasures
  "What prep actions do I need for multiple treasures?
   Returns a vector of prep-ids in execution order."
  [treasures]
  (let [deps (all-preps-for-treasures treasures)]
    (or (topological-sort deps) deps)))

(defn missing-preps
  "Given current game state flags, what preps are still needed for a treasure?"
  [game-state treasure]
  (let [needed (preps-for-treasure treasure)
        completed (set (filter #(get game-state %) needed))]
    (vec (remove completed needed))))

(defn next-prep
  "Given current game state, what's the next prep action to do for a treasure?"
  [game-state treasure]
  (first (missing-preps game-state treasure)))

(defn can-do-prep?
  "Check if a prep action can be done given current game state."
  [game-state prep-id]
  (let [requires (prep/prep-requires prep-id)]
    (every? (fn [req]
              (or (get game-state req)                    ; flag is set
                  (contains? (get game-state :inventory #{}) req)))  ; have item
            requires)))

(defn ready-preps
  "Get all prep actions that can be done right now given game state."
  [game-state]
  (filter #(can-do-prep? game-state %) (prep/all-preps)))

;;; ---------------------------------------------------------------------------
;;; PREP CHAINS
;;; ---------------------------------------------------------------------------

(defn prep-chain
  "Get the full chain of preps needed, from scratch, for a prep action.
   Returns in execution order."
  [prep-id]
  (transitive-prep-deps prep-id))

(defn longest-prep-chain
  "Find the longest dependency chain (critical path)."
  []
  (let [chains (map #(vector % (count (prep-chain %))) (prep/all-preps))]
    (apply max-key second chains)))

(defn independent-preps
  "Find prep actions that have no dependencies on other preps.
   These can potentially be done in any order."
  []
  (filter #(empty? (prep-depends-on-prep %)) (prep/all-preps)))

(defn prep-levels
  "Group preps by their dependency level.
   Level 0 = no prep dependencies, Level 1 = depends on level 0 only, etc."
  []
  (loop [remaining (set (prep/all-preps))
         levels []
         current-level 0]
    (if (empty? remaining)
      levels
      (let [;; Find preps whose deps are all in previous levels
            previous (set (apply concat levels))
            ready (filter (fn [p]
                            (every? #(contains? previous %)
                                    (prep-depends-on-prep p)))
                          remaining)]
        (if (empty? ready)
          ;; Shouldn't happen if no cycles
          levels
          (recur (set/difference remaining (set ready))
                 (conj levels (vec ready))
                 (inc current-level)))))))

;;; ---------------------------------------------------------------------------
;;; ANALYSIS / DEBUG
;;; ---------------------------------------------------------------------------

(defn analyze-treasure
  "Analyze what's needed to obtain a treasure."
  [treasure]
  (let [preps (preps-for-treasure treasure)
        items (set (mapcat #(prep/prep-requires %) preps))
        item-deps (filter #(not (contains? prep/prep-actions %)) items)]
    {:treasure treasure
     :preps-needed preps
     :prep-count (count preps)
     :items-needed (vec item-deps)
     :item-count (count item-deps)
     :has-timed-effects (boolean (some prep/timed-prep? preps))
     :has-combat (boolean (some prep/combat-prep? preps))}))

(defn analyze-all-treasures
  "Analyze all treasures and their requirements."
  []
  (map analyze-treasure (all-treasures)))

(defn complexity-ranking
  "Rank treasures by complexity (number of preps needed)."
  []
  (->> (analyze-all-treasures)
       (sort-by :prep-count)
       (map #(select-keys % [:treasure :prep-count]))))

(defn print-dependency-tree
  "Print a visual dependency tree for a prep action."
  [prep-id & {:keys [indent] :or {indent 0}}]
  (let [prefix (apply str (repeat indent "  "))
        deps (prep-depends-on-prep prep-id)]
    (println (str prefix "- " (name prep-id)))
    (doseq [dep deps]
      (print-dependency-tree dep :indent (inc indent)))))

(defn print-treasure-analysis
  "Print analysis for a treasure."
  [treasure]
  (let [analysis (analyze-treasure treasure)]
    (println (str "=== " (name treasure) " ==="))
    (println (str "  Preps needed: " (count (:preps-needed analysis))))
    (println (str "  Items needed: " (count (:items-needed analysis))))
    (println (str "  Prep chain: " (vec (map name (:preps-needed analysis)))))
    (println (str "  Items: " (vec (map name (:items-needed analysis)))))
    (when (:has-timed-effects analysis)
      (println "  ⏱ Has timed effects"))
    (when (:has-combat analysis)
      (println "  ⚔ Requires combat"))))
