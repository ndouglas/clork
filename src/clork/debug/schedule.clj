(ns clork.debug.schedule
  "Debug commands for schedule inspection.

   $schedule <treasures>  - Preview schedule for treasures
   $schedule validate     - Validate a schedule
   $schedule easy         - Preview schedule for easy treasures
   $schedule all          - Preview schedule for all treasures"
  (:require [clork.utils :as utils]
            [clork.planner2.executor :as executor]
            [clork.planner2.schedule :as schedule]
            [clork.planner2.route :as route]
            [clork.planner2.deps :as deps]
            [clork.planner2.prep :as prep]
            [clojure.string :as str]))

;; =============================================================================
;; Treasure Sets
;; =============================================================================

(def easy-treasures
  "Treasures that don't require complex puzzles."
  [:egg :painting :bag-of-coins])

(def medium-treasures
  "Treasures requiring troll and dome flags."
  [:egg :painting :bag-of-coins :platinum-bar :ivory-torch :sceptre])

(def all-treasures
  "All treasures in the game."
  (vec (keys route/treasure-locations)))

;; =============================================================================
;; Schedule Preview
;; =============================================================================

(defn- format-entry
  "Format a schedule entry for display."
  [idx entry]
  (str (inc idx) ". "
       (case (:type entry)
         :move (str "MOVE to " (name (:to entry)))
         :prep (str "PREP " (name (:id entry))
                    (when-let [loc (prep/prep-location (:id entry))]
                      (str " at " (if (keyword? loc) (name loc) (str loc)))))
         :collect (str "COLLECT " (name (:treasure entry))
                       " at " (name (route/treasure-location (:treasure entry))))
         :deposit-all "DEPOSIT ALL treasures at living-room"
         :parallel-work (str "PARALLEL WORK: " (str/join ", " (map name (:treasures entry))))
         :atomic-sequence (str "ATOMIC: " (:name entry))
         (str (:type entry)))))

(defn preview-treasures
  "Preview schedule for given treasure keywords."
  [game-state treasure-kws]
  (let [sched (schedule/generate-schedule game-state treasure-kws)
        preps (deps/preps-for-treasures treasure-kws)]
    (str "=== Schedule Preview ===\n"
         "Treasures: " (str/join ", " (map name treasure-kws)) "\n"
         "Required preps: " (str/join ", " (map name preps)) "\n"
         "Schedule entries: " (count sched) "\n"
         "Estimated turns: " (schedule/schedule-turns sched) "\n"
         "\n"
         (str/join "\n" (map-indexed format-entry sched))
         "\n")))

;; =============================================================================
;; Schedule Validation
;; =============================================================================

(defn validate-treasures
  "Validate schedule for given treasures."
  [game-state treasure-kws]
  (let [sched (schedule/generate-schedule game-state treasure-kws)
        issues (atom [])]
    ;; Check for empty schedule
    (when (empty? sched)
      (swap! issues conj "ERROR: Schedule is empty!"))

    ;; Check for deposit-all
    (when-not (some #(= :deposit-all (:type %)) sched)
      (swap! issues conj "WARN: No deposit-all entry found"))

    ;; Check collect entries match treasures
    (let [collects (filter #(= :collect (:type %)) sched)]
      (when (not= (count collects) (count treasure-kws))
        (swap! issues conj
               (str "WARN: Collect entries (" (count collects)
                    ") != treasures (" (count treasure-kws) ")"))))

    ;; Check for unknown entry types
    (let [valid-types #{:move :prep :collect :deposit-all :parallel-work :atomic-sequence}
          unknown (remove #(valid-types (:type %)) sched)]
      (when (seq unknown)
        (swap! issues conj
               (str "WARN: Unknown entry types: " (set (map :type unknown))))))

    (if (empty? @issues)
      "Validation passed - no issues found!\n"
      (str "=== Validation Issues ===\n"
           (str/join "\n" @issues)
           "\n"))))

;; =============================================================================
;; Dependency Analysis
;; =============================================================================

(defn analyze-deps
  "Analyze dependencies for treasures."
  [treasure-kws]
  (let [preps (deps/preps-for-treasures treasure-kws)
        by-level (deps/prep-levels)
        combat-preps (filter prep/combat-prep? preps)
        timed-preps (filter prep/timed-prep? preps)]
    (str "=== Dependency Analysis ===\n"
         "Treasures: " (str/join ", " (map name treasure-kws)) "\n"
         "Total preps needed: " (count preps) "\n"
         "\nPreps in order:\n"
         (str/join "\n" (map #(str "  - " (name %)) preps))
         "\n"
         "\nCombat preps: " (if (seq combat-preps)
                              (str/join ", " (map name combat-preps))
                              "none")
         "\nTimed preps: " (if (seq timed-preps)
                             (str/join ", " (map name timed-preps))
                             "none")
         "\n")))

;; =============================================================================
;; Command Handlers
;; =============================================================================

(defn- parse-treasures
  "Parse treasure names from args."
  [args]
  (mapv keyword args))

(defn- get-init-game
  "Get the init-game function, breaking cyclic dependency."
  []
  (require 'clork.core)
  @(resolve 'clork.core/init-game))

(defn- cmd-schedule-preview
  "Preview schedule for specified treasures."
  [game-state args]
  (if (empty? args)
    (utils/tell game-state
                (str "Usage: $schedule <treasure1> [treasure2] ...\n"
                     "Or: $schedule easy | medium | all\n"
                     "\nAvailable treasures:\n"
                     (str/join "\n" (map #(str "  " (name %)) (sort all-treasures)))
                     "\n"))
    (let [treasures (case (first args)
                      "easy" easy-treasures
                      "medium" medium-treasures
                      "all" all-treasures
                      (parse-treasures args))
          ;; Use fresh game state for preview (lazy require to break cycle)
          init-game (get-init-game)
          fresh-gs (init-game)]
      (utils/tell game-state (preview-treasures fresh-gs treasures)))))

(defn- cmd-schedule-validate
  "Validate schedule."
  [game-state args]
  (let [treasures (if (empty? args)
                    easy-treasures
                    (case (first args)
                      "easy" easy-treasures
                      "medium" medium-treasures
                      "all" all-treasures
                      (parse-treasures args)))
        init-game (get-init-game)
        fresh-gs (init-game)]
    (utils/tell game-state (validate-treasures fresh-gs treasures))))

(defn- cmd-schedule-deps
  "Show dependency analysis."
  [game-state args]
  (let [treasures (if (empty? args)
                    easy-treasures
                    (case (first args)
                      "easy" easy-treasures
                      "medium" medium-treasures
                      "all" all-treasures
                      (parse-treasures args)))]
    (utils/tell game-state (analyze-deps treasures))))

(defn- cmd-schedule-route
  "Show route plan (TSP optimization)."
  [game-state args]
  (let [treasures (if (empty? args)
                    easy-treasures
                    (case (first args)
                      "easy" easy-treasures
                      "medium" medium-treasures
                      "all" all-treasures
                      (parse-treasures args)))
        init-game (get-init-game)
        fresh-gs (init-game)
        plan (route/plan-treasure-route fresh-gs treasures)]
    (utils/tell game-state
                (str "=== Route Plan ===\n"
                     "Requested: " (str/join ", " (map name treasures)) "\n"
                     "Reachable: " (str/join ", " (map name (:treasures plan))) "\n"
                     "Preps needed: " (str/join ", " (map name (:preps plan))) "\n"
                     "Estimated distance: " (:total-distance plan) " moves\n"
                     "\nOptimized treasure order:\n"
                     (str/join "\n" (map-indexed
                                     (fn [i t] (str "  " (inc i) ". " (name t)
                                                    " at " (name (route/treasure-location t))))
                                     (:treasures plan)))
                     "\n"))))

(defn cmd-schedule
  "Main $schedule command dispatcher."
  [game-state args]
  (if (empty? args)
    (-> game-state
        (utils/tell "Usage: $schedule <subcommand> [args]\n\n")
        (utils/tell "Subcommands:\n")
        (utils/tell "  <treasures>  - Preview schedule (e.g., $schedule egg painting)\n")
        (utils/tell "  easy         - Preview schedule for easy treasures\n")
        (utils/tell "  medium       - Preview schedule for medium treasures\n")
        (utils/tell "  all          - Preview schedule for all treasures\n")
        (utils/tell "  validate     - Validate schedule\n")
        (utils/tell "  deps         - Show dependency analysis\n")
        (utils/tell "  route        - Show optimized route\n"))
    (let [subcmd (first args)
          sub-args (rest args)]
      (case subcmd
        "validate" (cmd-schedule-validate game-state sub-args)
        "deps" (cmd-schedule-deps game-state sub-args)
        "route" (cmd-schedule-route game-state sub-args)
        "easy" (cmd-schedule-preview game-state ["easy"])
        "medium" (cmd-schedule-preview game-state ["medium"])
        "all" (cmd-schedule-preview game-state ["all"])
        ;; Default: treat as treasure list
        (cmd-schedule-preview game-state args)))))

;; =============================================================================
;; Exports
;; =============================================================================

(def subcommands
  "Subcommand definitions for help system."
  {:validate {:handler cmd-schedule-validate
              :help "Validate schedule for issues"}
   :deps {:handler cmd-schedule-deps
          :help "Show dependency analysis"}
   :route {:handler cmd-schedule-route
           :help "Show optimized route plan"}
   :easy {:handler (fn [gs _] (cmd-schedule-preview gs ["easy"]))
          :help "Preview easy treasure schedule"}
   :medium {:handler (fn [gs _] (cmd-schedule-preview gs ["medium"]))
            :help "Preview medium treasure schedule"}
   :all {:handler (fn [gs _] (cmd-schedule-preview gs ["all"]))
         :help "Preview all treasure schedule"}})
