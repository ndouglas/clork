(ns clork.debug
  "Debug command system with $ prefix.

   All debug commands start with $ and are intercepted before normal parsing.
   This provides:
   - State inspection ($debug ...)
   - Parser debugging ($parser ...)
   - State manipulation ($goto, $purloin, etc.)
   - Undo/redo ($undo, $redo)
   - Tracing ($trace ...)
   - Daemon tracking ($daemon ...)
   - Test scenarios ($scenario ...)

   Commands are always available (no toggle required)."
  (:require [clork.utils :as utils]
            [clojure.string :as str]
            [clork.debug.state :as debug-state]
            [clork.debug.parser :as debug-parser]
            [clork.debug.manipulation :as debug-manip]
            [clork.debug.trace :as debug-trace]
            [clork.debug.daemon :as debug-daemon]
            [clork.debug.test :as debug-test]
            [clork.debug.thief :as debug-thief]
            [clork.debug.scenarios :as debug-scenarios]
            [clork.debug.inspect :as debug-inspect]
            [clork.debug.pathfind :as debug-pathfind]
            [clork.debug.plan :as debug-plan]
            [clork.undo :as undo]))

;;; ---------------------------------------------------------------------------
;;; COMMAND REGISTRY
;;; ---------------------------------------------------------------------------
;;; Commands are registered as maps with :handler and :help keys.
;;; Subcommands use nested maps under :subcommands.

(def ^:private commands (atom {}))

(defn register-command!
  "Register a debug command.

   cmd-name: keyword like :debug or :goto
   handler: (fn [game-state args] ...) returns updated game-state
   help: short help string
   subcommands: optional map of subcommand-name -> {:handler :help}"
  [cmd-name handler help & {:keys [subcommands]}]
  (swap! commands assoc cmd-name
         {:handler handler
          :help help
          :subcommands (or subcommands {})}))

;;; ---------------------------------------------------------------------------
;;; ARGUMENT PARSING
;;; ---------------------------------------------------------------------------

(defn- parse-args
  "Parse command line into [command & args].
   Input: '$debug state' -> ['debug' 'state']
   Input: '$goto living-room' -> ['goto' 'living-room']"
  [input]
  (let [trimmed (str/trim input)]
    (if (str/starts-with? trimmed "$")
      (let [without-dollar (subs trimmed 1)
            parts (str/split without-dollar #"\s+")]
        (if (empty? (first parts))
          nil
          parts))
      nil)))

(defn debug-command?
  "Returns true if input is a $ debug command."
  [input]
  (and (string? input)
       (str/starts-with? (str/trim input) "$")))

;;; ---------------------------------------------------------------------------
;;; COMMAND DISPATCH
;;; ---------------------------------------------------------------------------

(defn- unknown-command
  "Handle unknown command."
  [game-state cmd-name]
  (-> game-state
      (utils/tell (str "Unknown debug command: $" cmd-name "\n"))
      (utils/tell "Type $help for a list of commands.\n")))

(defn- show-command-help
  "Show help for a specific command."
  [game-state cmd-name cmd-info]
  (let [base-help (str "$" (name cmd-name) " - " (:help cmd-info) "\n")
        subcommands (:subcommands cmd-info)]
    (if (empty? subcommands)
      (utils/tell game-state base-help)
      (reduce (fn [gs [sub-name sub-info]]
                (utils/tell gs (str "  $" (name cmd-name) " " (name sub-name)
                                    " - " (:help sub-info) "\n")))
              (utils/tell game-state base-help)
              subcommands))))

(defn dispatch
  "Dispatch a debug command. Returns updated game-state.

   If input is not a $ command, returns nil (signal to continue normal parsing)."
  [game-state input]
  (if-let [parts (parse-args input)]
    (let [cmd-name (keyword (first parts))
          args (rest parts)
          cmd-info (get @commands cmd-name)]
      (if cmd-info
        (let [handler (:handler cmd-info)]
          (handler game-state args))
        (unknown-command game-state (first parts))))
    ;; Not a $ command - return nil to signal normal parsing
    nil))

;;; ---------------------------------------------------------------------------
;;; BUILT-IN COMMANDS: $help and $version
;;; ---------------------------------------------------------------------------

(defn- cmd-help
  "Handle $help command."
  [game-state args]
  (if (empty? args)
    ;; Show all commands
    (let [all-cmds @commands
          sorted-cmds (sort-by first all-cmds)]
      (reduce (fn [gs [cmd-name cmd-info]]
                (utils/tell gs (str "  $" (name cmd-name) " - " (:help cmd-info) "\n")))
              (utils/tell game-state "Debug Commands:\n")
              sorted-cmds))
    ;; Show help for specific command
    (let [cmd-name (keyword (first args))
          cmd-info (get @commands cmd-name)]
      (if cmd-info
        (show-command-help game-state cmd-name cmd-info)
        (utils/tell game-state (str "Unknown command: $" (first args) "\n"))))))

(defn- cmd-version
  "Handle $version command."
  [game-state _args]
  (-> game-state
      (utils/tell "Clork Debug System v0.1.0\n")
      (utils/tell "JLine: ")
      (utils/tell (if (try
                        (Class/forName "org.jline.reader.LineReader")
                        true
                        (catch ClassNotFoundException _ false))
                    "available\n"
                    "not available\n"))))

(defn- cmd-quit
  "Handle $quit command - exit the game immediately."
  [game-state _args]
  (-> game-state
      (utils/tell "Goodbye!\n")
      (assoc :quit true)))

;;; ---------------------------------------------------------------------------
;;; INITIALIZATION
;;; ---------------------------------------------------------------------------

(defn init-commands!
  "Initialize built-in debug commands. Called once at startup."
  []
  (register-command! :help cmd-help
                     "Show debug command help")
  (register-command! :version cmd-version
                     "Show debug system version")
  (register-command! :quit cmd-quit
                     "Exit the game immediately")
  (register-command! :debug debug-state/cmd-debug
                     "Inspect game state"
                     :subcommands debug-state/subcommands)
  (register-command! :parser debug-parser/cmd-parser
                     "Parser debugging"
                     :subcommands debug-parser/subcommands)
  ;; State manipulation commands
  (register-command! :goto debug-manip/cmd-goto
                     "Teleport to any room")
  (register-command! :purloin debug-manip/cmd-purloin
                     "Take any object (bypass checks)")
  (register-command! :move debug-manip/cmd-move
                     "Move object to location")
  (register-command! :flag debug-manip/cmd-flag
                     "Set a flag on object/room")
  (register-command! :unflag debug-manip/cmd-unflag
                     "Clear a flag on object/room")
  (register-command! :frotz debug-manip/cmd-frotz
                     "Make object give light")
  (register-command! :where debug-manip/cmd-where
                     "Find where an object is located")
  ;; Undo commands
  (register-command! :undo undo/cmd-undo
                     "Undo last command(s)")
  (register-command! :redo undo/cmd-redo
                     "Redo undone command(s)")
  (register-command! :history undo/cmd-history
                     "Show command history")
  ;; Trace commands
  (register-command! :trace debug-trace/cmd-trace
                     "Runtime tracing control"
                     :subcommands debug-trace/subcommands)
  ;; Daemon commands
  (register-command! :daemon debug-daemon/cmd-daemon
                     "Daemon inspection and control"
                     :subcommands debug-daemon/subcommands)
  ;; Test commands
  (register-command! :test debug-test/cmd-test
                     "Test object actions and behaviors"
                     :subcommands debug-test/subcommands)
  ;; Thief observation commands
  (register-command! :thief debug-thief/cmd-thief
                     "Thief observation and control"
                     :subcommands debug-thief/subcommands)
  ;; Quick state inspection
  (register-command! :state debug-state/cmd-state
                     "Quick state inspection ($state <key>)")
  ;; Deep inspection with flag analysis
  (register-command! :inspect debug-inspect/cmd-inspect
                     "Deep object/room inspection with flag analysis")
  ;; Test scenarios
  (register-command! :scenario debug-scenarios/cmd-scenario
                     "Load test scenarios (equipped player, troll dead, etc.)"
                     :subcommands debug-scenarios/subcommands)
  ;; Pathfinding commands
  (register-command! :path debug-pathfind/cmd-path
                     "Find shortest path between rooms")
  (register-command! :path! debug-pathfind/cmd-path-strict
                     "Find path using ONLY unconditional exits")
  (register-command! :reachable debug-pathfind/cmd-reachable
                     "Show all rooms reachable from a location")
  (register-command! :route debug-pathfind/cmd-route
                     "Plan optimal route through multiple rooms")
  ;; Speedrun planning commands
  (register-command! :plan debug-plan/cmd-plan
                     "Speedrun planning (treasure, flag, kill-thief, win)"
                     :subcommands debug-plan/subcommands)
  (register-command! :run debug-plan/cmd-run
                     "Execute speedrun (with adaptive combat)"
                     :subcommands debug-plan/run-subcommands))

;; Auto-initialize on namespace load
(init-commands!)
