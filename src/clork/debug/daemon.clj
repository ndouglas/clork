(ns clork.debug.daemon
  "Debug commands for daemon inspection and control.

   Provides:
   - $daemon list: Show all daemons with status
   - $daemon status <name>: Detailed daemon info
   - $daemon timeline [n]: Predict upcoming daemon activity
   - $daemon enable <name>: Enable a daemon
   - $daemon disable <name>: Disable a daemon
   - $daemon tick <name> <n>: Set daemon tick counter
   - $daemon history [name]: Show daemon execution history"
  (:require [clork.utils :as utils]
            [clork.daemon :as daemon]
            [clojure.string :as str]))

;;; ---------------------------------------------------------------------------
;;; HELPER FUNCTIONS
;;; ---------------------------------------------------------------------------

(defn- format-tick
  "Format tick value for display."
  [tick]
  (cond
    (= tick -1) "continuous"
    (= tick 0) "this turn"
    (> tick 0) (str "in " tick " turn" (when (> tick 1) "s"))
    :else "unknown"))

(defn- daemon-exists?
  "Check if daemon exists in game state."
  [game-state daemon-id]
  (contains? (:daemons game-state) daemon-id))

(defn- parse-daemon-id
  "Parse daemon id from string argument."
  [arg]
  (keyword arg))

;;; ---------------------------------------------------------------------------
;;; $daemon list
;;; ---------------------------------------------------------------------------

(defn- cmd-daemon-list
  "List all daemons with their status."
  [game-state]
  (let [daemons (:daemons game-state)]
    (if (empty? daemons)
      (utils/tell game-state "No daemons registered.\n")
      (reduce (fn [gs [id daemon]]
                (utils/tell gs (str "  " (name id) ": "
                                    (if (:enabled daemon) "ENABLED" "disabled")
                                    " [" (format-tick (:tick daemon)) "]"
                                    (when-not (:handler daemon) " (no handler)")
                                    "\n")))
              (utils/tell game-state "Registered daemons:\n")
              (sort-by first daemons)))))

;;; ---------------------------------------------------------------------------
;;; $daemon status <name>
;;; ---------------------------------------------------------------------------

(defn- cmd-daemon-status
  "Show detailed status of a specific daemon."
  [game-state daemon-id]
  (if-let [daemon (daemon/get-daemon game-state daemon-id)]
    (-> game-state
        (utils/tell (str "Daemon: " (name daemon-id) "\n"))
        (utils/tell (str "  Status: " (if (:enabled daemon) "ENABLED" "disabled") "\n"))
        (utils/tell (str "  Tick: " (:tick daemon) " (" (format-tick (:tick daemon)) ")\n"))
        (utils/tell (str "  Handler: " (if (:handler daemon) "registered" "NONE") "\n"))
        (utils/tell (str "  Data: " (pr-str (:data daemon {})) "\n")))
    (utils/tell game-state (str "Unknown daemon: " (name daemon-id) "\n"))))

;;; ---------------------------------------------------------------------------
;;; $daemon timeline [n]
;;; ---------------------------------------------------------------------------

(defn- predict-timeline
  "Predict daemon activity for next n moves."
  [game-state n]
  (let [daemons (:daemons game-state)
        current-turn (:moves game-state 0)]
    (->> daemons
         (filter (fn [[_ d]] (and (:enabled d) (:handler d))))
         (mapcat (fn [[id d]]
                   (let [tick (:tick d)]
                     (cond
                       ;; Continuous daemon - runs every turn
                       (= tick -1)
                       (for [t (range n)]
                         {:turn (+ current-turn t 1)
                          :daemon id
                          :type :continuous})
                       ;; Countdown daemon
                       (>= tick 0)
                       (when (<= tick n)
                         [{:turn (+ current-turn (max 1 tick))
                           :daemon id
                           :type :countdown}])
                       :else nil))))
         (sort-by :turn)
         (take (* n 3))))) ; Limit output

(defn- cmd-daemon-timeline
  "Show predicted daemon activity for next n moves."
  [game-state n]
  (let [timeline (predict-timeline game-state n)
        current-turn (:moves game-state 0)]
    (if (empty? timeline)
      (utils/tell game-state "No daemon activity predicted.\n")
      (reduce (fn [gs {:keys [turn daemon type]}]
                (utils/tell gs (str "  Turn " turn ": " (name daemon)
                                    " (" (name type) ")\n")))
              (utils/tell game-state (str "Daemon timeline (next " n " turns from turn " current-turn "):\n"))
              timeline))))

;;; ---------------------------------------------------------------------------
;;; $daemon enable/disable <name>
;;; ---------------------------------------------------------------------------

(defn- cmd-daemon-enable
  "Enable a daemon."
  [game-state daemon-id]
  (if (daemon-exists? game-state daemon-id)
    (-> game-state
        (daemon/enable daemon-id)
        (utils/tell (str "Daemon " (name daemon-id) " enabled.\n")))
    (utils/tell game-state (str "Unknown daemon: " (name daemon-id) "\n"))))

(defn- cmd-daemon-disable
  "Disable a daemon."
  [game-state daemon-id]
  (if (daemon-exists? game-state daemon-id)
    (-> game-state
        (daemon/disable daemon-id)
        (utils/tell (str "Daemon " (name daemon-id) " disabled.\n")))
    (utils/tell game-state (str "Unknown daemon: " (name daemon-id) "\n"))))

;;; ---------------------------------------------------------------------------
;;; $daemon tick <name> <n>
;;; ---------------------------------------------------------------------------

(defn- cmd-daemon-tick
  "Set daemon tick counter."
  [game-state daemon-id tick-value]
  (if (daemon-exists? game-state daemon-id)
    (-> game-state
        (daemon/queue daemon-id tick-value)
        (utils/tell (str "Daemon " (name daemon-id) " tick set to " tick-value
                         " (" (format-tick tick-value) ").\n")))
    (utils/tell game-state (str "Unknown daemon: " (name daemon-id) "\n"))))

;;; ---------------------------------------------------------------------------
;;; $daemon history [name]
;;; ---------------------------------------------------------------------------

(defn- cmd-daemon-history
  "Show daemon execution history."
  [game-state daemon-id n]
  (let [history (daemon/get-daemon-history game-state daemon-id n)]
    (if (empty? history)
      (utils/tell game-state (str "No history"
                                  (when daemon-id (str " for " (name daemon-id)))
                                  ".\n"))
      (reduce (fn [gs {:keys [turn daemon event]}]
                (utils/tell gs (str "  [" turn "] " (name daemon) ": " (name event) "\n")))
              (utils/tell game-state (str "Daemon history"
                                          (when daemon-id (str " for " (name daemon-id)))
                                          ":\n"))
              history))))

;;; ---------------------------------------------------------------------------
;;; MAIN COMMAND DISPATCH
;;; ---------------------------------------------------------------------------

(defn cmd-daemon
  "Handle $daemon command."
  [game-state args]
  (if (empty? args)
    (cmd-daemon-list game-state)
    (let [subcmd (first args)
          rest-args (rest args)]
      (case subcmd
        "list" (cmd-daemon-list game-state)

        "status" (if (empty? rest-args)
                   (utils/tell game-state "Usage: $daemon status <name>\n")
                   (cmd-daemon-status game-state (parse-daemon-id (first rest-args))))

        "timeline" (let [n (if (empty? rest-args) 10 (Integer/parseInt (first rest-args)))]
                     (cmd-daemon-timeline game-state n))

        "enable" (if (empty? rest-args)
                   (utils/tell game-state "Usage: $daemon enable <name>\n")
                   (cmd-daemon-enable game-state (parse-daemon-id (first rest-args))))

        "disable" (if (empty? rest-args)
                    (utils/tell game-state "Usage: $daemon disable <name>\n")
                    (cmd-daemon-disable game-state (parse-daemon-id (first rest-args))))

        "tick" (if (< (count rest-args) 2)
                 (utils/tell game-state "Usage: $daemon tick <name> <value>\n")
                 (cmd-daemon-tick game-state
                                  (parse-daemon-id (first rest-args))
                                  (Integer/parseInt (second rest-args))))

        "history" (let [daemon-id (when (seq rest-args) (parse-daemon-id (first rest-args)))
                        n (if (and (seq rest-args) (second rest-args))
                            (Integer/parseInt (second rest-args))
                            20)]
                    (cmd-daemon-history game-state daemon-id n))

        ;; Unknown subcommand
        (utils/tell game-state (str "Unknown subcommand: " subcmd "\n"
                                    "Valid subcommands: list, status, timeline, enable, disable, tick, history\n"))))))

;; Subcommands for help system
(def subcommands
  {:list {:handler (fn [gs _] (cmd-daemon-list gs))
          :help "Show all daemons with status"}
   :status {:handler (fn [gs args] (if (empty? args)
                                     (utils/tell gs "Usage: $daemon status <name>\n")
                                     (cmd-daemon-status gs (parse-daemon-id (first args)))))
            :help "Detailed daemon info"}
   :timeline {:handler (fn [gs args] (cmd-daemon-timeline gs (if (empty? args) 10 (Integer/parseInt (first args)))))
              :help "Predict upcoming daemon activity"}
   :enable {:handler (fn [gs args] (if (empty? args)
                                     (utils/tell gs "Usage: $daemon enable <name>\n")
                                     (cmd-daemon-enable gs (parse-daemon-id (first args)))))
            :help "Enable a daemon"}
   :disable {:handler (fn [gs args] (if (empty? args)
                                      (utils/tell gs "Usage: $daemon disable <name>\n")
                                      (cmd-daemon-disable gs (parse-daemon-id (first args)))))
             :help "Disable a daemon"}
   :tick {:handler (fn [gs args] (if (< (count args) 2)
                                   (utils/tell gs "Usage: $daemon tick <name> <value>\n")
                                   (cmd-daemon-tick gs (parse-daemon-id (first args)) (Integer/parseInt (second args)))))
          :help "Set daemon tick counter"}
   :history {:handler (fn [gs args] (let [daemon-id (when (seq args) (parse-daemon-id (first args)))
                                          n (if (and (seq args) (second args)) (Integer/parseInt (second args)) 20)]
                                      (cmd-daemon-history gs daemon-id n)))
             :help "Show daemon execution history"}})
