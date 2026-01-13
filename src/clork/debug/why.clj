(ns clork.debug.why
  "Debug command for goal regression and causal reasoning.

   Commands:
   - $why <flag>         - Explain why a game flag is not set
   - $why held <object>  - Explain why object is not held
   - $why at <room>      - Explain why player is not at room
   - $why flag <obj> <f> - Explain why object doesn't have flag"
  (:require [clork.utils :as utils]
            [clork.intel.goals :as goals]
            [clojure.string :as str]))

;;; ---------------------------------------------------------------------------
;;; FORMATTING
;;; ---------------------------------------------------------------------------

(defn- format-precond-status
  "Format a precondition status line."
  [precond-analysis]
  (let [{:keys [precond status]} precond-analysis
        symbol (case status
                 :ok "✓"
                 :blocked "✗"
                 "?")]
    (str "    " symbol " " (pr-str precond))))

(defn- format-achiever
  "Format an achiever explanation."
  [achiever indent-level]
  (let [{:keys [affordance-id verb description preconditions status]} achiever
        indent (apply str (repeat indent-level "  "))]
    (str indent "→ " (name affordance-id) " (" (name verb) ")\n"
         indent "  " description "\n"
         indent "  Status: " (name status) "\n"
         indent "  Preconditions:\n"
         (str/join "\n"
                   (map #(str indent (format-precond-status %)) preconditions)))))

(defn- format-explanation-tree
  "Format a full explanation tree recursively."
  [explanation depth max-display-depth]
  (let [{:keys [status goal details achievable-via]} explanation
        indent (apply str (repeat depth "  "))]
    (str indent "Goal: " (pr-str goal) "\n"
         indent "Status: " (name status) "\n"
         indent "Details: " details "\n"
         (when (and achievable-via (not= status :satisfied) (< depth max-display-depth))
           (str indent "Achievable via:\n"
                (str/join "\n"
                          (for [achiever achievable-via]
                            (str (format-achiever achiever (inc depth))
                                 (when-let [blocking (:blocking achiever)]
                                   (str "\n" indent "  Blocking goals:\n"
                                        (str/join "\n"
                                                  (map #(format-explanation-tree % (+ depth 2) max-display-depth)
                                                       blocking))))))))))))

;;; ---------------------------------------------------------------------------
;;; $why COMMAND
;;; ---------------------------------------------------------------------------

(defn cmd-why
  "Handle $why command.

   Usage:
     $why <flag>           - Explain why game flag is not set
     $why held <object>    - Explain why object is not held
     $why at <room>        - Explain why not at room
     $why flag <obj> <f>   - Explain why object lacks flag
     $why roots <flag>     - Show root causes for flag not being set"
  [game-state args]
  (cond
    (empty? args)
    (-> game-state
        (utils/tell "Usage:\n")
        (utils/tell "  $why <flag>           - Explain why game flag is not set\n")
        (utils/tell "  $why held <object>    - Explain why object is not held\n")
        (utils/tell "  $why at <room>        - Explain why not at room\n")
        (utils/tell "  $why flag <obj> <f>   - Explain why object lacks flag\n")
        (utils/tell "  $why roots <flag>     - Show root causes for flag\n")
        (utils/tell "\nCommon flags:\n")
        (utils/tell "  troll-flag, cyclops-flag, magic-flag, lld-flag\n")
        (utils/tell "  xb, xc, loud-flag, rainbow-flag, dome-flag\n")
        (utils/tell "  rug-moved, trap-door-open, gates-open, gate-flag\n"))

    ;; $why held <object>
    (= (first args) "held")
    (if (< (count args) 2)
      (utils/tell game-state "Usage: $why held <object>\n")
      (let [obj-id (keyword (second args))
            explanation (goals/why-not-held? game-state obj-id)]
        (-> game-state
            (utils/tell (str "\n" (format-explanation-tree explanation 0 3) "\n")))))

    ;; $why at <room>
    (= (first args) "at")
    (if (< (count args) 2)
      (utils/tell game-state "Usage: $why at <room>\n")
      (let [room-id (keyword (second args))
            explanation (goals/why-cant-reach? game-state room-id)]
        (-> game-state
            (utils/tell (str "\n" (format-explanation-tree explanation 0 3) "\n")))))

    ;; $why flag <object> <flag>
    (= (first args) "flag")
    (if (< (count args) 3)
      (utils/tell game-state "Usage: $why flag <object> <flag>\n")
      (let [obj-id (keyword (second args))
            flag (keyword (nth args 2))
            goal {:type :object-flag :object obj-id :flag flag}
            explanation (goals/explain-goal game-state goal)]
        (-> game-state
            (utils/tell (str "\n" (format-explanation-tree explanation 0 3) "\n")))))

    ;; $why roots <flag>
    (= (first args) "roots")
    (if (< (count args) 2)
      (utils/tell game-state "Usage: $why roots <flag>\n")
      (let [flag (keyword (second args))
            root-causes (goals/find-root-causes game-state {:type :game-flag :flag flag})]
        (if (empty? root-causes)
          (utils/tell game-state (str "Flag " (name flag) " is either satisfied or has no known achievers.\n"))
          (-> game-state
              (utils/tell (str "\nRoot causes for " (name flag) " not being set:\n\n"))
              (as-> gs
                    (reduce (fn [gs {:keys [goal reason]}]
                              (-> gs
                                  (utils/tell (str "  • " (pr-str goal) "\n"))
                                  (utils/tell (str "    " reason "\n"))))
                            gs
                            root-causes))))))

    ;; Default: $why <flag> - assume it's a game flag
    :else
    (let [flag (keyword (first args))
          check (goals/check-goal game-state {:type :game-flag :flag flag})]
      (if (:satisfied check)
        (utils/tell game-state (str "Flag " (name flag) " is already set.\n"))
        (let [explanation (goals/why-not-flag? game-state flag)]
          (-> game-state
              (utils/tell (str "\n" (format-explanation-tree explanation 0 3) "\n"))))))))

;;; ---------------------------------------------------------------------------
;;; SUBCOMMANDS
;;; ---------------------------------------------------------------------------

(def subcommands
  "Subcommand definitions for help display."
  {"<flag>" "Explain why a game flag is not set (e.g., lld-flag, troll-flag)"
   "held <obj>" "Explain why an object is not held"
   "at <room>" "Explain why player is not at a room"
   "flag <obj> <f>" "Explain why object lacks a flag"
   "roots <flag>" "Show root causes for a blocked goal"})
