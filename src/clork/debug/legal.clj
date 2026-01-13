(ns clork.debug.legal
  "Debug command for displaying legal actions from the affordance registry.

   Commands:
   - $legal           - Show all legal actions in current state
   - $legal summary   - Show summary breakdown by category
   - $legal <verb>    - Show legal actions for a specific verb
   - $legal check <id> - Check if a specific affordance is applicable"
  (:require [clork.utils :as utils]
            [clork.intel.affordances :as aff]
            [clojure.string :as str]))

;;; ---------------------------------------------------------------------------
;;; FORMATTING HELPERS
;;; ---------------------------------------------------------------------------

(defn- format-action
  "Format a single action for display."
  [action]
  (let [{:keys [verb direct-object indirect-object]} action]
    (cond
      (and direct-object indirect-object)
      (str (name verb) " " (name direct-object) " " (name indirect-object))

      direct-object
      (str (name verb) " " (name direct-object))

      :else
      (name verb))))

(defn- format-action-list
  "Format a list of actions for display."
  [actions]
  (if (empty? actions)
    "  (none)\n"
    (str/join "\n" (map #(str "  " (format-action %)) actions))))

;;; ---------------------------------------------------------------------------
;;; $legal
;;; ---------------------------------------------------------------------------

(defn cmd-legal
  "Handle $legal command.

   Usage:
     $legal           - Show all legal actions
     $legal summary   - Show summary breakdown
     $legal <verb>    - Show actions for specific verb
     $legal check <id> - Check if affordance is applicable"
  [game-state args]
  (cond
    ;; No args: show all legal actions
    (empty? args)
    (let [actions (aff/legal-actions game-state)
          grouped (group-by :verb actions)]
      (-> game-state
          (utils/tell (str "Legal actions (" (count actions) " total):\n\n"))
          (as-> gs
                (reduce (fn [gs [verb verb-actions]]
                          (-> gs
                              (utils/tell (str (name verb) " (" (count verb-actions) "):\n"))
                              (utils/tell (format-action-list verb-actions))
                              (utils/tell "\n")))
                        gs
                        (sort-by (comp name first) grouped)))))

    ;; "summary" subcommand
    (= (first args) "summary")
    (let [explained (aff/explain-legal-actions game-state)
          summary (:summary explained)]
      (-> game-state
          (utils/tell (str "Legal actions summary (" (:count explained) " total):\n\n"))
          (utils/tell (str "  Movement:  " (:movement summary) "\n"))
          (utils/tell (str "  Take:      " (:take summary) "\n"))
          (utils/tell (str "  Drop:      " (:drop summary) "\n"))
          (utils/tell (str "  Open:      " (:open summary) "\n"))
          (utils/tell (str "  Close:     " (:close summary) "\n"))
          (utils/tell (str "  Lamp-on:   " (:lamp-on summary) "\n"))
          (utils/tell (str "  Lamp-off:  " (:lamp-off summary) "\n"))
          (utils/tell (str "  Read:      " (:read summary) "\n"))
          (utils/tell (str "  Attack:    " (:attack summary) "\n"))
          (utils/tell (str "  Put:       " (:put summary) "\n"))
          (utils/tell (str "  Special:   " (:special summary) "\n"))))

    ;; "check" subcommand
    (= (first args) "check")
    (if (< (count args) 2)
      (utils/tell game-state "Usage: $legal check <affordance-id>\n")
      (let [aff-id (keyword (second args))
            result (aff/check-affordance game-state aff-id)]
        (if (:error result)
          (utils/tell game-state (str "Error: " (:error result) "\n"))
          (if (:applicable result)
            (utils/tell game-state (str "Affordance " (name aff-id) " is APPLICABLE\n"))
            (-> game-state
                (utils/tell (str "Affordance " (name aff-id) " is NOT applicable\n"))
                (utils/tell "Failed preconditions:\n")
                (as-> gs
                      (reduce (fn [gs precond]
                                (utils/tell gs (str "  - " (pr-str precond) "\n")))
                              gs
                              (:failed-preconds result))))))))

    ;; "achievers" subcommand - find what can achieve a goal
    (= (first args) "achievers")
    (if (< (count args) 2)
      (utils/tell game-state "Usage: $legal achievers <flag-name>\n")
      (let [flag (keyword (second args))
            achievers (aff/achievers-of {:type :game-flag :flag flag})]
        (if (empty? achievers)
          (utils/tell game-state (str "No affordances achieve flag: " (name flag) "\n"))
          (-> game-state
              (utils/tell (str "Affordances that set " (name flag) ":\n"))
              (as-> gs
                    (reduce (fn [gs ach]
                              (-> gs
                                  (utils/tell (str "  " (name (:affordance-id ach)) "\n"))
                                  (utils/tell (str "    via: " (pr-str (:via-effect ach)) "\n"))))
                            gs
                            achievers))))))

    ;; "list" subcommand - list all affordance IDs
    (= (first args) "list")
    (let [ids (aff/list-affordances)
          grouped (group-by #(keyword (namespace %)) ids)]
      (-> game-state
          (utils/tell (str "All affordances (" (count ids) " total):\n\n"))
          (utils/tell (str/join ", " (map name (sort ids))))
          (utils/tell "\n")))

    ;; Filter by verb
    :else
    (let [verb (keyword (first args))
          actions (aff/legal-actions game-state)
          filtered (filter #(= verb (:verb %)) actions)]
      (if (empty? filtered)
        (utils/tell game-state (str "No legal actions for verb: " (name verb) "\n"))
        (-> game-state
            (utils/tell (str (name verb) " actions (" (count filtered) "):\n"))
            (utils/tell (format-action-list filtered))
            (utils/tell "\n"))))))

;;; ---------------------------------------------------------------------------
;;; SUBCOMMANDS
;;; ---------------------------------------------------------------------------

(def subcommands
  "Subcommand definitions for help display."
  {"" "Show all legal actions in current state"
   "summary" "Show summary breakdown by category"
   "<verb>" "Show legal actions for a specific verb (e.g., take, drop)"
   "check <id>" "Check if a specific affordance is applicable"
   "achievers <flag>" "Find affordances that can set a game flag"
   "list" "List all affordance IDs"})
