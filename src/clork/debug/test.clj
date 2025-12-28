(ns clork.debug.test
  "Test commands for debugging object behaviors.

   Commands:
   - $test action :object-id :verb - Call an object's action function directly"
  (:require [clork.utils :as utils]
            [clork.game-state :as gs]
            [clork.parser.state :as parser-state]
            [clojure.string :as str]))

;;; ---------------------------------------------------------------------------
;;; HELPERS
;;; ---------------------------------------------------------------------------

(defn- format-keyword [kw]
  (if (keyword? kw) (name kw) (str kw)))

(defn- tell-line [game-state label value]
  (utils/tell game-state (str "  " label ": " value "\n")))

;;; ---------------------------------------------------------------------------
;;; $test action :object-id :verb
;;; ---------------------------------------------------------------------------

(defn cmd-test-action
  "Test an object's action function directly.

   Usage: $test action :mailbox :take
   Usage: $test action :rug :move

   This calls the object's :action function with a mock parser state
   where :prsa is set to the given verb and :prso is set to the object.
   Useful for testing whether an object's action handler fires correctly."
  [game-state args]
  (cond
    (< (count args) 2)
    (utils/tell game-state
                (str "Usage: $test action <object-id> <verb>\n"
                     "Example: $test action :mailbox :take\n"
                     "         $test action rug move\n"))

    :else
    (let [;; Parse object-id and verb, handling with or without leading colon
          obj-str (first args)
          obj-id (if (str/starts-with? obj-str ":")
                   (keyword (subs obj-str 1))
                   (keyword obj-str))
          verb-str (second args)
          verb (if (str/starts-with? verb-str ":")
                 (keyword (subs verb-str 1))
                 (keyword verb-str))
          obj (gs/get-thing game-state obj-id)]
      (cond
        ;; Object doesn't exist
        (nil? obj)
        (utils/tell game-state (str "Object not found: " obj-id "\n"
                                    "Use $debug things to list all objects.\n"))

        ;; Object has no action function
        (nil? (:action obj))
        (utils/tell game-state (str "Object :" (name obj-id)
                                    " has no action function.\n"))

        ;; Run the action!
        :else
        (let [;; Set up game state with mock parser result
              gs-with-parser (-> game-state
                                 (assoc-in [:parser :prsa] verb)
                                 (assoc-in [:parser :prso] [obj-id])
                                 (assoc-in [:parser :prsi] nil))
              action-fn (:action obj)
              _ (utils/tell game-state
                            (str "=== Testing action on :" (name obj-id)
                                 " with verb :" (name verb) " ===\n"))
              result (try
                       (action-fn gs-with-parser)
                       (catch Exception e
                         {:error e}))]
          (cond
            ;; Exception occurred
            (and (map? result) (:error result))
            (utils/tell game-state
                        (str "ACTION THREW EXCEPTION:\n"
                             "  " (.getMessage (:error result)) "\n"))

            ;; Action returned nil (did not handle)
            (nil? result)
            (utils/tell game-state
                        (str "Action returned: nil (did not handle verb)\n"
                             "This means the default handler will be used.\n"))

            ;; Action returned game-state (handled)
            :else
            (do
              ;; The action may have output text via utils/tell
              ;; Extract it from the :output key
              (let [original-output (:output game-state "")
                    new-output (:output result "")
                    action-output (if (str/starts-with? new-output original-output)
                                    (subs new-output (count original-output))
                                    new-output)]
                (-> game-state
                    (utils/tell (str "Action returned: game-state (handled)\n"
                                     "Action output: " (pr-str (str/trim action-output)) "\n")))))))))))

;;; ---------------------------------------------------------------------------
;;; $test flags :object-id
;;; ---------------------------------------------------------------------------

(defn cmd-test-flags
  "Show all flags set on an object.

   Usage: $test flags :mailbox"
  [game-state args]
  (if (empty? args)
    (utils/tell game-state "Usage: $test flags <object-id>\n")
    (let [obj-str (first args)
          obj-id (if (str/starts-with? obj-str ":")
                   (keyword (subs obj-str 1))
                   (keyword obj-str))
          obj (gs/get-thing game-state obj-id)]
      (if (nil? obj)
        (utils/tell game-state (str "Object not found: " obj-id "\n"))
        (let [flags (or (:flags obj) #{})]
          (-> game-state
              (utils/tell (str "Flags on :" (name obj-id) ":\n"))
              ((fn [gs]
                 (if (empty? flags)
                   (utils/tell gs "  (no flags)\n")
                   (reduce (fn [g f]
                             (utils/tell g (str "  :" (name f) "\n")))
                           gs
                           (sort-by name flags)))))))))))

;;; ---------------------------------------------------------------------------
;;; MAIN TEST DISPATCHER
;;; ---------------------------------------------------------------------------

(def subcommands
  {:action {:handler cmd-test-action :help "Test object's action function ($test action :obj :verb)"}
   :flags  {:handler cmd-test-flags  :help "Show flags on an object ($test flags :obj)"}})

(defn cmd-test
  "Main $test command dispatcher."
  [game-state args]
  (if (empty? args)
    (reduce (fn [gs [name info]]
              (utils/tell gs (str "  $test " (format-keyword name) " - " (:help info) "\n")))
            (utils/tell game-state "$test subcommands:\n")
            (sort-by first subcommands))
    (let [subcmd (keyword (first args))
          sub-args (rest args)
          sub-info (get subcommands subcmd)]
      (if sub-info
        ((:handler sub-info) game-state sub-args)
        (utils/tell game-state (str "Unknown subcommand: " (first args) "\nType $test for list.\n"))))))
