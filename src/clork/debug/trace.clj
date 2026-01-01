(ns clork.debug.trace
  "Trace system for runtime debugging visibility.

   Provides toggleable tracing for:
   - verbs: Log verb handler calls, action function invocations
   - parser: Log parser pipeline stages (snarfem, get-object, etc.)
   - actions: Log room/object action calls
   - daemons: Log daemon execution

   Usage:
     $trace on           - Enable all tracing
     $trace off          - Disable all tracing
     $trace parser       - Toggle parser tracing
     $trace verbs        - Toggle verb tracing
     $trace status       - Show what's enabled

   Trace output is prefixed with [TRACE:<category>] for easy filtering."
  (:require [clork.utils :as utils]
            [clojure.string :as str]))

;;; ---------------------------------------------------------------------------
;;; TRACE STATE ACCESS
;;; ---------------------------------------------------------------------------

(defn trace-enabled?
  "Check if a specific trace category is enabled."
  [game-state category]
  (get-in game-state [:trace category] false))

(defn any-trace-enabled?
  "Check if any tracing is enabled."
  [game-state]
  (some true? (vals (get game-state :trace {}))))

(defn enable-trace
  "Enable a trace category."
  [game-state category]
  (assoc-in game-state [:trace category] true))

(defn disable-trace
  "Disable a trace category."
  [game-state category]
  (assoc-in game-state [:trace category] false))

(defn enable-all-traces
  "Enable all trace categories."
  [game-state]
  (assoc game-state :trace {:verbs true
                            :parser true
                            :actions true
                            :daemons true
                            :thief true}))

(defn disable-all-traces
  "Disable all trace categories."
  [game-state]
  (assoc game-state :trace {:verbs false
                            :parser false
                            :actions false
                            :daemons false
                            :thief false}))

;;; ---------------------------------------------------------------------------
;;; TRACE OUTPUT
;;; ---------------------------------------------------------------------------

(defn trace-log
  "Log a trace message if the category is enabled.
   Returns game-state unchanged.

   Usage:
     (trace-log game-state :verb \"Action: :take, PRSO: [:lamp]\")
     (trace-log game-state :parser \"Lexer input: 'take lamp'\")"
  [game-state category message]
  (if (trace-enabled? game-state category)
    (utils/tell game-state (str "\n[TRACE:" (name category) "] " message))
    game-state))

(defn trace-verb
  "Log verb execution trace."
  [game-state action prso prsi]
  (trace-log game-state :verbs
             (str "Action: " action
                  (when prso (str ", PRSO: " prso))
                  (when prsi (str ", PRSI: " prsi)))))

(defn trace-verb-dispatch
  "Log verb handler dispatch."
  [game-state action handler-name multi? all-mode?]
  (trace-log game-state :verbs
             (str "Dispatch: " action " -> " handler-name
                  (when multi? " [MULTI]")
                  (when all-mode? " [ALL-MODE]"))))

(defn trace-verb-object
  "Log processing of individual object in multi-object loop."
  [game-state obj-id obj-name]
  (trace-log game-state :verbs
             (str "  Object: " obj-id " (" obj-name ")")))

(defn trace-action-call
  "Log object/room action function being called."
  [game-state obj-id verb]
  (trace-log game-state :verbs
             (str "  Calling action: " obj-id " for verb " verb)))

(defn trace-action-result
  "Log result of object/room action function."
  [game-state obj-id returned?]
  (trace-log game-state :verbs
             (str "  Action result: " obj-id " -> " (if returned? "handled" "nil (pass-through)"))))

(defn trace-parser
  "Log parser stage trace."
  [game-state stage message]
  (trace-log game-state :parser
             (str stage ": " message)))

(defn trace-parser-snarfem
  "Log snarfem processing a word."
  [game-state word word-type]
  (trace-log game-state :parser
             (str "snarfem: \"" word "\" -> " word-type)))

(defn trace-parser-getflags
  "Log getflags being set."
  [game-state flag-name flag-value]
  (trace-log game-state :parser
             (str "snarfem: set " flag-name " flag (getflags=" flag-value ")")))

(defn trace-parser-get-object
  "Log get-object search."
  [game-state here lit? contents]
  (trace-log game-state :parser
             (str "get-object: room=" here ", lit=" lit? ", contents=" contents)))

(defn trace-parser-matches
  "Log objects found by get-object."
  [game-state matches source]
  (trace-log game-state :parser
             (str "get-object: found " (count matches) " match(es) in " source ": " matches)))

(defn trace-parser-result
  "Log final parser result."
  [game-state prso prsi]
  (trace-log game-state :parser
             (str "result: PRSO=" prso ", PRSI=" prsi)))

(defn trace-action
  "Log room/object action trace."
  [game-state thing-id arg result]
  (trace-log game-state :actions
             (str (name thing-id) " action called with " arg
                  (when result (str " -> " result)))))

(defn trace-daemon
  "Log daemon execution trace."
  [game-state daemon-id message]
  (trace-log game-state :daemons
             (str (name daemon-id) ": " message)))

(defn trace-thief
  "Log thief behavior trace."
  [game-state message]
  (trace-log game-state :thief message))

;;; ---------------------------------------------------------------------------
;;; RETURN VALUE TRACING
;;; ---------------------------------------------------------------------------

(defn trace-return
  "Trace function return with key state info.
   Shows function name and key details about the returned state.

   Usage:
     (-> game-state
         (some-function)
         (trace-return :verbs \"some-function\"))"
  [game-state category fn-name & [extra-info]]
  (if (trace-enabled? game-state category)
    (let [room-count (count (:rooms game-state {}))
          obj-count (count (:objects game-state {}))
          here (:here game-state)
          valid? (and (some? game-state)
                      (pos? room-count)
                      (pos? obj-count))]
      (utils/tell game-state
                  (str "\n[TRACE:" (name category) "] <- " fn-name
                       " {rooms: " room-count
                       ", objects: " obj-count
                       ", here: " here
                       (when-not valid? " *** INVALID ***")
                       (when extra-info (str ", " extra-info))
                       "}")))
    game-state))

(defn trace-enter
  "Trace function entry with arguments.

   Usage:
     (-> game-state
         (trace-enter :verbs \"goto\" {:room-id room-id})
         (goto-impl room-id))"
  [game-state category fn-name args-map]
  (if (trace-enabled? game-state category)
    (let [args-str (str/join ", " (map (fn [[k v]] (str (name k) "=" v)) args-map))]
      (utils/tell game-state
                  (str "\n[TRACE:" (name category) "] -> " fn-name " {" args-str "}")))
    game-state))

(defmacro with-trace
  "Wrap a body with entry and return tracing.

   Usage:
     (with-trace game-state :verbs \"goto\" {:room room-id}
       (-> game-state
           (assoc :here room-id)
           (do-stuff)))"
  [game-state category fn-name args-map & body]
  `(let [gs# (trace-enter ~game-state ~category ~fn-name ~args-map)
         result# (do ~@body)]
     (trace-return result# ~category ~fn-name)))

;;; ---------------------------------------------------------------------------
;;; DEBUG COMMANDS
;;; ---------------------------------------------------------------------------

(def ^:private trace-categories [:verbs :parser :actions :daemons :thief])

(defn- cmd-trace-on
  "Enable all traces."
  [game-state]
  (-> game-state
      (enable-all-traces)
      (utils/tell "All tracing enabled.\n")))

(defn- cmd-trace-off
  "Disable all traces."
  [game-state]
  (-> game-state
      (disable-all-traces)
      (utils/tell "All tracing disabled.\n")))

(defn- cmd-trace-toggle
  "Toggle a specific trace category."
  [game-state category]
  (let [cat-key (keyword category)]
    (if (some #{cat-key} trace-categories)
      (if (trace-enabled? game-state cat-key)
        (-> game-state
            (disable-trace cat-key)
            (utils/tell (str (name cat-key) " tracing disabled.\n")))
        (-> game-state
            (enable-trace cat-key)
            (utils/tell (str (name cat-key) " tracing enabled.\n"))))
      (utils/tell game-state (str "Unknown trace category: " category "\n"
                                  "Valid categories: " (str/join ", " (map name trace-categories)) "\n")))))

(defn- cmd-trace-status
  "Show trace status."
  [game-state]
  (let [trace-state (get game-state :trace {})]
    (reduce (fn [gs cat]
              (utils/tell gs (str "  " (name cat) ": "
                                  (if (get trace-state cat false) "ON" "OFF") "\n")))
            (utils/tell game-state "Trace status:\n")
            trace-categories)))

(defn cmd-trace
  "Handle $trace command."
  [game-state args]
  (if (empty? args)
    (cmd-trace-status game-state)
    (case (first args)
      "on" (cmd-trace-on game-state)
      "off" (cmd-trace-off game-state)
      "status" (cmd-trace-status game-state)
      ;; Otherwise try to toggle a category
      (cmd-trace-toggle game-state (first args)))))

;; Subcommands for help system
(def subcommands
  {:on {:handler (fn [gs _] (cmd-trace-on gs))
        :help "Enable all tracing"}
   :off {:handler (fn [gs _] (cmd-trace-off gs))
         :help "Disable all tracing"}
   :verbs {:handler (fn [gs _] (cmd-trace-toggle gs "verbs"))
           :help "Toggle verb handler tracing"}
   :parser {:handler (fn [gs _] (cmd-trace-toggle gs "parser"))
            :help "Toggle parser stage tracing"}
   :actions {:handler (fn [gs _] (cmd-trace-toggle gs "actions"))
             :help "Toggle room/object action tracing"}
   :daemons {:handler (fn [gs _] (cmd-trace-toggle gs "daemons"))
             :help "Toggle daemon execution tracing"}
   :thief {:handler (fn [gs _] (cmd-trace-toggle gs "thief"))
           :help "Toggle thief behavior tracing"}
   :status {:handler (fn [gs _] (cmd-trace-status gs))
            :help "Show which traces are active"}})
