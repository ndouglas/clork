(ns clork.script
  "Script mode support for CI testing and transcript validation.

   Provides exit codes and configuration for running Clork non-interactively
   with controlled failure conditions.")

;;; ---------------------------------------------------------------------------
;;; EXIT CODES
;;; ---------------------------------------------------------------------------

(def exit-codes
  "Exit codes for script mode.

   0 - Success (normal quit or game won)
   1 - Player died
   2 - Parser error (bad command, can't go that way, etc.)
   3 - Uncaught exception
   4 - Max turns exceeded
   5 - Generic error"
  {:success          0
   :death            1
   :parser-error     2
   :exception        3
   :max-turns        4
   :error            5})

;;; ---------------------------------------------------------------------------
;;; CONFIGURATION
;;; ---------------------------------------------------------------------------

(def default-config
  "Default script mode configuration (all checks disabled)."
  {:script-mode?       false   ; Whether running in script mode at all
   :fail-on-death      false   ; Exit non-zero if player dies
   :fail-on-parser-error false ; Exit non-zero on parser errors
   :strict             false   ; Exit non-zero on ANY error (death or parser)
   :max-turns          nil     ; Maximum turns before forced exit (nil = unlimited)
   :quiet              false   ; Suppress game output
   :input-file         nil     ; File to read commands from (nil = stdin)
   :seed               nil     ; Random seed for reproducibility (nil = random)
   :ml-mode            false   ; Run in ML training mode (JSON line protocol)
   :ml-rewards         false}) ; Include reward signals in ML mode

(defn parse-args
  "Parse command-line arguments into a script config map.

   Supported flags:
     --strict, -s           Exit non-zero on any error
     --fail-on-death        Exit non-zero if player dies
     --fail-on-parser-error Exit non-zero on parser errors
     --max-turns N          Limit to N turns
     --quiet, -q            Suppress game output
     --input FILE, -i FILE  Read commands from FILE
     --seed N               Set random seed for reproducibility
     --ml                   Run in ML training mode (JSON line protocol)
     --ml-rewards           Include reward signals in ML mode output"
  [args]
  (loop [args args
         config default-config]
    (if (empty? args)
      ;; If any failure flag is set, we're in script mode
      (assoc config :script-mode?
             (boolean (or (:strict config)
                          (:fail-on-death config)
                          (:fail-on-parser-error config)
                          (:max-turns config)
                          (:input-file config))))
      (let [[arg & rest-args] args]
        (case arg
          ("--strict" "-s")
          (recur rest-args (assoc config :strict true
                                  :fail-on-death true
                                  :fail-on-parser-error true))

          "--fail-on-death"
          (recur rest-args (assoc config :fail-on-death true))

          "--fail-on-parser-error"
          (recur rest-args (assoc config :fail-on-parser-error true))

          ("--quiet" "-q")
          (recur rest-args (assoc config :quiet true))

          ("--max-turns" "-m")
          (if-let [n (first rest-args)]
            (recur (rest rest-args)
                   (assoc config :max-turns (Integer/parseInt n)))
            (do (binding [*out* *err*]
                  (println "Error: --max-turns requires a number"))
                (assoc config :error "Missing argument for --max-turns")))

          ("--input" "-i")
          (if-let [file (first rest-args)]
            (recur (rest rest-args) (assoc config :input-file file))
            (do (binding [*out* *err*]
                  (println "Error: --input requires a filename"))
                (assoc config :error "Missing argument for --input")))

          "--seed"
          (if-let [n (first rest-args)]
            (recur (rest rest-args)
                   (assoc config :seed (Long/parseLong n)))
            (do (binding [*out* *err*]
                  (println "Error: --seed requires a number"))
                (assoc config :error "Missing argument for --seed")))

          "--ml"
          (recur rest-args (assoc config :ml-mode true))

          "--ml-rewards"
          (recur rest-args (assoc config :ml-rewards true))

          ;; Unknown argument - could be game-specific, ignore for now
          (recur rest-args config))))))

;;; ---------------------------------------------------------------------------
;;; EXIT CODE DETERMINATION
;;; ---------------------------------------------------------------------------

(defn determine-exit-code
  "Determine the exit code based on final game state and script config.

   Returns an exit code from exit-codes map."
  [game-state config]
  (cond
    ;; Config parse error
    (:error config)
    (:error exit-codes)

    ;; Check max turns exceeded (via flag set by main-loop)
    (:max-turns-exceeded game-state)
    (:max-turns exit-codes)

    ;; Check death condition
    (and (:fail-on-death config)
         (pos? (:deaths game-state 0)))
    (:death exit-codes)

    ;; Check parser errors
    (and (:fail-on-parser-error config)
         (pos? (:parser-error-count game-state 0)))
    (:parser-error exit-codes)

    ;; Success
    :else
    (:success exit-codes)))

;;; ---------------------------------------------------------------------------
;;; OUTPUT CONTROL
;;; ---------------------------------------------------------------------------

(defn wrap-tell-for-quiet
  "Returns a wrapped tell function that respects quiet mode."
  [original-tell quiet?]
  (if quiet?
    (fn [game-state message]
      ;; Still update game-state but don't print
      (update game-state :output-buffer (fnil str "") message))
    original-tell))
