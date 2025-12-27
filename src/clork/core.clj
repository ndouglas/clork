(ns clork.core
  (:gen-class)
  (:require [clork.game-state :as game-state]
            [clork.verbs :as verbs]
            [clork.utils :as utils]
            [clork.rooms :as rooms]
            [clork.objects :as objects]
            [clork.verbs-look :as verbs-look]
            [clork.main-loop :as main-loop]
            [clork.readline :as readline]
            [clork.script :as script]
            [clork.random :as random]
            [clojure.java.io :as io]))

;; Re-export the essential API for creating and running games
(def initial-game-state game-state/initial-game-state)

(defn initial-version
  "Print out the version information when starting the game"
  [game-state]
  (if (game-state/set-here-flag? game-state :touch)
    game-state
    (-> game-state
        (verbs/v-version)
        (utils/crlf))))

(defn go
  "The GO routine."
  ([game-state]
   (go game-state nil))
  ([game-state config]
   (-> game-state
       (assoc :script-config config)
       (game-state/add-rooms [rooms/west-of-house])
       (game-state/add-objects [objects/adventurer,
                                objects/mailbox,
                                objects/leaflet])
       (utils/this-is-it :mailbox)
       (initial-version)
       (game-state/set-here-flag :lit)
       (verbs-look/v-look)
       (main-loop/main-loop))))

(defn print-usage
  "Print usage information."
  []
  (println "Usage: clork [options]")
  (println "")
  (println "Options:")
  (println "  --strict, -s           Exit non-zero on any error (death or parser)")
  (println "  --fail-on-death        Exit non-zero if player dies")
  (println "  --fail-on-parser-error Exit non-zero on parser errors")
  (println "  --max-turns N, -m N    Limit game to N turns")
  (println "  --quiet, -q            Suppress game output (for CI)")
  (println "  --input FILE, -i FILE  Read commands from FILE")
  (println "  --seed N               Set random seed for reproducibility")
  (println "  --help, -h             Show this help message")
  (println "")
  (println "Exit codes:")
  (println "  0 - Success (normal quit or game won)")
  (println "  1 - Player died")
  (println "  2 - Parser error (bad command)")
  (println "  3 - Uncaught exception")
  (println "  4 - Max turns exceeded")
  (println "  5 - Generic error"))

(defn -main
  "Main function for CLORK.

   Supports script mode for CI testing with controlled failure conditions.
   See --help for options."
  [& args]
  ;; Check for help flag first
  (when (some #{"--help" "-h"} args)
    (print-usage)
    (System/exit 0))

  (let [config (script/parse-args args)]
    ;; Handle config parse errors
    (when (:error config)
      (System/exit (:error script/exit-codes)))

    ;; Initialize random number generator (with seed if specified)
    (if-let [seed (:seed config)]
      (random/init! seed)
      (random/init!))

    ;; Set up input redirection if --input FILE specified
    (let [input-reader (when-let [file (:input-file config)]
                         (try
                           (io/reader file)
                           (catch Exception e
                             (binding [*out* *err*]
                               (println (str "Error: Cannot open input file: " file))
                               (println (.getMessage e)))
                             nil)))]

      ;; If input file specified but couldn't open, exit with error
      (when (and (:input-file config) (nil? input-reader))
        (System/exit (:error script/exit-codes)))

      ;; Initialize readline (skipped if we have a file reader)
      (when-not input-reader
        (readline/init!))

      (try
        (let [final-state
              (if input-reader
                ;; Read from file - bind *in* to file reader
                (binding [*in* input-reader]
                  (go (initial-game-state) config))
                ;; Normal mode - read from stdin
                (go (initial-game-state) config))

              exit-code (script/determine-exit-code final-state config)]

          ;; Only call System/exit if in script mode or exit code is non-zero
          (when (or (:script-mode? config) (pos? exit-code))
            (System/exit exit-code)))

        (catch Exception e
          (binding [*out* *err*]
            (println (str "Fatal error: " (.getMessage e)))
            (when-not (:quiet config)
              (.printStackTrace e)))
          (System/exit (:exception script/exit-codes)))

        (finally
          (when input-reader
            (.close input-reader))
          (when-not input-reader
            (readline/shutdown!)))))))
