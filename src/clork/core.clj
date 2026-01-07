(ns clork.core
  (:gen-class)
  (:require [clork.game-state :as game-state]
            [clork.verbs-meta :as verbs-meta]
            [clork.utils :as utils]
            [clork.rooms :as rooms]
            [clork.objects :as objects]
            [clork.verbs-look :as verbs-look]
            [clork.verb-defs :as verb-defs]
            [clork.main-loop :as main-loop]
            [clork.readline :as readline]
            [clork.script :as script]
            [clork.random :as random]
            [clork.ml :as ml]
            [clork.daemon :as daemon]
            [clork.combat :as combat]
            [clork.sword :as sword]
            [clork.thief :as thief]
            [clork.cyclops :as cyclops]
            [clork.light :as light]
            [clork.forest :as forest]
            [clojure.java.io :as io]))

;; Re-export the essential API for creating and running games
(def initial-game-state game-state/initial-game-state)

(defn initial-version
  "Print out the version information when starting the game"
  [game-state]
  (if (game-state/set-here-flag? game-state :touch)
    game-state
    (-> game-state
        (verbs-meta/v-version)
        (utils/crlf))))

(defn init-game
  "Initialize game state without starting the main loop.
   Returns a ready-to-play game state.

   This is used by ML mode and can be used for testing."
  ([]
   (init-game nil))
  ([config]
   (let [gs (-> (game-state/initial-game-state)
                (assoc :script-config config)
                (game-state/add-rooms rooms/all-rooms)
                (game-state/add-objects objects/all-objects))]
     ;; Register object vocabulary for parser
     (verb-defs/register-object-vocabulary! (:objects gs))
     ;; Initialize the game state and store a copy for restart
     (let [init-gs (-> gs
                       (utils/this-is-it :mailbox)
                       (game-state/set-here-flag :lit)
                       ;; Register combat daemon (runs each turn)
                       (daemon/register-daemon :i-fight combat/combat-daemon :tick -1)
                       ;; Register sword glow daemon (checks for nearby enemies)
                       ;; ZIL: <QUEUE I-SWORD -1> in 1dungeon.zil line 2653
                       (daemon/register-daemon :i-sword sword/i-sword :tick -1)
                       ;; Register thief wandering daemon
                       ;; ZIL: <ENABLE <QUEUE I-THIEF -1>> in 1dungeon.zil line 2656
                       (daemon/register-daemon :i-thief thief/i-thief :tick -1)
                       ;; Register cyclops daemon (starts disabled, enabled when player enters room)
                       ;; ZIL: I-CYCLOPS in 1actions.zil lines 1609-1627
                       (daemon/register-daemon :i-cyclops cyclops/i-cyclops :tick -1 :enabled false)
                       ;; Register light source daemons (start disabled, enabled when light is turned on)
                       ;; ZIL: I-LANTERN in 1actions.zil line 2328
                       (daemon/register-daemon :i-lantern light/i-lantern :tick 100 :enabled false)
                       ;; ZIL: I-CANDLES in 1actions.zil line 2334
                       (daemon/register-daemon :i-candles light/i-candles :tick 20 :enabled false)
                       ;; ZIL: I-MATCH in 1actions.zil line 2321
                       (daemon/register-daemon :i-match light/i-match :tick 2 :enabled false)
                       ;; ZIL: I-FOREST-ROOM (1actions.zil line 3009)
                       ;; Starts disabled, enabled when entering forest rooms
                       (daemon/register-daemon :i-forest-room forest/i-forest-room :tick -1 :enabled false))]
       ;; Store initial state for restart
       (assoc init-gs :restart-state init-gs)))))

(defn go
  "The GO routine."
  ([game-state]
   (go game-state nil))
  ([game-state config]
   (let [gs (-> game-state
                (assoc :script-config config)
                (game-state/add-rooms rooms/all-rooms)
                (game-state/add-objects objects/all-objects))]
     ;; Register object vocabulary for parser
     (verb-defs/register-object-vocabulary! (:objects gs))
     ;; Initialize the game state and store a copy for restart
     (let [init-gs (-> gs
                       (utils/this-is-it :mailbox)
                       (game-state/set-here-flag :lit)
                       ;; Register combat daemon (runs each turn)
                       (daemon/register-daemon :i-fight combat/combat-daemon :tick -1)
                       ;; Register sword glow daemon (checks for nearby enemies)
                       ;; ZIL: <QUEUE I-SWORD -1> in 1dungeon.zil line 2653
                       (daemon/register-daemon :i-sword sword/i-sword :tick -1)
                       ;; Register thief wandering daemon
                       ;; ZIL: <ENABLE <QUEUE I-THIEF -1>> in 1dungeon.zil line 2656
                       (daemon/register-daemon :i-thief thief/i-thief :tick -1)
                       ;; Register cyclops daemon (starts disabled, enabled when player enters room)
                       ;; ZIL: I-CYCLOPS in 1actions.zil lines 1609-1627
                       (daemon/register-daemon :i-cyclops cyclops/i-cyclops :tick -1 :enabled false)
                       ;; Register light source daemons (start disabled, enabled when light is turned on)
                       ;; ZIL: I-LANTERN in 1actions.zil line 2328
                       (daemon/register-daemon :i-lantern light/i-lantern :tick 100 :enabled false)
                       ;; ZIL: I-CANDLES in 1actions.zil line 2334
                       (daemon/register-daemon :i-candles light/i-candles :tick 20 :enabled false)
                       ;; ZIL: I-MATCH in 1actions.zil line 2321
                       (daemon/register-daemon :i-match light/i-match :tick 2 :enabled false)
                       ;; ZIL: I-FOREST-ROOM (1actions.zil line 3009)
                       ;; Starts disabled, enabled when entering forest rooms
                       (daemon/register-daemon :i-forest-room forest/i-forest-room :tick -1 :enabled false))
           ;; Store initial state for restart (before version/look output)
           gs-with-restart (assoc init-gs :restart-state init-gs)]
       (-> gs-with-restart
           (initial-version)
           (verbs-look/v-look)
           (main-loop/main-loop))))))

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
  (println "  --ml                   Run in ML training mode (JSON lines on stdin/stdout)")
  (println "  --ml-rewards           Include reward signals in ML mode output")
  (println "  --help, -h             Show this help message")
  (println "")
  (println "ML Mode:")
  (println "  In ML mode, the game communicates via JSON lines:")
  (println "  - Outputs game state as JSON after each action")
  (println "  - Reads actions as JSON from stdin")
  (println "  - Action format: {\"verb\": \"look\"} or {\"verb\": \"go\", \"direction\": \"north\"}")
  (println "  - Special actions: {\"verb\": \"quit\"}, {\"verb\": \"reset\"}, {\"verb\": \"stats\"}")
  (println "")
  (println "  With --ml-rewards, output includes:")
  (println "  - Reward signals (score_delta, novel_room, novel_message, etc.)")
  (println "  - Composite reward (weighted sum of signals)")
  (println "  - Session statistics (rooms discovered, objects collected, etc.)")
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

  ;; Check for ML mode
  (when (some #{"--ml"} args)
    (let [seed-idx (.indexOf (vec args) "--seed")
          seed (when (and (>= seed-idx 0) (< (inc seed-idx) (count args)))
                 (try (Long/parseLong (nth args (inc seed-idx)))
                      (catch Exception _ nil)))
          use-rewards? (some #{"--ml-rewards"} args)]
      ;; Initialize random number generator
      (if seed
        (random/init! seed)
        (random/init!))
      ;; Run ML mode (with or without rewards)
      (try
        (if use-rewards?
          (ml/json-line-mode-with-rewards init-game)
          (ml/json-line-mode init-game))
        (System/exit 0)
        (catch Exception e
          (binding [*out* *err*]
            (println (str "Fatal error: " (.getMessage e)))
            (.printStackTrace e))
          (System/exit (:exception script/exit-codes))))))

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
