(ns clork.utils
  "Output utilities for Clork."
  (:require [clojure.java.io :as io]))

;;; ---------------------------------------------------------------------------
;;; TRANSCRIPT SUPPORT
;;; ---------------------------------------------------------------------------
;;; ZIL: SCRIPT/UNSCRIPT commands enable/disable transcription to a file

(def ^:private transcript-writer
  "Atom holding the current transcript writer, or nil if not scripting."
  (atom nil))

(defn script-enabled?
  "Returns true if transcript mode is currently enabled."
  []
  (some? @transcript-writer))

(defn start-script!
  "Start transcription to the specified file.
   Returns true on success, false on failure."
  [filename]
  (try
    (let [writer (io/writer filename)]
      (reset! transcript-writer writer)
      true)
    (catch Exception e
      (binding [*out* *err*]
        (println (str "Failed to start transcript: " (.getMessage e))))
      false)))

(defn stop-script!
  "Stop transcription and close the file.
   Returns true on success, false if no transcript was active."
  []
  (if-let [writer @transcript-writer]
    (do
      (try
        (.close writer)
        (catch Exception _))
      (reset! transcript-writer nil)
      true)
    false))

(defn- write-to-transcript
  "Write message to transcript file if transcription is enabled."
  [message]
  (when-let [writer @transcript-writer]
    (try
      (.write writer message)
      (.flush writer)
      (catch Exception _))))

(defn tell
  "Tell the player something, and return the game state.
   Also writes to transcript if scripting is enabled.

   If :output-buffer is set in game-state (an atom), appends message there
   instead of printing. This allows ML mode to capture output."
  [game-state message]
  (if-let [buffer (:output-buffer game-state)]
    ;; ML mode: capture to buffer
    (do
      (swap! buffer conj message)
      game-state)
    ;; Normal mode: print to stdout
    (do
      (print message)
      (flush)
      ;; Also write to transcript if enabled
      (write-to-transcript message)
      game-state)))

(defn crlf
  "Print a carriage return and line feed."
  [game-state]
  (tell game-state "\n"))

(defn crlf-if
  "Print a carriage return and line feed if condition is true."
  [game-state if-cond]
  (if if-cond
    (crlf game-state)
    game-state))

(defn this-is-it
  "Sets 'it' to refer to the passed object"
  [game-state it]
  (assoc game-state :it it))
