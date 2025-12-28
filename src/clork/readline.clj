(ns clork.readline
  "JLine integration for terminal input with tab completion and history.

   This module provides:
   - Terminal-aware input reading with history (up/down arrows)
   - Tab completion for $ debug commands
   - Graceful fallback to read-line when JLine unavailable"
  (:import [org.jline.reader LineReader LineReaderBuilder EndOfFileException UserInterruptException
            Completer Candidate]
           [org.jline.reader.impl.completer StringsCompleter AggregateCompleter]
           [org.jline.terminal TerminalBuilder]
           [java.util List])
  (:require [clojure.string :as str]))

;;; ---------------------------------------------------------------------------
;;; TERMINAL AND READER MANAGEMENT
;;; ---------------------------------------------------------------------------

(def ^:private terminal-atom (atom nil))
(def ^:private reader-atom (atom nil))
(def ^:private interactive-atom (atom nil))

(defn- detect-interactive
  "Detect if we're running interactively.
   Checks if stdin has data immediately available - if so, input is likely piped."
  []
  (try
    ;; If there's data available on stdin at startup, we're non-interactive
    (zero? (.available System/in))
    (catch Exception _
      ;; If we can't check, assume interactive
      true)))

(defn- create-terminal
  "Create a JLine terminal. Returns nil on failure.
   Lets JLine detect the terminal type rather than checking System/console,
   since lein run doesn't provide a console even when interactive."
  []
  (if (Boolean/getBoolean "clork.dumb-terminal")
    nil
    (try
      ;; Suppress JLine's noisy warnings during terminal creation
      (let [logger (java.util.logging.Logger/getLogger "org.jline")
            old-level (.getLevel logger)]
        (.setLevel logger java.util.logging.Level/SEVERE)
        (try
          (-> (TerminalBuilder/builder)
              (.system true)
              (.jna true)
              (.build))
          (finally
            (.setLevel logger old-level))))
      (catch Exception e
        nil))))

;; Forward declaration - actual implementation is below after completer is defined
(declare create-reader-with-completion)

(defn init!
  "Initialize JLine terminal and reader. Call once at startup.
   Returns true if JLine initialized successfully, false otherwise."
  []
  (if @reader-atom
    true  ; Already initialized
    (let [interactive (detect-interactive)
          terminal (when interactive (create-terminal))
          reader (create-reader-with-completion terminal)]
      (reset! interactive-atom interactive)
      (reset! terminal-atom terminal)
      (reset! reader-atom reader)
      (boolean reader))))

(defn shutdown!
  "Shutdown JLine terminal and reader. Call on exit."
  []
  ;; LineReader doesn't need explicit closing - just close the terminal
  (when-let [^org.jline.terminal.Terminal terminal @terminal-atom]
    (try
      (.close terminal)
      (catch Exception _)))
  (reset! reader-atom nil)
  (reset! terminal-atom nil))

;;; ---------------------------------------------------------------------------
;;; INPUT READING
;;; ---------------------------------------------------------------------------

(defn read-input
  "Read a line of input from the user with the given prompt.

   Uses JLine if available (with history support), falls back to read-line.
   Returns nil on EOF (Ctrl-D).

   Handles:
   - Up/down arrows for command history
   - Ctrl-C interrupts (returns empty string)
   - Ctrl-D EOF (returns nil)"
  [prompt]
  (if-let [^LineReader reader @reader-atom]
    (try
      (.readLine reader ^String prompt)
      (catch EndOfFileException _
        nil)
      (catch UserInterruptException _
        ""))  ; Ctrl-C returns empty string, user can type new command
    ;; Fallback to standard read-line
    (do
      (print prompt)
      (flush)
      (read-line))))

(defn jline-available?
  "Returns true if JLine is initialized and available."
  []
  (boolean @reader-atom))

(defn interactive?
  "Returns true if running interactively (with a TTY).
   Determined during init! by checking terminal type."
  []
  (boolean @interactive-atom))

;;; ---------------------------------------------------------------------------
;;; TAB COMPLETION
;;; ---------------------------------------------------------------------------

;; Atom to hold game state accessor for dynamic completion
(def ^:private game-state-atom (atom nil))

(defn set-game-state-source!
  "Set the game state accessor function for dynamic completion.
   The function should return the current game state when called."
  [state-fn]
  (reset! game-state-atom state-fn))

;; Debug command structure for completion
(def ^:private debug-commands
  {"$debug"   {:subcommands ["state" "here" "object" "room" "tree" "flags" "find"]}
   "$parser"  {:subcommands ["result" "lexv" "itbl" "vocab" "syntax" "verbs"]}
   "$goto"    {:arg :rooms}
   "$purloin" {:arg :objects}
   "$move"    {:arg :objects :arg2 :things}
   "$flag"    {:arg :things :arg2 :flags}
   "$unflag"  {:arg :things :arg2 :flags}
   "$frotz"   {:arg :objects}
   "$undo"    {}
   "$redo"    {}
   "$history" {}
   "$help"    {}
   "$version" {}
   "$quit"    {}
   "$trace"   {:subcommands ["on" "off" "verbs" "parser" "actions" "daemons" "status"]}
   "$daemon"  {:subcommands ["list" "status" "timeline" "enable" "disable" "tick" "history"]}})

(defn- get-completions-for
  "Get completion candidates based on context."
  [arg-type]
  (when-let [state-fn @game-state-atom]
    (let [state (state-fn)]
      (case arg-type
        :rooms (map name (keys (:rooms state)))
        :objects (map name (keys (:objects state)))
        :things (concat (map name (keys (:rooms state)))
                        (map name (keys (:objects state))))
        :flags (try
                 (require 'clork.flags)
                 (map name (keys @(resolve 'clork.flags/flag-names)))
                 (catch Exception _ []))
        []))))

(defn- make-debug-completer
  "Create a JLine Completer for $ debug commands."
  []
  (reify Completer
    (complete [_this _reader line candidates]
      (let [buffer (.line line)
            words (str/split (str buffer) #"\s+")
            word-count (count words)
            cursor (.cursor line)
            ;; Are we completing a partial word or starting a new one?
            at-space? (and (pos? (count buffer))
                           (= \space (last buffer)))]
        (cond
          ;; Empty or just starting with $
          (or (empty? buffer) (= buffer "$"))
          (doseq [cmd (keys debug-commands)]
            (.add ^List candidates (Candidate. cmd)))

          ;; Completing first word (a $ command)
          (and (= word-count 1) (not at-space?))
          (let [partial (first words)]
            (doseq [cmd (keys debug-commands)]
              (when (str/starts-with? cmd partial)
                (.add ^List candidates (Candidate. cmd)))))

          ;; Completing second word (subcommand or first arg)
          (or (and (= word-count 1) at-space?)
              (and (= word-count 2) (not at-space?)))
          (let [cmd (first words)
                partial (if (and (= word-count 2) (not at-space?))
                          (second words)
                          "")
                cmd-info (get debug-commands cmd)]
            (when cmd-info
              ;; Check for subcommands
              (when-let [subs (:subcommands cmd-info)]
                (doseq [sub subs]
                  (when (str/starts-with? sub partial)
                    (.add ^List candidates (Candidate. sub)))))
              ;; Check for arg completions
              (when-let [arg-type (:arg cmd-info)]
                (doseq [comp (get-completions-for arg-type)]
                  (when (str/starts-with? comp partial)
                    (.add ^List candidates (Candidate. comp)))))))

          ;; Completing third word (second arg for some commands)
          (or (and (= word-count 2) at-space?)
              (and (= word-count 3) (not at-space?)))
          (let [cmd (first words)
                partial (if (and (= word-count 3) (not at-space?))
                          (nth words 2)
                          "")
                cmd-info (get debug-commands cmd)]
            (when-let [arg-type (:arg2 cmd-info)]
              (doseq [comp (get-completions-for arg-type)]
                (when (str/starts-with? comp partial)
                  (.add ^List candidates (Candidate. comp)))))))))))

(defn- create-reader-with-completion
  "Create a JLine LineReader with completion support."
  [terminal]
  (when terminal
    (try
      (-> (LineReaderBuilder/builder)
          (.terminal terminal)
          (.completer (make-debug-completer))
          (.build))
      (catch Exception e
        (binding [*out* *err*]
          (println "Warning: Could not create JLine reader:" (.getMessage e)))
        nil))))
