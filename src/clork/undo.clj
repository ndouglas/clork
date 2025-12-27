(ns clork.undo
  "Undo/redo system for game state.

   Maintains a history of game states that can be navigated with $undo/$redo.
   State snapshots are stored without the undo/redo stacks themselves to
   avoid circular storage."
  (:require [clork.utils :as utils]))

;;; ---------------------------------------------------------------------------
;;; CONFIGURATION
;;; ---------------------------------------------------------------------------

(def default-limit 100)

;;; ---------------------------------------------------------------------------
;;; STATE SNAPSHOT MANAGEMENT
;;; ---------------------------------------------------------------------------

(defn- clean-state-for-snapshot
  "Remove undo-specific fields from state before saving snapshot.
   This prevents nested storage of history."
  [game-state]
  (dissoc game-state :undo-stack :redo-stack :undo-limit))

(defn- make-snapshot
  "Create a snapshot record for the undo stack."
  [game-state input]
  {:state (clean-state-for-snapshot game-state)
   :input input
   :turn (:moves game-state 0)})

;;; ---------------------------------------------------------------------------
;;; UNDO OPERATIONS
;;; ---------------------------------------------------------------------------

(defn push-undo
  "Push current state onto undo stack before executing a command.
   Clears redo stack (new timeline branch)."
  [game-state input]
  (let [stack (or (:undo-stack game-state) [])
        limit (or (:undo-limit game-state) default-limit)
        snapshot (make-snapshot game-state input)
        new-stack (conj stack snapshot)
        ;; Trim to limit (keep most recent)
        trimmed (if (> (count new-stack) limit)
                  (vec (drop (- (count new-stack) limit) new-stack))
                  new-stack)]
    (-> game-state
        (assoc :undo-stack trimmed)
        (assoc :redo-stack []))))

(defn can-undo?
  "Returns true if there's state to undo."
  [game-state]
  (pos? (count (or (:undo-stack game-state) []))))

(defn can-redo?
  "Returns true if there's state to redo."
  [game-state]
  (pos? (count (or (:redo-stack game-state) []))))

(defn do-undo
  "Undo the last command, returning the previous state.
   Returns [new-game-state undone-input] or nil if can't undo."
  [game-state]
  (when (can-undo? game-state)
    (let [undo-stack (:undo-stack game-state)
          redo-stack (or (:redo-stack game-state) [])
          ;; Pop from undo
          snapshot (peek undo-stack)
          new-undo-stack (pop undo-stack)
          ;; Push current state to redo
          current-snapshot (make-snapshot game-state (:input game-state))
          new-redo-stack (conj redo-stack current-snapshot)
          ;; Restore the snapshot, preserving undo/redo stacks
          restored-state (-> (:state snapshot)
                             (assoc :undo-stack new-undo-stack)
                             (assoc :redo-stack new-redo-stack)
                             (assoc :undo-limit (or (:undo-limit game-state) default-limit)))]
      [restored-state (:input snapshot)])))

(defn do-redo
  "Redo the last undone command, returning the next state.
   Returns [new-game-state redone-input] or nil if can't redo."
  [game-state]
  (when (can-redo? game-state)
    (let [undo-stack (or (:undo-stack game-state) [])
          redo-stack (:redo-stack game-state)
          ;; Pop from redo
          snapshot (peek redo-stack)
          new-redo-stack (pop redo-stack)
          ;; Push current state to undo
          current-snapshot (make-snapshot game-state (:input game-state))
          new-undo-stack (conj undo-stack current-snapshot)
          ;; Restore the snapshot, preserving undo/redo stacks
          restored-state (-> (:state snapshot)
                             (assoc :undo-stack new-undo-stack)
                             (assoc :redo-stack new-redo-stack)
                             (assoc :undo-limit (or (:undo-limit game-state) default-limit)))]
      [restored-state (:input snapshot)])))

;;; ---------------------------------------------------------------------------
;;; HISTORY ACCESS
;;; ---------------------------------------------------------------------------

(defn get-history
  "Get the last n items from the undo stack."
  [game-state n]
  (let [stack (or (:undo-stack game-state) [])]
    (take-last n stack)))

(defn history-count
  "Get the number of states in the undo history."
  [game-state]
  (count (or (:undo-stack game-state) [])))

;;; ---------------------------------------------------------------------------
;;; DEBUG COMMANDS
;;; ---------------------------------------------------------------------------

(defn cmd-undo
  "Handle $undo command."
  [game-state args]
  (let [n (if (empty? args) 1 (Integer/parseInt (first args)))]
    (loop [gs game-state
           remaining n]
      (if (or (zero? remaining) (not (can-undo? gs)))
        (if (= remaining n)
          (utils/tell gs "Nothing to undo.\n")
          gs)
        (let [[new-gs undone-input] (do-undo gs)]
          (recur (utils/tell new-gs (str "Undone: " undone-input "\n"))
                 (dec remaining)))))))

(defn cmd-redo
  "Handle $redo command."
  [game-state args]
  (let [n (if (empty? args) 1 (Integer/parseInt (first args)))]
    (loop [gs game-state
           remaining n]
      (if (or (zero? remaining) (not (can-redo? gs)))
        (if (= remaining n)
          (utils/tell gs "Nothing to redo.\n")
          gs)
        (let [[new-gs redone-input] (do-redo gs)]
          (recur (utils/tell new-gs (str "Redone: " redone-input "\n"))
                 (dec remaining)))))))

(defn cmd-history
  "Handle $history command."
  [game-state args]
  (let [n (if (empty? args) 10 (Integer/parseInt (first args)))
        history (get-history game-state n)
        total (history-count game-state)]
    (if (empty? history)
      (utils/tell game-state "No history available.\n")
      (reduce (fn [gs {:keys [input turn]}]
                (utils/tell gs (str "  [" turn "] " input "\n")))
              (utils/tell game-state (str "History (showing " (count history) " of " total "):\n"))
              history))))
