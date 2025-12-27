(ns clork.utils
  "Output utilities for Clork.")

(defn tell
  "Tell the player something, and return the game state."
  [game-state message]
  (print message)
  (flush)
  game-state)

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
