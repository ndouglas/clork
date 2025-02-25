(ns clork.core
  (:gen-class))

(require '[clojure.pprint :refer [pprint]])

(load "utils")
(load "verbs")
(load "initial_game_state")
(load "flags")
(load "game_state")
(load "rooms")
(load "main_loop")
(load "objects")
(load "verbs_look")

(defn tell
  "Tell the player something, and return the game state."
  [game-state message]
  (print message)
  game-state
)

(defn add-room
  "Add a room to the game state"
  [game-state room]
  (assoc game-state :rooms
    (assoc (:rooms game-state) (:id room) room)))

(defn add-rooms
  "Add each of the list of rooms to the game state"
  [game-state rooms]
  (reduce add-room game-state rooms))

(defn add-object
  "Add an object to the game state"
  [game-state object]
  (assoc game-state :objects
    (assoc (:objects game-state) (:id object) object)))

(defn add-objects
  "Add each of the list of objects to the game state"
  [game-state objects]
  (reduce add-object game-state objects))

(defn this-is-it
  "Sets 'it' to refer to the passed object"
  [game-state it]
  (assoc game-state :it it))

(defn initial-version
  "Print out the version information when starting the game"
  [game-state]
  (when-not (set-here-flag? game-state :touch)
    (-> game-state
      (v-version)
      (crlf)))
  game-state)

(defn go
  "The GO routine."
  []
  (-> initial-game-state
    (add-rooms [
      west-of-house,
    ])
    (add-objects[
      adventurer,
      mailbox,
    ])
    (this-is-it :mailbox)
    (initial-version)
    (set-here-flag :lit)
    (v-look)
    (main-loop)))

(defn -main
  "Main function for CLORK."
  [& args]
  (go))
