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

(defn go
  "The GO routine."
  []
  ;; Set special global variables
  (def game-state initial-game-state)

  ;; Add rooms.
  (def game-state (add-rooms game-state [
    west-of-house,
  ]))

  ;; Add objects.
  (def game-state (add-objects game-state [
    adventurer,
    mailbox,
  ]))

  ;; THIS-IS-IT sets "IT" to refer to the mailbox. So "OPEN IT" will open the
  ;; mailbox.
  (def game-state (this-is-it game-state :mailbox))

  ;; If we haven't been here before, then show V-VERSION text.
  (if-not
    (set-here-flag? game-state :touch)
    (do
      (v-version)
      (crlf)))

  ;; Set LIT to T, so everything is lit.
  (def game-state (set-here-flag game-state :lit))

  ;; Call V-LOOK to describe the current location
  (v-look game-state)

  ;; Call the MAIN-LOOP
  (main-loop game-state)
)

(defn -main
  "Main function for CLORK."
  [& args]
  (go))
