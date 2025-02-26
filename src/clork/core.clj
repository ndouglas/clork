(ns clork.core
  (:gen-class))

(require '[clojure.pprint :refer [pprint]])

(load "utils")
(load "game_state")
(load "verbs")
(load "flags")
(load "parser")
(load "rooms")
(load "objects")
(load "verbs_look")
(load "main_loop")

(defn go
  "The GO routine."
  [game-state]
  (-> game-state
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
  (go (initial-game-state)))
