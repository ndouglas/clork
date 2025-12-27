(ns clork.core
  (:gen-class)
  (:require [clork.game-state :as game-state]
            [clork.verbs :as verbs]
            [clork.utils :as utils]
            [clork.rooms :as rooms]
            [clork.objects :as objects]
            [clork.verbs-look :as verbs-look]
            [clork.main-loop :as main-loop]
            [clork.readline :as readline]))

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
  [game-state]
  (-> game-state
    (game-state/add-rooms [
      rooms/west-of-house,
    ])
    (game-state/add-objects [
      objects/adventurer,
      objects/mailbox,
    ])
    (utils/this-is-it :mailbox)
    (initial-version)
    (game-state/set-here-flag :lit)
    (verbs-look/v-look)
    (main-loop/main-loop)))

(defn -main
  "Main function for CLORK."
  [& args]
  (readline/init!)
  (try
    (go (initial-game-state))
    (finally
      (readline/shutdown!))))
