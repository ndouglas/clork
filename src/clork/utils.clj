(in-ns 'clork.core)

(defn tell
  "Tell the player something, and return the game state."
  [game-state message]
  (print message)
  (flush)
  game-state
)

(defn crlf
  "Print a carriage return and line feed."
  [game-state]
  (tell game-state "\n"))

(defn this-is-it
  "Sets 'it' to refer to the passed object"
  [game-state it]
  (assoc game-state :it it))

(declare set-here-flag? v-version)
(defn initial-version
  "Print out the version information when starting the game"
  [game-state]
  (if (set-here-flag? game-state :touch)
    game-state
    (-> game-state
      (v-version)
      (crlf))))
