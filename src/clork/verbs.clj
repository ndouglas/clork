(in-ns 'clork.core)

(defn v-version
  "Prints information about the current version of the game."
  [game-state]
  (tell game-state "ZORK I: The Great Underground Empire\n")
  (tell game-state "Infocom interactive fiction - a fantasy story\n")
  (tell game-state "Copyright (c) 1981, 1982, 1983, 1984, 1985, 1986 Infocom, Inc. All rights reserved.\n")
  (tell game-state "ZORK is a registered trademark of Infocom, Inc.\n")
  (tell game-state "Release 1 / Serial number 1\n")
  (tell game-state "Clojure port by Nathan Douglas\n"))

(defn v-verbose
  "Turns on verbose mode."
  [game-state]
  (assoc game-state :verbose true)
  (assoc game-state :super-brief false)
  (tell game-state "Maximum verbosity."))
