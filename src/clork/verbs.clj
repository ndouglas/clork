(in-ns 'clork.core)

(defn v-version
  "Prints information about the current version of the game."
  []
  (do
    (println "ZORK I: The Great Underground Empire")
    (println "Infocom interactive fiction - a fantasy story")
    (println "Copyright (c) 1981, 1982, 1983, 1984, 1985, 1986 Infocom, Inc. All rights reserved.")
    (println "ZORK is a registered trademark of Infocom, Inc.")
    (println "Release 1 / Serial number 1")
    (println "Clojure port by Nathan Douglas")
  ))

(defn v-verbose
  "Turns on verbose mode."
  [game-state]
  (assoc game-state :verbose true)
  (assoc game-state :super-brief false)
  (println "Maximum verbosity."))
