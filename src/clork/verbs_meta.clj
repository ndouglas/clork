(ns clork.verbs-meta
  "Meta-verb handlers: version, verbosity settings, inventory.

   ZIL Reference: gverbs.zil contains V-VERSION, V-VERBOSE, V-BRIEF, V-SUPER-BRIEF."
  (:require [clork.utils :as utils]
            [clork.game-state :as gs]))

;;; ---------------------------------------------------------------------------
;;; META-VERB HANDLERS
;;; ---------------------------------------------------------------------------
;;; Each verb has a handler function that performs the action.
;;; Handler functions take game-state and return updated game-state.

(defn v-version
  "Prints information about the current version of the game.

   ZIL: V-VERSION in gverbs.zil"
  [game-state]
  (-> game-state
      (utils/tell "ZORK I: The Great Underground Empire\n")
      (utils/tell "Infocom interactive fiction - a fantasy story\n")
      (utils/tell "Copyright (c) 1981, 1982, 1983, 1984, 1985, 1986 Infocom, Inc. All rights reserved.\n")
      (utils/tell "ZORK is a registered trademark of Infocom, Inc.\n")
      (utils/tell "Release 1 / Serial number 1\n")
      (utils/tell "Clojure port by Nathan Douglas\n")))

(defn v-verbose
  "Turns on verbose mode - always describe rooms fully.

   ZIL: V-VERBOSE in gverbs.zil
     <ROUTINE V-VERBOSE ()
       <SETG VERBOSE T>
       <SETG SUPER-BRIEF <>>
       <TELL \"Maximum verbosity.\" CR>>"
  [game-state]
  (-> game-state
      (assoc :verbose true)
      (assoc :super-brief false)
      (utils/tell "Maximum verbosity.")))

(defn v-brief
  "Turns on brief mode - describe rooms only on first visit.

   ZIL: V-BRIEF in gverbs.zil
     <ROUTINE V-BRIEF ()
       <SETG VERBOSE <>>
       <SETG SUPER-BRIEF <>>
       <TELL \"Brief descriptions.\" CR>>"
  [game-state]
  (-> game-state
      (assoc :verbose false)
      (assoc :super-brief false)
      (utils/tell "Brief descriptions.")))

(defn v-super-brief
  "Turns on super-brief mode - never describe rooms automatically.

   ZIL: V-SUPER-BRIEF in gverbs.zil
     <ROUTINE V-SUPER-BRIEF ()
       <SETG SUPER-BRIEF T>
       <TELL \"Superbrief descriptions.\" CR>>"
  [game-state]
  (-> game-state
      (assoc :super-brief true)
      (utils/tell "Superbrief descriptions.")))

(defn v-inventory
  "Prints the player's inventory.

   ZIL: V-INVENTORY in gverbs.zil
     <ROUTINE V-INVENTORY ()
       <COND (<FIRST? ,WINNER> <PRINT-CONT ,WINNER>)
             (T <TELL \"You are empty-handed.\" CR>)>>"
  [game-state]
  (let [winner-id (:winner game-state)
        contents (gs/get-contents game-state winner-id)]
    (if (empty? contents)
      (utils/tell game-state "You are empty-handed.")
      (reduce (fn [state obj-id]
                (let [obj-name (gs/thing-name state obj-id)]
                  (utils/tell state (str "  A " obj-name "\n"))))
              (utils/tell game-state "You are carrying:\n")
              contents))))
