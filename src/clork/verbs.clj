(ns clork.verbs
  "Verb handler functions."
  (:require [clork.utils :as utils]
            [clork.game-state :as gs]))

;;; ---------------------------------------------------------------------------
;;; VERB HANDLERS
;;; ---------------------------------------------------------------------------
;;; Each verb has a handler function that performs the action.
;;; Handler functions take game-state and return updated game-state.
;;;
;;; ZIL Reference: gverbs.zil contains meta-verb handlers like V-VERSION,
;;; V-VERBOSE, V-BRIEF, V-SUPER-BRIEF.

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

;;; ---------------------------------------------------------------------------
;;; Constants for fight/health system
;;; ---------------------------------------------------------------------------
;;; ZIL: STRENGTH-MAX = 7, STRENGTH-MIN = 2
;;; Base fight strength is calculated from score, ranging from 2 to 7.
;;; For now, we use a fixed base of 4 since scoring isn't implemented.

(def ^:private base-fight-strength
  "Base fighting strength (before wounds). ZIL calculates this from score."
  4)

(defn- fight-strength
  "Calculate the player's current fighting ability.

   ZIL: FIGHT-STRENGTH routine in 1actions.zil
   Returns base strength (4) plus wound modifier (negative when wounded).
   If adjust? is false, returns just the base (for survivability display)."
  ([game-state] (fight-strength game-state true))
  ([game-state adjust?]
   (let [wound-modifier (get-in game-state [:objects (:winner game-state) :strength] 0)]
     (if adjust?
       (+ base-fight-strength wound-modifier)
       base-fight-strength))))

(defn- wound-description
  "Return the description of wounds based on wound level.

   ZIL: Wound levels from V-DIAGNOSE:
     1 = light wound
     2 = serious wound
     3 = several wounds
     >3 = serious wounds"
  [wound-level]
  (cond
    (= wound-level 1) "a light wound"
    (= wound-level 2) "a serious wound"
    (= wound-level 3) "several wounds"
    (> wound-level 3) "serious wounds"))

(defn- survivability-description
  "Return description of how much more damage the player can take.

   ZIL: Based on remaining strength (RS = MS + WD):
     0 = expect death soon
     1 = killed by one more light wound
     2 = killed by a serious wound
     3 = survive one serious wound
     >3 = survive several wounds"
  [remaining-strength]
  (cond
    (<= remaining-strength 0) "expect death soon"
    (= remaining-strength 1) "be killed by one more light wound"
    (= remaining-strength 2) "be killed by a serious wound"
    (= remaining-strength 3) "survive one serious wound"
    (> remaining-strength 3) "survive several wounds"))

(defn v-diagnose
  "Reports the player's health status.

   ZIL: V-DIAGNOSE in 1actions.zil
     Reports current wounds, time to heal, survivability, and death count.

     <ROUTINE V-DIAGNOSE (\"AUX\" (MS <FIGHT-STRENGTH <>>)
                          (WD <GETP ,WINNER ,P?STRENGTH>) (RS <+ .MS .WD>))
       ...>"
  [game-state]
  (let [winner-id (:winner game-state)
        ;; WD = wound modifier (0 = healthy, negative = wounded)
        wound-modifier (get-in game-state [:objects winner-id :strength] 0)
        ;; Convert to positive wound level for display
        wound-level (- wound-modifier)
        ;; RS = remaining strength = base + wounds
        remaining-strength (fight-strength game-state)
        deaths (get game-state :deaths 0)]
    (-> game-state
        ;; Report wound status
        (as-> gs
              (if (zero? wound-level)
                (utils/tell gs "You are in perfect health.")
                (-> gs
                    (utils/tell "You have ")
                    (utils/tell (wound-description wound-level))
                    (utils/tell "."))))
        ;; Report survivability
        (utils/tell "\nYou can ")
        (utils/tell (survivability-description remaining-strength))
        (utils/tell ".")
        ;; Report death count if any
        (as-> gs
              (if (pos? deaths)
                (-> gs
                    (utils/tell "\nYou have been killed ")
                    (utils/tell (if (= deaths 1) "once" "twice"))
                    (utils/tell "."))
                gs)))))
