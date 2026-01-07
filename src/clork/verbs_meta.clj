(ns clork.verbs-meta
  "Meta-verb handlers: version, verbosity settings, inventory, wait.

   ZIL Reference: gverbs.zil contains V-VERSION, V-VERBOSE, V-BRIEF, V-SUPER-BRIEF, V-WAIT."
  (:require [clork.utils :as utils]
            [clork.game-state :as gs]
            [clork.daemon :as daemon]))

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
      ;; Inventory items each get their own paragraph (paragraph break after each)
      (reduce (fn [state obj-id]
                (let [obj (gs/get-thing state obj-id)
                      obj-name (:desc obj)
                      ;; Use gs/set-thing-flag? to check flags properly
                      ;; (it checks both direct keys and :flags set)
                      suffix (cond
                               (gs/set-thing-flag? state obj-id :on) " (providing light)"
                               (and (gs/set-thing-flag? state obj-id :wear)
                                    (= (:in obj) winner-id)) " (being worn)"
                               :else "")]
                  (utils/tell state (str "A " obj-name suffix "\n\n"))))
              (utils/tell game-state "You are carrying:\n\n")
              contents))))

(defn v-wait
  "Wait/pass time. Runs daemons for several turns.

   ZIL: V-WAIT in gverbs.zil lines 1530-1535
     <ROUTINE V-WAIT (\"OPTIONAL\" (NUM 3))
       <TELL \"Time passes...\" CR>
       <REPEAT ()
         <COND (<L? <SET NUM <- .NUM 1>> 0> <RETURN>)
               (<CLOCKER> <RETURN>)>>
       <SETG CLOCK-WAIT T>>

   Calls CLOCKER for one turn by default.
   If something 'interesting' happens (combat, death, etc.), stops early.
   Sets CLOCK-WAIT to prevent the normal post-action CLOCKER from running.

   Note: ZIL code shows NUM=3 default, but actual Zork behavior processes
   one turn per 'wait' command based on MIT transcript analysis."
  ([game-state]
   (v-wait game-state 1))
  ([game-state num-turns]
   (let [gs (utils/tell game-state "Time passes...")]
     (loop [gs gs
            remaining num-turns]
       (if (<= remaining 0)
         ;; Done waiting - set clock-wait flag to skip post-action clocker
         (assoc gs :clock-wait true)
         ;; Run clocker for this turn
         (let [old-moves (:moves gs 0)
               gs-after-clocker (-> gs
                                    (update :moves inc)  ; Increment turn counter
                                    daemon/clocker)
               ;; Check if something "interesting" happened
               ;; ZIL checks if CLOCKER returned true (something happened)
               ;; We detect this by checking if player died or moved rooms
               interesting? (or (:dead gs-after-clocker)
                                (not= (:here gs) (:here gs-after-clocker)))]
           (if interesting?
             ;; Something happened - stop waiting
             (assoc gs-after-clocker :clock-wait true)
             ;; Continue waiting
             (recur gs-after-clocker (dec remaining)))))))))
