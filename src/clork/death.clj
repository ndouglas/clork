(ns clork.death
  "Player death handling - JIGS-UP and related routines.

   ZIL Reference: JIGS-UP, FINISH, RANDOMIZE-OBJECTS, KILL-INTERRUPTS
   in 1actions.zil lines 4059-4148."
  (:require [clork.utils :as utils]
            [clork.game-state :as gs]
            [clork.verbs-health :as health]
            [clork.verbs-look :as look]
            [clork.random :as random]))

;;; ---------------------------------------------------------------------------
;;; FINISH - Game Over Prompt
;;; ---------------------------------------------------------------------------
;;; ZIL: FINISH in gverbs.zil lines 38-58
;;;
;;; When the game is truly over (third death, or other fatal ending),
;;; show the score and prompt to restart, restore, or quit.

;; Dynamic var to allow testing without actual readline
(def ^:dynamic *read-input-fn*
  "Function used to read user input. Can be rebound for testing."
  nil)

(defn- read-finish-response
  "Read user response for FINISH prompt."
  []
  (if *read-input-fn*
    (*read-input-fn*)
    (do
      (println "\nWould you like to restart the game from the beginning, restore a saved")
      (println "game position, or end this session of the game?")
      (print "(Type RESTART, RESTORE, or QUIT):\n>")
      (flush)
      (read-line))))

(defn finish
  "End the game - show score and offer restart/restore/quit.

   ZIL: FINISH in gverbs.zil lines 38-58
   Called after permanent death or winning the game."
  [game-state]
  ;; Show score first
  (let [gs (health/v-score game-state)]
    (loop [gs gs]
      (let [response (read-finish-response)
            word (when response
                   (-> response
                       clojure.string/trim
                       clojure.string/lower-case))]
        (cond
          (= word "restart")
          (-> gs
              (utils/tell "\nRestarting.")
              (assoc :restart true))

          (= word "restore")
          ;; For now, just mark as wanting restore
          ;; The main loop handles actual restoration
          (-> gs
              (utils/tell "\nRestore not implemented in FINISH yet.")
              (recur))

          (or (= word "quit") (= word "q"))
          (assoc gs :quit true)

          :else
          (do
            (println "Please type RESTART, RESTORE, or QUIT.")
            (recur gs)))))))

;;; ---------------------------------------------------------------------------
;;; KILL-INTERRUPTS - Disable All Daemons
;;; ---------------------------------------------------------------------------
;;; ZIL: KILL-INTERRUPTS in 1actions.zil lines 4138-4148
;;;
;;; Disables all active daemons/interrupts when player dies.

(defn- safe-unset-flag
  "Unset a flag on an object/room only if it exists in the game state."
  [game-state thing-id flag]
  (if (or (contains? (:objects game-state) thing-id)
          (contains? (:rooms game-state) thing-id))
    (gs/unset-thing-flag game-state thing-id flag)
    game-state))

(defn kill-interrupts
  "Disable all active daemon interrupts.

   ZIL: KILL-INTERRUPTS (1actions.zil line 4138)
   Called when player dies to reset all daemon timers."
  [game-state]
  ;; Clear all daemons
  ;; In ZIL this disables: I-XB, I-XC, I-CYCLOPS, I-LANTERN, I-CANDLES,
  ;; I-SWORD, I-FOREST-ROOM, I-MATCH
  ;; Also clears MATCH ONBIT (if match exists)
  (-> game-state
      (assoc :daemons {})
      ;; Turn off any lit match (if it exists in the game)
      (safe-unset-flag :match :on)))

;;; ---------------------------------------------------------------------------
;;; RANDOMIZE-OBJECTS - Scatter Player's Belongings
;;; ---------------------------------------------------------------------------
;;; ZIL: RANDOMIZE-OBJECTS in 1actions.zil lines 4114-4136
;;;
;;; When player dies, their possessions are scattered:
;;; - Lamp always goes to living room
;;; - Coffin always goes to Egypt room
;;; - Sword loses its treasure value
;;; - Treasures (TVALUE > 0) scatter to random land rooms
;;; - Other objects scatter to above-ground locations

(def ^:private above-ground-rooms
  "Rooms where non-treasure items can be placed on death.
   ZIL: ABOVE-GROUND table"
  [:west-of-house :north-of-house :south-of-house :behind-house
   :kitchen :living-room :attic :forest-1 :forest-3 :forest-path
   :clearing :up-a-tree])

(def ^:private land-rooms
  "Rooms that have RLANDBIT (for treasure scattering).
   For now, just use above-ground rooms."
  above-ground-rooms)

(defn- scatter-object
  "Scatter an object to a random room."
  [game-state obj-id rooms]
  (let [room-id (random/rand-nth* rooms)]
    (gs/move-object game-state obj-id room-id :death-scatter)))

(defn randomize-objects
  "Scatter player's possessions when they die.

   ZIL: RANDOMIZE-OBJECTS (1actions.zil line 4114)

   - Lamp goes to living room
   - Coffin goes to Egypt room (not implemented yet)
   - Sword loses treasure value
   - Treasures scatter to random land rooms
   - Other items scatter to above-ground locations"
  [game-state]
  (let [winner-id (:winner game-state)
        ;; Get all objects held by winner
        held-objects (->> (:objects game-state)
                          (filter (fn [[_ obj]] (= (:in obj) winner-id)))
                          (map first))]
    (reduce
     (fn [gs obj-id]
       (cond
         ;; Lamp always goes to living room
         (= obj-id :lamp)
         (gs/move-object gs :lamp :living-room :death-scatter)

         ;; Coffin always goes to Egypt room (placeholder - use living room for now)
         (= obj-id :coffin)
         (gs/move-object gs :coffin :living-room :death-scatter)

         ;; Check if it's a treasure (has :value > 0)
         (pos? (get-in gs [:objects obj-id :value] 0))
         (scatter-object gs obj-id land-rooms)

         ;; Regular objects go to above-ground rooms
         :else
         (scatter-object gs obj-id above-ground-rooms)))
     ;; Also clear sword's treasure value
     (assoc-in game-state [:objects :sword :value] 0)
     held-objects)))

;;; ---------------------------------------------------------------------------
;;; JIGS-UP - Player Death Handler
;;; ---------------------------------------------------------------------------
;;; ZIL: JIGS-UP in 1actions.zil lines 4059-4112
;;;
;;; Called when the player dies. Handles:
;;; 1. Double-death (already dead) -> FINISH
;;; 2. Third death -> permanent death (FINISH)
;;; 3. Resurrection in forest or Hades

(defn jigs-up
  "Handle player death.

   ZIL: JIGS-UP (1actions.zil line 4059)

   Parameters:
     game-state - Current game state
     desc - Death description message to display

   Flow:
   1. Set WINNER to ADVENTURER
   2. If already dead (double-death) -> FINISH
   3. Print death message
   4. Deduct 10 points
   5. Print '****  You have died  ****'
   6. If 3rd death -> permanent death (FINISH)
   7. Otherwise resurrect:
      - If visited SOUTH-TEMPLE -> Hades (special dead mode)
      - Else -> resurrect in FOREST-1
   8. Scatter objects, clear interrupts"
  [game-state desc]
  (let [;; Reset winner to adventurer
        gs (assoc game-state :winner (:adventurer game-state))
        deaths (get gs :deaths 0)]

    ;; Check for double-death (already dead)
    (if (:dead gs)
      (-> gs
          (utils/tell "\n\nIt takes a talented person to be killed while already dead. YOU are such\na talent. Unfortunately, it takes a talented person to deal with it.\nI am not such a talent. Sorry.")
          (finish))

      ;; Normal death processing
      (let [;; Print death description
            gs (-> gs
                   (utils/tell (str "\n" desc))
                   ;; Deduct 10 points
                   (health/score-upd -10)
                   ;; Show death banner
                   (utils/tell "\n\n    ****  You have died  ****\n\n"))]

        ;; Check for third death (permanent)
        (if (>= deaths 2)
          (-> gs
              (utils/tell "You clearly are a suicidal maniac. We don't allow psychotics in the\ncave, since they may harm other adventurers. Your remains will be\ninstalled in the Land of the Living Dead, where your fellow\nadventurers may gloat over them.")
              (finish))

          ;; Resurrection
          (let [;; Increment death count
                gs (update gs :deaths (fnil inc 0))
                ;; Check if visited SOUTH-TEMPLE (for Hades resurrection)
                ;; Only check if the room exists in the game state
                visited-temple? (and (contains? (:rooms gs) :south-temple)
                                     (gs/set-thing-flag? gs :south-temple :touch))]

            (if visited-temple?
              ;; Hades resurrection (not fully implemented yet)
              (-> gs
                  (utils/tell "As you take your last breath, you feel relieved of your burdens. The\nfeeling passes as you find yourself before the gates of Hell, where\nthe spirits jeer at you and deny you entry. Your senses are\ndisturbed. The objects in the dungeon appear indistinct, bleached of\ncolor, even unreal.\n\n")
                  (gs/set-game-flag :dead)
                  (gs/set-game-flag :troll-flag)
                  (assoc :always-lit true)
                  ;; Would go to :entrance-to-hades, but use :forest-1 for now
                  (gs/set-location :forest-1 :hades-resurrection)
                  (gs/move-object (:adventurer gs) :forest-1 :hades-resurrection)
                  (assoc :lit true)
                  ;; Clear trap door touch bit (if exists)
                  (safe-unset-flag :trap-door :touch)
                  (randomize-objects)
                  (kill-interrupts)
                  ;; Heal player wounds
                  (assoc-in [:objects (:adventurer gs) :strength] 0)
                  (look/v-look))

              ;; Normal forest resurrection
              (-> gs
                  (utils/tell "Now, let's take a look here...\nWell, you probably deserve another chance. I can't quite fix you\nup completely, but you can't have everything.\n\n")
                  (gs/set-location :forest-1 :forest-resurrection)
                  (gs/move-object (:adventurer gs) :forest-1 :forest-resurrection)
                  (assoc :lit true)
                  ;; Clear trap door touch bit (if exists)
                  (safe-unset-flag :trap-door :touch)
                  (randomize-objects)
                  (kill-interrupts)
                  ;; Heal player wounds (but not completely - ZIL quirk)
                  ;; Actually in ZIL, strength is left damaged but player survives
                  ;; For simplicity, we'll heal to -1 (slightly wounded)
                  (assoc-in [:objects (:adventurer gs) :strength] -1)
                  (look/v-look)))))))))
