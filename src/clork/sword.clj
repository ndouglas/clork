(ns clork.sword
  "Sword glowing daemon and helpers.

   The elvish sword glows when enemies are near:
   - Glows very brightly (level 2) when an enemy is in the same room
   - Glows faintly (level 1) when an enemy is in an adjacent room
   - Does not glow (level 0) when no enemies are nearby

   ZIL Reference: I-SWORD routine in 1actions.zil lines 3862-3892
                  INFESTED? routine in 1actions.zil lines 3894-3899
                  SWORD-FCN routine in 1actions.zil lines 2445-2455"
  (:require [clork.game-state :as gs]
            [clork.utils :as utils]
            [clork.parser.state :as parser-state]
            [clork.daemon :as daemon]))

;;; ---------------------------------------------------------------------------
;;; INFESTED? - Check for visible enemies in a room
;;; ---------------------------------------------------------------------------
;;; ZIL: INFESTED? (1actions.zil line 3894)
;;;
;;; <ROUTINE INFESTED? (R "AUX" (F <FIRST? .R>))
;;;   <REPEAT ()
;;;     <COND (<NOT .F> <RFALSE>)
;;;           (<AND <FSET? .F ,ACTORBIT> <NOT <FSET? .F ,INVISIBLE>>>
;;;            <RTRUE>)
;;;           (<NOT <SET F <NEXT? .F>>> <RFALSE>)>>>

(defn infested?
  "Check if a room contains a visible actor (enemy).

   ZIL: INFESTED? (1actions.zil line 3894)

   Returns true if the room contains any object with :actor flag
   that is NOT invisible. This excludes the player (who has :invisible)."
  [game-state room-id]
  (let [contents (gs/get-contents game-state room-id)]
    (some (fn [obj-id]
            (and (gs/set-thing-flag? game-state obj-id :actor)
                 (not (gs/set-thing-flag? game-state obj-id :invisible))))
          contents)))

;;; ---------------------------------------------------------------------------
;;; Get adjacent rooms from exits
;;; ---------------------------------------------------------------------------

(defn- get-exit-room
  "Get the destination room from an exit value.
   Handles simple keywords, maps with :to, and strings (blocked exits)."
  [exit-value]
  (cond
    (keyword? exit-value) exit-value
    (map? exit-value) (:to exit-value)
    :else nil))

(defn adjacent-rooms
  "Get all rooms directly adjacent to the given room via exits.
   Only returns actual room keywords, not blocked exits (strings)."
  [game-state room-id]
  (let [room (gs/get-thing game-state room-id)
        exits (:exits room {})]
    (->> (vals exits)
         (map get-exit-room)
         (filter keyword?)
         (distinct))))

;;; ---------------------------------------------------------------------------
;;; I-SWORD Daemon
;;; ---------------------------------------------------------------------------
;;; ZIL: I-SWORD (1actions.zil line 3864)
;;;
;;; <ROUTINE I-SWORD ("AUX" (DEM <INT I-SWORD>) (G <GETP ,SWORD ,P?TVALUE>)
;;;                         (NG 0) P T L)
;;;   <COND (<IN? ,SWORD ,ADVENTURER>
;;;          <COND (<INFESTED? ,HERE> <SET NG 2>)
;;;                (T
;;;                 <SET P 0>
;;;                 <REPEAT ()
;;;                   <COND (<0? <SET P <NEXTP ,HERE .P>>>
;;;                          <RETURN>)
;;;                         (<NOT <L? .P ,LOW-DIRECTION>>
;;;                          <SET T <GETPT ,HERE .P>>
;;;                          <SET L <PTSIZE .T>>
;;;                          <COND (<EQUAL? .L ,UEXIT ,CEXIT ,DEXIT>
;;;                                 <COND (<INFESTED? <GETB .T 0>>
;;;                                        <SET NG 1>
;;;                                        <RETURN>)>)>)>>)>
;;;          <COND (<EQUAL? .NG .G> <RFALSE>)
;;;                (<EQUAL? .NG 2>
;;;                 <TELL "Your sword has begun to glow very brightly." CR>)
;;;                (<1? .NG>
;;;                 <TELL "Your sword is glowing with a faint blue glow." CR>)
;;;                (<0? .NG>
;;;                 <TELL "Your sword is no longer glowing." CR>)>
;;;          <PUTP ,SWORD ,P?TVALUE .NG>
;;;          <RTRUE>)
;;;         (T
;;;          <PUT .DEM ,C-ENABLED? 0>
;;;          <RFALSE>)>>

(defn calculate-glow-level
  "Calculate what the sword's glow level should be based on enemy positions.

   Returns:
     2 - Enemy in same room (very bright glow)
     1 - Enemy in adjacent room (faint glow)
     0 - No enemies nearby (no glow)"
  [game-state]
  (let [here (:here game-state)]
    (cond
      ;; Enemy in same room -> very bright
      (infested? game-state here)
      2

      ;; Check adjacent rooms for enemies -> faint glow
      (some #(infested? game-state %) (adjacent-rooms game-state here))
      1

      ;; No enemies nearby
      :else
      0)))

(defn i-sword
  "Sword glowing daemon.

   ZIL: I-SWORD (1actions.zil line 3864)

   Called each turn. If the sword is in the player's inventory:
   - Calculates new glow level based on enemy positions
   - If level changed, prints appropriate message
   - Stores glow level in sword's :tvalue property

   If sword is not in player's inventory, does nothing (waits for sword
   to be picked up)."
  [game-state]
  (let [winner (:winner game-state)
        sword-loc (gs/get-thing-loc-id game-state :sword)]
    (if (= sword-loc winner)
      ;; Sword is in player's inventory
      (let [current-glow (get-in game-state [:objects :sword :tvalue] 0)
            new-glow (calculate-glow-level game-state)]
        (if (= current-glow new-glow)
          ;; No change in glow level
          game-state
          ;; Glow level changed - print message and update
          ;; Messages start with newline to separate from previous output
          (-> game-state
              (cond->
                (= new-glow 2)
                (utils/tell "\nYour sword has begun to glow very brightly.")

                (= new-glow 1)
                (utils/tell "\nYour sword is glowing with a faint blue glow.")

                (= new-glow 0)
                (utils/tell "\nYour sword is no longer glowing."))
              (assoc-in [:objects :sword :tvalue] new-glow))))

      ;; Sword not in player's inventory - do nothing
      ;; (daemon stays enabled, waiting for sword to be picked up)
      game-state)))

;;; ---------------------------------------------------------------------------
;;; SWORD-FCN - Sword action handler
;;; ---------------------------------------------------------------------------
;;; ZIL: SWORD-FCN (1actions.zil line 2445)
;;;
;;; <ROUTINE SWORD-FCN ("AUX" G)
;;;   <COND (<AND <VERB? TAKE> <EQUAL? ,WINNER ,ADVENTURER>>
;;;          <ENABLE <QUEUE I-SWORD -1>>
;;;          <>)
;;;         (<VERB? EXAMINE>
;;;          <COND (<EQUAL? <SET G <GETP ,SWORD ,P?TVALUE>> 1>
;;;                 <TELL "Your sword is glowing with a faint blue glow." CR>)
;;;                (<EQUAL? .G 2>
;;;                 <TELL "Your sword is glowing very brightly." CR>)>)>>

(defn sword-action
  "Sword action handler.

   ZIL: SWORD-FCN (1actions.zil line 2445)

   EXAMINE: Describes the current glow state based on tvalue.

   Note: In ZIL, TAKE enables the I-SWORD daemon. In our implementation,
   the daemon is always enabled (registered at game start), so TAKE doesn't
   need special handling."
  [game-state]
  (let [verb (parser-state/get-prsa game-state)]
    (cond
      ;; EXAMINE - describe glow state
      (= verb :examine)
      (let [glow (get-in game-state [:objects :sword :tvalue] 0)]
        (cond
          (= glow 1)
          (utils/tell game-state "Your sword is glowing with a faint blue glow.")

          (= glow 2)
          (utils/tell game-state "Your sword is glowing very brightly.")

          ;; No special message for no glow - let default examine handle it
          :else
          nil))

      ;; Other verbs - not handled
      :else nil)))
