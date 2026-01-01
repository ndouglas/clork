(ns clork.cyclops
  "Cyclops NPC implementation - ZIL CYCLOPS-FCN equivalent.

   The Cyclops is a stationary NPC that blocks access to the Treasure Room.
   Unlike the Thief, the Cyclops doesn't move around the dungeon.

   Two ways to defeat:
   1. Say ODYSSEUS/ULYSSES - cyclops flees, opens east wall (MAGIC-FLAG)
   2. Give LUNCH (peppers), then WATER - cyclops falls asleep (CYCLOPS-FLAG)

   ZIL Reference:
   - CYCLOPS-FCN in 1actions.zil (lines 1528-1607)
   - I-CYCLOPS daemon in 1actions.zil (lines 1609-1627)
   - CYCLOPS-ROOM-FCN in 1actions.zil (lines 1629-1656)
   - V-ODYSSEUS in gverbs.zil (lines 961-977)"
  (:require [clork.utils :as utils]
            [clork.game-state :as gs]
            [clork.parser.state :as parser-state]
            [clork.daemon :as daemon]))

;;; ---------------------------------------------------------------------------
;;; CYCLOPS DESCRIPTIONS
;;; ---------------------------------------------------------------------------

;; ZIL: CYCLOMAD table (1actions.zil lines 1657-1664)
(def cyclomad
  "Messages for increasingly agitated cyclops."
  ["The cyclops seems somewhat agitated."
   "The cyclops appears to be getting more agitated."
   "The cyclops is moving about the room, looking for something."
   "The cyclops was looking for salt and pepper. I think he wants to eat you."
   "The cyclops is advancing."])

;;; ---------------------------------------------------------------------------
;;; HELPER FUNCTIONS
;;; ---------------------------------------------------------------------------

(defn- remove-object
  "Remove an object from the game world.
   ZIL: REMOVE-CAREFULLY - sets object's location to nil."
  [game-state obj-id]
  (assoc-in game-state [:objects obj-id :in] nil))

(defn cyclops-here?
  "Check if cyclops is in the same room as the player."
  [game-state]
  (let [here (:here game-state)
        cyclops-loc (gs/get-thing-loc-id game-state :cyclops)]
    (= cyclops-loc here)))

(defn cyclops-asleep?
  "Check if cyclops is asleep (CYCLOPS-FLAG is true)."
  [game-state]
  (:cyclops-flag game-state false))

(defn cyclops-fled?
  "Check if cyclops has fled (MAGIC-FLAG is true)."
  [game-state]
  (:magic-flag game-state false))

(defn get-cyclowrath
  "Get the cyclops's current anger level."
  [game-state]
  (:cyclowrath game-state 0))

(defn set-cyclowrath
  "Set the cyclops's anger level."
  [game-state value]
  (assoc game-state :cyclowrath value))

(defn ate-peppers?
  "Check if cyclops ate the hot peppers (cyclowrath < 0)."
  [game-state]
  (neg? (get-cyclowrath game-state)))

;;; ---------------------------------------------------------------------------
;;; CYCLOPS ACTION HANDLER (CYCLOPS-FCN)
;;; ---------------------------------------------------------------------------

(defn cyclops-action
  "Cyclops action handler - ZIL: CYCLOPS-FCN (1actions.zil line 1528)

   Handles all interactions with the cyclops:
   - GIVE food/water
   - EXAMINE
   - ATTACK/THROW/MUNG
   - TAKE
   - TIE
   - LISTEN"
  [game-state & [_mode]]
  (let [prsa (parser-state/get-prsa game-state)
        prso (parser-state/get-prso game-state)
        prsi (parser-state/get-prsi game-state)
        wrath (get-cyclowrath game-state)
        asleep? (cyclops-asleep? game-state)]
    (cond
      ;; ZIL: <COND (,CYCLOPS-FLAG ...) - cyclops is asleep
      asleep?
      (cond
        (= prsa :examine)
        (utils/tell game-state
                    "The cyclops is sleeping like a baby, albeit a very ugly one.\n")

        ;; ZIL: <VERB? ALARM KICK ATTACK BURN MUNG>
        (#{:alarm :kick :attack :burn :mung} prsa)
        (-> game-state
            (utils/tell "The cyclops yawns and stares at the thing that woke him up.\n")
            (assoc :cyclops-flag false)
            (gs/set-thing-flag :cyclops :fight)
            ;; ZIL: Reset wrath based on previous state
            (set-cyclowrath (if (neg? wrath) (- wrath) wrath))
            ;; Enable the daemon
            (daemon/queue :i-cyclops -1))

        :else game-state)

      ;; ZIL: <VERB? EXAMINE> - examining awake cyclops
      (= prsa :examine)
      (utils/tell game-state
                  "A hungry cyclops is standing at the foot of the stairs.\n")

      ;; ZIL: <AND <VERB? GIVE> <EQUAL? ,PRSI ,CYCLOPS>>
      (and (= prsa :give) (= prsi :cyclops))
      (cond
        ;; ZIL: <EQUAL? ,PRSO ,LUNCH>
        (= prso :lunch)
        (if (not (neg? wrath))
          ;; Cyclops eats the lunch
          (-> game-state
              (remove-object :lunch)
              (utils/tell
               "The cyclops says \"Mmm Mmm. I love hot peppers! But oh, could I use a drink. Perhaps I could drink the blood of that thing.\"  From the gleam in his eye, it could be surmised that you are \"that thing\".\n")
              ;; ZIL: <SETG CYCLOWRATH <MIN -1 <- .COUNT>>>
              (set-cyclowrath (min -1 (- wrath)))
              (daemon/queue :i-cyclops -1))
          ;; Already ate peppers - still enable daemon
          (-> game-state
              (remove-object :lunch)
              (utils/tell "The cyclops eats the lunch and belches.\n")
              (daemon/queue :i-cyclops -1)))

        ;; ZIL: <OR <EQUAL? ,PRSO ,WATER> <AND <EQUAL? ,PRSO ,BOTTLE> <IN? ,WATER ,BOTTLE>>>
        (or (= prso :water)
            (and (= prso :bottle)
                 (= (gs/get-thing-loc-id game-state :water) :bottle)))
        (if (neg? wrath)
          ;; Cyclops ate peppers and is thirsty - falls asleep!
          (-> game-state
              (remove-object :water)
              ;; Drop the bottle in the room
              (assoc-in [:objects :bottle :in] :cyclops-room)
              (gs/set-thing-flag :bottle :open)
              (gs/unset-thing-flag :cyclops :fight)
              (utils/tell
               "The cyclops takes the bottle, checks that it's open, and drinks the water. A moment later, he lets out a yawn that nearly blows you over, and then falls fast asleep (what did you put in that drink, anyway?).\n")
              (assoc :cyclops-flag true)
              (daemon/disable :i-cyclops))
          ;; Not thirsty
          (utils/tell game-state
                      "The cyclops apparently is not thirsty and refuses your generous offer.\n"))

        ;; ZIL: <EQUAL? ,PRSO ,GARLIC>
        (= prso :garlic)
        (utils/tell game-state
                    "The cyclops may be hungry, but there is a limit.\n")

        ;; ZIL: default - won't eat that
        :else
        (utils/tell game-state
                    "The cyclops is not so stupid as to eat THAT!\n"))

      ;; ZIL: <VERB? THROW ATTACK MUNG>
      (#{:throw :attack :mung} prsa)
      (let [gs (daemon/queue game-state :i-cyclops -1)]
        (if (= prsa :mung)
          (utils/tell gs
                      "\"Do you think I'm as stupid as my father was?\", he says, dodging.\n")
          ;; Attack or throw
          (let [gs (utils/tell gs
                               "The cyclops shrugs but otherwise ignores your pitiful attempt.\n")]
            ;; If throwing, drop the object
            (if (= prsa :throw)
              (assoc-in gs [:objects prso :in] (:here game-state))
              gs))))

      ;; ZIL: <VERB? TAKE>
      (= prsa :take)
      (utils/tell game-state
                  "The cyclops doesn't take kindly to being grabbed.\n")

      ;; ZIL: <VERB? TIE>
      (= prsa :tie)
      (utils/tell game-state
                  "You cannot tie the cyclops, though he is fit to be tied.\n")

      ;; ZIL: <VERB? LISTEN>
      (= prsa :listen)
      (utils/tell game-state
                  "You can hear his stomach rumbling.\n")

      :else game-state)))

;;; ---------------------------------------------------------------------------
;;; I-CYCLOPS DAEMON
;;; ---------------------------------------------------------------------------

(defn i-cyclops
  "Cyclops daemon - ZIL: I-CYCLOPS (1actions.zil lines 1609-1627)

   Called each turn while in the cyclops room. Tracks cyclops anger
   and eventually kills the player if they don't solve the puzzle."
  [game-state]
  (let [asleep? (cyclops-asleep? game-state)
        here (:here game-state)
        wrath (get-cyclowrath game-state)]
    (cond
      ;; ZIL: <COND (<OR ,CYCLOPS-FLAG ,DEAD> <RTRUE>)>
      asleep?
      game-state

      ;; ZIL: <NOT <EQUAL? ,HERE ,CYCLOPS-ROOM>>
      (not= here :cyclops-room)
      (daemon/disable game-state :i-cyclops)

      ;; ZIL: <G? <ABS ,CYCLOWRATH> 5> - cyclops is too angry, kills player
      (> (Math/abs (int wrath)) 5)
      (-> game-state
          (daemon/disable :i-cyclops)
          (assoc :dead true)
          (utils/tell
           "The cyclops, tired of all of your games and trickery, grabs you firmly. As he licks his chops, he says \"Mmm. Just like Mom used to make 'em.\" It's nice to be appreciated.\n"))

      ;; Increment wrath and show message
      :else
      (let [;; ZIL: Increment or decrement based on sign
            new-wrath (if (neg? wrath)
                        (dec wrath)
                        (inc wrath))
            gs (set-cyclowrath game-state new-wrath)]
        ;; ZIL: <TELL <NTH ,CYCLOMAD <- <ABS ,CYCLOWRATH> 1>> CR>
        (if-not (cyclops-asleep? gs)
          (let [msg-idx (min (dec (Math/abs (int new-wrath))) (dec (count cyclomad)))]
            (utils/tell gs (str (nth cyclomad msg-idx) "\n")))
          gs)))))

;;; ---------------------------------------------------------------------------
;;; V-ODYSSEUS VERB HANDLER
;;; ---------------------------------------------------------------------------

(defn v-odysseus
  "Say ODYSSEUS/ULYSSES to scare the cyclops.

   ZIL: V-ODYSSEUS (gverbs.zil lines 961-977)

   If the cyclops is present and awake, saying the name of his father's
   deadly nemesis causes him to flee, smashing through the east wall."
  [game-state]
  (let [here (:here game-state)
        cyclops-in-room? (= (gs/get-thing-loc-id game-state :cyclops) here)
        asleep? (cyclops-asleep? game-state)]
    (cond
      ;; Cyclops in room and awake
      (and (= here :cyclops-room)
           cyclops-in-room?
           (not asleep?))
      (-> game-state
          (daemon/disable :i-cyclops)
          (assoc :cyclops-flag true)
          (utils/tell
           "The cyclops, hearing the name of his father's deadly nemesis, flees the room by knocking down the wall on the east of the room.\n")
          (assoc :magic-flag true)
          (gs/unset-thing-flag :cyclops :fight)
          (remove-object :cyclops))

      ;; Default response
      :else
      (utils/tell game-state "Wasn't he a sailor?\n"))))

;;; ---------------------------------------------------------------------------
;;; CYCLOPS-ROOM-FCN (Room Action)
;;; ---------------------------------------------------------------------------

(defn cyclops-room-action
  "Room action for cyclops room - ZIL: CYCLOPS-ROOM-FCN (1actions.zil line 1629)

   Handles :look and :enter room arguments."
  [game-state room-arg]
  (let [asleep? (cyclops-asleep? game-state)
        fled? (cyclops-fled? game-state)
        wrath (get-cyclowrath game-state)]
    (case room-arg
      ;; ZIL: <EQUAL? .RARG ,M-LOOK>
      :look
      (let [gs (utils/tell game-state
                           "This room has an exit on the northwest, and a staircase leading up.\n")]
        (cond
          ;; ZIL: <AND ,CYCLOPS-FLAG <NOT ,MAGIC-FLAG>>
          (and asleep? (not fled?))
          (utils/tell gs
                      "The cyclops is sleeping blissfully at the foot of the stairs.\n")

          ;; ZIL: ,MAGIC-FLAG
          fled?
          (utils/tell gs
                      "The east wall, previously solid, now has a cyclops-sized opening in it.\n")

          ;; ZIL: <0? ,CYCLOWRATH>
          (zero? wrath)
          (utils/tell gs
                      "A cyclops, who looks prepared to eat horses (much less mere adventurers), blocks the staircase. From his state of health, and the bloodstains on the walls, you gather that he is not very friendly, though he likes people.\n")

          ;; ZIL: <G? ,CYCLOWRATH 0>
          (pos? wrath)
          (utils/tell gs
                      "The cyclops is standing in the corner, eyeing you closely. I don't think he likes you very much. He looks extremely hungry, even for a cyclops.\n")

          ;; ZIL: <L? ,CYCLOWRATH 0>
          (neg? wrath)
          (utils/tell gs
                      "The cyclops, having eaten the hot peppers, appears to be gasping. His enflamed tongue protrudes from his man-sized mouth.\n")

          :else gs))

      ;; ZIL: <EQUAL? .RARG ,M-ENTER>
      :enter
      (if (not (zero? wrath))
        ;; Enable the daemon if wrath is non-zero
        (daemon/queue game-state :i-cyclops -1)
        game-state)

      ;; Default
      game-state)))
