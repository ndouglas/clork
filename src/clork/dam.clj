(ns clork.dam
  "Dam/Reservoir area implementation.

   The Dam area contains the Flood Control Dam #3 puzzle, which controls
   water levels in the reservoir and affects multiple rooms.

   ZIL Reference:
   - DAM-ROOM-FCN in 1actions.zil (lines 1169-1198)
   - RESERVOIR-SOUTH-FCN in 1actions.zil (lines 1428-1455)
   - RESERVOIR-FCN in 1actions.zil (lines 1457-1475)
   - RESERVOIR-NORTH-FCN in 1actions.zil (lines 1477-1498)
   - I-RFILL daemon in 1actions.zil (lines 1239-1272)
   - I-REMPTY daemon in 1actions.zil (lines 1276-1295)
   - I-MAINT-ROOM daemon in 1actions.zil (lines 1356-1373)"
  (:require [clork.utils :as utils]
            [clork.game-state :as gs]
            [clork.parser.state :as parser-state]
            [clork.daemon :as daemon]
            [clork.death :as death]
            [clork.debug.trace :as trace]))

;;; ---------------------------------------------------------------------------
;;; FORWARD DECLARATIONS
;;; ---------------------------------------------------------------------------

(declare i-rfill i-rempty i-maint-room fix-maint-leak)

;;; ---------------------------------------------------------------------------
;;; CONSTANTS
;;; ---------------------------------------------------------------------------

(def drownings
  "Water level descriptions for maintenance room flooding.
   ZIL: DROWNINGS table (1actions.zil lines 1297-1306)"
  ["up to your ankles."
   "up to your shin."
   "up to your knees."
   "up to your hips."
   "up to your waist."
   "up to your chest."
   "up to your neck."
   "over your head."
   "high in your lungs."])

;;; ---------------------------------------------------------------------------
;;; ROOM ACTION FUNCTIONS
;;; ---------------------------------------------------------------------------

(defn dam-room-action
  "Room action for DAM-ROOM.
   ZIL: DAM-ROOM-FCN (1actions.zil lines 1169-1198)"
  [game-state rarg]
  (case rarg
    :look
    (let [low-tide? (:low-tide game-state)
          gates-open? (:gates-open game-state)
          gate-flag? (:gate-flag game-state)]
      (-> game-state
          (utils/tell "You are standing on the top of the Flood Control Dam #3, which was quite a tourist attraction in times far distant. There are paths to the north, south, and west, and a scramble down.")
          ;; Paragraph break between sections
          (utils/tell "\n")
          ;; Water level description based on state
          (cond->
           (and low-tide? gates-open?)
            (-> (utils/tell "The water level behind the dam is low: The sluice gates have been opened. Water rushes through the dam and downstream.")
                (utils/tell "\n"))

            (and gates-open? (not low-tide?))
            (-> (utils/tell "The sluice gates are open, and water rushes through the dam. The water level behind the dam is still high.")
                (utils/tell "\n"))

            (and low-tide? (not gates-open?))
            (-> (utils/tell "The sluice gates are closed. The water level in the reservoir is quite low, but the level is rising quickly.")
                (utils/tell "\n"))

            (and (not low-tide?) (not gates-open?))
            (-> (utils/tell "The sluice gates on the dam are closed. Behind the dam, there can be seen a wide reservoir. Water is pouring over the top of the now abandoned dam.")
                (utils/tell "\n")))
          ;; Control panel description
          (utils/tell "There is a control panel here, on which a large metal bolt is mounted. Directly above the bolt is a small green plastic bubble")
          (cond->
           gate-flag?
            (utils/tell " which is glowing serenely"))
          (utils/tell ".")
          ;; Paragraph break after room description
          (utils/tell "\n")))

    ;; Default - use default handling
    (gs/use-default game-state)))

(defn reservoir-south-action
  "Room action for RESERVOIR-SOUTH.
   ZIL: RESERVOIR-SOUTH-FCN (1actions.zil lines 1428-1455)"
  [game-state rarg]
  (case rarg
    :look
    (let [low-tide? (:low-tide game-state)
          gates-open? (:gates-open game-state)]
      (-> game-state
          ;; Main description based on water state
          (cond->
           (and low-tide? gates-open?)
            (utils/tell "You are in a long room, to the north of which was formerly a lake. However, with the water level lowered, there is merely a wide stream running through the center of the room.")

            (and gates-open? (not low-tide?))
            (utils/tell "You are in a long room. To the north is a large lake, too deep to cross. You notice, however, that the water level appears to be dropping at a rapid rate. Before long, it might be possible to cross to the other side from here.")

            (and low-tide? (not gates-open?))
            (utils/tell "You are in a long room, to the north of which is a wide area which was formerly a reservoir, but now is merely a stream. You notice, however, that the level of the stream is rising quickly and that before long it will be impossible to cross here.")

            (and (not low-tide?) (not gates-open?))
            (utils/tell "You are in a long room on the south shore of a large lake, far too deep and wide for crossing."))
          ;; Single newline before exits description (same paragraph)
          (utils/tell "\n")
          ;; Exits description
          (utils/tell "There is a path along the stream to the east or west, a steep pathway climbing southwest along the edge of a chasm, and a path leading into a canyon to the southeast.\n")))

    ;; Default
    (gs/use-default game-state)))

(defn reservoir-action
  "Room action for RESERVOIR.
   ZIL: RESERVOIR-FCN (1actions.zil lines 1457-1475)"
  [game-state rarg]
  (case rarg
    :look
    (let [low-tide? (:low-tide game-state)]
      (-> game-state
          (cond->
           low-tide?
            (utils/tell "You are on what used to be a large lake, but which is now a large mud pile. There are \"shores\" to the north and south.")

            (not low-tide?)
            (utils/tell "You are on the lake. Beaches can be seen north and south. Upstream a small stream enters the lake through a narrow cleft in the rocks. The dam can be seen downstream."))
          ;; Paragraph break after room description
          (utils/tell "\n")))

    ;; M-END: Warning about rising water
    :m-end
    (let [low-tide? (:low-tide game-state)
          gates-open? (:gates-open game-state)
          ;; TODO: Check if player is in boat (VEHBIT) - for now assume not
          in-boat? false]
      (if (and (not in-boat?) (not gates-open?) low-tide?)
        (-> game-state
            (utils/tell "You notice that the water level here is rising rapidly. The currents are also becoming stronger. Staying here seems quite perilous!")
            (utils/crlf))
        game-state))

    ;; Default
    (gs/use-default game-state)))

(defn reservoir-north-action
  "Room action for RESERVOIR-NORTH.
   ZIL: RESERVOIR-NORTH-FCN (1actions.zil lines 1477-1498)"
  [game-state rarg]
  (case rarg
    :look
    (let [low-tide? (:low-tide game-state)
          gates-open? (:gates-open game-state)]
      (-> game-state
          (cond->
           (and low-tide? gates-open?)
            (utils/tell "You are in a large cavernous room, the south of which was formerly a lake. However, with the water level lowered, there is merely a wide stream running through there.")

            (and gates-open? (not low-tide?))
            (utils/tell "You are in a large cavernous area. To the south is a wide lake, whose water level appears to be falling rapidly.")

            (and low-tide? (not gates-open?))
            (utils/tell "You are in a cavernous area, to the south of which is a very wide stream. The level of the stream is rising rapidly, and it appears that before long it will be impossible to cross to the other side.")

            (and (not low-tide?) (not gates-open?))
            (utils/tell "You are in a large cavernous room, north of a large lake."))
          ;; Paragraph break after main description
          (utils/tell "\n")
          (utils/tell "There is a slimy stairway leaving the room to the north.")
          ;; Paragraph break after room description
          (utils/tell "\n")))

    ;; Default
    (gs/use-default game-state)))

;;; ---------------------------------------------------------------------------
;;; OBJECT ACTION FUNCTIONS
;;; ---------------------------------------------------------------------------

(defn bolt-action
  "Object action for BOLT.
   ZIL: BOLT-F (1actions.zil lines 1200-1230)"
  [game-state]
  (let [prsa (parser-state/get-prsa game-state)
        prsi (parser-state/get-prsi game-state)
        gate-flag? (:gate-flag game-state)
        gates-open? (:gates-open game-state)]
    (cond
      ;; TURN verb - main interaction
      (= prsa :turn)
      (cond
        ;; Using wrench
        (= prsi :wrench)
        (if gate-flag?
          ;; Gate flag is set, bolt can turn
          (if gates-open?
            ;; Close the gates
            (-> game-state
                (gs/unset-game-flag :gates-open)
                (gs/unset-thing-flag :reservoir-south :touch)
                ;; Queue I-RFILL daemon to fill reservoir after 8 turns
                (daemon/register-daemon :i-rfill i-rfill :tick 8)
                (daemon/unregister-daemon :i-rempty)
                (utils/tell "The sluice gates close and water starts to collect behind the dam."))
            ;; Open the gates
            (-> game-state
                (gs/set-game-flag :gates-open)
                ;; Queue I-REMPTY daemon to empty reservoir after 8 turns
                (daemon/register-daemon :i-rempty i-rempty :tick 8)
                (daemon/unregister-daemon :i-rfill)
                (utils/tell "The sluice gates open and water pours through the dam.")))
          ;; Gate flag not set, bolt won't turn
          (utils/tell game-state "The bolt won't turn with your best effort."))

        ;; Using something else
        prsi
        (utils/tell game-state (str "The bolt won't turn using the " (gs/thing-name game-state prsi) "."))

        ;; No indirect object
        :else
        (utils/tell game-state "You'll need a tool to turn the bolt."))

      ;; TAKE verb
      (= prsa :take)
      (utils/tell game-state "It is an integral part of the control panel.")

      ;; Default - no special handling
      :else nil)))

(defn bubble-action
  "Object action for BUBBLE.
   ZIL: BUBBLE-F (1actions.zil lines 1232-1234)"
  [game-state]
  (let [prsa (parser-state/get-prsa game-state)]
    (cond
      (= prsa :take)
      (utils/tell game-state "It is an integral part of the control panel.")

      (= prsa :examine)
      (let [gate-flag? (:gate-flag game-state)]
        (if gate-flag?
          (utils/tell game-state "The green bubble is glowing serenely.")
          (utils/tell game-state "The green bubble is not glowing.")))

      :else nil)))

(defn button-action
  "Object action for colored buttons.
   ZIL: BUTTON-F (1actions.zil lines 1311-1343)"
  [game-state]
  (let [prsa (parser-state/get-prsa game-state)
        prso (parser-state/get-prso game-state)
        water-level (:water-level game-state)]
    (cond
      ;; READ verb
      (= prsa :read)
      (utils/tell game-state "They're greek to you.")

      ;; PUSH verb (mapped to :move in verb_defs.clj)
      (= prsa :move)
      (cond
        ;; Blue button - creates leak
        (= prso :blue-button)
        (if (zero? water-level)
          (-> game-state
              (gs/unset-thing-flag :leak :invisible)
              (assoc :water-level 1)
              (daemon/register-daemon :i-maint-room i-maint-room :tick -1)
              (utils/tell "There is a rumbling sound and a stream of water appears to burst from the east wall of the room (apparently, a leak has occurred in a pipe)."))
          (utils/tell game-state "The blue button appears to be jammed."))

        ;; Red button - toggles lights
        (= prso :red-button)
        (let [here (:here game-state)
              room-lit? (gs/set-thing-flag? game-state here :lit)]
          (if room-lit?
            (-> game-state
                (gs/unset-thing-flag here :lit)
                (utils/tell "The lights within the room shut off."))
            (-> game-state
                (gs/set-thing-flag here :lit)
                (utils/tell "The lights within the room come on."))))

        ;; Brown button - clears gate flag
        (= prso :brown-button)
        (-> game-state
            (gs/unset-game-flag :gate-flag)
            (gs/unset-thing-flag :dam-room :touch)
            (utils/tell "Click."))

        ;; Yellow button - sets gate flag
        (= prso :yellow-button)
        (-> game-state
            (gs/set-game-flag :gate-flag)
            (gs/unset-thing-flag :dam-room :touch)
            (utils/tell "Click."))

        :else nil)

      :else nil)))

(defn leak-action
  "Object action for LEAK.
   ZIL: LEAK-FUNCTION (1actions.zil lines 1375-1383)"
  [game-state]
  (let [prsa (parser-state/get-prsa game-state)
        prso (parser-state/get-prso game-state)
        prsi (parser-state/get-prsi game-state)
        water-level (:water-level game-state)]
    (when (pos? water-level)
      (cond
        ;; PUT putty on leak
        (and (#{:put :put-on} prsa) (= prso :putty))
        (fix-maint-leak game-state)

        ;; PLUG leak with putty
        (and (= prsa :plug) (= prsi :putty))
        (fix-maint-leak game-state)

        ;; PLUG with something else
        (= prsa :plug)
        (if prsi
          (utils/tell game-state (str "With a " (gs/thing-name game-state prsi) "?"))
          (utils/tell game-state "With what?"))

        :else nil))))

(defn- fix-maint-leak
  "Fix the leak in the maintenance room.
   ZIL: FIX-MAINT-LEAK (1actions.zil lines 1385-1390)"
  [game-state]
  (-> game-state
      (assoc :water-level -1)
      (daemon/unregister-daemon :i-maint-room)
      (utils/tell "By some miracle of Zorkian technology, you have managed to stop the leak in the dam.")))

(defn putty-action
  "Object action for PUTTY.
   ZIL: PUTTY-FCN (1actions.zil lines 1392-1397)"
  [game-state]
  (let [prsa (parser-state/get-prsa game-state)
        prso (parser-state/get-prso game-state)
        prsi (parser-state/get-prsi game-state)]
    (cond
      ;; OIL with putty or PUT putty (trying to use as oil)
      (or (and (= prsa :oil) (= prsi :putty))
          (and (= prsa :put) (= prso :putty) (= prsi :bolt)))
      (utils/tell game-state "The all-purpose gunk isn't a lubricant.")

      :else nil)))

(defn tube-action
  "Object action for TUBE.
   ZIL: TUBE-FUNCTION (1actions.zil lines 1399-1411)"
  [game-state]
  (let [prsa (parser-state/get-prsa game-state)
        prso (parser-state/get-prso game-state)
        prsi (parser-state/get-prsi game-state)
        tube-open? (gs/set-thing-flag? game-state :tube :open)
        putty-in-tube? (= (gs/get-thing-loc-id game-state :putty) :tube)]
    (cond
      ;; PUT something in tube
      (and (= prsa :put) (= prsi :tube))
      (utils/tell game-state "The tube refuses to accept anything.")

      ;; SQUEEZE tube
      (= prsa :squeeze)
      (cond
        (and tube-open? putty-in-tube?)
        (-> game-state
            (gs/move-object :putty :adventurer :squeeze-tube)
            (utils/tell "The viscous material oozes into your hand."))

        tube-open?
        (utils/tell game-state "The tube is apparently empty.")

        :else
        (utils/tell game-state "The tube is closed."))

      :else nil)))

(defn dam-obj-action
  "Object action for DAM object.
   ZIL: DAM-FUNCTION (1actions.zil lines 1413-1423)"
  [game-state]
  (let [prsa (parser-state/get-prsa game-state)
        prsi (parser-state/get-prsi game-state)]
    (cond
      ;; OPEN/CLOSE the dam
      (#{:open :close} prsa)
      (utils/tell game-state "Sounds reasonable, but this isn't how.")

      ;; PLUG the dam
      (= prsa :plug)
      (if (= prsi :hands)
        (utils/tell game-state "Are you the little Dutch boy, then? Sorry, this is a big dam.")
        (if prsi
          (utils/tell game-state (str "With a " (gs/thing-name game-state prsi) "? Do you know how big this dam is? You could only stop a tiny leak with that."))
          (utils/tell game-state "With what? This is a big dam.")))

      :else nil)))

;;; ---------------------------------------------------------------------------
;;; DAEMONS
;;; ---------------------------------------------------------------------------

(defn i-rfill
  "Daemon that fills the reservoir.
   ZIL: I-RFILL (1actions.zil lines 1239-1272)"
  [game-state]
  (let [here (:here game-state)]
    (-> game-state
        ;; Set reservoir to water room
        (gs/set-thing-flag :reservoir :rwater)
        ;; Clear touch flags for related rooms
        (gs/unset-thing-flag :reservoir-south :touch)
        (gs/unset-thing-flag :reservoir-north :touch)
        ;; Set low-tide to false (water is high)
        (gs/unset-game-flag :low-tide)
        ;; Disable the daemon (runs once)
        (daemon/unregister-daemon :i-rfill)
        ;; Location-specific messages
        (cond->
         (= here :reservoir)
          ;; Player is in reservoir - they drown!
          (death/jigs-up "You are lifted up by the rising river! You try to swim, but the currents are too strong. You come closer, closer to the awesome structure of Flood Control Dam #3. The dam beckons to you. The roar of the water nearly deafens you, but you remain conscious as you tumble over the dam toward your certain doom among the rocks at its base.")

          (#{:reservoir-north :reservoir-south} here)
          (-> (utils/tell "You notice that the water level has risen to the point that it is impossible to cross.")
              (utils/crlf))))))

(defn i-rempty
  "Daemon that empties the reservoir.
   ZIL: I-REMPTY (1actions.zil lines 1276-1295)"
  [game-state]
  (let [here (:here game-state)]
    (-> game-state
        ;; Set reservoir to land room
        (gs/unset-thing-flag :reservoir :rwater)
        ;; Clear touch flags so player sees new descriptions after water change
        ;; ZIL clears DEEP-CANYON and LOUD-ROOM, but we also need reservoir areas
        ;; to show their new water-state-dependent descriptions
        (gs/unset-thing-flag :reservoir-south :touch)
        (gs/unset-thing-flag :reservoir-north :touch)
        (gs/unset-thing-flag :deep-canyon :touch)
        (gs/unset-thing-flag :loud-room :touch)
        ;; Make trunk visible now that water is low (ZIL: FCLEAR TRUNK INVISIBLE)
        (gs/unset-thing-flag :trunk-of-jewels :invisible)
        ;; Set low-tide to true (water is low)
        (gs/set-game-flag :low-tide)
        ;; Disable the daemon (runs once)
        (daemon/unregister-daemon :i-rempty)
        ;; Location-specific messages
        (cond->
         (#{:reservoir-north :reservoir-south} here)
          (-> (utils/tell "The water level is now quite low here and you could easily cross over to the other side.")
              (utils/crlf))))))

(defn i-maint-room
  "Daemon that handles flooding in maintenance room.
   ZIL: I-MAINT-ROOM (1actions.zil lines 1356-1373)"
  [game-state]
  (let [here (:here game-state)
        water-level (:water-level game-state)
        in-maint? (= here :maintenance-room)]
    (if (neg? water-level)
      ;; Leak has been fixed, disable daemon
      (daemon/unregister-daemon game-state :i-maint-room)
      ;; Otherwise, increase water level
      (let [level-idx (min (quot water-level 2) (dec (count drownings)))
            new-water-level (inc water-level)
            gs (-> game-state
                   (cond->
                    in-maint?
                     (-> (utils/tell (str "The water level here is now " (get drownings level-idx)))
                         (utils/crlf)))
                   (assoc :water-level new-water-level))]
        (if (>= new-water-level 14)
          ;; Room is flooded
          (-> gs
              (daemon/unregister-daemon :i-maint-room)
              ;; TODO: MUNG-ROOM - make room unenterable
              (cond->
               in-maint?
                (death/jigs-up "I'm afraid you have done drowned yourself.")))
          gs)))))
