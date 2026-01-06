(ns clork.objects
  "Object definitions for Clork."
  (:require [clork.utils :as utils]
            [clork.flags :as flags]
            [clork.game-state :as gs]
            [clork.parser.state :as parser-state]
            [clork.verbs-look :as verbs-look]
            [clork.verbs-movement :as verbs-movement]
            [clork.random :as random]
            [clork.sword :as sword]
            [clork.thief :as thief]
            [clork.cyclops :as cyclops]
            [clork.dam :as dam]
            [clork.light :as light]
            [clork.death :as death]))

;; <OBJECT MAILBOX
;;	(IN WEST-OF-HOUSE)
;;	(SYNONYM MAILBOX BOX)
;;	(ADJECTIVE SMALL)
;;	(DESC "small mailbox")
;;	(FLAGS CONTBIT TRYTAKEBIT)
;;	(CAPACITY 10)
;;	(ACTION MAILBOX-F)>
;;
;; <ROUTINE MAILBOX-F ()
;; 	 <COND (<AND <VERB? TAKE> <EQUAL? ,PRSO ,MAILBOX>>
;; 		 <TELL "It is securely anchored." CR>)>>

(def mailbox
  {:id :mailbox
   :in :west-of-house
   :synonym ["mailbox" "box"]
   :adjective "small"
   :desc "small mailbox"
   :flags (flags/flags :cont :trytake)
   :capacity 10
   :action (fn [game-state]
             ;; Only handles :take verb - returns nil for other verbs
             ;; ZIL: <COND (<AND <VERB? TAKE> <EQUAL? ,PRSO ,MAILBOX>>
             (when (and (= (parser-state/get-prsa game-state) :take)
                        (= (parser-state/get-prso game-state) :mailbox))
               (utils/tell game-state "It is securely anchored.")))})

;; <OBJECT ADVERTISEMENT
;;   (IN MAILBOX)
;;   (SYNONYM ADVERTISEMENT LEAFLET BOOKLET MAIL)
;;   (ADJECTIVE SMALL)
;;   (DESC "leaflet")
;;   (FLAGS READBIT TAKEBIT BURNBIT)
;;   (LDESC "A small leaflet is on the ground.")
;;   (TEXT
;; "\"WELCOME TO ZORK!|
;; |
;; ZORK is a game of adventure, danger, and low cunning. In it you
;; will explore some of the most amazing territory ever seen by mortals.
;; No computer should be without one!\"")
;;   (SIZE 2)>

(def leaflet
  {:id :leaflet
   :in :mailbox
   :synonym ["advertisement" "leaflet" "booklet" "mail"]
   :adjective "small"
   :desc "leaflet"
   :flags (flags/flags :read :take :burn)
   :ldesc "A small leaflet is on the ground."
   :text "\"WELCOME TO ZORK!\n\nZORK is a game of adventure, danger, and low cunning. In it you will explore some of the most amazing territory ever seen by mortals. No computer should be without one!\""
   :size 2})

;; <OBJECT ADVENTURER
;; 	(SYNONYM ADVENTURER)
;; 	(DESC "cretin")
;; 	(FLAGS NDESCBIT INVISIBLE SACREDBIT ACTORBIT)
;; 	(STRENGTH 0)
;; 	(ACTION 0)>

(def adventurer
  {:id :adventurer
   :synonym ["adventurer"]
   :desc "cretin"
   :flags (flags/flags :ndesc :invisible :sacred :actor)
   :strength 0})

;;; ---------------------------------------------------------------------------
;;; GLOBAL OBJECTS (visible from multiple rooms)
;;; ---------------------------------------------------------------------------

;; <OBJECT WHITE-HOUSE
;;	(IN LOCAL-GLOBALS)
;;	(SYNONYM HOUSE)
;;	(ADJECTIVE WHITE BEAUTI COLONI)
;;	(DESC "white house")
;;	(FLAGS NDESCBIT)
;;	(ACTION WHITE-HOUSE-F)>
;;
;; ZIL: WHITE-HOUSE-F in 1actions.zil handles:
;;   WALK-AROUND: cycles through exterior rooms (west→north→east→south→west)
;;                or interior rooms (living→kitchen→attic→kitchen)
;;   THROUGH/OPEN: if at behind-house and window is open, enter kitchen

(def house-around
  "Cycle order for walking around the exterior of the house."
  [:west-of-house :north-of-house :behind-house :south-of-house :west-of-house])

(def in-house-around
  "Cycle order for walking around inside the house."
  [:living-room :kitchen :attic :kitchen])

(defn go-next
  "Find the next room in a cycle table from the current location.
   ZIL: GO-NEXT routine."
  [game-state table]
  (let [here (:here game-state)
        idx (.indexOf table here)]
    (when (>= idx 0)
      (get table (inc idx)))))

(defn- room-lit?
  "Check if a room is lit (has light source)."
  [game-state room-id]
  (let [room (gs/get-thing game-state room-id)
        room-flags (or (:flags room) #{})]
    (or (contains? room-flags :lit)
        (contains? room-flags :on))))

(defn- score-room
  "Score a room's value (if any) and set its value to 0.
   Rooms with a :value property award points on first entry."
  [game-state room-id]
  (let [room (gs/get-thing game-state room-id)
        room-value (get room :value 0)]
    (if (pos? room-value)
      (-> game-state
          (update :base-score + room-value)
          (update :score + room-value)
          (assoc-in [:rooms room-id :value] 0))
      game-state)))

(defn- move-to-room
  "Move the player to a new room and describe it.
   Similar to GOTO in verbs.clj but callable from objects."
  [game-state room-id]
  (let [winner (:winner game-state)
        ;; Move the winner to the new room
        gs (assoc-in game-state [:objects winner :in] room-id)
        ;; Update HERE
        gs (assoc gs :here room-id)
        ;; Update LIT flag
        gs (assoc gs :lit (room-lit? gs room-id))
        ;; Score the room (ZIL: SCORE-OBJ .RM)
        gs (score-room gs room-id)]
    ;; Describe the room (V-FIRST-LOOK)
    ;; ZIL: V-FIRST-LOOK only calls DESCRIBE-OBJECTS if room is lit
    (verbs-look/v-first-look gs)))

(def white-house
  {:id :white-house
   :in :local-globals  ; Visible from many rooms
   :synonym ["house"]
   :adjective ["white" "beautiful" "colonial"]
   :desc "white house"
   :flags (flags/flags :ndesc)
   :action (fn [game-state]
             (let [verb (parser-state/get-prsa game-state)
                   here (:here game-state)
                   inside? (contains? #{:kitchen :living-room :attic} here)
                   outside-house? (contains? #{:behind-house :west-of-house
                                               :north-of-house :south-of-house} here)]
               (cond
                 ;; WALK-AROUND: cycle through house rooms
                 (= verb :walk-around)
                 (let [table (if inside? in-house-around house-around)
                       next-room (go-next game-state table)]
                   (if next-room
                     ;; Move to next room in cycle
                     (move-to-room game-state next-room)
                     ;; Not at house, can't walk around it
                     (if outside-house?
                       (utils/tell game-state "Use compass directions for movement.")
                       (utils/tell game-state "You're not at the house."))))

                 ;; THROUGH/OPEN: enter the house
                 (= verb :through)
                 (cond
                   ;; At behind-house - can enter through window if open
                   (= here :behind-house)
                   (if (gs/set-thing-flag? game-state :kitchen-window :open)
                     ;; Window is open - go to kitchen
                     (move-to-room game-state :kitchen)
                     ;; Window is closed
                     (-> game-state
                         (utils/tell "The window is closed.")
                         (utils/this-is-it :kitchen-window)))

                   ;; Not at a place where you can enter the house
                   :else
                   (utils/tell game-state "I can't see how to get in from here."))

                 ;; Other verbs - not handled
                 :else nil)))})

;;; ---------------------------------------------------------------------------
;;; DOORS AND WINDOWS
;;; ---------------------------------------------------------------------------

;; <OBJECT KITCHEN-WINDOW
;;	(IN LOCAL-GLOBALS)
;;	(SYNONYM WINDOW)
;;	(ADJECTIVE KITCHEN SMALL)
;;	(DESC "kitchen window")
;;	(FLAGS DOORBIT NDESCBIT)
;;	(ACTION KITCHEN-WINDOW-F)>
;;
;; ZIL: KITCHEN-WINDOW-F in 1actions.zil handles:
;;   WALK/BOARD/THROUGH: walk through the window
;;     - From kitchen: walk east to behind-house
;;     - From outside: walk west to kitchen (if open)

;; ZIL: KITCHEN-WINDOW-F in 1actions.zil handles OPEN/CLOSE with custom messages
(def kitchen-window
  {:id :kitchen-window
   :in :local-globals  ; Global object, visible from both sides
   :synonym ["window"]
   :adjective ["kitchen" "small"]
   :desc "kitchen window"
   :flags (flags/flags :door :ndesc)  ; Starts closed (slightly ajar), not open
   :action (fn [game-state]
             (let [verb (parser-state/get-prsa game-state)
                   here (:here game-state)]
               (cond
                 ;; OPEN: custom message for opening the window
                 (= verb :open)
                 (if (gs/set-thing-flag? game-state :kitchen-window :open)
                   (utils/tell game-state "It is already open.")
                   (let [state (-> game-state
                                   (gs/set-thing-flag :kitchen-window :open)
                                   (gs/set-thing-flag :kitchen-window :touch))]
                     (utils/tell state "With great effort, you open the window far enough to allow entry.")))

                 ;; CLOSE: custom message for closing the window
                 (= verb :close)
                 (if (not (gs/set-thing-flag? game-state :kitchen-window :open))
                   (utils/tell game-state "It is already closed.")
                   (let [state (gs/unset-thing-flag game-state :kitchen-window :open)]
                     (utils/tell state "The window closes (more easily than it opened).")))

                 ;; EXAMINE: special message when window hasn't been touched
                 (= verb :examine)
                 (if (not (gs/set-thing-flag? game-state :kitchen-window :touch))
                   (utils/tell game-state "The window is slightly ajar, but not enough to allow entry.")
                   nil)  ; Let default examine handle it

                 ;; THROUGH: walk through the window
                 (= verb :through)
                 (cond
                   ;; From kitchen - go east to behind-house
                   (= here :kitchen)
                   (if (gs/set-thing-flag? game-state :kitchen-window :open)
                     (move-to-room game-state :behind-house)
                     (utils/tell game-state "The kitchen window is closed."))

                   ;; From behind-house - go west to kitchen
                   (= here :behind-house)
                   (if (gs/set-thing-flag? game-state :kitchen-window :open)
                     (move-to-room game-state :kitchen)
                     (utils/tell game-state "The kitchen window is closed."))

                   ;; Not near the window
                   :else
                   nil)

                 ;; Other verbs - not handled by window
                 :else nil)))})

;; <OBJECT SLIDE
;;     (IN LOCAL-GLOBALS)
;;     (SYNONYM CHUTE RAMP SLIDE)
;;     (ADJECTIVE STEEP METAL TWISTING)
;;     (DESC "chute")
;;     (FLAGS CLIMBBIT)
;;     (ACTION SLIDE-FUNCTION)>

(def ^:private yuks
  "Humorous responses for trying to do impossible things.
   ZIL: YUKS global in gverbs.zil"
  ["A valiant attempt."
   "You can't be serious."
   "An interesting idea..."
   "What a concept!"])

(defn slide-action
  "Action handler for the kitchen/cellar slide.

   ZIL: SLIDE-FUNCTION in 1actions.zil (lines 3099-3109)
   SLIDER routine (lines 3111-3117)

   The slide connects the kitchen chimney to the cellar.
   - THROUGH/CLIMB: From cellar goes west, otherwise tumble down to cellar
   - PUT object IN slide: object falls to cellar"
  [game-state]
  (let [prsa (parser-state/get-prsa game-state)
        prso (parser-state/get-prso game-state)
        here (:here game-state)]
    (cond
      ;; THROUGH, CLIMB variations, or PUT ME IN
      (or (contains? #{:through :climb-up :climb-down :climb} prsa)
          (and (= prsa :put) (= prso :adventurer)))
      (if (= here :cellar)
        ;; From cellar - can go west (back up)
        (-> game-state
            (assoc :here :kitchen)
            (utils/tell "You climb up the slide."))
        ;; From kitchen - tumble down
        (-> game-state
            (assoc :here :cellar)
            (utils/tell "You tumble down the slide....")))

      ;; PUT object IN slide
      (and (= prsa :put) (= (parser-state/get-prsi game-state) :slide))
      (let [obj (gs/get-thing game-state prso)
            takeable? (contains? (or (:flags obj) #{}) :take)]
        (if takeable?
          ;; Takeable objects fall to cellar
          (if (= prso :water)
            ;; Water disappears
            (-> game-state
                (assoc-in [:objects prso :in] :limbo)
                (utils/tell (str "The " (:desc obj) " falls into the slide and is gone.")))
            ;; Other items end up in cellar
            (-> game-state
                (assoc-in [:objects prso :in] :cellar)
                (utils/tell (str "The " (:desc obj) " falls into the slide and is gone."))))
          ;; Not takeable - humor response
          (utils/tell game-state (random/rand-nth* yuks))))

      ;; Default
      :else nil)))

(def slide
  {:id :slide
   :in :local-globals  ; Present in kitchen and cellar
   :synonym ["chute" "ramp" "slide"]
   :adjective ["steep" "metal" "twisting"]
   :desc "chute"
   :flags (flags/flags :climb)
   :action slide-action})

;;; ---------------------------------------------------------------------------
;;; CHIMNEY OBJECT
;;; ---------------------------------------------------------------------------

;; ZIL: <OBJECT CHIMNEY
;;     (IN LOCAL-GLOBALS)
;;     (SYNONYM CHIMNEY)
;;     (DESC "chimney")
;;     (FLAGS CLIMBBIT NDESCBIT)>
;;
;; The chimney is present in Studio and Kitchen. In Studio, "up chimney" goes
;; to kitchen via up-chimney-function. In Kitchen, the slide already handles
;; going down.

(defn chimney-action
  "Action handler for the chimney in studio/kitchen.

   ZIL: CHIMNEY-F in 1actions.zil (lines 558-564)
   Only handles EXAMINE - prints direction info.
   Climbing is handled by room exits:
   - Studio UP: uses UP-CHIMNEY-FUNCTION per function
   - Kitchen DOWN: blocked with 'Only Santa Claus' message"
  [game-state]
  (let [prsa (parser-state/get-prsa game-state)
        here (:here game-state)]
    (cond
      ;; EXAMINE - describe the chimney direction
      (= prsa :examine)
      (let [direction (if (= here :kitchen) "down" "up")]
        (utils/tell game-state (str "The chimney leads " direction "ward, and looks climbable.")))

      ;; Default - let room exits or other handlers deal with climbing
      :else nil)))

(def chimney
  {:id :chimney
   :in :local-globals  ; Present in studio and kitchen
   :synonym ["chimney" "fireplace"]
   :adjective ["dark" "narrow"]
   :desc "chimney"
   :flags (flags/flags :climb :ndesc)
   :action chimney-action})

;;; ---------------------------------------------------------------------------
;;; SAND/BEACH OBJECTS
;;; ---------------------------------------------------------------------------

;; <OBJECT SAND
;;     (IN SANDY-CAVE)
;;     (SYNONYM SAND)
;;     (DESC "sand")
;;     (FLAGS NDESCBIT)
;;     (ACTION SAND-FUNCTION)>

(def ^:private dig-messages
  "Progressive messages when digging in sand.
   ZIL: BDIGS table in 1actions.zil"
  ["You seem to be digging a hole here."
   "The hole is getting deeper, but that's about it."
   "You are surrounded by a wall of sand on all sides."])

(defn sand-action
  "Action handler for the sand in sandy cave.

   ZIL: SAND-FUNCTION in 1actions.zil (lines 2868-2882)

   Dig in the sand with a shovel to find the scarab:
   - Dig 1-2: Progressive messages
   - Dig 3: Reveal scarab if not already taken
   - Dig 4+: Hole collapses, killing player"
  [game-state]
  (let [prsa (parser-state/get-prsa game-state)
        prsi (parser-state/get-prsi game-state)]
    (cond
      ;; DIG with shovel
      (and (= prsa :dig) (= prsi :shovel))
      (let [beach-dig (get game-state :beach-dig 0)
            new-dig (inc beach-dig)]
        (cond
          ;; Dig 4+: collapse!
          (> new-dig 3)
          (let [gs (-> game-state
                       (assoc :beach-dig 0))]
            ;; If scarab still here and visible, hide it again
            (death/jigs-up gs "The hole collapses, smothering you."))

          ;; Dig 3: reveal scarab
          (= new-dig 3)
          (let [scarab-invisible? (gs/set-thing-flag? game-state :jeweled-scarab :invisible)]
            (if scarab-invisible?
              (-> game-state
                  (assoc :beach-dig new-dig)
                  (gs/unset-thing-flag :jeweled-scarab :invisible)
                  (utils/tell "You can see a scarab here in the sand."))
              (-> game-state
                  (assoc :beach-dig new-dig)
                  (utils/tell (nth dig-messages 2)))))

          ;; Dig 1-2: progressive message
          :else
          (-> game-state
              (assoc :beach-dig new-dig)
              (utils/tell (nth dig-messages (dec new-dig))))))

      ;; Default
      :else nil)))

(def sand
  {:id :sand
   :in :sandy-cave
   :synonym ["sand"]
   :desc "sand"
   :flags (flags/flags :ndesc)
   :action sand-action})

;; <OBJECT SCARAB
;;     (IN SANDY-CAVE)
;;     (SYNONYM SCARAB BUG BEETLE TREASURE)
;;     (ADJECTIVE BEAUTI CARVED JEWELED)
;;     (DESC "beautiful jeweled scarab")
;;     (FLAGS TAKEBIT INVISIBLE)
;;     (SIZE 8)
;;     (VALUE 5)
;;     (TVALUE 5)>

(def jeweled-scarab
  {:id :jeweled-scarab
   :in :sandy-cave
   :synonym ["scarab" "bug" "beetle" "treasure"]
   :adjective ["beautiful" "carved" "jeweled"]
   :desc "beautiful jeweled scarab"
   :flags (flags/flags :take :invisible)  ; Hidden until dug up
   :size 8
   :value 5
   :tvalue 5})

;; <OBJECT SHOVEL
;;     (IN SANDY-BEACH)
;;     (SYNONYM SHOVEL TOOL TOOLS)
;;     (DESC "shovel")
;;     (FLAGS TAKEBIT TOOLBIT)
;;     (SIZE 15)>

(def shovel
  {:id :shovel
   :in :sandy-beach
   :synonym ["shovel" "tool" "tools"]
   :desc "shovel"
   :flags (flags/flags :take :tool)
   :size 15})

;; <OBJECT TRAP-DOOR
;;	(SYNONYM DOOR TRAPDOOR TRAP-DOOR COVER)
;;	(ADJECTIVE TRAP DUSTY)
;;	(DESC "trap door")
;;	(FLAGS DOORBIT NDESCBIT INVISIBLE)
;;	(ACTION TRAP-DOOR-FCN)>
;;
;; <ROUTINE TRAP-DOOR-FCN ()
;;     <COND (<VERB? RAISE>
;; 	   <PERFORM ,V?OPEN ,TRAP-DOOR>
;; 	   <RTRUE>)
;; 	  (<AND <VERB? OPEN CLOSE>
;; 		<EQUAL? ,HERE ,LIVING-ROOM>>
;; 	   <OPEN-CLOSE ,PRSO
;; "The door reluctantly opens to reveal a rickety staircase descending into
;; darkness."
;; "The door swings shut and closes.">)
;; 	  (<AND <VERB? LOOK-UNDER> <EQUAL? ,HERE LIVING-ROOM>>
;; 	   <COND (<FSET? ,TRAP-DOOR ,OPENBIT>
;; 		  <TELL
;; "You see a rickety staircase descending into darkness." CR>)
;; 		 (T <TELL "It's closed." CR>)>)
;; 	  (<EQUAL? ,HERE ,CELLAR>
;; 	   <COND (<AND <VERB? OPEN UNLOCK>
;; 		       <NOT <FSET? ,TRAP-DOOR ,OPENBIT>>>
;; 		  <TELL
;; "The door is locked from above." CR>)
;; 		 (<AND <VERB? CLOSE> <NOT <FSET? ,TRAP-DOOR ,OPENBIT>>>
;; 		  <FCLEAR ,TRAP-DOOR ,TOUCHBIT>
;; 		  <FCLEAR ,TRAP-DOOR ,OPENBIT>
;; 		  <TELL "The door closes and locks." CR>)
;; 		 (<VERB? OPEN CLOSE>
;; 		  <TELL <PICK-ONE ,DUMMY> CR>)>)>>

(def trap-door
  {:id :trap-door
   :in :living-room
   :synonym ["door" "trapdoor" "trap-door" "trap" "cover"]
   :adjective ["trap" "dusty"]
   :desc "trap door"
   :flags (flags/flags :door :ndesc :invisible)  ; Starts closed, hidden under rug
   :action (fn [game-state]
             (let [verb (parser-state/get-prsa game-state)
                   here (:here game-state)
                   is-open? (gs/set-thing-flag? game-state :trap-door :open)]
               (cond
                 ;; RAISE -> same as OPEN
                 ;; ZIL: OPEN-CLOSE sets OPENBIT but NOT TOUCHBIT
                 ;; TOUCHBIT is set by CELLAR-FCN when door crashes shut on first descent
                 (= verb :raise)
                 (if is-open?
                   (utils/tell game-state "It is already open.")
                   (-> game-state
                       (gs/set-thing-flag :trap-door :open)
                       (utils/tell "The door reluctantly opens to reveal a rickety staircase descending into darkness.")))

                 ;; OPEN/CLOSE from living-room
                 (and (#{:open :close} verb) (= here :living-room))
                 (cond
                   ;; OPEN
                   ;; ZIL: OPEN-CLOSE sets OPENBIT but NOT TOUCHBIT
                   (= verb :open)
                   (if is-open?
                     (utils/tell game-state "It is already open.")
                     (-> game-state
                         (gs/set-thing-flag :trap-door :open)
                         (utils/tell "The door reluctantly opens to reveal a rickety staircase descending into darkness.")))
                   ;; CLOSE
                   (= verb :close)
                   (if is-open?
                     (-> game-state
                         (gs/unset-thing-flag :trap-door :open)
                         (utils/tell "The door swings shut and closes."))
                     (utils/tell game-state "It is already closed.")))

                 ;; LOOK-UNDER from living-room
                 (and (= verb :look-under) (= here :living-room))
                 (if is-open?
                   (utils/tell game-state "You see a rickety staircase descending into darkness.")
                   (utils/tell game-state "It's closed."))

                 ;; From cellar
                 (= here :cellar)
                 (cond
                   ;; OPEN/UNLOCK when closed -> locked from above
                   (and (#{:open :unlock} verb) (not is-open?))
                   (utils/tell game-state "The door is locked from above.")

                   ;; CLOSE when closed -> closes and locks
                   (and (= verb :close) (not is-open?))
                   (-> game-state
                       (gs/unset-thing-flag :trap-door :touch)
                       (gs/unset-thing-flag :trap-door :open)
                       (utils/tell "The door closes and locks."))

                   ;; OPEN/CLOSE otherwise -> dummy message
                   (#{:open :close} verb)
                   (utils/tell game-state (random/rand-nth* ["Look around."
                                                             "Too late for that."
                                                             "Have your eyes checked."]))

                   ;; Not handled
                   :else nil)

                 ;; Not handled
                 :else nil)))})

;;; ---------------------------------------------------------------------------
;;; KITCHEN OBJECTS
;;; ---------------------------------------------------------------------------

;; <OBJECT KITCHEN-TABLE
;;	(IN KITCHEN)
;;	(SYNONYM TABLE)
;;	(ADJECTIVE KITCHEN)
;;	(DESC "kitchen table")
;;	(FLAGS NDESCBIT CONTBIT OPENBIT SURFACEBIT)
;;	(CAPACITY 50)>

(def kitchen-table
  {:id :kitchen-table
   :in :kitchen
   :synonym ["table"]
   :adjective ["kitchen"]
   :desc "kitchen table"
   :flags (flags/flags :ndesc :cont :open :surface)
   :capacity 50})

;; <OBJECT SANDWICH-BAG
;;	(IN KITCHEN-TABLE)
;;	(SYNONYM BAG SACK)
;;	(ADJECTIVE BROWN ELONGATED SMELLY)
;;	(DESC "brown sack")
;;	(FLAGS TAKEBIT CONTBIT BURNBIT)
;;	(FDESC "On the table is an elongated brown sack, smelling of hot peppers.")
;;	(CAPACITY 9)
;;	(SIZE 9)
;;	(ACTION SANDWICH-BAG-FCN)>

(def brown-sack
  {:id :brown-sack
   :in :kitchen-table
   :synonym ["bag" "sack"]
   :adjective ["brown" "elongated" "smelly"]
   :desc "brown sack"
   :flags (flags/flags :take :cont :burn)
   :fdesc "On the table is an elongated brown sack, smelling of hot peppers."
   :capacity 9
   :size 9})

;; <OBJECT LUNCH
;;	(IN SANDWICH-BAG)
;;	(SYNONYM FOOD SANDWICH LUNCH DINNER)
;;	(ADJECTIVE HOT PEPPER)
;;	(DESC "lunch")
;;	(FLAGS TAKEBIT FOODBIT)
;;	(LDESC "A hot pepper sandwich is here.")>

(def lunch
  {:id :lunch
   :in :brown-sack
   :synonym ["food" "sandwich" "lunch" "dinner"]
   :adjective ["hot" "pepper"]
   :desc "lunch"
   :flags (flags/flags :take :food)
   :ldesc "A hot pepper sandwich is here."})

;; <OBJECT GARLIC
;;	(IN SANDWICH-BAG)
;;	(SYNONYM GARLIC CLOVE)
;;	(DESC "clove of garlic")
;;	(FLAGS TAKEBIT FOODBIT)
;;	(ACTION GARLIC-F)
;;	(SIZE 4)>

(def garlic
  {:id :garlic
   :in :brown-sack
   :synonym ["garlic" "clove"]
   :desc "clove of garlic"
   :flags (flags/flags :take :food)
   :size 4})

;; <OBJECT BOTTLE
;;	(IN KITCHEN-TABLE)
;;	(SYNONYM BOTTLE CONTAINER)
;;	(ADJECTIVE CLEAR GLASS)
;;	(DESC "glass bottle")
;;	(FLAGS TAKEBIT TRANSBIT CONTBIT)
;;	(ACTION BOTTLE-FUNCTION)
;;	(FDESC "A bottle is sitting on the table.")
;;	(CAPACITY 4)>

(defn bottle-action
  "Bottle action handler - handles throwing, breaking, shaking.

   ZIL: BOTTLE-FUNCTION (1actions.zil lines 1504-1520)

   - THROW: shatters bottle, spills water
   - MUNG: destroys bottle, spills water
   - SHAKE when open with water: spills water"
  [game-state]
  (let [prsa (parser-state/get-prsa game-state)
        prso (parser-state/get-prso game-state)
        has-water? (= (gs/get-thing-loc-id game-state :water) :bottle)
        is-open? (gs/set-thing-flag? game-state :bottle :open)]
    (cond
      ;; THROW bottle - shatters it
      (and (= prsa :throw) (= prso :bottle))
      (let [gs (-> game-state
                   (assoc-in [:objects :bottle :in] :limbo)
                   (utils/tell "The bottle hits the far wall and shatters."))]
        (if has-water?
          (-> gs
              (assoc-in [:objects :water :in] :limbo)
              (utils/tell " The water spills to the floor and evaporates."))
          gs))

      ;; MUNG/DESTROY bottle
      (and (= prsa :mung) (= prso :bottle))
      (let [gs (-> game-state
                   (assoc-in [:objects :bottle :in] :limbo)
                   (utils/tell "A brilliant maneuver destroys the bottle."))]
        (if has-water?
          (-> gs
              (assoc-in [:objects :water :in] :limbo)
              (utils/tell " The water spills to the floor and evaporates."))
          gs))

      ;; SHAKE open bottle with water - spills water
      (and (= prsa :shake) is-open? has-water?)
      (-> game-state
          (assoc-in [:objects :water :in] :limbo)
          (utils/tell "The water spills to the floor and evaporates."))

      ;; Not handled
      :else nil)))

(def bottle
  {:id :bottle
   :in :kitchen-table
   :synonym ["bottle" "container"]
   :adjective ["clear" "glass"]
   :desc "glass bottle"
   :flags (flags/flags :take :cont :trans)
   :fdesc "A bottle is sitting on the table."
   :capacity 4
   :action bottle-action})

;; <OBJECT WATER
;;	(IN BOTTLE)
;;	(SYNONYM WATER QUANTITY LIQUID H2O)
;;	(DESC "quantity of water")
;;	(FLAGS TRYTAKEBIT TAKEBIT DRINKBIT)
;;	(ACTION WATER-F)
;;	(SIZE 4)>

(def water
  {:id :water
   :in :bottle
   :synonym ["water" "quantity" "liquid" "h2o"]
   :desc "quantity of water"
   :flags (flags/flags :trytake :take :drink)
   :size 4})

;; <OBJECT LANTERN
;;	(IN LIVING-ROOM)
;;	(SYNONYM LAMP LANTERN LIGHT)
;;	(ADJECTIVE BRASS)
;;	(DESC "brass lantern")
;;	(FLAGS TAKEBIT LIGHTBIT)
;;	(ACTION LANTERN-F)
;;	(SIZE 15)>

;; ZIL: (FDESC "A battery-powered brass lantern is on the trophy case.")
;;      (LDESC "There is a brass lantern (battery-powered) here.")
(def brass-lantern
  {:id :brass-lantern
   :in :living-room
   :synonym ["lamp" "lantern" "light"]
   :adjective ["brass"]
   :desc "brass lantern"
   :flags (flags/flags :take :light)
   :fdesc "A battery-powered brass lantern is on the trophy case."
   :ldesc "There is a brass lantern (battery-powered) here."
   :size 15
   :action light/lantern-action})

;;; ---------------------------------------------------------------------------
;;; LIVING ROOM OBJECTS
;;; ---------------------------------------------------------------------------

;; <OBJECT TROPHY-CASE
;;	(IN LIVING-ROOM)
;;	(SYNONYM CASE)
;;	(ADJECTIVE TROPHY)
;;	(DESC "trophy case")
;;	(FLAGS TRANSBIT CONTBIT NDESCBIT TRYTAKEBIT SEARCHBIT)
;;	(ACTION TROPHY-CASE-FCN)
;;	(CAPACITY 10000)>

;; ZIL: TROPHY-CASE-FCN in 1actions.zil (lines 455-458)
;; <ROUTINE TROPHY-CASE-FCN ()
;;     <COND (<AND <VERB? TAKE> <EQUAL? ,PRSO ,TROPHY-CASE>>
;;            <TELL "The trophy case is securely fastened to the wall." CR>)>>

(def trophy-case
  {:id :trophy-case
   :in :living-room
   :synonym ["case"]
   :adjective ["trophy"]
   :desc "trophy case"
   :flags (flags/flags :cont :ndesc :trans :trytake :search)
   :capacity 10000
   :action (fn [game-state]
             ;; ZIL: TROPHY-CASE-FCN - only handles TAKE verb
             (when (and (= (parser-state/get-prsa game-state) :take)
                        (= (parser-state/get-prso game-state) :trophy-case))
               (utils/tell game-state "The trophy case is securely fastened to the wall.")))})

;; <OBJECT SWORD
;;	(IN LIVING-ROOM)
;;	(SYNONYM SWORD ORCRIST GLAMDRING BLADE)
;;	(ADJECTIVE ELVISH OLD ANTIQUE)
;;	(DESC "sword")
;;	(FLAGS TAKEBIT WEAPONBIT TRYTAKEBIT)
;;	(ACTION SWORD-FCN)
;;	(FDESC "Above the trophy case hangs an elvish sword of great antiquity.")
;;	(SIZE 30)
;;	(TVALUE 0)>
(def sword-obj
  {:id :sword
   :in :living-room
   :synonym ["sword" "orcrist" "glamdring" "blade"]
   :adjective ["elvish" "old" "antique"]
   :desc "sword"
   :flags (flags/flags :take :trytake :weapon)
   :fdesc "Above the trophy case hangs an elvish sword of great antiquity."
   :size 30
   :tvalue 0
   :action sword/sword-action})

;; <OBJECT RUG
;;	(IN LIVING-ROOM)
;;	(SYNONYM RUG CARPET)
;;	(ADJECTIVE LARGE ORIENTAL)
;;	(DESC "carpet")
;;	(FLAGS NDESCBIT TRYTAKEBIT)
;;	(ACTION RUG-FCN)>
;;
;; <ROUTINE RUG-FCN ()
;;    <COND (<VERB? RAISE>
;;           <TELL "The rug is too heavy to lift">
;;           <COND (,RUG-MOVED <TELL "." CR>)
;;                 (T <TELL ", but in trying to take it you have
;; noticed an irregularity beneath it." CR>)>)
;;          (<VERB? MOVE PUSH>
;;           <COND (,RUG-MOVED
;;                  <TELL "Having moved the carpet previously, you find it
;; impossible to move it again." CR>)
;;                 (T
;;                  <TELL "With a great effort, the rug is moved to one side
;; of the room, revealing the dusty cover of a closed trap door." CR>
;;                  <FCLEAR ,TRAP-DOOR ,INVISIBLE>
;;                  <THIS-IS-IT ,TRAP-DOOR>
;;                  <SETG RUG-MOVED T>)>)
;;          (<VERB? TAKE>
;;           <TELL "The rug is extremely heavy and cannot be carried." CR>)
;;          (<AND <VERB? LOOK-UNDER>
;;                <NOT ,RUG-MOVED>
;;                <NOT <FSET? ,TRAP-DOOR ,OPENBIT>>>
;;           <TELL "Underneath the rug is a closed trap door. As you drop
;; the corner of the rug, the trap door is once again concealed from view." CR>)>>

(def rug
  {:id :rug
   :in :living-room
   :synonym ["rug" "carpet"]
   :adjective ["large" "oriental"]
   :desc "large oriental rug"
   :flags (flags/flags :ndesc :trytake)
   :action (fn [game-state]
             (let [verb (parser-state/get-prsa game-state)
                   rug-moved? (get game-state :rug-moved false)]
               (cond
                 ;; RAISE verb
                 (= verb :raise)
                 (if rug-moved?
                   (utils/tell game-state "The rug is too heavy to lift.")
                   (utils/tell game-state "The rug is too heavy to lift, but in trying to take it you have noticed an irregularity beneath it."))

                 ;; MOVE/PUSH verb
                 (= verb :move)
                 (if rug-moved?
                   (utils/tell game-state "Having moved the carpet previously, you find it impossible to move it again.")
                   (-> game-state
                       (assoc :rug-moved true)
                       (gs/unset-thing-flag :trap-door :invisible)
                       (utils/this-is-it :trap-door)
                       (utils/tell "With a great effort, the rug is moved to one side of the room, revealing the dusty cover of a closed trap door.")))

                 ;; TAKE verb
                 (= verb :take)
                 (utils/tell game-state "The rug is extremely heavy and cannot be carried.")

                 ;; LOOK-UNDER verb (if rug not moved and trap door not open)
                 (and (= verb :look-under)
                      (not rug-moved?)
                      (not (gs/set-thing-flag? game-state :trap-door :open)))
                 (utils/tell game-state "Underneath the rug is a closed trap door. As you drop the corner of the rug, the trap door is once again concealed from view.")

                 ;; Not handled by rug
                 :else nil)))})

;;; ---------------------------------------------------------------------------
;;; ATTIC OBJECTS
;;; ---------------------------------------------------------------------------

;; <OBJECT ATTIC-TABLE
;;	(IN ATTIC)
;;	(SYNONYM TABLE)
;;	(DESC "table")
;;	(FLAGS NDESCBIT CONTBIT OPENBIT SURFACEBIT)
;;	(CAPACITY 40)>

(def attic-table
  {:id :attic-table
   :in :attic
   :synonym ["table"]
   :desc "table"
   :flags (flags/flags :ndesc :cont :open :surface)
   :capacity 40})

;; <OBJECT ROPE
;;	(IN ATTIC)
;;	(SYNONYM ROPE HEMP COIL)
;;	(ADJECTIVE LARGE)
;;	(DESC "rope")
;;	(FLAGS TAKEBIT SACREDBIT TRYTAKEBIT)
;;	(ACTION ROPE-FUNCTION)
;;	(FDESC "A large coil of rope is lying in the corner.")
;;	(SIZE 10)>

(defn rope-action
  "Rope action handler - handles tying to railing, climbing, untying.

   ZIL: ROPE-FUNCTION (1actions.zil lines 3043-3091)

   - TIE to RAILING (in Dome Room): ties rope, enables descent
   - CLIMB-DOWN when tied: move down to Torch Room
   - UNTIE: unties the rope
   - DROP (in Dome Room): drops rope to floor below
   - TAKE when tied: rope is tied to railing
   - TIE-UP actor: various messages"
  [game-state]
  (let [prsa (parser-state/get-prsa game-state)
        prso (parser-state/get-prso game-state)
        prsi (parser-state/get-prsi game-state)
        here (:here game-state)
        dome-flag (get game-state :dome-flag false)]
    (cond
      ;; Not in Dome Room - clear flag, reject TIE
      (not= here :dome-room)
      (cond
        (= prsa :tie)
        (-> game-state
            (assoc :dome-flag false)
            (utils/tell "You can't tie the rope to that."))
        :else nil)

      ;; TIE rope to railing
      (and (= prsa :tie) (= prsi :railing))
      (if dome-flag
        (utils/tell game-state "The rope is already tied to it.")
        (-> game-state
            (assoc :dome-flag true)
            (gs/set-thing-flag :rope :ndesc)
            ;; Move rope to dome room if not already there
            (assoc-in [:objects :rope :in] :dome-room)
            (utils/tell "The rope drops over the side and comes within ten feet of the floor.")))

      ;; CLIMB-DOWN the rope when tied
      (and (= prsa :climb-down) (or (= prso :rope) (nil? prso)) dome-flag)
      (-> game-state
          (assoc :here :torch-room)
          (utils/tell "Hand over hand you climb down the rope."))

      ;; TIE-UP an actor with rope
      (and (= prsa :tie-up) (= prsi :rope))
      (let [target (gs/get-thing game-state prso)
            is-actor? (gs/set-thing-flag? game-state prso :actor)
            strength (get target :strength 0)]
        (cond
          (and is-actor? (neg? strength))
          (utils/tell game-state (str "Your attempt to tie up the " (:desc target) " awakens him."))
          is-actor?
          (utils/tell game-state (str "The " (:desc target) " struggles and you cannot tie him up."))
          :else
          (utils/tell game-state (str "Why would you tie up a " (:desc target) "?"))))

      ;; UNTIE the rope
      (= prsa :untie)
      (if dome-flag
        (-> game-state
            (assoc :dome-flag false)
            (gs/unset-thing-flag :rope :ndesc)
            (utils/tell "The rope is now untied."))
        (utils/tell game-state "It is not tied to anything."))

      ;; DROP rope in Dome Room when not tied
      (and (= prsa :drop) (= here :dome-room) (not dome-flag))
      (-> game-state
          (assoc-in [:objects :rope :in] :torch-room)
          (utils/tell "The rope drops gently to the floor below."))

      ;; TAKE rope when tied
      (and (= prsa :take) dome-flag)
      (utils/tell game-state "The rope is tied to the railing.")

      ;; Not handled
      :else nil)))

(def rope
  {:id :rope
   :in :attic
   :synonym ["rope" "hemp" "coil"]
   :adjective ["large"]
   :desc "rope"
   :flags (flags/flags :take :sacred :trytake)
   :fdesc "A large coil of rope is lying in the corner."
   :size 10
   :action rope-action})

;; ZIL: <OBJECT RAILING (IN DOME-ROOM) ...>
(def railing
  {:id :railing
   :in :dome-room
   :synonym ["railing" "rail"]
   :adjective ["wooden"]
   :desc "wooden railing"
   :flags (flags/flags :ndesc)})

;; <OBJECT KNIFE
;;	(IN ATTIC-TABLE)
;;	(SYNONYM KNIVES KNIFE BLADE)
;;	(ADJECTIVE NASTY UNRUSTY)
;;	(DESC "nasty knife")
;;	(FLAGS TAKEBIT WEAPONBIT TRYTAKEBIT)
;;	(FDESC "On a table is a nasty-looking knife.")
;;	(ACTION KNIFE-F)>

(def knife
  {:id :knife
   :in :attic-table
   :synonym ["knives" "knife" "blade"]
   :adjective ["nasty" "unrusty"]
   :desc "nasty knife"
   :flags (flags/flags :take :weapon :trytake)
   :fdesc "On a table is a nasty-looking knife."})

;;; ---------------------------------------------------------------------------
;;; GALLERY OBJECTS
;;; ---------------------------------------------------------------------------

;; <OBJECT PAINTING
;;	(IN GALLERY)
;;	(SYNONYM PAINTING ART CANVAS TREASURE)
;;	(ADJECTIVE BEAUTI)
;;	(DESC "painting")
;;	(FLAGS TAKEBIT BURNBIT)
;;	(ACTION PAINTING-FCN)
;;	(FDESC "Fortunately, there is still one chance for you to be a vandal, for on
;;	        the far wall is a painting of unparalleled beauty.")
;;	(LDESC "A painting by a neglected genius is here.")
;;	(SIZE 15)
;;	(VALUE 4)
;;	(TVALUE 6)>

(defn painting-action
  "Painting action handler - handles vandalism.

   ZIL: PAINTING-FCN (1actions.zil lines 2220-2227)

   - MUNG: destroys the painting, sets tvalue to 0"
  [game-state]
  (let [prsa (parser-state/get-prsa game-state)]
    (when (= prsa :mung)
      (-> game-state
          (assoc-in [:objects :painting :tvalue] 0)
          (assoc-in [:objects :painting :ldesc] "There is a worthless piece of canvas here.")
          (utils/tell "Congratulations! Unlike the other vandals, who merely stole the artist's masterpieces, you have destroyed one.")))))

(def painting
  {:id :painting
   :in :gallery
   :synonym ["painting" "art" "canvas" "treasure"]
   :adjective ["beautiful"]
   :desc "painting"
   :flags (flags/flags :take :burn)
   :fdesc "Fortunately, there is still one chance for you to be a vandal, for on the far wall is a painting of unparalleled beauty."
   :ldesc "A painting by a neglected genius is here."
   :size 15
   :value 4
   :tvalue 6
   :action painting-action})

;;; ---------------------------------------------------------------------------
;;; TREASURES
;;; ---------------------------------------------------------------------------

;; <OBJECT COFFIN
;;	(IN EGYPT-ROOM)
;;	(SYNONYM COFFIN CASKET TREASURE)
;;	(ADJECTIVE GOLD SOLID)
;;	(DESC "gold coffin")
;;	(FLAGS TAKEBIT CONTBIT SACREDBIT SEARCHBIT)
;;	(LDESC "The solid-gold coffin used for the burial of Ramses II is here.")
;;	(CAPACITY 35)
;;	(SIZE 55)
;;	(VALUE 10)
;;	(TVALUE 15)>

(def gold-coffin
  {:id :gold-coffin
   :in :egypt-room
   :synonym ["coffin" "casket" "treasure"]
   :adjective ["gold" "solid"]
   :desc "gold coffin"
   :flags (flags/flags :take :cont :sacred :search)
   :ldesc "The solid-gold coffin used for the burial of Ramses II is here."
   :capacity 35
   :size 55
   :value 10
   :tvalue 15})

;; <OBJECT SCEPTRE
;;	(IN COFFIN)
;;	(SYNONYM SCEPTRE SCEPTER TREASURE)
;;	(ADJECTIVE SHARP EGYPTIAN ANCIENT ENAMELED)
;;	(DESC "sceptre")
;;	(FLAGS TAKEBIT WEAPONBIT)
;;	(ACTION SCEPTRE-FUNCTION)
;;	(LDESC "An ornamented sceptre, tapering to a sharp point, is here.")
;;	(FDESC "A sceptre, possibly that of ancient Egypt itself, is in the coffin.
;;          The sceptre is ornamented with colored enamel, and tapers to a sharp point.")
;;	(SIZE 3)
;;	(VALUE 4)
;;	(TVALUE 6)>

(def sceptre
  {:id :sceptre
   :in :gold-coffin
   :synonym ["sceptre" "scepter" "treasure"]
   :adjective ["sharp" "egyptian" "ancient" "enameled"]
   :desc "sceptre"
   :flags (flags/flags :take :weapon)
   :ldesc "An ornamented sceptre, tapering to a sharp point, is here."
   :fdesc "A sceptre, possibly that of ancient Egypt itself, is in the coffin. The sceptre is ornamented with colored enamel, and tapers to a sharp point."
   :size 3
   :value 4
   :tvalue 6
   :action (fn [game-state]
             ;; ZIL: SCEPTRE-FUNCTION - WAVE/RAISE at falls/rainbow reveals pot of gold
             (let [prsa (parser-state/get-prsa game-state)
                   here (:here game-state)
                   rainbow-flag (get game-state :rainbow-flag false)]
               (cond
                 ;; WAVE/RAISE at Aragain Falls or End of Rainbow
                 (and (#{:wave :raise} prsa)
                      (#{:aragain-falls :end-of-rainbow} here))
                 (if rainbow-flag
                   ;; Rainbow already solid
                   (utils/tell game-state "The rainbow seems as solid as ever.")
                   ;; Make the rainbow solid and reveal pot of gold
                   (-> game-state
                       (assoc :rainbow-flag true)
                       (gs/unset-thing-flag :pot-of-gold :invisible)
                       (utils/tell "Suddenly, the rainbow appears to become solid and a shimmering, magical staircase leads upward to the end of the rainbow. You could swear that there's a pot of gold there.")))

                 ;; Default - no special handling
                 :else nil)))})

;; <OBJECT TORCH
;;	(IN TORCH-ROOM)
;;	(SYNONYM TORCH IVORY TREASURE)
;;	(ADJECTIVE FLAMING IVORY)
;;	(DESC "torch")
;;	(FLAGS TAKEBIT FLAMEBIT ONBIT LIGHTBIT)
;;	(ACTION TORCH-OBJECT)
;;	(FDESC "Sitting on the pedestal is a flaming torch, made of ivory.")
;;	(SIZE 20)
;;	(VALUE 14)
;;	(TVALUE 6)>

(def ivory-torch
  {:id :ivory-torch
   :in :torch-room
   :synonym ["torch" "ivory" "treasure"]
   :adjective ["flaming" "ivory"]
   :desc "torch"
   :flags (flags/flags :take :flame :on :light)
   :fdesc "Sitting on the pedestal is a flaming torch, made of ivory."
   :size 20
   :value 14
   :tvalue 6
   :action (fn [game-state]
             ;; ZIL: TORCH-OBJECT - examine, pour-on, lamp-off
             (let [prsa (parser-state/get-prsa game-state)
                   prso (parser-state/get-prso game-state)]
               (cond
                 ;; EXAMINE
                 (= prsa :examine)
                 (utils/tell game-state "The torch is burning.")

                 ;; POUR-ON with water
                 (and (= prsa :pour-on) (= prso :water))
                 (utils/tell game-state "The water evaporates before it gets close.")

                 ;; LAMP-OFF (extinguish)
                 (= prsa :lamp-off)
                 (utils/tell game-state "You nearly burn your hand trying to extinguish the flame.")

                 ;; Default
                 :else nil)))})

;; <OBJECT BRASS-BELL
;;     (IN NORTH-TEMPLE)
;;     (SYNONYM BELL)
;;     (ADJECTIVE SMALL BRASS)
;;     (DESC "brass bell")
;;     (FLAGS TAKEBIT)
;;     (ACTION BELL-F)>

(defn bell-action
  "Action handler for the brass bell.

   ZIL: BELL-F in 1actions.zil (lines 356-362)
   <ROUTINE BELL-F ()
     <COND (<VERB? RING>
            <COND (<AND <EQUAL? ,HERE ,LLD-ROOM>
                        <NOT ,LLD-FLAG>>
                   <RFALSE>)
                  (T
                   <TELL \"Ding, dong.\" CR>)>)>>

   The bell is part of the exorcism puzzle. When rung in the
   Entrance to Hades before the exorcism is complete, the
   room action handles it instead."
  [game-state]
  (let [prsa (parser-state/get-prsa game-state)
        here (:here game-state)
        lld-flag (get game-state :lld-flag false)]
    (cond
      ;; RING
      (= prsa :ring)
      (if (and (= here :entrance-to-hades)
               (not lld-flag))
        ;; Return nil to let room handle this
        nil
        ;; Normal ring
        (utils/tell game-state "Ding, dong."))

      ;; Default - no special handling
      :else nil)))

(def brass-bell
  {:id :brass-bell
   :in :north-temple
   :synonym ["bell"]
   :adjective ["small" "brass"]
   :desc "brass bell"
   :flags (flags/flags :take)
   :action bell-action})

;; <OBJECT HOT-BELL
;;     (SYNONYM BELL)
;;     (ADJECTIVE BRASS HOT RED SMALL)
;;     (DESC "red hot brass bell")
;;     (FLAGS TRYTAKEBIT)
;;     (ACTION HOT-BELL-F)
;;     (LDESC "On the ground is a red hot bell.")>

(defn hot-bell-action
  "Action handler for the red hot brass bell.

   ZIL: HOT-BELL-F in 1actions.zil (lines 364-381)
   The bell becomes hot during the exorcism sequence when dropped.
   It can be cooled with water or will eventually cool on its own."
  [game-state]
  (let [prsa (parser-state/get-prsa game-state)
        prsi (parser-state/get-prsi game-state)
        prsi-obj (when prsi (gs/get-thing game-state prsi))]
    (cond
      ;; TAKE
      (= prsa :take)
      (utils/tell game-state "The bell is very hot and cannot be taken.")

      ;; RUB or RING with an instrument
      (or (= prsa :rub)
          (and (= prsa :ring) prsi))
      (cond
        ;; If prsi has burn flag, it burns up
        (and prsi-obj (contains? (:flags prsi-obj) :burn))
        (-> game-state
            (assoc-in [:objects prsi :in] :limbo)
            (utils/tell (str "The " (:desc prsi-obj) " burns and is consumed.")))

        ;; Using hands
        (= prsi :hands)
        (utils/tell game-state "The bell is too hot to touch.")

        ;; Other object
        :else
        (utils/tell game-state "The heat from the bell is too intense."))

      ;; POUR-ON (water cools it)
      (= prsa :pour-on)
      (let [prso (parser-state/get-prso game-state)]
        (-> game-state
            (assoc-in [:objects prso :in] :limbo)  ; Remove water
            (utils/tell "The water cools the bell and is evaporated.")
            ;; Trigger I-XBH to swap hot bell back to normal bell
            ;; For now, just do the swap directly
            (assoc-in [:objects :hot-bell :in] :limbo)
            (assoc-in [:objects :brass-bell :in] (:here game-state))))

      ;; RING (without instrument)
      (= prsa :ring)
      (utils/tell game-state "The bell is too hot to reach.")

      ;; Default - no special handling
      :else nil)))

(def hot-bell
  {:id :hot-bell
   :in :limbo  ; Starts in limbo, appears during exorcism
   :synonym ["bell"]
   :adjective ["brass" "hot" "red" "small"]
   :desc "red hot brass bell"
   :flags (flags/flags :trytake)
   :ldesc "On the ground is a red hot bell."
   :action hot-bell-action})

;; <OBJECT CANDLES
;;	(IN SOUTH-TEMPLE)
;;	(SYNONYM CANDLES PAIR)
;;	(ADJECTIVE BURNING)
;;	(DESC "pair of candles")
;;	(FLAGS TAKEBIT FLAMEBIT ONBIT LIGHTBIT)
;;	(ACTION CANDLES-FCN)
;;	(FDESC "On the two ends of the altar are burning candles.")
;;	(SIZE 10)>

(def candles
  {:id :candles
   :in :south-temple
   :synonym ["candles" "pair"]
   :adjective ["burning"]
   :desc "pair of candles"
   :flags (flags/flags :take :flame :on :light)  ; Start burning!
   :fdesc "On the two ends of the altar are burning candles."
   :size 10
   :action light/candles-action})

;; <OBJECT TRUNK
;;	(IN RESERVOIR)
;;	(SYNONYM TRUNK CHEST JEWELS TREASURE)
;;	(ADJECTIVE OLD)
;;	(DESC "trunk of jewels")
;;	(FLAGS TAKEBIT INVISIBLE)
;;	(FDESC "Lying half buried in the mud is an old trunk, bulging with jewels.")
;;	(LDESC "There is an old trunk here, bulging with assorted jewels.")
;;	(SIZE 35)
;;	(VALUE 15)
;;	(TVALUE 5)>

(def trunk-of-jewels
  {:id :trunk-of-jewels
   :in :reservoir
   :synonym ["trunk" "chest" "jewels" "treasure"]
   :adjective ["old"]
   :desc "trunk of jewels"
   :flags (flags/flags :take :invisible)  ; Hidden until reservoir drains
   :fdesc "Lying half buried in the mud is an old trunk, bulging with jewels."
   :ldesc "There is an old trunk here, bulging with assorted jewels."
   :size 35
   :value 15
   :tvalue 5})

;; <OBJECT DIAMOND
;;	(SYNONYM DIAMOND TREASURE)
;;	(ADJECTIVE HUGE ENORMOUS)
;;	(DESC "huge diamond")
;;	(FLAGS TAKEBIT)
;;	(LDESC "There is an enormous diamond (perfectly cut) here.")
;;	(VALUE 10)
;;	(TVALUE 10)>
;;
;; Note: Diamond starts in limbo and appears in machine when coal is compressed

(def huge-diamond
  {:id :huge-diamond
   :in :limbo  ; Appears when coal machine puzzle is solved
   :synonym ["diamond" "treasure"]
   :adjective ["huge" "enormous"]
   :desc "huge diamond"
   :flags (flags/flags :take)
   :ldesc "There is an enormous diamond (perfectly cut) here."
   :size 6
   :value 10
   :tvalue 10})

;; <OBJECT EMERALD
;;	(IN BUOY)
;;	(SYNONYM EMERALD TREASURE)
;;	(ADJECTIVE LARGE)
;;	(DESC "large emerald")
;;	(FLAGS TAKEBIT)
;;	(VALUE 5)
;;	(TVALUE 10)>

(def large-emerald
  {:id :large-emerald
   :in :buoy
   :synonym ["emerald" "treasure"]
   :adjective ["large"]
   :desc "large emerald"
   :flags (flags/flags :take)
   :size 6
   :value 5
   :tvalue 10})

;; <OBJECT POT-OF-GOLD
;;	(IN END-OF-RAINBOW)
;;	(SYNONYM POT GOLD TREASURE)
;;	(ADJECTIVE GOLD)
;;	(DESC "pot of gold")
;;	(FLAGS TAKEBIT INVISIBLE)
;;	(FDESC "At the end of the rainbow is a pot of gold.")
;;	(SIZE 15)
;;	(VALUE 10)
;;	(TVALUE 10)>

(def pot-of-gold
  {:id :pot-of-gold
   :in :end-of-rainbow
   :synonym ["pot" "gold" "treasure"]
   :adjective ["gold"]
   :desc "pot of gold"
   :flags (flags/flags :take :invisible)  ; Hidden until rainbow is solid
   :fdesc "At the end of the rainbow is a pot of gold."
   :size 15
   :value 10
   :tvalue 10})

;; <OBJECT CANARY
;;	(IN EGG)
;;	(SYNONYM CANARY TREASURE)
;;	(ADJECTIVE CLOCKWORK GOLD GOLDEN)
;;	(DESC "golden clockwork canary")
;;	(FLAGS TAKEBIT SEARCHBIT)
;;	(ACTION CANARY-OBJECT)
;;	(VALUE 6)
;;	(TVALUE 4)
;;	(FDESC "There is a golden clockwork canary nestled in the egg. It has ruby eyes
;;          and a silver beak. Through a crystal window below its left wing you can
;;          see intricate machinery inside. It appears to have wound down.")>

(def clockwork-canary
  {:id :clockwork-canary
   :in :egg
   :synonym ["canary" "treasure"]
   :adjective ["clockwork" "gold" "golden"]
   :desc "golden clockwork canary"
   :flags (flags/flags :take :search)
   :fdesc "There is a golden clockwork canary nestled in the egg. It has ruby eyes and a silver beak. Through a crystal window below its left wing you can see intricate machinery inside. It appears to have wound down."
   :size 8
   :value 6
   :tvalue 4
   :action (fn [game-state]
             ;; ZIL: CANARY-OBJECT - WIND in forest room creates bauble
             (let [prsa (parser-state/get-prsa game-state)
                   here (:here game-state)
                   forest-rooms #{:forest-1 :forest-2 :forest-3 :path :up-a-tree :clearing :grating-clearing}
                   in-forest? (contains? forest-rooms here)
                   sung? (get game-state :canary-sung false)]
               (cond
                 ;; WIND in forest room
                 (and (= prsa :wind) in-forest? (not sung?))
                 (let [bauble-loc (if (= here :up-a-tree) :path here)]
                   (-> game-state
                       (assoc :canary-sung true)
                       (assoc-in [:objects :brass-bauble :in] bauble-loc)
                       (gs/unset-thing-flag :brass-bauble :invisible)
                       (utils/tell "The canary chirps, slightly off-key, an aria from a forgotten opera. From out of the greenery appears a scarlet songbird with ruby eyes, who pauses briefly to listen to the mechanical bird's song. It drops a small brass bauble at your feet and flies away.")))

                 ;; WIND but already sang
                 (and (= prsa :wind) in-forest? sung?)
                 (utils/tell game-state "The canary chirps blithely, but no songbird appears.")

                 ;; WIND not in forest
                 (= prsa :wind)
                 (utils/tell game-state "The canary chirps, but otherwise nothing happens. Perhaps the canary needs to be in the forest?")

                 ;; Default
                 :else nil)))})

;; <OBJECT BAUBLE
;;	(SYNONYM BAUBLE TREASURE)
;;	(ADJECTIVE BRASS BEAUTI)
;;	(DESC "beautiful brass bauble")
;;	(FLAGS TAKEBIT INVISIBLE)
;;	(VALUE 1)
;;	(TVALUE 1)>
;;
;; Note: Bauble starts invisible in limbo, appears when canary sings in forest

(def brass-bauble
  {:id :brass-bauble
   :in :limbo
   :synonym ["bauble" "treasure"]
   :adjective ["brass" "beautiful"]
   :desc "beautiful brass bauble"
   :flags (flags/flags :take :invisible)
   :size 5
   :value 1
   :tvalue 1})

;; <OBJECT BUOY
;;	(IN RIVER-4)
;;	(SYNONYM BUOY)
;;	(ADJECTIVE RED)
;;	(DESC "red buoy")
;;	(FLAGS TAKEBIT CONTBIT SEARCHBIT OPENBIT)
;;	(FDESC "There is a red buoy here (probably a warning).")
;;	(ACTION BUOY-FCN)
;;	(CAPACITY 20)
;;	(SIZE 10)>

(def buoy
  {:id :buoy
   :in :river-4
   :synonym ["buoy"]
   :adjective ["red"]
   :desc "red buoy"
   :flags (flags/flags :take :cont :search :open)
   :fdesc "There is a red buoy here (probably a warning)."
   :capacity 20
   :size 10})

;; <OBJECT JADE
;;	(IN BAT-ROOM)
;;	(SYNONYM FIGURINE TREASURE)
;;	(ADJECTIVE EXQUISITE JADE)
;;	(DESC "jade figurine")
;;	(FLAGS TAKEBIT)
;;	(LDESC "There is an exquisite jade figurine here.")
;;	(SIZE 10)
;;	(VALUE 5)
;;	(TVALUE 5)>

(def jade-figurine
  {:id :jade-figurine
   :in :bat-room
   :synonym ["figurine" "treasure"]
   :adjective ["exquisite" "jade"]
   :desc "jade figurine"
   :flags (flags/flags :take)
   :ldesc "There is an exquisite jade figurine here."
   :size 10
   :value 5
   :tvalue 5})

;; <OBJECT TRIDENT
;;	(IN ATLANTIS-ROOM)
;;	(SYNONYM TRIDENT FORK TREASURE)
;;	(ADJECTIVE POSEIDON OWN CRYSTAL)
;;	(DESC "crystal trident")
;;	(FLAGS TAKEBIT)
;;	(FDESC "On the shore lies Poseidon's own crystal trident.")
;;	(SIZE 20)
;;	(VALUE 4)
;;	(TVALUE 11)>

(def crystal-trident
  {:id :crystal-trident
   :in :atlantis-room
   :synonym ["trident" "fork" "treasure"]
   :adjective ["poseidon" "crystal"]
   :desc "crystal trident"
   :flags (flags/flags :take)
   :fdesc "On the shore lies Poseidon's own crystal trident."
   :size 20
   :value 4
   :tvalue 11})

;; <OBJECT BRACELET
;;	(IN GAS-ROOM)
;;	(SYNONYM BRACELET JEWEL SAPPHIRE TREASURE)
;;	(ADJECTIVE SAPPHIRE)
;;	(DESC "sapphire-encrusted bracelet")
;;	(FLAGS TAKEBIT)
;;	(SIZE 10)
;;	(VALUE 5)
;;	(TVALUE 5)>

(def sapphire-bracelet
  {:id :sapphire-bracelet
   :in :gas-room
   :synonym ["bracelet" "jewel" "sapphire" "treasure"]
   :adjective ["sapphire"]
   :desc "sapphire-encrusted bracelet"
   :flags (flags/flags :take)
   :size 10
   :value 5
   :tvalue 5})

;; <OBJECT SCARAB
;;	(IN SANDY-CAVE)
;;	(SYNONYM SCARAB BUG BEETLE TREASURE)
;;	(ADJECTIVE BEAUTI CARVED JEWELED)
;;	(DESC "beautiful jeweled scarab")
;;	(FLAGS TAKEBIT INVISIBLE)
;;	(SIZE 8)
;;	(VALUE 5)
;;	(TVALUE 5)>
;;
;; Note: The scarab is INVISIBLE until discovered by digging in the sand.

(def jeweled-scarab
  {:id :jeweled-scarab
   :in :sandy-cave
   :synonym ["scarab" "bug" "beetle" "treasure"]
   :adjective ["beautiful" "carved" "jeweled"]
   :desc "beautiful jeweled scarab"
   :flags (flags/flags :take :invisible)
   :size 8
   :value 5
   :tvalue 5})

;; <OBJECT CHALICE
;;	(IN TREASURE-ROOM)
;;	(SYNONYM CHALICE CUP SILVER TREASURE)
;;	(ADJECTIVE SILVER ENGRAVINGS)
;;	(DESC "chalice")
;;	(FLAGS TAKEBIT TRYTAKEBIT CONTBIT)
;;	(ACTION CHALICE-FCN)
;;	(LDESC "There is a silver chalice, intricately engraved, here.")
;;	(CAPACITY 5)
;;	(SIZE 10)
;;	(VALUE 10)
;;	(TVALUE 5)>
;;
;; ZIL: CHALICE-FCN in 1actions.zil (lines 2136-2149)
;; - TAKE: If thief is in treasure room and fighting, "You'd be stabbed in the back first."
;; - PUT: "You can't. It's not a very good chalice, is it?"
;; - Otherwise: acts as a (poor) container

(def silver-chalice
  {:id :silver-chalice
   :in :treasure-room
   :synonym ["chalice" "cup" "silver" "treasure"]
   :adjective ["silver" "engraved"]
   :desc "chalice"
   :flags (flags/flags :take :trytake :cont)
   :ldesc "There is a silver chalice, intricately engraved, here."
   :capacity 5
   :size 10
   :value 10
   :tvalue 5
   :action (fn [game-state]
             (let [prsa (parser-state/get-prsa game-state)
                   prsi (parser-state/get-prsi game-state)
                   here (:here game-state)
                   thief-here? (= (gs/get-thing-loc-id game-state :thief) here)
                   thief-fighting? (gs/set-thing-flag? game-state :thief :fight)
                   thief-visible? (not (gs/set-thing-flag? game-state :thief :invisible))]
               (cond
                 ;; TAKE: If thief is fighting in treasure room
                 (and (= prsa :take)
                      (= here :treasure-room)
                      thief-here?
                      thief-fighting?
                      thief-visible?)
                 (utils/tell game-state "You'd be stabbed in the back first.")

                 ;; PUT: Can't put things in the chalice
                 (and (= prsa :put) (= prsi :silver-chalice))
                 (utils/tell game-state "You can't. It's not a very good chalice, is it?")

                 ;; Default - let normal handling take over
                 :else nil)))})

;;; ---------------------------------------------------------------------------
;;; FOREST OBJECTS
;;; ---------------------------------------------------------------------------

;; <OBJECT TREE
;;   (IN LOCAL-GLOBALS)
;;   (SYNONYM TREE BRANCH)
;;   (ADJECTIVE LARGE STORM)
;;   (DESC "tree")
;;   (FLAGS NDESCBIT CLIMBBIT)>

(def tree
  {:id :tree
   :in :local-globals  ; Visible from forest rooms
   :synonym ["tree" "branch"]
   :adjective ["large" "storm-tossed"]
   :desc "tree"
   :flags (flags/flags :ndesc :climb)})

;; <OBJECT NEST
;;	(IN UP-A-TREE)
;;	(SYNONYM NEST)
;;	(ADJECTIVE BIRDS)
;;	(DESC "bird's nest")
;;	(FLAGS TAKEBIT BURNBIT CONTBIT OPENBIT SEARCHBIT)
;;	(FDESC "Beside you on the branch is a small bird's nest.")
;;	(CAPACITY 20)>

(def nest
  {:id :nest
   :in :up-a-tree
   :synonym ["nest"]
   :adjective ["birds"]
   :desc "bird's nest"
   :flags (flags/flags :take :cont :burn :open :search)
   :fdesc "Beside you on the branch is a small bird's nest."
   :capacity 20})

;; <OBJECT EGG
;;	(IN NEST)
;;	(SYNONYM EGG TREASURE)
;;	(ADJECTIVE BIRDS ENCRUSTED JEWELED)
;;	(DESC "jewel-encrusted egg")
;;	(FLAGS TAKEBIT CONTBIT SEARCHBIT)
;;	(ACTION EGG-OBJECT)
;;	(VALUE 5)
;;	(TVALUE 5)
;;	(CAPACITY 6)
;;	(FDESC "In the bird's nest is a large egg encrusted with precious jewels,
;;	        apparently scavenged by a childless songbird. The egg is covered with
;;	        fine gold inlay, and ornamented in lapis lazuli and mother-of-pearl.
;;	        Unlike most eggs, this one is hinged and closed with a delicate looking
;;	        clasp. The egg appears extremely fragile.")>

;; Forward declare bad-egg since it's called by egg-action
(declare bad-egg)

(defn egg-action
  "Egg action handler - handles opening, breaking, and sitting on the egg.

   ZIL: EGG-OBJECT (1actions.zil lines 2932-2971)

   The egg is a fragile container. Opening it requires care:
   - Opening with hands: won't work without tools
   - Opening with weapons/tools: breaks the egg and damages canary
   - Sitting on (CLIMB-ON): crushes the egg
   - Throwing: breaks the egg
   - Using other items: gives various humorous responses

   Only the thief can open the egg properly (via thief code, not here)."
  [game-state]
  (let [prsa (parser-state/get-prsa game-state)
        prso (parser-state/get-prso game-state)
        prsi (parser-state/get-prsi game-state)
        prsi-obj (when prsi (gs/get-thing game-state prsi))
        is-open? (gs/set-thing-flag? game-state :egg :open)
        ;; FIGHTBIT tracks if player has tried an original approach
        tried-original? (gs/set-thing-flag? game-state :egg :fight)]
    (cond
      ;; OPEN or MUNG (break) with egg as direct object
      (and (contains? #{:open :mung :break :destroy :smash} prsa)
           (= prso :egg))
      (cond
        ;; Already open
        is-open?
        (utils/tell game-state "The egg is already open.")

        ;; No indirect object (no tool specified)
        (nil? prsi)
        (utils/tell game-state "You have neither the tools nor the expertise.")

        ;; Using bare hands
        (= prsi :hands)
        (utils/tell game-state "I doubt you could do that without damaging it.")

        ;; Using weapon or tool - breaks the egg!
        (or (gs/set-thing-flag? game-state prsi :weapon)
            (gs/set-thing-flag? game-state prsi :tool)
            (contains? #{:mung :break :destroy :smash} prsa))
        (-> game-state
            (utils/tell "The egg is now open, but the clumsiness of your attempt has seriously compromised its esthetic appeal.")
            bad-egg)

        ;; Using a fighting item (but not weapon/tool) - original!
        tried-original?
        (utils/tell game-state (str "Not to say that using the "
                                    (:desc prsi-obj) " isn't original too..."))

        ;; First time trying something unusual
        :else
        (-> game-state
            (gs/set-thing-flag :egg :fight)
            (utils/tell (str "The concept of using a "
                             (:desc prsi-obj) " is certainly original."))))

      ;; CLIMB-ON (sit on) the egg - crushes it
      (= prsa :climb-on)
      (-> game-state
          (utils/tell "There is a noticeable crunch from beneath you, and inspection reveals that the egg is lying open, badly damaged.")
          bad-egg)

      ;; THROW the egg - breaks it
      ;; Note: The broken egg moves to current room (handled by bad-egg)
      (= prsa :throw)
      (-> game-state
          (utils/tell "Your rather indelicate handling of the egg has caused it some damage, although you have succeeded in opening it.")
          bad-egg)

      ;; Not handled - let default verb handle it
      :else
      nil)))

(defn bad-egg
  "Replace the egg with broken egg and damage the canary.

   ZIL: BAD-EGG (1actions.zil lines 2973-2979)

   If the canary is inside the egg, it becomes damaged (broken canary).
   The broken-egg replaces the egg at the egg's current location.
   The canary (if in egg) is moved to broken-egg and replaced with broken-canary."
  [game-state]
  (let [egg-loc (gs/get-thing-loc-id game-state :egg)
        canary-in-egg? (= (gs/get-thing-loc-id game-state :clockwork-canary) :egg)
        ;; Get broken-canary fdesc for the message if canary was in egg
        broken-canary-fdesc (get-in game-state [:objects :broken-canary :fdesc])]
    (-> game-state
        ;; If canary was in egg, show the broken canary description
        (cond-> canary-in-egg?
          (utils/tell (str " " broken-canary-fdesc)))
        ;; Move broken-egg to where egg was
        (assoc-in [:objects :broken-egg :in] egg-loc)
        ;; If canary was in egg, move broken-canary into broken-egg
        (cond-> canary-in-egg?
          (assoc-in [:objects :broken-canary :in] :broken-egg))
        ;; Remove the original egg from the game
        (assoc-in [:objects :egg :in] :limbo)
        ;; Remove the original canary if it was in the egg
        (cond-> canary-in-egg?
          (assoc-in [:objects :clockwork-canary :in] :limbo)))))

(def egg
  {:id :egg
   :in :nest
   :synonym ["egg" "treasure"]
   :adjective ["birds" "encrusted" "jeweled"]
   :desc "jewel-encrusted egg"
   :flags (flags/flags :take :cont :search)
   :fdesc "In the bird's nest is a large egg encrusted with precious jewels, apparently scavenged by a childless songbird. The egg is covered with fine gold inlay, and ornamented in lapis lazuli and mother-of-pearl. Unlike most eggs, this one is hinged and closed with a delicate looking clasp. The egg appears extremely fragile."
   :capacity 6
   :value 5
   :tvalue 5
   :action egg-action})

;; ZIL: BROKEN-EGG (1dungeon.zil lines 1171-1178)
;; <OBJECT BROKEN-EGG
;;	(SYNONYM EGG TREASURE)
;;	(ADJECTIVE BROKEN BIRDS ENCRUSTED JEWEL)
;;	(DESC "broken jewel-encrusted egg")
;;	(FLAGS TAKEBIT CONTBIT OPENBIT)
;;	(CAPACITY 6)
;;	(TVALUE 2)
;;	(LDESC "There is a somewhat ruined egg here.")>

(def broken-egg
  {:id :broken-egg
   :in :limbo  ; starts out of play, created when egg is damaged
   :synonym ["egg" "treasure"]
   :adjective ["broken" "birds" "encrusted" "jewel"]
   :desc "broken jewel-encrusted egg"
   :flags (flags/flags :take :cont :open)  ; already open
   :capacity 6
   :tvalue 2  ; worth less than intact egg (no :value means 0 pickup value)
   :ldesc "There is a somewhat ruined egg here."})

;; ZIL: BROKEN-CANARY (1dungeon.zil lines 1203-1217)
;; <OBJECT BROKEN-CANARY
;;	(IN BROKEN-EGG)
;;	(SYNONYM CANARY TREASURE)
;;	(ADJECTIVE BROKEN CLOCKWORK GOLD GOLDEN)
;;	(DESC "broken clockwork canary")
;;	(FLAGS TAKEBIT)
;;	(ACTION CANARY-OBJECT)
;;	(TVALUE 1)
;;	(FDESC
;; "There is a golden clockwork canary nestled in the egg. It seems to
;; have recently had a bad experience. The mountings for its jewel-like
;; eyes are empty, and its silver beak is crumpled. Through a cracked
;; crystal window below its left wing you can see the remains of
;; intricate machinery. It is not clear what result winding it would
;; have, as the mainspring seems sprung.")>

(def broken-canary
  {:id :broken-canary
   :in :limbo  ; starts out of play, created when egg is damaged
   :synonym ["canary" "treasure"]
   :adjective ["broken" "clockwork" "gold" "golden"]
   :desc "broken clockwork canary"
   :flags (flags/flags :take)
   :tvalue 1  ; worth less than intact canary
   :fdesc "There is a golden clockwork canary nestled in the egg. It seems to have recently had a bad experience. The mountings for its jewel-like eyes are empty, and its silver beak is crumpled. Through a cracked crystal window below its left wing you can see the remains of intricate machinery. It is not clear what result winding it would have, as the mainspring seems sprung."
   ;; Note: broken canary uses same action as intact canary (CANARY-OBJECT)
   ;; but the wind command will fail since it's broken
   :action (fn [game-state]
             (let [prsa (parser-state/get-prsa game-state)]
               (when (= prsa :wind)
                 (utils/tell game-state "The canary is broken and won't wind."))))})

;;; ---------------------------------------------------------------------------
;;; TROLL AND AXE
;;; ---------------------------------------------------------------------------

;; <OBJECT TROLL
;;	(IN TROLL-ROOM)
;;	(SYNONYM TROLL)
;;	(ADJECTIVE NASTY)
;;	(DESC "troll")
;;	(FLAGS ACTORBIT OPENBIT TRYTAKEBIT)
;;	(ACTION TROLL-FCN)
;;	(LDESC "A nasty-looking troll, brandishing a bloody axe, blocks all
;;          passages out of the room.")
;;	(STRENGTH 2)>

(defn troll-action
  "Troll action handler.

   ZIL: TROLL-FCN (1actions.zil line 653-777)

   Modes:
   :f-busy?      - Check if troll is busy (recovering weapon)
   :f-dead       - Troll has been killed
   :f-unconscious - Troll knocked unconscious
   :f-conscious  - Troll wakes up
   :f-first?     - Should troll attack first? (33% chance)
   nil           - Normal verb handling"
  [game-state & [mode]]
  (let [prsa (parser-state/get-prsa game-state)
        prso (parser-state/get-prso game-state)
        prsi (parser-state/get-prsi game-state)
        here (:here game-state)
        troll-here? (= (gs/get-thing-loc-id game-state :troll) here)
        axe-in-troll? (= (gs/get-thing-loc-id game-state :axe) :troll)
        axe-in-room? (= (gs/get-thing-loc-id game-state :axe) here)]
    (case mode
      ;; F-BUSY? - Check if troll is recovering weapon
      :f-busy?
      (cond
        ;; Already has axe - not busy
        axe-in-troll?
        game-state

        ;; Axe is in room - 75% chance to recover it
        (and axe-in-room? (< (random/rand-int* 100) 75))
        (-> game-state
            (gs/set-thing-flag :axe :ndesc)
            (gs/unset-thing-flag :axe :weapon)
            (assoc-in [:objects :axe :in] :troll)
            (assoc-in [:objects :troll :ldesc]
                      "A nasty-looking troll, brandishing a bloody axe, blocks all passages out of the room.")
            (cond-> troll-here?
              (utils/tell "The troll, angered and humiliated, recovers his weapon. He appears to have an axe to grind with you.")))

        ;; Troll is disarmed, pathetic
        troll-here?
        (-> game-state
            (assoc-in [:objects :troll :ldesc]
                      "A pathetically babbling troll is here.")
            (utils/tell "The troll, disarmed, cowers in terror, pleading for his life in the guttural tongue of the trolls."))

        :else
        game-state)

      ;; F-DEAD - Troll killed
      :f-dead
      (-> game-state
          ;; Drop axe if holding it
          (cond-> axe-in-troll?
            (-> (assoc-in [:objects :axe :in] here)
                (gs/unset-thing-flag :axe :ndesc)
                (gs/set-thing-flag :axe :weapon)))
          ;; Set troll-flag to open passages
          (assoc :troll-flag true))

      ;; F-UNCONSCIOUS - Troll knocked out
      :f-unconscious
      (-> game-state
          (gs/unset-thing-flag :troll :fight)
          ;; Drop axe if holding it
          (cond-> axe-in-troll?
            (-> (assoc-in [:objects :axe :in] here)
                (gs/unset-thing-flag :axe :ndesc)
                (gs/set-thing-flag :axe :weapon)))
          ;; Update description
          (assoc-in [:objects :troll :ldesc]
                    "An unconscious troll is sprawled on the floor. All passages out of the room are open.")
          ;; Open passages
          (assoc :troll-flag true))

      ;; F-CONSCIOUS - Troll wakes up
      :f-conscious
      (let [gs (-> game-state
                   ;; If troll in room, resume fighting
                   (cond-> troll-here?
                     (-> (gs/set-thing-flag :troll :fight)
                         (utils/tell "The troll stirs, quickly resuming a fighting stance."))))]
        (cond
          ;; Has axe already
          (= (gs/get-thing-loc-id gs :axe) :troll)
          (-> gs
              (assoc-in [:objects :troll :ldesc]
                        "A nasty-looking troll, brandishing a bloody axe, blocks all passages out of the room.")
              (assoc :troll-flag false))

          ;; Axe is in troll room - pick it up
          (= (gs/get-thing-loc-id gs :axe) :troll-room)
          (-> gs
              (gs/set-thing-flag :axe :ndesc)
              (gs/unset-thing-flag :axe :weapon)
              (assoc-in [:objects :axe :in] :troll)
              (assoc-in [:objects :troll :ldesc]
                        "A nasty-looking troll, brandishing a bloody axe, blocks all passages out of the room.")
              (assoc :troll-flag false))

          ;; No axe available
          :else
          (-> gs
              (assoc-in [:objects :troll :ldesc] "A troll is here.")
              (assoc :troll-flag false))))

      ;; F-FIRST? - Should troll strike first? (33% chance)
      :f-first?
      (when (< (random/rand-int* 100) 33)
        (-> game-state
            (gs/set-thing-flag :troll :fight)))

      ;; Default - verb handling
      nil
      (cond
        ;; EXAMINE - show long description
        (= prsa :examine)
        (utils/tell game-state (get-in game-state [:objects :troll :ldesc]))

        ;; LISTEN
        (= prsa :listen)
        (utils/tell game-state "Every so often the troll says something, probably uncomplimentary, in his guttural tongue.")

        ;; HELLO when troll is dead
        (and (= prsa :hello) (:troll-flag game-state))
        (utils/tell game-state "Unfortunately, the troll can't hear you.")

        ;; TAKE/MOVE troll
        (#{:take :move} prsa)
        (utils/tell game-state "The troll spits in your face, grunting \"Better luck next time\" in a rather barbarous accent.")

        ;; Default - no special handling
        :else
        nil)

      ;; Unknown mode - return unchanged
      game-state)))

(def troll
  {:id :troll
   :in :troll-room
   :synonym ["troll"]
   :adjective ["nasty"]
   :desc "troll"
   :flags (flags/flags :actor :open :trytake)
   :ldesc "A nasty-looking troll, brandishing a bloody axe, blocks all passages out of the room."
   :strength 2
   :action troll-action})

;; <OBJECT AXE
;;	(IN TROLL)
;;	(SYNONYM AXE AX)
;;	(ADJECTIVE BLOODY)
;;	(DESC "bloody axe")
;;	(FLAGS WEAPONBIT TRYTAKEBIT TAKEBIT NDESCBIT)
;;	(ACTION AXE-F)
;;	(SIZE 25)>

(def axe
  {:id :axe
   :in :troll
   :synonym ["axe" "ax"]
   :adjective ["bloody"]
   :desc "bloody axe"
   :flags (flags/flags :weapon :trytake :take :ndesc)
   :size 25})

;;; ---------------------------------------------------------------------------
;;; THIEF AND ASSOCIATED OBJECTS
;;; ---------------------------------------------------------------------------

;; <OBJECT STILETTO
;;	(IN THIEF)
;;	(SYNONYM STILETTO)
;;	(ADJECTIVE VICIOUS)
;;	(DESC "stiletto")
;;	(ACTION STILETTO-FUNCTION)
;;	(FLAGS WEAPONBIT TRYTAKEBIT TAKEBIT NDESCBIT)
;;	(SIZE 10)>

(def stiletto
  {:id :stiletto
   :in :thief
   :synonym ["stiletto"]
   :adjective ["vicious"]
   :desc "stiletto"
   :flags (flags/flags :weapon :trytake :take :ndesc)
   :size 10})

;; <OBJECT LARGE-BAG
;;	(IN THIEF)
;;	(SYNONYM BAG)
;;	(ADJECTIVE LARGE THIEFS)
;;	(DESC "large bag")
;;	(ACTION LARGE-BAG-F)
;;	(FLAGS TRYTAKEBIT NDESCBIT)>

(def large-bag
  {:id :large-bag
   :in :thief
   :synonym ["bag"]
   :adjective ["large" "thiefs" "thief's"]
   :desc "large bag"
   :flags (flags/flags :trytake :ndesc)})

;; <OBJECT THIEF
;;	(IN ROUND-ROOM)
;;	(SYNONYM THIEF ROBBER MAN PERSON)
;;	(ADJECTIVE SHADY SUSPICIOUS SEEDY)
;;	(DESC "thief")
;;	(FLAGS ACTORBIT INVISIBLE CONTBIT OPENBIT TRYTAKEBIT)
;;	(ACTION ROBBER-FUNCTION)
;;	(LDESC "There is a suspicious-looking individual, holding a large bag, leaning
;;          against one wall. He is armed with a deadly stiletto.")
;;	(STRENGTH 5)>

(def thief
  {:id :thief
   :in :round-room
   :synonym ["thief" "robber" "man" "person" "individual"]
   :adjective ["shady" "suspicious" "seedy"]
   :desc "thief"
   :flags (flags/flags :actor :invisible :cont :open :trytake)
   :ldesc "There is a suspicious-looking individual, holding a large bag, leaning against one wall. He is armed with a deadly stiletto."
   :strength 5
   :action thief/thief-action})

;; <OBJECT CYCLOPS
;;	(IN CYCLOPS-ROOM)
;;	(SYNONYM CYCLOPS MONSTER EYE)
;;	(ADJECTIVE HUNGRY GIANT)
;;	(DESC "cyclops")
;;	(FLAGS ACTORBIT NDESCBIT TRYTAKEBIT)
;;	(ACTION CYCLOPS-FCN)
;;	(STRENGTH 10000)>

(def cyclops
  {:id :cyclops
   :in :cyclops-room
   :synonym ["cyclops" "monster" "eye"]
   :adjective ["hungry" "giant"]
   :desc "cyclops"
   :flags (flags/flags :actor :ndesc :trytake)
   :strength 10000
   :action cyclops/cyclops-action})

;;; ---------------------------------------------------------------------------
;;; MAZE OBJECTS
;;; ---------------------------------------------------------------------------

;; <OBJECT BONES
;;	(IN MAZE-5)
;;	(SYNONYM BONES SKELETON BODY)
;;	(DESC "skeleton")
;;	(FLAGS TRYTAKEBIT NDESCBIT)
;;	(ACTION SKELETON)>
;;
;; Note: The skeleton is scenery that appears in the MAZE-5 description.
;; It has TRYTAKEBIT (try to take) and NDESCBIT (not described separately).

(def skeleton
  {:id :skeleton
   :in :maze-5
   :synonym ["bones" "skeleton" "body"]
   :desc "skeleton"
   :flags (flags/flags :trytake :ndesc)
   :action (fn [game-state]
             ;; ZIL: SKELETON action in 1actions.zil handles EXAMINE
             (when (= (parser-state/get-prsa game-state) :examine)
               (utils/tell game-state "A skeleton lies here, its fleshless hand clutching a rusty knife.")))})

;; <OBJECT BURNED-OUT-LANTERN
;;	(IN MAZE-5)
;;	(SYNONYM LANTERN LAMP)
;;	(ADJECTIVE RUSTY BURNED DEAD USELESS)
;;	(DESC "burned-out lantern")
;;	(FLAGS TAKEBIT)
;;	(FDESC "The deceased adventurer's useless lantern is here.")
;;	(SIZE 20)>

(def burned-out-lantern
  {:id :burned-out-lantern
   :in :maze-5
   :synonym ["lantern" "lamp"]
   :adjective ["rusty" "burned" "dead" "useless"]
   :desc "burned-out lantern"
   :flags (flags/flags :take)
   :fdesc "The deceased adventurer's useless lantern is here."
   :size 20})

;; <OBJECT BAG-OF-COINS
;;	(IN MAZE-5)
;;	(SYNONYM BAG COINS TREASURE)
;;	(ADJECTIVE OLD LEATHER)
;;	(DESC "leather bag of coins")
;;	(FLAGS TAKEBIT)
;;	(LDESC "An old leather bag, bulging with coins, is here.")
;;	(ACTION BAG-OF-COINS-F)
;;	(SIZE 15)
;;	(VALUE 10)
;;	(TVALUE 5)>

(def bag-of-coins
  {:id :bag-of-coins
   :in :maze-5
   :synonym ["bag" "coins" "treasure"]
   :adjective ["old" "leather"]
   :desc "leather bag of coins"
   :flags (flags/flags :take)
   :ldesc "An old leather bag, bulging with coins, is here."
   :size 15
   :value 10   ; base value
   :tvalue 5}) ; trophy case value

;; <OBJECT BAR
;;	(IN LOUD-ROOM)
;;	(SYNONYM BAR PLATINUM TREASURE)
;;	(ADJECTIVE PLATINUM LARGE)
;;	(DESC "platinum bar")
;;	(FLAGS TAKEBIT SACREDBIT)
;;	(LDESC "On the ground is a large platinum bar.")
;;	(SIZE 20)
;;	(VALUE 10)
;;	(TVALUE 5)>
;;
;; Note: The bar cannot be taken until the echo puzzle is solved.
;; In the original ZIL, this was enforced by a special input mode in the loud room
;; that only accepts limited commands (movement, echo, save/restore/quit).
;; The :sacred flag protects from the thief and is cleared when the puzzle is solved.

(def platinum-bar
  {:id :platinum-bar
   :in :loud-room
   :synonym ["bar" "platinum" "treasure"]
   :adjective ["platinum" "large"]
   :desc "platinum bar"
   :flags (flags/flags :take :sacred)  ; sacred = protects from thief
   :ldesc "On the ground is a large platinum bar."
   :size 20
   :value 10   ; base value
   :tvalue 5}) ; trophy case value

;; <OBJECT RUSTY-KNIFE
;;	(IN MAZE-5)
;;	(SYNONYM KNIVES KNIFE)
;;	(ADJECTIVE RUSTY)
;;	(DESC "rusty knife")
;;	(FLAGS TAKEBIT TRYTAKEBIT WEAPONBIT TOOLBIT)
;;	(ACTION RUSTY-KNIFE-FCN)
;;	(FDESC "Beside the skeleton is a rusty knife.")
;;	(SIZE 20)>
;;
;; ZIL: RUSTY-KNIFE-FCN has special behavior:
;; - When taken while holding the elvish sword, the sword glows
;; - If used as a weapon, it turns and slits the player's throat

(def rusty-knife
  {:id :rusty-knife
   :in :maze-5
   :synonym ["knives" "knife"]
   :adjective ["rusty"]
   :desc "rusty knife"
   :flags (flags/flags :take :trytake :weapon :tool)
   :fdesc "Beside the skeleton is a rusty knife."
   :size 20
   :action (fn [game-state]
             (let [prsa (parser-state/get-prsa game-state)
                   prso (parser-state/get-prso game-state)
                   prsi (parser-state/get-prsi game-state)
                   has-sword? (= (gs/get-thing-loc-id game-state :sword) :adventurer)]
               (cond
                 ;; Taking the rusty knife while carrying the sword
                 (and (= prsa :take)
                      has-sword?)
                 (-> game-state
                     (utils/tell "As you touch the rusty knife, your sword gives a single pulse of blinding blue light."))

                 ;; Trying to use the rusty knife as a weapon is fatal
                 ;; ZIL: <OR <AND <EQUAL? ,PRSI ,RUSTY-KNIFE> <VERB? ATTACK>>
                 ;;         <AND <VERB? SWING> <EQUAL? ,PRSO ,RUSTY-KNIFE> ,PRSI>>
                 (or (and (= prsi :rusty-knife) (= prsa :attack))
                     (and (= prsa :swing) (= prso :rusty-knife) prsi))
                 (let [jigs-up (requiring-resolve 'clork.verbs-health/jigs-up)]
                   (jigs-up game-state "As the knife approaches its victim, your mind is submerged by an overmastering will. Slowly, your hand turns, until the rusty blade is an inch from your neck. The knife seems to sing as it savagely slits your throat."))

                 ;; Default - no special handling
                 :else nil)))})

;; <OBJECT KEYS
;;	(IN MAZE-5)
;;	(SYNONYM KEY)
;;	(ADJECTIVE SKELETON)
;;	(DESC "skeleton key")
;;	(FLAGS TAKEBIT TOOLBIT)
;;	(SIZE 10)>

(def skeleton-key
  {:id :skeleton-key
   :in :maze-5
   :synonym ["key" "keys"]
   :adjective ["skeleton"]
   :desc "skeleton key"
   :flags (flags/flags :take :tool)
   :size 10})

;; <OBJECT LEAVES
;;	(IN GRATING-CLEARING)
;;	(SYNONYM LEAVES LEAF PILE)
;;	(DESC "pile of leaves")
;;	(FLAGS TAKEBIT BURNBIT TRYTAKEBIT)
;;	(ACTION LEAF-PILE)
;;	(LDESC "On the ground is a pile of leaves.")
;;	(SIZE 25)>
;;
;; ZIL: LEAF-PILE in 1actions.zil (lines 799-826)
;; ZIL: LEAVES-APPEAR in 1actions.zil (lines 787-797)
;; Moving, taking, or burning the leaves reveals the grating underneath.

(defn- leaves-appear
  "Reveal the grating when leaves are disturbed.

   ZIL: LEAVES-APPEAR in 1actions.zil (lines 787-797)"
  [game-state verb]
  (let [grate-open? (gs/set-thing-flag? game-state :grate :open)
        grate-revealed? (get game-state :grate-revealed false)]
    (if (and (not grate-open?) (not grate-revealed?))
      (-> game-state
          (utils/tell (if (#{:move :take} verb)
                        "In disturbing the pile of leaves, a grating is revealed."
                        "With the leaves moved, a grating is revealed."))
          (gs/unset-thing-flag :grate :invisible)
          (assoc :grate-revealed true))
      game-state)))

(def leaves
  {:id :leaves
   :in :grating-clearing
   :synonym ["leaves" "leaf" "pile"]
   :desc "pile of leaves"
   :ldesc "On the ground is a pile of leaves."
   :flags (flags/flags :take :burn :trytake)
   :size 25
   :action (fn [game-state]
             ;; ZIL: LEAF-PILE in 1actions.zil (lines 799-826)
             (let [prsa (parser-state/get-prsa game-state)
                   grate-revealed? (get game-state :grate-revealed false)]
               (cond
                 ;; COUNT
                 (= prsa :count)
                 (utils/tell game-state "There are 69,105 leaves here.")

                 ;; BURN - burn leaves (fatal if holding them!)
                 (= prsa :burn)
                 (let [held? (= (gs/get-thing-loc-id game-state :leaves) :adventurer)
                       gs (-> game-state
                              (leaves-appear :burn)
                              (assoc-in [:objects :leaves :in] :limbo))]
                   (if held?
                     ;; Holding burning leaves = death
                     (death/jigs-up gs "The leaves burn, and so do you.")
                     (utils/tell gs "The leaves burn.")))

                 ;; MOVE - reveal grating if not already revealed
                 (= prsa :move)
                 (-> game-state
                     (utils/tell "Done.")
                     (leaves-appear :move))

                 ;; TAKE - reveal grating if not already revealed, then let default take happen
                 (= prsa :take)
                 (if grate-revealed?
                   nil  ; let default take happen
                   (leaves-appear game-state :take))

                 ;; LOOK-UNDER - peek under the leaves
                 (and (= prsa :look-under) (not grate-revealed?))
                 (utils/tell game-state "Underneath the pile of leaves is a grating. As you release the leaves, the grating is once again concealed from view.")

                 ;; CUT - rustle leaves, reveal grating
                 (= prsa :cut)
                 (-> game-state
                     (utils/tell "You rustle the leaves around, making quite a mess.")
                     (leaves-appear :cut))

                 ;; Default - no special handling
                 :else nil)))})

;; <OBJECT GRATE
;;	(IN LOCAL-GLOBALS)
;;	(SYNONYM GRATING GRATE)
;;	(DESC "grating")
;;	(FLAGS NDESCBIT DOORBIT)
;;	(ACTION GRATE-FUNCTION)>
;;
;; The grate connects the grating-room (underground) to the grating-clearing (above ground).
;; It starts locked with a skull-and-crossbones lock.

(def grate
  {:id :grate
   :in :local-globals
   :synonym ["grating" "grate"]
   :desc "grating"
   :flags (flags/flags :ndesc :door :invisible)  ; starts closed, locked, and hidden under leaves
   :action (fn [game-state]
             (let [prsa (parser-state/get-prsa game-state)
                   prsi (parser-state/get-prsi game-state)
                   here (:here game-state)
                   grunlock (get game-state :grunlock false)
                   grate-open? (gs/set-thing-flag? game-state :grate :open)]
               (cond
                 ;; OPEN with KEYS -> perform UNLOCK
                 (and (= prsa :open) (= prsi :skeleton-key))
                 (if (= here :grating-room)
                   (-> game-state
                       (assoc :grunlock true)
                       (utils/tell "The grate is unlocked."))
                   (utils/tell game-state "You can't reach the lock from here."))

                 ;; UNLOCK with KEYS
                 (and (= prsa :unlock) (= prsi :skeleton-key))
                 (cond
                   (= here :grating-room)
                   (-> game-state
                       (assoc :grunlock true)
                       (utils/tell "The grate is unlocked."))
                   (= here :grating-clearing)
                   (utils/tell game-state "You can't reach the lock from here.")
                   :else
                   (utils/tell game-state (str "Can you unlock a grating with a " (:desc (gs/get-thing game-state prsi)) "?")))

                 ;; LOCK
                 (= prsa :lock)
                 (cond
                   (= here :grating-room)
                   (-> game-state
                       (assoc :grunlock false)
                       (utils/tell "The grate is locked."))
                   (= here :grating-clearing)
                   (utils/tell game-state "You can't lock it from this side."))

                 ;; PICK (lock picking)
                 (= prsa :pick)
                 (utils/tell game-state "You can't pick the lock.")

                 ;; OPEN/CLOSE
                 (#{:open :close} prsa)
                 (if grunlock
                   (cond
                     (= prsa :open)
                     (if grate-open?
                       (utils/tell game-state "It is already open.")
                       (let [gs (-> game-state
                                    (gs/set-thing-flag :grate :open)
                                    (gs/set-thing-flag :grating-room :on))]  ; Light enters
                         (if (= here :grating-clearing)
                           (utils/tell gs "The grating opens.")
                           ;; First time opening from below - leaves fall
                           (if (get game-state :grate-revealed false)
                             (utils/tell gs "The grating opens to reveal trees above you.")
                             (-> gs
                                 (assoc :grate-revealed true)
                                 (assoc-in [:objects :leaves :in] here)
                                 (utils/tell "The grating opens to reveal trees above you.")
                                 (utils/crlf)
                                 (utils/tell "A pile of leaves falls onto your head and to the ground."))))))
                     (= prsa :close)
                     (if grate-open?
                       (-> game-state
                           (gs/unset-thing-flag :grate :open)
                           (gs/unset-thing-flag :grating-room :on)  ; No more light
                           (utils/tell "The grating is closed."))
                       (utils/tell game-state "It is already closed.")))
                   (utils/tell game-state "The grating is locked."))

                 ;; PUT something through grate (from clearing above)
                 (and (= prsa :put) (= prsi :grate))
                 (let [prso (parser-state/get-prso game-state)
                       obj (gs/get-thing game-state prso)
                       size (get obj :size 5)]
                   (if (> size 20)
                     (utils/tell game-state "It won't fit through the grating.")
                     (-> game-state
                         (assoc-in [:objects prso :in] :grating-room)
                         (utils/tell (str "The " (:desc obj) " goes through the grating into the darkness below.")))))

                 ;; Default - no special handling
                 :else nil)))})

;;; ---------------------------------------------------------------------------
;;; DAM/RESERVOIR AREA OBJECTS
;;; ---------------------------------------------------------------------------

;; <OBJECT DAM
;;     (IN DAM-ROOM)
;;     (SYNONYM DAM GATE GATES FCD\#3)
;;     (DESC "dam")
;;     (FLAGS NDESCBIT TRYTAKEBIT)
;;     (ACTION DAM-FUNCTION)>

(def dam-obj
  {:id :dam
   :in :dam-room
   :synonym ["dam" "gate" "gates" "fcd#3"]
   :desc "dam"
   :flags (flags/flags :ndesc :trytake)
   :action dam/dam-obj-action})

;; <OBJECT CONTROL-PANEL
;;     (IN DAM-ROOM)
;;     (SYNONYM PANEL)
;;     (ADJECTIVE CONTROL)
;;     (DESC "control panel")
;;     (FLAGS NDESCBIT)>

(def control-panel
  {:id :control-panel
   :in :dam-room
   :synonym ["panel"]
   :adjective "control"
   :desc "control panel"
   :flags (flags/flags :ndesc)})

;; <OBJECT BOLT
;;     (IN DAM-ROOM)
;;     (SYNONYM BOLT NUT)
;;     (ADJECTIVE METAL LARGE)
;;     (DESC "bolt")
;;     (FLAGS NDESCBIT TURNBIT TRYTAKEBIT)
;;     (ACTION BOLT-F)>

(def bolt
  {:id :bolt
   :in :dam-room
   :synonym ["bolt" "nut"]
   :adjective ["metal" "large"]
   :desc "bolt"
   :flags (flags/flags :ndesc :turn :trytake)
   :action dam/bolt-action})

;; <OBJECT BUBBLE
;;     (IN DAM-ROOM)
;;     (SYNONYM BUBBLE)
;;     (ADJECTIVE SMALL GREEN PLASTIC)
;;     (DESC "green bubble")
;;     (FLAGS NDESCBIT TRYTAKEBIT)
;;     (ACTION BUBBLE-F)>

(def bubble
  {:id :bubble
   :in :dam-room
   :synonym ["bubble"]
   :adjective ["small" "green" "plastic"]
   :desc "green bubble"
   :flags (flags/flags :ndesc :trytake)
   :action dam/bubble-action})

;; <OBJECT LEAK
;;     (IN MAINTENANCE-ROOM)
;;     (SYNONYM LEAK DRIP PIPE)
;;     (DESC "leak")
;;     (FLAGS NDESCBIT INVISIBLE)
;;     (ACTION LEAK-FUNCTION)>

(def leak
  {:id :leak
   :in :maintenance-room
   :synonym ["leak" "drip" "pipe"]
   :desc "leak"
   :flags (flags/flags :ndesc :invisible)
   :action dam/leak-action})

;; <OBJECT TUBE
;;     (IN MAINTENANCE-ROOM)
;;     (SYNONYM TUBE TOOTH PASTE)
;;     (DESC "tube")
;;     (FLAGS TAKEBIT CONTBIT READBIT)
;;     (ACTION TUBE-FUNCTION)
;;     (LDESC "There is an object which looks like a tube of toothpaste here.")
;;     (CAPACITY 7)
;;     (SIZE 5)
;;     (TEXT "---> Frobozz Magic Gunk Company <---\n      All-Purpose Gunk")>

(def tube
  {:id :tube
   :in :maintenance-room
   :synonym ["tube" "toothpaste" "paste"]
   :desc "tube"
   :flags (flags/flags :take :cont :read)
   :ldesc "There is an object which looks like a tube of toothpaste here."
   :capacity 7
   :size 5
   :text "---> Frobozz Magic Gunk Company <---\n      All-Purpose Gunk"
   :action dam/tube-action})

;; <OBJECT PUTTY
;;     (IN TUBE)
;;     (SYNONYM MATERIAL GUNK PUTTY)
;;     (ADJECTIVE VISCOUS)
;;     (DESC "viscous material")
;;     (FLAGS TAKEBIT TOOLBIT)
;;     (SIZE 6)
;;     (ACTION PUTTY-FCN)>

(def putty
  {:id :putty
   :in :tube
   :synonym ["material" "gunk" "putty"]
   :adjective "viscous"
   :desc "viscous material"
   :flags (flags/flags :take :tool)
   :size 6
   :action dam/putty-action})

;; <OBJECT GUIDE
;;     (IN DAM-LOBBY)
;;     (SYNONYM GUIDE BOOK BOOKS GUIDEBOOKS)
;;     (ADJECTIVE TOUR GUIDE)
;;     (DESC "tour guidebook")
;;     (FLAGS READBIT TAKEBIT BURNBIT)
;;     (FDESC "Some guidebooks entitled \"Flood Control Dam #3\" are on the reception desk.")
;;     (TEXT "...")>

(def guidebook
  {:id :guidebook
   :in :dam-lobby
   :synonym ["guide" "guidebook" "guidebooks" "book" "books"]
   :adjective ["tour" "guide"]
   :desc "tour guidebook"
   :flags (flags/flags :read :take :burn)
   :fdesc "Some guidebooks entitled \"Flood Control Dam #3\" are on the reception desk."
   :text "\"Flood Control Dam #3\n\nFCD#3 was constructed in year 783 of the Great Underground Empire to harness the mighty Frigid River. This work was supported by a grant of 37 million zorkmids from your omnipotent local tyrant Lord Dimwit Flathead the Excessive. This impressive structure is composed of 370,000 cubic feet of concrete, is 256 feet tall at the center, and 193 feet wide at the top. The lake created behind the dam has a volume of 1.7 billion cubic feet, an area of 12 million square feet, and a shore line of 36 thousand feet.\n\nThe construction of FCD#3 took 112 days from ground breaking to the dedication. It required a work force of 384 slaves, 34 slave drivers, 12 engineers, 2 turtle doves, and a partridge in a pear tree. The work was managed by a command team composed of 2345 bureaucrats, 2347 secretaries (at least two of whom could type), 12,256 paper shufflers, 52,469 rubber stampers, 245,193 red tape processors, and nearly one million dead trees.\n\nWe will now point out some of the more interesting features of FCD#3 as we conduct you on a guided tour of the facilities:\n\n1) You start your tour here in the Dam Lobby. You will notice on your right that....\""})

;; <OBJECT MATCH
;;     (IN DAM-LOBBY)
;;     (SYNONYM MATCH MATCHES MATCHBOOK)
;;     (ADJECTIVE MATCH)
;;     (DESC "matchbook")
;;     (FLAGS READBIT TAKEBIT)
;;     (ACTION MATCH-FUNCTION)
;;     (LDESC "There is a matchbook whose cover says \"Visit Beautiful FCD#3\" here.")
;;     (SIZE 2)
;;     (TEXT "...")>

(def matchbook
  {:id :matchbook
   :in :dam-lobby
   :synonym ["match" "matches" "matchbook"]
   :adjective "match"
   :desc "matchbook"
   :flags (flags/flags :read :take :light)
   :ldesc "There is a matchbook whose cover says \"Visit Beautiful FCD#3\" here."
   :size 2
   :text "(Close cover before striking)\n\nVisit Beautiful FCD#3\n\nThe Greatest Dam in the GUE!"
   :action light/match-action})

;; <OBJECT WRENCH
;;     (IN MAINTENANCE-ROOM)
;;     (SYNONYM WRENCH TOOL TOOLS)
;;     (DESC "wrench")
;;     (FLAGS TAKEBIT TOOLBIT)
;;     (SIZE 10)>

(def wrench
  {:id :wrench
   :in :maintenance-room
   :synonym ["wrench" "tool" "tools"]
   :desc "wrench"
   :flags (flags/flags :take :tool)
   :size 10})

;; <OBJECT YELLOW-BUTTON
;;     (IN MAINTENANCE-ROOM)
;;     (SYNONYM BUTTON SWITCH)
;;     (ADJECTIVE YELLOW)
;;     (DESC "yellow button")
;;     (FLAGS NDESCBIT)
;;     (ACTION BUTTON-F)>

(def yellow-button
  {:id :yellow-button
   :in :maintenance-room
   :synonym ["button" "switch"]
   :adjective "yellow"
   :desc "yellow button"
   :flags (flags/flags :ndesc)
   :action dam/button-action})

;; <OBJECT BROWN-BUTTON
;;     (IN MAINTENANCE-ROOM)
;;     (SYNONYM BUTTON SWITCH)
;;     (ADJECTIVE BROWN)
;;     (DESC "brown button")
;;     (FLAGS NDESCBIT)
;;     (ACTION BUTTON-F)>

(def brown-button
  {:id :brown-button
   :in :maintenance-room
   :synonym ["button" "switch"]
   :adjective "brown"
   :desc "brown button"
   :flags (flags/flags :ndesc)
   :action dam/button-action})

;; <OBJECT BLUE-BUTTON
;;     (IN MAINTENANCE-ROOM)
;;     (SYNONYM BUTTON SWITCH)
;;     (ADJECTIVE BLUE)
;;     (DESC "blue button")
;;     (FLAGS NDESCBIT)
;;     (ACTION BUTTON-F)>

(def blue-button
  {:id :blue-button
   :in :maintenance-room
   :synonym ["button" "switch"]
   :adjective "blue"
   :desc "blue button"
   :flags (flags/flags :ndesc)
   :action dam/button-action})

;; <OBJECT RED-BUTTON
;;     (IN MAINTENANCE-ROOM)
;;     (SYNONYM BUTTON SWITCH)
;;     (ADJECTIVE RED)
;;     (DESC "red button")
;;     (FLAGS NDESCBIT)
;;     (ACTION BUTTON-F)>

(def red-button
  {:id :red-button
   :in :maintenance-room
   :synonym ["button" "switch"]
   :adjective "red"
   :desc "red button"
   :flags (flags/flags :ndesc)
   :action dam/button-action})

;; <OBJECT PUMP
;;     (IN RESERVOIR-NORTH)
;;     (SYNONYM PUMP AIR-PUMP TOOL TOOLS)
;;     (ADJECTIVE SMALL HAND-HELD)
;;     (DESC "hand-held air pump")
;;     (FLAGS TAKEBIT TOOLBIT)>

(def pump
  {:id :pump
   :in :reservoir-north
   :synonym ["pump" "air-pump"]
   :adjective ["small" "hand-held"]
   :desc "hand-held air pump"
   :flags (flags/flags :take :tool)})

;;; ---------------------------------------------------------------------------
;;; MACHINE ROOM OBJECTS
;;; ---------------------------------------------------------------------------

;; <OBJECT MACHINE
;;     (IN MACHINE-ROOM)
;;     (SYNONYM MACHINE PDP10 DRYER LID)
;;     (DESC "machine")
;;     (FLAGS CONTBIT NDESCBIT TRYTAKEBIT)
;;     (ACTION MACHINE-F)
;;     (CAPACITY 50)>

(def ^:private dummy-responses
  "Humorous responses for already-done actions.
   ZIL: DUMMY global in gverbs.zil"
  ["It's already open."
   "You can't be serious."
   "An interesting idea..."])

(defn machine-action
  "Action handler for the coal machine.

   ZIL: MACHINE-F in 1actions.zil (lines 2515-2542)
   The machine transforms coal into a diamond when activated with
   the screwdriver while closed. Other objects become gunk."
  [game-state]
  (let [prsa (parser-state/get-prsa game-state)
        prso (parser-state/get-prso game-state)
        machine-open? (gs/set-thing-flag? game-state :machine :open)]
    (cond
      ;; TAKE the machine
      (and (= prsa :take) (= prso :machine))
      (utils/tell game-state "It is far too large to carry.")

      ;; OPEN
      (= prsa :open)
      (cond
        ;; Already open
        machine-open?
        (utils/tell game-state (random/rand-nth* dummy-responses))

        ;; Has contents - show them
        :else
        (let [contents (filter (fn [[_ obj]] (= (:in obj) :machine))
                               (:objects game-state))]
          (if (seq contents)
            (let [content-descs (map (fn [[_ obj]] (:desc obj)) contents)]
              (-> game-state
                  (gs/set-thing-flag :machine :open)
                  (utils/tell (str "The lid opens, revealing " (clojure.string/join ", " content-descs) "."))))
            (-> game-state
                (gs/set-thing-flag :machine :open)
                (utils/tell "The lid opens.")))))

      ;; CLOSE
      (= prsa :close)
      (if machine-open?
        (-> game-state
            (gs/unset-thing-flag :machine :open)
            (utils/tell "The lid closes."))
        (utils/tell game-state (random/rand-nth* dummy-responses)))

      ;; LAMP-ON (turn on) - redirect to switch
      (= prsa :lamp-on)
      (let [prsi (parser-state/get-prsi game-state)]
        (if (nil? prsi)
          (utils/tell game-state "It's not clear how to turn it on with your bare hands.")
          ;; Redirect to turn the switch with the tool
          nil))  ; Let the verb handler deal with it

      ;; Default
      :else nil)))

(def machine
  {:id :machine
   :in :machine-room
   :synonym ["machine" "pdp10" "dryer" "lid"]
   :desc "machine"
   :flags (flags/flags :cont :ndesc :trytake)
   :capacity 50
   :action machine-action})

;; <OBJECT MACHINE-SWITCH
;;     (IN MACHINE-ROOM)
;;     (SYNONYM SWITCH)
;;     (DESC "switch")
;;     (FLAGS NDESCBIT TURNBIT)
;;     (ACTION MSWITCH-FUNCTION)>

(defn machine-switch-action
  "Action handler for the machine switch.

   ZIL: MSWITCH-FUNCTION in 1actions.zil (lines 2544-2564)
   Turn the switch with a screwdriver to activate the machine.
   - If lid is open: machine does nothing
   - If coal is inside: coal becomes a diamond
   - Otherwise: contents become gunk"
  [game-state]
  (let [prsa (parser-state/get-prsa game-state)
        prsi (parser-state/get-prsi game-state)
        machine-open? (gs/set-thing-flag? game-state :machine :open)]
    (cond
      ;; TURN with screwdriver
      (and (= prsa :turn) (= prsi :screwdriver))
      (cond
        ;; Lid is open - nothing happens
        machine-open?
        (utils/tell game-state "The machine doesn't seem to want to do anything.")

        ;; Lid is closed - activate!
        :else
        (let [coal-in-machine? (= (gs/get-thing-loc-id game-state :coal) :machine)
              ;; Light show message
              gs (utils/tell game-state
                   "The machine comes to life (figuratively) with a dazzling display of colored lights and bizarre noises. After a few moments, the excitement abates.")]
          (if coal-in-machine?
            ;; Coal -> Diamond
            (-> gs
                (assoc-in [:objects :coal :in] :limbo)
                (assoc-in [:objects :huge-diamond :in] :machine))
            ;; Everything else -> Gunk
            (let [;; Remove all contents
                  contents (filter (fn [[id obj]] (= (:in obj) :machine)) (:objects gs))
                  gs-cleared (reduce (fn [state [id _]]
                                       (assoc-in state [:objects id :in] :limbo))
                                     gs contents)]
              (assoc-in gs-cleared [:objects :gunk :in] :machine)))))

      ;; TURN with other tool
      (= prsa :turn)
      (if prsi
        (utils/tell game-state (str "It seems that a " (:desc (gs/get-thing game-state prsi)) " won't do."))
        (utils/tell game-state "You need to use a tool to turn the switch."))

      ;; Default
      :else nil)))

(def machine-switch
  {:id :machine-switch
   :in :machine-room
   :synonym ["switch"]
   :desc "switch"
   :flags (flags/flags :ndesc :turn)
   :action machine-switch-action})

;; <OBJECT SCREWDRIVER
;;     (IN MAINTENANCE-ROOM)
;;     (SYNONYM SCREWDRIVER TOOL TOOLS DRIVER)
;;     (ADJECTIVE SCREW)
;;     (DESC "screwdriver")
;;     (FLAGS TAKEBIT TOOLBIT)>

(def screwdriver
  {:id :screwdriver
   :in :maintenance-room
   :synonym ["screwdriver" "tool" "tools" "driver"]
   :adjective "screw"
   :desc "screwdriver"
   :flags (flags/flags :take :tool)})

;; <OBJECT COAL
;;     (IN DEAD-END-5)
;;     (SYNONYM COAL PILE HEAP)
;;     (ADJECTIVE SMALL)
;;     (DESC "small pile of coal")
;;     (FLAGS TAKEBIT BURNBIT)
;;     (SIZE 20)>

(def coal
  {:id :coal
   :in :dead-end-5
   :synonym ["coal" "pile" "heap"]
   :adjective "small"
   :desc "small pile of coal"
   :flags (flags/flags :take :burn)
   :size 20})

;; <OBJECT GUNK
;;     (SYNONYM GUNK PIECE SLAG)
;;     (ADJECTIVE SMALL VITREOUS)
;;     (DESC "small piece of vitreous slag")
;;     (FLAGS TAKEBIT TRYTAKEBIT)
;;     (ACTION GUNK-FUNCTION)
;;     (SIZE 10)>

(defn gunk-action
  "Action handler for gunk (vitreous slag).

   ZIL: GUNK-FUNCTION in 1actions.zil (lines 2566-2569)
   The gunk crumbles when touched/taken."
  [game-state]
  (let [prsa (parser-state/get-prsa game-state)]
    (when (= prsa :take)
      (-> game-state
          (assoc-in [:objects :gunk :in] :limbo)
          (utils/tell "The slag was rather insubstantial, and crumbles into dust at your touch.")))))

(def gunk
  {:id :gunk
   :in :limbo  ; Appears when machine destroys non-coal items
   :synonym ["gunk" "piece" "slag"]
   :adjective ["small" "vitreous"]
   :desc "small piece of vitreous slag"
   :flags (flags/flags :take :trytake)
   :size 10
   :action gunk-action})

;;; ---------------------------------------------------------------------------
;;; SHAFT/BASKET OBJECTS
;;; ---------------------------------------------------------------------------

;; <OBJECT LOWERED-BASKET
;;     (IN LOWER-SHAFT)
;;     (SYNONYM CAGE DUMBWAITER BASKET)
;;     (ADJECTIVE LOWERED)
;;     (LDESC "From the chain is suspended a basket.")
;;     (DESC "basket")
;;     (FLAGS TRYTAKEBIT)
;;     (ACTION BASKET-F)>

;; <OBJECT RAISED-BASKET
;;     (IN SHAFT-ROOM)
;;     (SYNONYM CAGE DUMBWAITER BASKET)
;;     (DESC "basket")
;;     (FLAGS TRANSBIT TRYTAKEBIT CONTBIT OPENBIT)
;;     (ACTION BASKET-F)
;;     (LDESC "At the end of the chain is a basket.")
;;     (CAPACITY 50)>

(defn basket-action
  "Action handler for the shaft basket.

   ZIL: BASKET-F in 1actions.zil (lines 290-319)

   The basket can be raised or lowered via a chain. It's used
   to transport objects between shaft-room (top) and lower-shaft
   (bottom). The player cannot enter the basket.

   CAGE-TOP global tracks basket position:
   - true: raised basket at top (shaft-room), lowered at bottom
   - false: raised basket at bottom (lower-shaft), lowered at top"
  [game-state]
  (let [prsa (parser-state/get-prsa game-state)
        prso (parser-state/get-prso game-state)
        cage-top? (get game-state :cage-top true)]
    (cond
      ;; RAISE - bring basket up
      (= prsa :raise)
      (if cage-top?
        (utils/tell game-state (random/rand-nth* dummy-responses))
        ;; Swap positions - "raise" brings basket to top
        (-> game-state
            (assoc-in [:objects :raised-basket :in] :shaft-room)
            (assoc-in [:objects :lowered-basket :in] :lower-shaft)
            (assoc :cage-top true)
            (utils/tell "The basket is raised to the top of the shaft.")))

      ;; LOWER - send basket down
      (= prsa :lower)
      (if (not cage-top?)
        (utils/tell game-state (random/rand-nth* dummy-responses))
        ;; Swap positions - "lower" sends basket to bottom
        (let [gs (-> game-state
                     (assoc-in [:objects :raised-basket :in] :lower-shaft)
                     (assoc-in [:objects :lowered-basket :in] :shaft-room)
                     (assoc :cage-top false)
                     (utils/tell "The basket is lowered to the bottom of the shaft."))]
          ;; Check if we just lowered light away - might go dark
          ;; ZIL: <COND (<AND ,LIT <NOT <SETG LIT <LIT? ,HERE>>>>...
          gs))

      ;; Interacting with lowered basket from wrong end
      (or (= prso :lowered-basket)
          (= (parser-state/get-prsi game-state) :lowered-basket))
      (utils/tell game-state "The basket is at the other end of the chain.")

      ;; TAKE basket
      (and (= prsa :take)
           (contains? #{:raised-basket :lowered-basket} prso))
      (utils/tell game-state "The cage is securely fastened to the iron chain.")

      ;; Default
      :else nil)))

(def raised-basket
  {:id :raised-basket
   :in :shaft-room  ; Starts at top
   :synonym ["cage" "dumbwaiter" "basket"]
   :desc "basket"
   :flags (flags/flags :trans :trytake :cont :open)
   :ldesc "At the end of the chain is a basket."
   :capacity 50
   :action basket-action})

(def lowered-basket
  {:id :lowered-basket
   :in :lower-shaft  ; Virtual - represents "other end of chain"
   :synonym ["cage" "dumbwaiter" "basket"]
   :adjective "lowered"
   :desc "basket"
   :flags (flags/flags :trytake)
   :ldesc "From the chain is suspended a basket."
   :action basket-action})

;;; ---------------------------------------------------------------------------
;;; RAINBOW
;;; ---------------------------------------------------------------------------

;; ZIL: <OBJECT RAINBOW
;;	(IN LOCAL-GLOBALS)
;;	(SYNONYM RAINBOW)
;;	(DESC "rainbow")
;;	(FLAGS NDESCBIT)
;;	(ACTION RAINBOW-FCN)>

(defn rainbow-action
  "Rainbow action handler - handles crossing and looking under.

   ZIL: RAINBOW-FCN (1actions.zil lines 2647-2663)

   - CROSS/THROUGH: walk across if rainbow is solid (rainbow-flag set)
   - LOOK-UNDER: shows the river flows under it"
  [game-state]
  (let [prsa (parser-state/get-prsa game-state)
        here (:here game-state)
        rainbow-solid? (get game-state :rainbow-flag false)]
    (cond
      ;; CROSS or THROUGH the rainbow
      (contains? #{:cross :through :enter} prsa)
      (cond
        ;; From Canyon View - too far
        (= here :canyon-view)
        (utils/tell game-state "From here?!?")

        ;; Rainbow is solid - can cross
        rainbow-solid?
        (cond
          (= here :aragain-falls)
          (-> game-state
              (assoc :here :end-of-rainbow)
              (utils/tell "You walk across the rainbow..."))
          (= here :end-of-rainbow)
          (-> game-state
              (assoc :here :aragain-falls)
              (utils/tell "You walk across the rainbow..."))
          :else
          (utils/tell game-state "You'll have to say which way..."))

        ;; Rainbow is not solid
        :else
        (utils/tell game-state "Can you walk on water vapor?"))

      ;; LOOK-UNDER the rainbow
      (= prsa :look-under)
      (utils/tell game-state "The Frigid River flows under the rainbow.")

      ;; Not handled
      :else nil)))

(def rainbow
  {:id :rainbow
   :in :local-globals  ; present in multiple rooms
   :synonym ["rainbow"]
   :desc "rainbow"
   :flags (flags/flags :ndesc)
   :action rainbow-action})

;;; ---------------------------------------------------------------------------
;;; ALL OBJECTS LIST
;;; ---------------------------------------------------------------------------

(def all-objects
  "List of all object definitions."
  [adventurer
   mailbox
   leaflet
   white-house
   kitchen-window
   slide
   chimney
   ;; Sand/beach objects
   sand
   jeweled-scarab
   shovel
   trap-door
   kitchen-table
   brown-sack
   lunch
   garlic
   bottle
   water
   brass-lantern
   trophy-case
   sword-obj
   rug
   attic-table
   rope
   railing
   knife
   painting
   tree
   nest
   egg
   broken-egg
   broken-canary
   troll
   axe
   ;; Thief and associated objects
   stiletto
   large-bag
   thief
   ;; Cyclops
   cyclops
   ;; Maze objects
   skeleton
   burned-out-lantern
   bag-of-coins
   rusty-knife
   skeleton-key
   ;; Grating area objects
   leaves
   grate
   ;; Dam/Reservoir area objects
   dam-obj
   control-panel
   bolt
   bubble
   leak
   tube
   putty
   guidebook
   matchbook
   wrench
   yellow-button
   brown-button
   blue-button
   red-button
   pump
   ;; Machine room objects
   machine
   machine-switch
   screwdriver
   coal
   gunk
   ;; Shaft/basket objects
   raised-basket
   lowered-basket
   rainbow
   ;; Loud Room treasure
   platinum-bar
   ;; Treasures
   jade-figurine
   crystal-trident
   sapphire-bracelet
   jeweled-scarab
   silver-chalice
   gold-coffin
   sceptre
   ivory-torch
   candles
   brass-bell
   hot-bell
   trunk-of-jewels
   huge-diamond
   large-emerald
   pot-of-gold
   clockwork-canary
   brass-bauble
   buoy])
