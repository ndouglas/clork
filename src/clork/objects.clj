(ns clork.objects
  "Object definitions for Clork."
  (:require [clork.utils :as utils]
            [clork.flags :as flags]
            [clork.game-state :as gs]
            [clork.parser.state :as parser-state]
            [clork.verbs-look :as verbs-look]
            [clork.random :as random]
            [clork.sword :as sword]))

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
                 (= verb :raise)
                 (if is-open?
                   (utils/tell game-state "It is already open.")
                   (-> game-state
                       (gs/set-thing-flag :trap-door :open)
                       (gs/set-thing-flag :trap-door :touch)
                       (utils/tell "The door reluctantly opens to reveal a rickety staircase descending into darkness.")))

                 ;; OPEN/CLOSE from living-room
                 (and (#{:open :close} verb) (= here :living-room))
                 (cond
                   ;; OPEN
                   (= verb :open)
                   (if is-open?
                     (utils/tell game-state "It is already open.")
                     (-> game-state
                         (gs/set-thing-flag :trap-door :open)
                         (gs/set-thing-flag :trap-door :touch)
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
                   (utils/tell game-state (rand-nth ["Look around."
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

(def bottle
  {:id :bottle
   :in :kitchen-table
   :synonym ["bottle" "container"]
   :adjective ["clear" "glass"]
   :desc "glass bottle"
   :flags (flags/flags :take :cont :trans)
   :fdesc "A bottle is sitting on the table."
   :capacity 4})

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
   :size 15})

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

(def trophy-case
  {:id :trophy-case
   :in :living-room
   :synonym ["case"]
   :adjective ["trophy"]
   :desc "trophy case"
   :flags (flags/flags :cont :ndesc :trans :trytake :search)
   :capacity 10000})

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

(def rope
  {:id :rope
   :in :attic
   :synonym ["rope" "hemp" "coil"]
   :adjective ["large"]
   :desc "coil of rope"
   :flags (flags/flags :take :sacred :trytake)
   :fdesc "A large coil of rope is lying in the corner."
   :size 10})

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
   :tvalue 6})

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
   :tvalue 5})

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
   :flags (flags/flags :ndesc :door)  ; starts closed and locked
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

                 ;; OPEN/CLOSE
                 (#{:open :close} prsa)
                 (if grunlock
                   (cond
                     (= prsa :open)
                     (if grate-open?
                       (utils/tell game-state "It is already open.")
                       (let [gs (gs/set-thing-flag game-state :grate :open)]
                         (if (= here :grating-clearing)
                           (utils/tell gs "The grating opens.")
                           (-> gs
                               (utils/tell "The grating opens to reveal trees above you.")))))
                     (= prsa :close)
                     (if grate-open?
                       (-> game-state
                           (gs/unset-thing-flag :grate :open)
                           (utils/tell "The grating is closed."))
                       (utils/tell game-state "It is already closed.")))
                   (utils/tell game-state "The grating is locked."))

                 ;; Default - no special handling
                 :else nil)))})

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
   knife
   painting
   tree
   nest
   egg
   troll
   axe
   ;; Maze objects
   skeleton
   burned-out-lantern
   bag-of-coins
   rusty-knife
   skeleton-key
   grate])
