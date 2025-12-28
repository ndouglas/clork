(ns clork.objects
  "Object definitions for Clork."
  (:require [clork.utils :as utils]
            [clork.flags :as flags]
            [clork.game-state :as gs]
            [clork.parser.state :as parser-state]
            [clork.verbs-look :as verbs-look]))

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
             (when (and (= (:verb game-state) :take) (= (:prso game-state) :mailbox))
               (utils/tell game-state "It is securely anchored.\n")))})

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
        gs (assoc gs :lit (room-lit? gs room-id))]
    ;; Describe the room
    (verbs-look/v-look gs)))

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
   :in :behind-house  ; It's a global, visible from both sides
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
;;	(SYNONYM DOOR TRAPDOOR)
;;	(ADJECTIVE TRAP)
;;	(DESC "trap door")
;;	(FLAGS DOORBIT INVISIBLE NDESCBIT)
;;	(ACTION TRAP-DOOR-F)>

(def trap-door
  {:id :trap-door
   :in :living-room
   :synonym ["door" "trapdoor"]
   :adjective ["trap"]
   :desc "trap door"
   :flags (flags/flags :door :ndesc)})  ; Starts closed, hidden under rug

;;; ---------------------------------------------------------------------------
;;; KITCHEN OBJECTS
;;; ---------------------------------------------------------------------------

;; <OBJECT BROWN-SACK
;;	(IN KITCHEN)
;;	(SYNONYM BAG SACK)
;;	(ADJECTIVE BROWN ELONGATED SMELLY)
;;	(DESC "brown sack")
;;	(FLAGS TAKEBIT CONTBIT BURNBIT)
;;	(CAPACITY 9)
;;	(SIZE 6)>

;; ZIL: SANDWICH-BAG in 1dungeon.zil
;;   (FDESC "On the table is an elongated brown sack, smelling of hot peppers.")
(def brown-sack
  {:id :brown-sack
   :in :kitchen
   :synonym ["bag" "sack"]
   :adjective ["brown" "elongated" "smelly"]
   :desc "brown sack"
   :flags (flags/flags :take :cont :burn)
   :fdesc "On the table is an elongated brown sack, smelling of hot peppers."
   :capacity 9
   :size 6})

;; <OBJECT BOTTLE
;;	(IN KITCHEN)
;;	(SYNONYM BOTTLE CONTAINER)
;;	(ADJECTIVE GLASS)
;;	(DESC "glass bottle")
;;	(FLAGS TAKEBIT CONTBIT TRANSBIT)
;;	(CAPACITY 4)
;;	(SIZE 6)>

;; ZIL: (FDESC "A bottle is sitting on the table.")
(def bottle
  {:id :bottle
   :in :kitchen
   :synonym ["bottle" "container"]
   :adjective ["glass"]
   :desc "glass bottle"
   :flags (flags/flags :take :cont :trans)
   :fdesc "A bottle is sitting on the table."
   :capacity 4
   :size 6})

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
;;	(FLAGS CONTBIT NDESCBIT TRANSBIT SEARCHBIT)
;;	(ACTION TROPHY-CASE-F)
;;	(CAPACITY 10000)>

(def trophy-case
  {:id :trophy-case
   :in :living-room
   :synonym ["case"]
   :adjective ["trophy"]
   :desc "trophy case"
   :flags (flags/flags :cont :ndesc :trans)
   :capacity 10000})

;; <OBJECT SWORD
;;	(IN LIVING-ROOM)
;;	(SYNONYM SWORD ORCHRIST GLAMDRING BLADE)
;;	(ADJECTIVE ELVISH ANTIQUE OLD)
;;	(DESC "elvish sword")
;;	(FLAGS TAKEBIT TRYTAKEBIT WEAPONBIT)
;;	(ACTION SWORD-F)
;;	(SIZE 10)
;;	(TVALUE 0)>

;; ZIL: (FDESC "Above the trophy case hangs an elvish sword of great antiquity.")
(def sword
  {:id :sword
   :in :living-room
   :synonym ["sword" "orchrist" "glamdring" "blade"]
   :adjective ["elvish" "antique" "old"]
   :desc "elvish sword"
   :flags (flags/flags :take :trytake :weapon :vowel)  ; :vowel for "an elvish"
   :fdesc "Above the trophy case hangs an elvish sword of great antiquity."
   :size 10
   :value 0})

;; <OBJECT RUG
;;	(IN LIVING-ROOM)
;;	(SYNONYM RUG CARPET)
;;	(ADJECTIVE LARGE ORIENTAL)
;;	(DESC "large oriental rug")
;;	(FLAGS NDESCBIT)
;;	(ACTION RUG-F)>

(def rug
  {:id :rug
   :in :living-room
   :synonym ["rug" "carpet"]
   :adjective ["large" "oriental"]
   :desc "large oriental rug"
   :flags (flags/flags :ndesc)})

;;; ---------------------------------------------------------------------------
;;; ATTIC OBJECTS
;;; ---------------------------------------------------------------------------

;; <OBJECT ROPE
;;	(IN ATTIC)
;;	(SYNONYM ROPE HEMP COIL)
;;	(ADJECTIVE LARGE)
;;	(DESC "coil of rope")
;;	(FLAGS TAKEBIT)
;;	(SIZE 10)>

(def rope
  {:id :rope
   :in :attic
   :synonym ["rope" "hemp" "coil"]
   :adjective ["large"]
   :desc "coil of rope"
   :flags (flags/flags :take)
   :size 10})

;; <OBJECT NASTY-KNIFE
;;	(IN ATTIC)
;;	(SYNONYM KNIFE)
;;	(ADJECTIVE NASTY)
;;	(DESC "nasty knife")
;;	(FLAGS TAKEBIT WEAPONBIT)
;;	(SIZE 4)>

(def knife
  {:id :knife
   :in :attic
   :synonym ["knife"]
   :adjective ["nasty"]
   :desc "nasty knife"
   :flags (flags/flags :take :weapon)
   :size 4})

;;; ---------------------------------------------------------------------------
;;; GALLERY OBJECTS
;;; ---------------------------------------------------------------------------

;; <OBJECT PAINTING
;;	(IN GALLERY)
;;	(SYNONYM PAINTING CANVAS)
;;	(ADJECTIVE BEAUTIFUL HUGE)
;;	(DESC "painting")
;;	(FLAGS TAKEBIT BURNBIT)
;;	(SIZE 15)
;;	(TVALUE 4)>

(def painting
  {:id :painting
   :in :gallery
   :synonym ["painting" "canvas"]
   :adjective ["beautiful" "huge"]
   :desc "painting"
   :flags (flags/flags :take :burn)
   :size 15
   :value 4})

;;; ---------------------------------------------------------------------------
;;; FOREST OBJECTS
;;; ---------------------------------------------------------------------------

;; <OBJECT TREE
;;   (IN LOCAL-GLOBALS)
;;   (SYNONYM TREE BRANCH)
;;   (ADJECTIVE LARGE STORM)
;;   (DESC "tree")
;;   (FLAGS NDESCBIT CLIMBBIT)>

;; Egg in the bird's nest
(def nest
  {:id :nest
   :in :up-a-tree
   :synonym ["nest"]
   :adjective ["bird's" "small"]
   :desc "bird's nest"
   :flags (flags/flags :take :cont)
   :capacity 6})

(def egg
  {:id :egg
   :in :nest
   :synonym ["egg" "bauble"]
   :adjective ["jewel-encrusted" "bird's"]
   :desc "jewel-encrusted egg"
   :flags (flags/flags :take :cont)
   :capacity 6
   :value 5})

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
   brown-sack
   bottle
   brass-lantern
   trophy-case
   sword
   rug
   rope
   knife
   painting
   nest
   egg])
