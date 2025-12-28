(ns clork.objects
  "Object definitions for Clork."
  (:require [clork.utils :as utils]
            [clork.flags :as flags]))

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
             (if (and (= (:verb game-state) :take) (= (:prso game-state) :mailbox))
               (utils/tell game-state "It is securely anchored.\n")
               game-state))})

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
;;; DOORS AND WINDOWS
;;; ---------------------------------------------------------------------------

;; <OBJECT KITCHEN-WINDOW
;;	(SYNONYM WINDOW)
;;	(ADJECTIVE KITCHEN SMALL)
;;	(DESC "kitchen window")
;;	(FLAGS DOORBIT NDESCBIT)
;;	(ACTION KITCHEN-WINDOW-F)>

(def kitchen-window
  {:id :kitchen-window
   :in :behind-house  ; It's a global, visible from both sides
   :synonym ["window"]
   :adjective ["kitchen" "small"]
   :desc "kitchen window"
   :flags (flags/flags :door :ndesc :open)})  ; Start slightly ajar per description

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

(def brown-sack
  {:id :brown-sack
   :in :kitchen
   :synonym ["bag" "sack"]
   :adjective ["brown" "elongated" "smelly"]
   :desc "brown sack"
   :flags (flags/flags :take :cont :burn)
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

(def bottle
  {:id :bottle
   :in :kitchen
   :synonym ["bottle" "container"]
   :adjective ["glass"]
   :desc "glass bottle"
   :flags (flags/flags :take :cont :trans)
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

(def brass-lantern
  {:id :brass-lantern
   :in :living-room
   :synonym ["lamp" "lantern" "light"]
   :adjective ["brass"]
   :desc "brass lantern"
   :flags (flags/flags :take :light)
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

(def sword
  {:id :sword
   :in :living-room
   :synonym ["sword" "orchrist" "glamdring" "blade"]
   :adjective ["elvish" "antique" "old"]
   :desc "elvish sword"
   :flags (flags/flags :take :trytake :weapon)
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
