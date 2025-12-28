(ns clork.rooms
  "Room definitions for Clork."
  (:require [clork.utils :as utils]
            [clork.game-state :as gs]))

;;; ---------------------------------------------------------------------------
;;; ABOVE GROUND - HOUSE EXTERIOR
;;; ---------------------------------------------------------------------------

;; <ROOM WEST-OF-HOUSE
;;       (IN ROOMS)
;;       (DESC "West of House")
;;       (NORTH TO NORTH-OF-HOUSE)
;;       (SOUTH TO SOUTH-OF-HOUSE)
;;       (NE TO NORTH-OF-HOUSE)
;;       (SE TO SOUTH-OF-HOUSE)
;;       (WEST TO FOREST-1)
;;       (EAST "The door is boarded and you can't remove the boards.")
;;       (SW TO STONE-BARROW IF WON-FLAG)
;;       (IN TO STONE-BARROW IF WON-FLAG)
;;       (ACTION WEST-HOUSE)
;;       (FLAGS RLANDBIT ONBIT SACREDBIT)
;;       (GLOBAL WHITE-HOUSE BOARD FOREST)>

(def west-of-house
  {:id :west-of-house
   :desc "West of House"
   :flags #{:lit}
   :exits {:north :north-of-house
           :south :south-of-house
           :ne :north-of-house
           :se :south-of-house
           :west :forest-1
           :east "The door is boarded and you can't remove the boards."
           :sw {:to :stone-barrow :if :won}
           :in {:to :stone-barrow :if :won}}
   :action (fn [game-state rarg]
             (-> game-state
                 (cond-> (= rarg :look)
                   (utils/tell "You are standing in an open field west of a white house, with a boarded front door."))
                 (cond-> (:won game-state)
                   (utils/tell " A secret path leads southwest into the forest."))
                 (utils/crlf)))})

;; <ROOM NORTH-OF-HOUSE
;;       (DESC "North of House")
;;       (LDESC "You are facing the north side of a white house...")

(def north-of-house
  {:id :north-of-house
   :desc "North of House"
   :ldesc "You are facing the north side of a white house. There is no door here, and all the windows are boarded up. To the north a narrow path winds through the trees."
   :flags #{:lit}
   :exits {:sw :west-of-house
           :se :behind-house
           :west :west-of-house
           :east :behind-house
           :north :forest-path
           :south "The windows are all boarded."}})

;; <ROOM SOUTH-OF-HOUSE
;;       (DESC "South of House")
;;       (LDESC "You are facing the south side of a white house...")

(def south-of-house
  {:id :south-of-house
   :desc "South of House"
   :ldesc "You are facing the south side of a white house. There is no door here, and all the windows are boarded."
   :flags #{:lit}
   :exits {:west :west-of-house
           :east :behind-house
           :ne :behind-house
           :nw :west-of-house
           :south :forest-3
           :north "The windows are all boarded."}})

;; <ROOM EAST-OF-HOUSE (BEHIND-HOUSE)
;;       (DESC "Behind House")
;;       (ACTION EAST-HOUSE)
;;
;; ZIL: EAST-HOUSE in 1actions.zil
;;   <COND (<EQUAL? .RARG ,M-LOOK>
;;     <TELL "You are behind the white house...which is ">
;;     <COND (<FSET? ,KITCHEN-WINDOW ,OPENBIT> <TELL "open.">)
;;           (T <TELL "slightly ajar.">)>

(def behind-house
  {:id :behind-house
   :desc "Behind House"
   :flags #{:lit}
   :exits {:north :north-of-house
           :south :south-of-house
           :sw :south-of-house
           :nw :north-of-house
           :east :clearing
           :west {:to :kitchen :door :kitchen-window}
           :in {:to :kitchen :door :kitchen-window}}
   :action (fn [game-state rarg]
             (when (= rarg :look)
               (let [window-open? (gs/set-thing-flag? game-state :kitchen-window :open)
                     window-state (if window-open? "open" "slightly ajar")]
                 (-> game-state
                     (utils/tell (str "You are behind the white house. A path leads into the forest to the east. In one corner of the house there is a small window which is " window-state "."))
                     (utils/crlf)))))})

;;; ---------------------------------------------------------------------------
;;; ABOVE GROUND - FOREST
;;; ---------------------------------------------------------------------------

(def forest-1
  {:id :forest-1
   :desc "Forest"
   :ldesc "This is a forest, with trees in all directions. To the east, there appears to be sunlight."
   :flags #{:lit}
   :exits {:east :west-of-house
           :north :forest-1
           :south :forest-1
           :west :forest-1}})

(def forest-3
  {:id :forest-3
   :desc "Forest"
   :ldesc "This is a dimly lit forest, with large trees all around."
   :flags #{:lit}
   :exits {:north :south-of-house
           :east :forest-3
           :south :forest-3
           :west :forest-3}})

(def forest-path
  {:id :forest-path
   :desc "Forest Path"
   :ldesc "This is a path winding through a dimly lit forest. The path heads north-south here. One particularly large tree with some low branches stands at the edge of the path."
   :flags #{:lit}
   :exits {:north :clearing
           :south :north-of-house
           :up :up-a-tree}})

(def clearing
  {:id :clearing
   :desc "Clearing"
   :ldesc "You are in a clearing, with a forest surrounding you on all sides. A path leads south."
   :flags #{:lit}
   :exits {:south :forest-path
           :west :behind-house}})

(def up-a-tree
  {:id :up-a-tree
   :desc "Up a Tree"
   :ldesc "You are about 10 feet above the ground nestled among some large branches. The nearest branch above you is above your reach. Beside you on the branch is a small bird's nest."
   :flags #{:lit}
   :exits {:down :forest-path}})

;;; ---------------------------------------------------------------------------
;;; INSIDE THE HOUSE
;;; ---------------------------------------------------------------------------

;; <ROOM KITCHEN
;;       (DESC "Kitchen")
;;       (ACTION KITCHEN-FCN)
;;
;; ZIL: KITCHEN-FCN in 1actions.zil
;;   <COND (<EQUAL? .RARG ,M-LOOK>
;;     <TELL "...to the east is a small window which is ">
;;     <COND (<FSET? ,KITCHEN-WINDOW ,OPENBIT> <TELL "open.">)
;;           (T <TELL "slightly ajar.">)>

(def kitchen
  {:id :kitchen
   :desc "Kitchen"
   :flags #{:lit}
   :exits {:west :living-room
           :up :attic
           :down :studio  ; via chimney, but only if small
           :east {:to :behind-house :door :kitchen-window}
           :out {:to :behind-house :door :kitchen-window}}
   :action (fn [game-state rarg]
             (when (= rarg :look)
               (let [window-open? (gs/set-thing-flag? game-state :kitchen-window :open)
                     window-state (if window-open? "open" "slightly ajar")]
                 (-> game-state
                     (utils/tell (str "You are in the kitchen of the white house. A table seems to have been used recently for the preparation of food. A passage leads to the west and a dark staircase can be seen leading upward. A dark chimney leads down and to the east is a small window which is " window-state "."))
                     (utils/crlf)))))})

;; <ROOM LIVING-ROOM
;;       (DESC "Living Room")
;;       (ACTION LIVING-ROOM-FCN)

(def living-room
  {:id :living-room
   :desc "Living Room"
   :ldesc "You are in the living room. There is a doorway to the east, a wooden door with strange gothic lettering to the west, which appears to be nailed shut, a trophy case, and a large oriental rug in the center of the room."
   :flags #{:lit}
   :exits {:east :kitchen
           :west "The door is nailed shut."
           :down {:to :cellar :door :trap-door}}})

;; <ROOM ATTIC
;;       (DESC "Attic")

(def attic
  {:id :attic
   :desc "Attic"
   :ldesc "This is the attic. The only exit is a stairway leading down. A large coil of rope is lying in the corner. On a table is a nasty-looking knife."
   :flags #{:lit}
   :exits {:down :kitchen}})

;;; ---------------------------------------------------------------------------
;;; CELLAR / UNDERGROUND
;;; ---------------------------------------------------------------------------

;; <ROOM CELLAR
;;       (DESC "Cellar")

(def cellar
  {:id :cellar
   :desc "Cellar"
   :ldesc "You are in a dark and damp cellar with a narrow passageway leading north, and a crawlway to the south. On the west is the bottom of a steep metal ramp which is unclimbable."
   :flags #{}  ; Underground - not lit
   :exits {:north :troll-room
           :south :east-of-chasm
           :up {:to :living-room :door :trap-door}}})

;; <ROOM TROLL-ROOM
;;       (DESC "The Troll Room")

(def troll-room
  {:id :troll-room
   :desc "The Troll Room"
   :ldesc "This is a small room with passages to the east and south and a forbidding hole leading west. Bloodstains and deep scratches (perhaps made by stroing, sharp claws) mar the walls."
   :flags #{}  ; Underground
   :exits {:south :cellar
           :east :east-west-passage
           :west :maze-1}})

;; <ROOM EAST-OF-CHASM
;;       (DESC "East of Chasm")

(def east-of-chasm
  {:id :east-of-chasm
   :desc "East of Chasm"
   :ldesc "You are on the east edge of a chasm, the bottom of which cannot be seen. A narrow passage goes north, and the path you are on continues to the east."
   :flags #{}  ; Underground
   :exits {:north :cellar
           :east :gallery}})

;; <ROOM GALLERY
;;       (DESC "Gallery")

(def gallery
  {:id :gallery
   :desc "Gallery"
   :ldesc "This is an art gallery. Most of the paintings have been stolen by vandals with exceptionally bad taste. On the far wall is the one painting in the gallery which has not been stolen: a picture of a great and terrible river god; beneath it sits a prayer mat. A staircase leads up."
   :flags #{}  ; Underground
   :exits {:west :east-of-chasm
           :north :studio}})

;; <ROOM STUDIO
;;       (DESC "Studio")

(def studio
  {:id :studio
   :desc "Studio"
   :ldesc "This is what appears to have been an artist's studio. The walls and floors are covered with paint of every color. Strangely enough, nothing of value is visible. At the north end of the room is an open door (but it is totally dark beyond). A chimney leads up from a fireplace; there is a small hearth at the base of the chimney which smells of wood smoke."
   :flags #{}  ; Underground
   :exits {:south :gallery
           :up :kitchen  ; via chimney
           :north :dark-area}})

;; Placeholder for other needed rooms
(def east-west-passage
  {:id :east-west-passage
   :desc "East-West Passage"
   :ldesc "This is a narrow east-west passageway. There is a narrow stairway leading down at the north end of the room."
   :flags #{}
   :exits {:west :troll-room}})

(def maze-1
  {:id :maze-1
   :desc "Maze"
   :ldesc "This is a maze of twisty little passages, all alike."
   :flags #{}
   :exits {:east :troll-room}})

(def dark-area
  {:id :dark-area
   :desc "Dark Area"
   :ldesc "It is pitch black. You are likely to be eaten by a grue."
   :flags #{}
   :exits {:south :studio}})

(def stone-barrow
  {:id :stone-barrow
   :desc "Stone Barrow"
   :ldesc "You are standing in front of a massive barrow of stone. In the east face is a huge stone door which is open. You cannot see into the dark of the tomb."
   :flags #{:lit}
   :exits {:ne :west-of-house}})

;;; ---------------------------------------------------------------------------
;;; ALL ROOMS LIST
;;; ---------------------------------------------------------------------------

(def all-rooms
  "List of all room definitions."
  [west-of-house
   north-of-house
   south-of-house
   behind-house
   forest-1
   forest-3
   forest-path
   clearing
   up-a-tree
   kitchen
   living-room
   attic
   cellar
   troll-room
   east-of-chasm
   gallery
   studio
   east-west-passage
   maze-1
   dark-area
   stone-barrow])
