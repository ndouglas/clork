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
;;       (IN ROOMS)
;;       (LDESC "You are facing the north side of a white house. There is no door here,
;;               and all the windows are boarded up. To the north a narrow path winds through
;;               the trees.")
;;       (DESC "North of House")
;;       (SW TO WEST-OF-HOUSE)
;;       (SE TO EAST-OF-HOUSE)
;;       (WEST TO WEST-OF-HOUSE)
;;       (EAST TO EAST-OF-HOUSE)
;;       (NORTH TO PATH)
;;       (SOUTH "The windows are all boarded.")
;;       (FLAGS RLANDBIT ONBIT SACREDBIT)
;;       (GLOBAL BOARDED-WINDOW BOARD WHITE-HOUSE FOREST)>

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
;;       (IN ROOMS)
;;       (LDESC "You are facing the south side of a white house. There is no door here,
;;               and all the windows are boarded.")
;;       (DESC "South of House")
;;       (WEST TO WEST-OF-HOUSE)
;;       (EAST TO EAST-OF-HOUSE)
;;       (NE TO EAST-OF-HOUSE)
;;       (NW TO WEST-OF-HOUSE)
;;       (SOUTH TO FOREST-3)
;;       (NORTH "The windows are all boarded.")
;;       (FLAGS RLANDBIT ONBIT SACREDBIT)
;;       (GLOBAL BOARDED-WINDOW BOARD WHITE-HOUSE FOREST)>

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

;; <ROOM EAST-OF-HOUSE
;;       (IN ROOMS)
;;       (DESC "Behind House")
;;       (NORTH TO NORTH-OF-HOUSE)
;;       (SOUTH TO SOUTH-OF-HOUSE)
;;       (SW TO SOUTH-OF-HOUSE)
;;       (NW TO NORTH-OF-HOUSE)
;;       (EAST TO CLEARING)
;;       (WEST TO KITCHEN IF KITCHEN-WINDOW IS OPEN)
;;       (IN TO KITCHEN IF KITCHEN-WINDOW IS OPEN)
;;       (ACTION EAST-HOUSE)
;;       (FLAGS RLANDBIT ONBIT SACREDBIT)
;;       (GLOBAL WHITE-HOUSE KITCHEN-WINDOW FOREST)>

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

;; <ROOM FOREST-1
;;       (IN ROOMS)
;;       (LDESC "This is a forest, with trees in all directions. To the east,
;;               there appears to be sunlight.")
;;       (DESC "Forest")
;;       (UP "There is no tree here suitable for climbing.")
;;       (NORTH TO GRATING-CLEARING)
;;       (EAST TO PATH)
;;       (SOUTH TO FOREST-3)
;;       (WEST "You would need a machete to go further west.")
;;       (ACTION FOREST-ROOM)
;;       (FLAGS RLANDBIT ONBIT SACREDBIT)
;;       (GLOBAL TREE SONGBIRD WHITE-HOUSE FOREST)>

(def forest-1
  {:id :forest-1
   :desc "Forest"
   :ldesc "This is a forest, with trees in all directions. To the east, there appears to be sunlight."
   :flags #{:lit}
   :exits {:up "There is no tree here suitable for climbing."
           :north "TODO: This exit leads to GRATING-CLEARING."
           :east :forest-path
           :south :forest-3
           :west "You would need a machete to go further west."}})

;; <ROOM FOREST-3
;;       (IN ROOMS)
;;       (LDESC "This is a dimly lit forest, with large trees all around.")
;;       (DESC "Forest")
;;       (UP "There is no tree here suitable for climbing.")
;;       (NORTH TO CLEARING)
;;       (EAST "The rank undergrowth prevents eastward movement.")
;;       (SOUTH "Storm-tossed trees block your way.")
;;       (WEST TO FOREST-1)
;;       (NW TO SOUTH-OF-HOUSE)
;;       (ACTION FOREST-ROOM)
;;       (FLAGS RLANDBIT ONBIT SACREDBIT)
;;       (GLOBAL TREE SONGBIRD WHITE-HOUSE FOREST)>

(def forest-3
  {:id :forest-3
   :desc "Forest"
   :ldesc "This is a dimly lit forest, with large trees all around."
   :flags #{:lit}
   :exits {:up "There is no tree here suitable for climbing."
           :north :clearing
           :east "The rank undergrowth prevents eastward movement."
           :south "Storm-tossed trees block your way."
           :west :forest-1
           :nw :south-of-house}})

;; <ROOM PATH
;;       (IN ROOMS)
;;       (LDESC "This is a path winding through a dimly lit forest. The path heads
;;               north-south here. One particularly large tree with some low branches
;;               stands at the edge of the path.")
;;       (DESC "Forest Path")
;;       (UP TO UP-A-TREE)
;;       (NORTH TO GRATING-CLEARING)
;;       (EAST TO FOREST-2)
;;       (SOUTH TO NORTH-OF-HOUSE)
;;       (WEST TO FOREST-1)
;;       (ACTION FOREST-ROOM)
;;       (FLAGS RLANDBIT ONBIT SACREDBIT)
;;       (GLOBAL TREE SONGBIRD WHITE-HOUSE FOREST)>

(def forest-path
  {:id :forest-path
   :desc "Forest Path"
   :ldesc "This is a path winding through a dimly lit forest. The path heads north-south here. One particularly large tree with some low branches stands at the edge of the path."
   :flags #{:lit}
   :exits {:up :up-a-tree
           :north "TODO: This exit leads to GRATING-CLEARING."
           :east "TODO: This exit leads to FOREST-2."
           :south :north-of-house
           :west :forest-1}})

;; <ROOM CLEARING
;;       (IN ROOMS)
;;       (LDESC "You are in a small clearing in a well marked forest path that
;;               extends to the east and west.")
;;       (DESC "Clearing")
;;       (UP "There is no tree here suitable for climbing.")
;;       (EAST TO CANYON-VIEW)
;;       (NORTH TO FOREST-2)
;;       (SOUTH TO FOREST-3)
;;       (WEST TO EAST-OF-HOUSE)
;;       (ACTION FOREST-ROOM)
;;       (FLAGS RLANDBIT ONBIT SACREDBIT)
;;       (GLOBAL TREE SONGBIRD WHITE-HOUSE FOREST)>

(def clearing
  {:id :clearing
   :desc "Clearing"
   :ldesc "You are in a small clearing in a well marked forest path that extends to the east and west."
   :flags #{:lit}
   :exits {:up "There is no tree here suitable for climbing."
           :east "TODO: This exit leads to CANYON-VIEW."
           :north "TODO: This exit leads to FOREST-2."
           :south :forest-3
           :west :behind-house}})

;; <ROOM UP-A-TREE
;;       (IN ROOMS)
;;       (DESC "Up a Tree")
;;       (DOWN TO PATH)
;;       (UP "You cannot climb any higher.")
;;       (ACTION TREE-ROOM)
;;       (FLAGS RLANDBIT ONBIT SACREDBIT)
;;       (GLOBAL TREE FOREST SONGBIRD WHITE-HOUSE)>

(def up-a-tree
  {:id :up-a-tree
   :desc "Up a Tree"
   :ldesc "You are about 10 feet above the ground nestled among some large branches. The nearest branch above you is above your reach. Beside you on the branch is a small bird's nest."
   :flags #{:lit}
   :exits {:down :forest-path
           :up "You cannot climb any higher."}})

;;; ---------------------------------------------------------------------------
;;; INSIDE THE HOUSE
;;; ---------------------------------------------------------------------------

;; <ROOM KITCHEN
;;       (IN ROOMS)
;;       (DESC "Kitchen")
;;       (EAST TO EAST-OF-HOUSE IF KITCHEN-WINDOW IS OPEN)
;;       (WEST TO LIVING-ROOM)
;;       (OUT TO EAST-OF-HOUSE IF KITCHEN-WINDOW IS OPEN)
;;       (UP TO ATTIC)
;;       (DOWN TO STUDIO IF FALSE-FLAG ELSE "Only Santa Claus climbs down chimneys.")
;;       (ACTION KITCHEN-FCN)
;;       (FLAGS RLANDBIT ONBIT SACREDBIT)
;;       (VALUE 10)
;;       (GLOBAL KITCHEN-WINDOW CHIMNEY STAIRS)>

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
;;       (IN ROOMS)
;;       (DESC "Living Room")
;;       (EAST TO KITCHEN)
;;       (WEST TO STRANGE-PASSAGE IF MAGIC-FLAG ELSE "The door is nailed shut.")
;;       (DOWN PER TRAP-DOOR-EXIT)
;;       (ACTION LIVING-ROOM-FCN)
;;       (FLAGS RLANDBIT ONBIT SACREDBIT)
;;       (GLOBAL STAIRS)
;;       (PSEUDO "NAILS" NAILS-PSEUDO "NAIL" NAILS-PSEUDO)>

(def living-room
  {:id :living-room
   :desc "Living Room"
   :ldesc "You are in the living room. There is a doorway to the east, a wooden door with strange gothic lettering to the west, which appears to be nailed shut, a trophy case, and a large oriental rug in the center of the room."
   :flags #{:lit}
   :exits {:east :kitchen
           :west "The door is nailed shut."
           :down {:to :cellar :door :trap-door}}})

;; <ROOM ATTIC
;;       (IN ROOMS)
;;       (LDESC "This is the attic. The only exit is a stairway leading down.")
;;       (DESC "Attic")
;;       (DOWN TO KITCHEN)
;;       (FLAGS RLANDBIT SACREDBIT)
;;       (GLOBAL STAIRS)>

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
;;       (IN ROOMS)
;;       (DESC "Cellar")
;;       (NORTH TO TROLL-ROOM)
;;       (SOUTH TO EAST-OF-CHASM)
;;       (UP TO LIVING-ROOM IF TRAP-DOOR IS OPEN)
;;       (WEST "You try to ascend the ramp, but it is impossible, and you slide back down.")
;;       (ACTION CELLAR-FCN)
;;       (FLAGS RLANDBIT)
;;       (VALUE 25)
;;       (GLOBAL TRAP-DOOR SLIDE STAIRS)>

(def cellar
  {:id :cellar
   :desc "Cellar"
   :ldesc "You are in a dark and damp cellar with a narrow passageway leading north, and a crawlway to the south. On the west is the bottom of a steep metal ramp which is unclimbable."
   :flags #{}  ; Underground - not lit
   :exits {:north :troll-room
           :south :east-of-chasm
           :up {:to :living-room :door :trap-door}
           :west "You try to ascend the ramp, but it is impossible, and you slide back down."}})

;; <ROOM TROLL-ROOM
;;       (IN ROOMS)
;;       (LDESC "This is a small room with passages to the east and south and a
;;               forbidding hole leading west. Bloodstains and deep scratches
;;               (perhaps made by an axe) mar the walls.")
;;       (DESC "The Troll Room")
;;       (SOUTH TO CELLAR)
;;       (EAST TO EW-PASSAGE IF TROLL-FLAG ELSE "The troll fends you off with a menacing gesture.")
;;       (WEST TO MAZE-1 IF TROLL-FLAG ELSE "The troll fends you off with a menacing gesture.")
;;       (FLAGS RLANDBIT)
;;       (ACTION TROLL-ROOM-F)>

(def troll-room
  {:id :troll-room
   :desc "The Troll Room"
   :ldesc "This is a small room with passages to the east and south and a forbidding hole leading west. Bloodstains and deep scratches (perhaps made by an axe) mar the walls."
   :flags #{}  ; Underground
   :exits {:south :cellar
           :east :east-west-passage
           :west :maze-1}})

;; <ROOM EAST-OF-CHASM
;;       (IN ROOMS)
;;       (LDESC "You are on the east edge of a chasm, the bottom of which cannot be
;;               seen. A narrow passage goes north, and the path you are on continues
;;               to the east.")
;;       (DESC "East of Chasm")
;;       (NORTH TO CELLAR)
;;       (EAST TO GALLERY)
;;       (DOWN "The chasm probably leads straight to the infernal regions.")
;;       (FLAGS RLANDBIT)
;;       (PSEUDO "CHASM" CHASM-PSEUDO)>

(def east-of-chasm
  {:id :east-of-chasm
   :desc "East of Chasm"
   :ldesc "You are on the east edge of a chasm, the bottom of which cannot be seen. A narrow passage goes north, and the path you are on continues to the east."
   :flags #{}  ; Underground
   :exits {:north :cellar
           :east :gallery
           :down "The chasm probably leads straight to the infernal regions."}})

;; <ROOM GALLERY
;;       (IN ROOMS)
;;       (LDESC "This is an art gallery. Most of the paintings have been stolen by
;;               vandals with exceptional taste. The vandals left through either the
;;               north or west exits.")
;;       (DESC "Gallery")
;;       (WEST TO EAST-OF-CHASM)
;;       (NORTH TO STUDIO)
;;       (FLAGS RLANDBIT ONBIT)>

(def gallery
  {:id :gallery
   :desc "Gallery"
   :ldesc "This is an art gallery. Most of the paintings have been stolen by vandals with exceptional taste. The vandals left through either the north or west exits."
   :flags #{:lit}  ; ZIL has ONBIT - gallery is lit
   :exits {:west :east-of-chasm
           :north :studio}})

;; <ROOM STUDIO
;;       (IN ROOMS)
;;       (LDESC "This appears to have been an artist's studio. The walls and floors are
;;               splattered with paints of 69 different colors. Strangely enough, nothing
;;               of value is hanging here. At the south end of the room is an open door
;;               (also covered with paint). A dark and narrow chimney leads up from a
;;               fireplace; although you might be able to get up it, it seems unlikely
;;               you could get back down.")
;;       (DESC "Studio")
;;       (SOUTH TO GALLERY)
;;       (UP PER UP-CHIMNEY-FUNCTION)
;;       (FLAGS RLANDBIT)
;;       (GLOBAL CHIMNEY)
;;       (PSEUDO "DOOR" DOOR-PSEUDO "PAINT" PAINT-PSEUDO)>

(def studio
  {:id :studio
   :desc "Studio"
   :ldesc "This appears to have been an artist's studio. The walls and floors are splattered with paints of 69 different colors. Strangely enough, nothing of value is hanging here. At the south end of the room is an open door (also covered with paint). A dark and narrow chimney leads up from a fireplace; although you might be able to get up it, it seems unlikely you could get back down."
   :flags #{}  ; Underground
   :exits {:south :gallery
           :up :kitchen  ; via chimney
           :north :dark-area}})

;; <ROOM EW-PASSAGE
;;       (IN ROOMS)
;;       (LDESC "This is a narrow east-west passageway. There is a narrow stairway
;;               leading down at the north end of the room.")
;;       (DESC "East-West Passage")
;;       (EAST TO ROUND-ROOM)
;;       (WEST TO TROLL-ROOM)
;;       (DOWN TO CHASM-ROOM)
;;       (NORTH TO CHASM-ROOM)
;;       (FLAGS RLANDBIT)
;;       (VALUE 5)
;;       (GLOBAL STAIRS)>

(def east-west-passage
  {:id :east-west-passage
   :desc "East-West Passage"
   :ldesc "This is a narrow east-west passageway. There is a narrow stairway leading down at the north end of the room."
   :flags #{}
   :exits {:west :troll-room
           :east "TODO: This exit leads to ROUND-ROOM."
           :down "TODO: This exit leads to CHASM-ROOM."
           :north "TODO: This exit leads to CHASM-ROOM."}})

;; <ROOM MAZE-1
;;       (IN ROOMS)
;;       (LDESC "This is part of a maze of twisty little passages, all alike.")
;;       (DESC "Maze")
;;       (EAST TO TROLL-ROOM)
;;       (NORTH TO MAZE-1)
;;       (SOUTH TO MAZE-2)
;;       (WEST TO MAZE-4)
;;       (FLAGS RLANDBIT MAZEBIT)>

(def maze-1
  {:id :maze-1
   :desc "Maze"
   :ldesc "This is part of a maze of twisty little passages, all alike."
   :flags #{}
   :exits {:east :troll-room
           :north :maze-1
           :south "TODO: This exit leads to MAZE-2."
           :west "TODO: This exit leads to MAZE-4."}})

;; Note: dark-area is a placeholder for unlit areas, not a specific ZIL room.
;; When the player enters an unlit room without a light source, they see
;; the "pitch black" message and may be eaten by a grue.

(def dark-area
  {:id :dark-area
   :desc "Dark Area"
   :ldesc "It is pitch black. You are likely to be eaten by a grue."
   :flags #{}
   :exits {:south :studio}})

;; <ROOM STONE-BARROW
;;       (IN ROOMS)
;;       (LDESC "You are standing in front of a massive barrow of stone. In the east face
;;               is a huge stone door which is open. You cannot see into the dark of the tomb.")
;;       (DESC "Stone Barrow")
;;       (NE TO WEST-OF-HOUSE)
;;       (ACTION STONE-BARROW-FCN)
;;       (FLAGS RLANDBIT ONBIT SACREDBIT)>

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
