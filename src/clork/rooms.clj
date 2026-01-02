(ns clork.rooms
  "Room definitions for Clork."
  (:require [clork.utils :as utils]
            [clork.game-state :as gs]
            [clork.thief :as thief]
            [clork.cyclops :as cyclops]
            [clork.dam :as dam]
            [clork.loud-room :as loud-room]))

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
   :flags #{:lit :sacred}
   :globals #{:white-house}  ; ZIL: (GLOBAL WHITE-HOUSE BOARD FOREST)
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
   :flags #{:lit :sacred}
   :globals #{:white-house}  ; ZIL: (GLOBAL BOARDED-WINDOW BOARD WHITE-HOUSE FOREST)
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
   :flags #{:lit :sacred}
   :globals #{:white-house}  ; ZIL: (GLOBAL BOARDED-WINDOW BOARD WHITE-HOUSE FOREST)
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
   :flags #{:lit :sacred}
   :globals #{:white-house :kitchen-window}  ; ZIL: (GLOBAL WHITE-HOUSE KITCHEN-WINDOW FOREST)
   :exits {:north :north-of-house
           :south :south-of-house
           :sw :south-of-house
           :nw :north-of-house
           :east :clearing
           :west {:to :kitchen :door :kitchen-window}
           :in {:to :kitchen :door :kitchen-window}}
   :action (fn [game-state rarg]
             (case rarg
               :look
               (let [window-open? (gs/set-thing-flag? game-state :kitchen-window :open)
                     window-state (if window-open? "open" "slightly ajar")]
                 (-> game-state
                     (utils/tell (str "You are behind the white house. A path leads into the forest to the east. In one corner of the house there is a small window which is " window-state "."))
                     (utils/crlf)))
               ;; Default - signal that default handling should be used
               (gs/use-default game-state)))})

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
   :flags #{:lit :sacred}
   :globals #{:tree :white-house}  ; ZIL: (GLOBAL TREE SONGBIRD WHITE-HOUSE FOREST)
   :exits {:up "There is no tree here suitable for climbing."
           :north :grating-clearing
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
   :flags #{:lit :sacred}
   :globals #{:tree :white-house}  ; ZIL: (GLOBAL TREE SONGBIRD WHITE-HOUSE FOREST)
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
   :flags #{:lit :sacred}
   :globals #{:tree :white-house}  ; ZIL: (GLOBAL TREE SONGBIRD WHITE-HOUSE FOREST)
   :exits {:up :up-a-tree
           :north :grating-clearing
           :east :forest-2
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
   :flags #{:lit :sacred}
   :globals #{:tree :white-house}  ; ZIL: (GLOBAL TREE SONGBIRD WHITE-HOUSE FOREST)
   :exits {:up "There is no tree here suitable for climbing."
           :east "TODO: This exit leads to CANYON-VIEW."
           :north :forest-2
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
   :flags #{:lit :sacred}
   :globals #{:tree :white-house}  ; ZIL: (GLOBAL TREE FOREST SONGBIRD WHITE-HOUSE)
   :exits {:down :forest-path
           :up "You cannot climb any higher."}})

;; <ROOM GRATING-CLEARING
;;       (IN ROOMS)
;;       (DESC "Clearing")
;;       (NORTH "The forest becomes impenetrable to the north.")
;;       (EAST TO FOREST-2)
;;       (WEST TO FOREST-1)
;;       (SOUTH TO PATH)
;;       (DOWN PER GRATING-EXIT)
;;       (ACTION CLEARING-FCN)
;;       (FLAGS RLANDBIT ONBIT SACREDBIT)
;;       (GLOBAL WHITE-HOUSE GRATE)>
;;
;; ZIL: CLEARING-FCN in 1actions.zil (lines 828-844)
;; This room has a dynamic description based on the grate state.

(def grating-clearing
  {:id :grating-clearing
   :desc "Clearing"
   ;; No :ldesc - CLEARING-FCN provides dynamic description via :look action
   :flags #{:lit :sacred}
   :globals #{:white-house :grate}
   :exits {:north "The forest becomes impenetrable to the north."
           :east :forest-2
           :west :forest-1
           :south :forest-path
           :down {:per :grating-exit}}
   :action (fn [game-state rarg]
             ;; ZIL: CLEARING-FCN in 1actions.zil (lines 828-844)
             (case rarg
               ;; M-ENTER: When entering, make grate invisible if not revealed
               :m-enter
               (if (not (get game-state :grate-revealed false))
                 (gs/set-thing-flag game-state :grate :invisible)
                 game-state)

               ;; M-LOOK: Dynamic room description based on grate state
               :look
               (let [grate-open? (gs/set-thing-flag? game-state :grate :open)
                     grate-revealed? (get game-state :grate-revealed false)]
                 (-> game-state
                     (utils/tell "You are in a clearing, with a forest surrounding you on all sides. A path leads south.")
                     (cond->
                       grate-open?
                       (-> (utils/crlf)
                           (utils/tell "There is an open grating, descending into darkness."))

                       (and grate-revealed? (not grate-open?))
                       (-> (utils/crlf)
                           (utils/tell "There is a grating securely fastened into the ground.")))
                     (utils/crlf)))

               ;; Default - signal that default handling should be used
               (gs/use-default game-state)))})

;; <ROOM FOREST-2
;;       (IN ROOMS)
;;       (LDESC "This is a dimly lit forest, with large trees all around.")
;;       (DESC "Forest")
;;       (UP "There is no tree here suitable for climbing.")
;;       (NORTH "The forest becomes impenetrable to the north.")
;;       (EAST TO MOUNTAINS)
;;       (SOUTH TO CLEARING)
;;       (WEST TO PATH)
;;       (ACTION FOREST-ROOM)
;;       (FLAGS RLANDBIT ONBIT SACREDBIT)
;;       (GLOBAL TREE SONGBIRD WHITE-HOUSE FOREST)>

(def forest-2
  {:id :forest-2
   :desc "Forest"
   :ldesc "This is a dimly lit forest, with large trees all around."
   :flags #{:lit :sacred}
   :globals #{:tree :white-house}
   :exits {:up "There is no tree here suitable for climbing."
           :north "The forest becomes impenetrable to the north."
           :east "TODO: This exit leads to MOUNTAINS."
           :south :clearing
           :west :forest-path}})

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
   :flags #{:lit :sacred}
   :globals #{:kitchen-window}  ; ZIL: (GLOBAL KITCHEN-WINDOW CHIMNEY STAIRS)
   :value 10   ; ZIL: (VALUE 10) - points for entering house first time
   :exits {:west :living-room
           :up :attic
           ;; ZIL: (DOWN TO STUDIO IF FALSE-FLAG ELSE "Only Santa Claus climbs down chimneys.")
           ;; FALSE-FLAG is never set, so this always fails
           :down "Only Santa Claus climbs down chimneys."
           :east {:to :behind-house :door :kitchen-window}
           :out {:to :behind-house :door :kitchen-window}}
   :action (fn [game-state rarg]
             (case rarg
               :look
               (let [window-open? (gs/set-thing-flag? game-state :kitchen-window :open)
                     window-state (if window-open? "open" "slightly ajar")]
                 (-> game-state
                     (utils/tell (str "You are in the kitchen of the white house. A table seems to have been used recently for the preparation of food. A passage leads to the west and a dark staircase can be seen leading upward. A dark chimney leads down and to the east is a small window which is " window-state "."))
                     (utils/crlf)))
               ;; Default - signal that default handling should be used
               (gs/use-default game-state)))})

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
   :flags #{:lit :sacred}
   :exits {:east :kitchen
           :west {:to :strange-passage
                  :if :magic-flag
                  :else "The door is nailed shut."}
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
   :flags #{:lit :sacred}
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

;; ZIL: CELLAR-FCN (1actions.zil lines 544-556)
;; <ROUTINE CELLAR-FCN (RARG)
;;   (<EQUAL? .RARG ,M-LOOK>
;;    <TELL "You are in a dark and damp cellar..." CR>)
;;   (<EQUAL? .RARG ,M-ENTER>
;;    <COND (<AND <FSET? ,TRAP-DOOR ,OPENBIT>
;;                <NOT <FSET? ,TRAP-DOOR ,TOUCHBIT>>>
;;           <FCLEAR ,TRAP-DOOR ,OPENBIT>
;;           <FSET ,TRAP-DOOR ,TOUCHBIT>
;;           <TELL "The trap door crashes shut, and you hear someone barring it." CR CR>)>)>>

(def cellar
  {:id :cellar
   :desc "Cellar"
   :ldesc "You are in a dark and damp cellar with a narrow passageway leading north, and a crawlway to the south. On the west is the bottom of a steep metal ramp which is unclimbable."
   :flags #{}  ; Underground - not lit
   :value 25   ; ZIL: (VALUE 25) - points for entering cellar first time
   :exits {:north :troll-room
           :south :east-of-chasm
           :up {:to :living-room :door :trap-door}
           :west "You try to ascend the ramp, but it is impossible, and you slide back down."}
   :action (fn [game-state rarg]
             (case rarg
               ;; M-ENTER: Close trap door on first descent
               :m-enter
               (let [trap-door-open? (gs/set-thing-flag? game-state :trap-door :open)
                     trap-door-touched? (gs/set-thing-flag? game-state :trap-door :touch)]
                 (if (and trap-door-open? (not trap-door-touched?))
                   (-> game-state
                       (gs/unset-thing-flag :trap-door :open)
                       (gs/set-thing-flag :trap-door :touch)
                       (utils/tell "The trap door crashes shut, and you hear someone barring it.\n\n"))
                   game-state))
               ;; Default - signal that default handling should be used
               (gs/use-default game-state)))})

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

;; <ROOM TROLL-ROOM
;;       ...
;;       (EAST TO E-W-PASSAGE IF TROLL-FLAG ELSE "The troll fends you off with a menacing gesture.")
;;       (WEST TO MAZE-1 IF TROLL-FLAG ELSE "The troll fends you off with a menacing gesture.")
;;       (FLAGS RLANDBIT)
;;       (ACTION TROLL-ROOM-F)>

(def troll-room
  {:id :troll-room
   :desc "The Troll Room"
   :ldesc "This is a small room with passages to the east and south and a forbidding hole leading west. Bloodstains and deep scratches (perhaps made by an axe) mar the walls."
   :flags #{}  ; Underground
   :exits {:south :cellar
           :east {:to :east-west-passage
                  :if :troll-flag
                  :else "The troll fends you off with a menacing gesture."}
           :west {:to :maze-1
                  :if :troll-flag
                  :else "The troll fends you off with a menacing gesture."}}})

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
           ;; ZIL: (UP PER UP-CHIMNEY-FUNCTION) - special exit that clears TOUCHBIT
           :up {:per :up-chimney-function}
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
   :value 5    ; ZIL: (VALUE 5) - points for first-time entry
   :exits {:west :troll-room
           :east :round-room
           :down :chasm-room
           :north :chasm-room}})

;; <ROOM ROUND-ROOM
;;       (IN ROOMS)
;;       (LDESC
;; "This is a circular stone room with passages in all directions. Several
;; of them have unfortunately been blocked by cave-ins.")
;;       (DESC "Round Room")
;;       (EAST TO LOUD-ROOM)
;;       (WEST TO EW-PASSAGE)
;;       (NORTH TO NS-PASSAGE)
;;       (SOUTH TO NARROW-PASSAGE)
;;       (SE TO ENGRAVINGS-CAVE)
;;       (FLAGS RLANDBIT)>

(def round-room
  {:id :round-room
   :desc "Round Room"
   :ldesc "This is a circular stone room with passages in all directions. Several of them have unfortunately been blocked by cave-ins."
   :flags #{}
   :exits {:west :east-west-passage
           :north :ns-passage
           :east :loud-room
           :south :narrow-passage
           :se :engravings-cave}})

;; <ROOM NS-PASSAGE
;;       (IN ROOMS)
;;       (LDESC
;; "This is a high north-south passage, which forks to the northeast.")
;;       (DESC "North-South Passage")
;;       (NORTH TO CHASM-ROOM)
;;       (NE TO DEEP-CANYON)
;;       (SOUTH TO ROUND-ROOM)
;;       (FLAGS RLANDBIT)>

(def ns-passage
  {:id :ns-passage
   :desc "North-South Passage"
   :ldesc "This is a high north-south passage, which forks to the northeast."
   :flags #{}
   :exits {:north :chasm-room
           :south :round-room
           :ne :deep-canyon}})

;; <ROOM CHASM-ROOM
;;       (IN ROOMS)
;;       (LDESC
;; "A chasm runs southwest to northeast and the path follows it. You are
;; on the south side of the chasm, where a crack opens into a passage.")
;;       (DESC "Chasm")
;;       (NE TO RESERVOIR-SOUTH)
;;       (SW TO EW-PASSAGE)
;;       (UP TO EW-PASSAGE)
;;       (SOUTH TO NS-PASSAGE)
;;       (DOWN "Are you out of your mind?")
;;       (FLAGS RLANDBIT)
;;       (GLOBAL CRACK STAIRS)
;;       (PSEUDO "CHASM" CHASM-PSEUDO)>

(def chasm-room
  {:id :chasm-room
   :desc "Chasm"
   :ldesc "A chasm runs southwest to northeast and the path follows it. You are on the south side of the chasm, where a crack opens into a passage."
   :flags #{}
   :exits {:sw :east-west-passage
           :up :east-west-passage
           :south :ns-passage
           :down "Are you out of your mind?"
           :ne :reservoir-south}})

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
   :flags #{:maze}
   :exits {:east :troll-room
           :north :maze-1
           :south :maze-2
           :west :maze-4}})

;; <ROOM MAZE-2
;;       (IN ROOMS)
;;       (LDESC "This is part of a maze of twisty little passages, all alike.")
;;       (DESC "Maze")
;;       (SOUTH TO MAZE-1)
;;       (DOWN PER MAZE-DIODES) ;"to MAZE-4"
;;       (EAST TO MAZE-3)
;;       (FLAGS RLANDBIT MAZEBIT)>

(def maze-2
  {:id :maze-2
   :desc "Maze"
   :ldesc "This is part of a maze of twisty little passages, all alike."
   :flags #{:maze}
   :exits {:south :maze-1
           :down {:per :maze-diodes}  ; one-way to MAZE-4
           :east :maze-3}})

;; <ROOM MAZE-3
;;       (IN ROOMS)
;;       (LDESC "This is part of a maze of twisty little passages, all alike.")
;;       (DESC "Maze")
;;       (WEST TO MAZE-2)
;;       (NORTH TO MAZE-4)
;;       (UP TO MAZE-5)
;;       (FLAGS RLANDBIT MAZEBIT)>

(def maze-3
  {:id :maze-3
   :desc "Maze"
   :ldesc "This is part of a maze of twisty little passages, all alike."
   :flags #{:maze}
   :exits {:west :maze-2
           :north :maze-4
           :up :maze-5}})

;; <ROOM MAZE-4
;;       (IN ROOMS)
;;       (LDESC "This is part of a maze of twisty little passages, all alike.")
;;       (DESC "Maze")
;;       (WEST TO MAZE-3)
;;       (NORTH TO MAZE-1)
;;       (EAST TO DEAD-END-1)
;;       (FLAGS RLANDBIT MAZEBIT)>

(def maze-4
  {:id :maze-4
   :desc "Maze"
   :ldesc "This is part of a maze of twisty little passages, all alike."
   :flags #{:maze}
   :exits {:west :maze-3
           :north :maze-1
           :east :dead-end-1}})

;; <ROOM DEAD-END-1
;;       (IN ROOMS)
;;       (DESC "Dead End")
;;       (LDESC "You have come to a dead end in the maze.")
;;       (SOUTH TO MAZE-4)
;;       (FLAGS RLANDBIT MAZEBIT)>

(def dead-end-1
  {:id :dead-end-1
   :desc "Dead End"
   :ldesc "You have come to a dead end in the maze."
   :flags #{:maze}
   :exits {:south :maze-4}})

;; <ROOM MAZE-5
;;       (IN ROOMS)
;;       (LDESC "This is part of a maze of twisty little passages, all alike.
;; A skeleton, probably the remains of a luckless adventurer, lies here.")
;;       (DESC "Maze")
;;       (EAST TO DEAD-END-2)
;;       (NORTH TO MAZE-3)
;;       (SW TO MAZE-6)
;;       (FLAGS RLANDBIT MAZEBIT)>

(def maze-5
  {:id :maze-5
   :desc "Maze"
   :ldesc "This is part of a maze of twisty little passages, all alike."
   :flags #{:maze}
   :exits {:east :dead-end-2
           :north :maze-3
           :sw :maze-6}})

;; <ROOM DEAD-END-2
;;       (IN ROOMS)
;;       (DESC "Dead End")
;;       (LDESC "You have come to a dead end in the maze.")
;;       (WEST TO MAZE-5)
;;       (FLAGS RLANDBIT MAZEBIT)>

(def dead-end-2
  {:id :dead-end-2
   :desc "Dead End"
   :ldesc "You have come to a dead end in the maze."
   :flags #{:maze}
   :exits {:west :maze-5}})

;; <ROOM MAZE-6
;;       (IN ROOMS)
;;       (LDESC "This is part of a maze of twisty little passages, all alike.")
;;       (DESC "Maze")
;;       (DOWN TO MAZE-5)
;;       (EAST TO MAZE-7)
;;       (WEST TO MAZE-6)
;;       (UP TO MAZE-9)
;;       (FLAGS RLANDBIT MAZEBIT)>

(def maze-6
  {:id :maze-6
   :desc "Maze"
   :ldesc "This is part of a maze of twisty little passages, all alike."
   :flags #{:maze}
   :exits {:down :maze-5
           :east :maze-7
           :west :maze-6  ; loops back to itself
           :up :maze-9}})

;; <ROOM MAZE-7
;;       (IN ROOMS)
;;       (LDESC "This is part of a maze of twisty little passages, all alike.")
;;       (DESC "Maze")
;;       (UP TO MAZE-14)
;;       (WEST TO MAZE-6)
;;       (DOWN PER MAZE-DIODES) ;"to DEAD-END-1"
;;       (EAST TO MAZE-8)
;;       (SOUTH TO MAZE-15)
;;       (FLAGS RLANDBIT MAZEBIT)>

(def maze-7
  {:id :maze-7
   :desc "Maze"
   :ldesc "This is part of a maze of twisty little passages, all alike."
   :flags #{:maze}
   :exits {:up :maze-14
           :west :maze-6
           :down {:per :maze-diodes}  ; one-way to DEAD-END-1
           :east :maze-8
           :south :maze-15}})

;; <ROOM MAZE-8
;;       (IN ROOMS)
;;       (LDESC "This is part of a maze of twisty little passages, all alike.")
;;       (DESC "Maze")
;;       (NE TO MAZE-7)
;;       (WEST TO MAZE-8)
;;       (SE TO DEAD-END-3)
;;       (FLAGS RLANDBIT MAZEBIT)>

(def maze-8
  {:id :maze-8
   :desc "Maze"
   :ldesc "This is part of a maze of twisty little passages, all alike."
   :flags #{:maze}
   :exits {:ne :maze-7
           :west :maze-8  ; loops back to itself
           :se :dead-end-3}})

;; <ROOM DEAD-END-3
;;       (IN ROOMS)
;;       (DESC "Dead End")
;;       (LDESC "You have come to a dead end in the maze.")
;;       (NORTH TO MAZE-8)
;;       (FLAGS RLANDBIT MAZEBIT)>

(def dead-end-3
  {:id :dead-end-3
   :desc "Dead End"
   :ldesc "You have come to a dead end in the maze."
   :flags #{:maze}
   :exits {:north :maze-8}})

;; <ROOM MAZE-9
;;       (IN ROOMS)
;;       (LDESC "This is part of a maze of twisty little passages, all alike.")
;;       (DESC "Maze")
;;       (NORTH TO MAZE-6)
;;       (DOWN PER MAZE-DIODES) ;"to MAZE-11"
;;       (EAST TO MAZE-10)
;;       (SOUTH TO MAZE-13)
;;       (WEST TO MAZE-12)
;;       (NW TO MAZE-9)
;;       (FLAGS RLANDBIT MAZEBIT)>

(def maze-9
  {:id :maze-9
   :desc "Maze"
   :ldesc "This is part of a maze of twisty little passages, all alike."
   :flags #{:maze}
   :exits {:north :maze-6
           :down {:per :maze-diodes}  ; one-way to MAZE-11
           :east :maze-10
           :south :maze-13
           :west :maze-12
           :nw :maze-9}})  ; loops back to itself

;; <ROOM MAZE-10
;;       (IN ROOMS)
;;       (LDESC "This is part of a maze of twisty little passages, all alike.")
;;       (DESC "Maze")
;;       (EAST TO MAZE-9)
;;       (WEST TO MAZE-13)
;;       (UP TO MAZE-11)
;;       (FLAGS RLANDBIT MAZEBIT)>

(def maze-10
  {:id :maze-10
   :desc "Maze"
   :ldesc "This is part of a maze of twisty little passages, all alike."
   :flags #{:maze}
   :exits {:east :maze-9
           :west :maze-13
           :up :maze-11}})

;; <ROOM MAZE-11
;;       (IN ROOMS)
;;       (LDESC "This is part of a maze of twisty little passages, all alike.")
;;       (DESC "Maze")
;;       (NE TO GRATING-ROOM)
;;       (DOWN TO MAZE-10)
;;       (NW TO MAZE-13)
;;       (SW TO MAZE-12)
;;       (FLAGS RLANDBIT MAZEBIT)>

(def maze-11
  {:id :maze-11
   :desc "Maze"
   :ldesc "This is part of a maze of twisty little passages, all alike."
   :flags #{:maze}
   :exits {:ne :grating-room
           :down :maze-10
           :nw :maze-13
           :sw :maze-12}})

;; <ROOM GRATING-ROOM
;;       (IN ROOMS)
;;       (DESC "Grating Room")
;;       (SW TO MAZE-11)
;;       (UP TO GRATING-CLEARING
;;        IF GRATE IS OPEN ELSE "The grating is closed.")
;;       (ACTION MAZE-11-FCN)
;;       (GLOBAL GRATE)
;;       (FLAGS RLANDBIT)>

(def grating-room
  {:id :grating-room
   :desc "Grating Room"
   :flags #{}  ; not a maze room, not lit
   :globals #{:grate}
   :exits {:sw :maze-11
           :up {:to :grating-clearing :door :grate :else "The grating is closed."}}
   :action (fn [game-state rarg]
             (case rarg
               :look
               (let [grate-open? (gs/set-thing-flag? game-state :grate :open)]
                 (-> game-state
                     (utils/tell "You are in a small room near the maze. There are twisty passages in the immediate vicinity.")
                     (utils/crlf)
                     (cond-> grate-open?
                       (-> (utils/tell "Above you is an open grating with sunlight pouring in.")
                           (utils/crlf)))
                     (cond-> (and (not grate-open?)
                                  (get game-state :grunlock false))
                       (-> (utils/tell "Above you is a grating.")
                           (utils/crlf)))
                     (cond-> (and (not grate-open?)
                                  (not (get game-state :grunlock false)))
                       (-> (utils/tell "Above you is a grating locked with a skull-and-crossbones lock.")
                           (utils/crlf)))))
               ;; Default - signal that default handling should be used
               (gs/use-default game-state)))})

;; <ROOM MAZE-12
;;       (IN ROOMS)
;;       (LDESC "This is part of a maze of twisty little passages, all alike.")
;;       (DESC "Maze")
;;       (DOWN PER MAZE-DIODES) ;"to MAZE-5"
;;       (SW TO MAZE-11)
;;       (EAST TO MAZE-13)
;;       (UP TO MAZE-9)
;;       (NORTH TO DEAD-END-4)
;;       (FLAGS RLANDBIT MAZEBIT)>

(def maze-12
  {:id :maze-12
   :desc "Maze"
   :ldesc "This is part of a maze of twisty little passages, all alike."
   :flags #{:maze}
   :exits {:down {:per :maze-diodes}  ; one-way to MAZE-5
           :sw :maze-11
           :east :maze-13
           :up :maze-9
           :north :dead-end-4}})

;; <ROOM DEAD-END-4
;;       (IN ROOMS)
;;       (DESC "Dead End")
;;       (LDESC "You have come to a dead end in the maze.")
;;       (SOUTH TO MAZE-12)
;;       (FLAGS RLANDBIT MAZEBIT)>

(def dead-end-4
  {:id :dead-end-4
   :desc "Dead End"
   :ldesc "You have come to a dead end in the maze."
   :flags #{:maze}
   :exits {:south :maze-12}})

;; <ROOM MAZE-13
;;       (IN ROOMS)
;;       (LDESC "This is part of a maze of twisty little passages, all alike.")
;;       (DESC "Maze")
;;       (EAST TO MAZE-9)
;;       (DOWN TO MAZE-12)
;;       (SOUTH TO MAZE-10)
;;       (WEST TO MAZE-11)
;;       (FLAGS RLANDBIT MAZEBIT)>

(def maze-13
  {:id :maze-13
   :desc "Maze"
   :ldesc "This is part of a maze of twisty little passages, all alike."
   :flags #{:maze}
   :exits {:east :maze-9
           :down :maze-12
           :south :maze-10
           :west :maze-11}})

;; <ROOM MAZE-14
;;       (IN ROOMS)
;;       (LDESC "This is part of a maze of twisty little passages, all alike.")
;;       (DESC "Maze")
;;       (WEST TO MAZE-15)
;;       (NW TO MAZE-14)
;;       (NE TO MAZE-7)
;;       (SOUTH TO MAZE-7)
;;       (FLAGS RLANDBIT MAZEBIT)>

(def maze-14
  {:id :maze-14
   :desc "Maze"
   :ldesc "This is part of a maze of twisty little passages, all alike."
   :flags #{:maze}
   :exits {:west :maze-15
           :nw :maze-14  ; loops back to itself
           :ne :maze-7
           :south :maze-7}})

;; <ROOM MAZE-15
;;       (IN ROOMS)
;;       (LDESC "This is part of a maze of twisty little passages, all alike.")
;;       (DESC "Maze")
;;       (WEST TO MAZE-14)
;;       (SOUTH TO MAZE-7)
;;       (SE TO CYCLOPS-ROOM)
;;       (FLAGS RLANDBIT MAZEBIT)>

(def maze-15
  {:id :maze-15
   :desc "Maze"
   :ldesc "This is part of a maze of twisty little passages, all alike."
   :flags #{:maze}
   :exits {:west :maze-14
           :south :maze-7
           :se :cyclops-room}})

;;; ---------------------------------------------------------------------------
;;; CYCLOPS AND THIEF'S HIDEAWAY
;;; ---------------------------------------------------------------------------

;; <ROOM CYCLOPS-ROOM
;;       (IN ROOMS)
;;       (DESC "Cyclops Room")
;;       (NW TO MAZE-15)
;;       (EAST TO STRANGE-PASSAGE IF MAGIC-FLAG ELSE "The east wall is solid rock.")
;;       (UP TO TREASURE-ROOM IF CYCLOPS-FLAG ELSE "The cyclops doesn't look like he'll let you past.")
;;       (ACTION CYCLOPS-ROOM-FCN)
;;       (FLAGS RLANDBIT)
;;       (GLOBAL STAIRS)>

(def cyclops-room
  {:id :cyclops-room
   :desc "Cyclops Room"
   ;; Note: ldesc is handled by cyclops-room-action for dynamic descriptions
   :flags #{}  ; Underground, not lit
   :exits {:nw :maze-15
           :east {:to :strange-passage
                  :if :magic-flag
                  :else "The east wall is solid rock."}
           :up {:to :treasure-room
                :if :cyclops-flag
                :else "The cyclops doesn't look like he'll let you past."}}
   :action cyclops/cyclops-room-action})

;; <ROOM STRANGE-PASSAGE
;;       (IN ROOMS)
;;       (LDESC "This is a long passage. To the west is one entrance. On the
;;               east there is an old wooden door, with a large opening in it (about
;;               cyclops sized).")
;;       (DESC "Strange Passage")
;;       (WEST TO CYCLOPS-ROOM)
;;       (IN TO CYCLOPS-ROOM)
;;       (EAST TO LIVING-ROOM)
;;       (FLAGS RLANDBIT)>

(def strange-passage
  {:id :strange-passage
   :desc "Strange Passage"
   :ldesc "This is a long passage. To the west is one entrance. On the east there is an old wooden door, with a large opening in it (about cyclops sized)."
   :flags #{}  ; Underground, not lit (RLANDBIT means on-land, no light)
   :exits {:west :cyclops-room
           :in :cyclops-room
           :east :living-room}})

;; <ROOM TREASURE-ROOM
;;       (IN ROOMS)
;;       (LDESC "This is a large room, whose east wall is solid granite. A number
;;               of discarded bags, which crumble at your touch, are scattered about
;;               on the floor. There is an exit down a staircase.")
;;       (DESC "Treasure Room")
;;       (DOWN TO CYCLOPS-ROOM)
;;       (ACTION TREASURE-ROOM-FCN)
;;       (FLAGS RLANDBIT)
;;       (VALUE 25)
;;       (GLOBAL STAIRS)>

(def treasure-room
  {:id :treasure-room
   :desc "Treasure Room"
   :ldesc "This is a large room, whose east wall is solid granite. A number of discarded bags, which crumble at your touch, are scattered about on the floor. There is an exit down a staircase."
   :flags #{}  ; Underground, not lit
   :value 25   ; Points for entering first time
   :exits {:down :cyclops-room}
   :action thief/treasure-room-action})

;;; ---------------------------------------------------------------------------
;;; UNDERGROUND - DAM/RESERVOIR AREA
;;; ---------------------------------------------------------------------------

;; <ROOM RESERVOIR-SOUTH
;;       (IN ROOMS)
;;       (DESC "Reservoir South")
;;       (SE TO DEEP-CANYON)
;;       (SW TO CHASM-ROOM)
;;       (EAST TO DAM-ROOM)
;;       (WEST TO STREAM-VIEW)
;;       (NORTH TO RESERVOIR IF LOW-TIDE ELSE "You would drown.")
;;       (ACTION RESERVOIR-SOUTH-FCN)
;;       (FLAGS RLANDBIT)
;;       (GLOBAL GLOBAL-WATER)
;;       (PSEUDO "LAKE" LAKE-PSEUDO "CHASM" CHASM-PSEUDO)>

(def reservoir-south
  {:id :reservoir-south
   :desc "Reservoir South"
   ;; ldesc is dynamic, handled by action function
   :flags #{}  ; Underground, not lit
   :exits {:sw :chasm-room
           :east :dam-room
           :west :stream-view
           :north {:to :reservoir
                   :if :low-tide
                   :else "You would drown."}
           :se :deep-canyon}
   :globals #{:global-water}
   :action dam/reservoir-south-action})

;; <ROOM RESERVOIR
;;       (IN ROOMS)
;;       (DESC "Reservoir")
;;       (NORTH TO RESERVOIR-NORTH)
;;       (SOUTH TO RESERVOIR-SOUTH)
;;       (UP TO IN-STREAM)
;;       (WEST TO IN-STREAM)
;;       (DOWN "The dam blocks your way.")
;;       (ACTION RESERVOIR-FCN)
;;       (FLAGS NONLANDBIT)
;;       (PSEUDO "STREAM" STREAM-PSEUDO)
;;       (GLOBAL GLOBAL-WATER)>

(def reservoir
  {:id :reservoir
   :desc "Reservoir"
   ;; ldesc is dynamic, handled by action function
   :flags #{:rwater}  ; NONLANDBIT = water room
   :exits {:north :reservoir-north
           :south :reservoir-south
           :up :in-stream
           :west :in-stream
           :down "The dam blocks your way."}
   :globals #{:global-water}
   :action dam/reservoir-action})

;; <ROOM RESERVOIR-NORTH
;;       (IN ROOMS)
;;       (DESC "Reservoir North")
;;       (NORTH TO ATLANTIS-ROOM)
;;       (SOUTH TO RESERVOIR IF LOW-TIDE ELSE "You would drown.")
;;       (ACTION RESERVOIR-NORTH-FCN)
;;       (FLAGS RLANDBIT)
;;       (GLOBAL GLOBAL-WATER STAIRS)
;;       (PSEUDO "LAKE" LAKE-PSEUDO)>

(def reservoir-north
  {:id :reservoir-north
   :desc "Reservoir North"
   ;; ldesc is dynamic, handled by action function
   :flags #{}  ; Underground, not lit
   :exits {:south {:to :reservoir
                   :if :low-tide
                   :else "You would drown."}
           :north "TODO: This exit leads to ATLANTIS-ROOM."}
   :globals #{:global-water}
   :action dam/reservoir-north-action})

;; <ROOM STREAM-VIEW
;;       (IN ROOMS)
;;       (LDESC
;; "You are standing on a path beside a gently flowing stream. The path
;; follows the stream, which flows from west to east.")
;;       (DESC "Stream View")
;;       (EAST TO RESERVOIR-SOUTH)
;;       (WEST "The stream emerges from a spot too small for you to enter.")
;;       (FLAGS RLANDBIT)
;;       (GLOBAL GLOBAL-WATER)
;;       (PSEUDO "STREAM" STREAM-PSEUDO)>

(def stream-view
  {:id :stream-view
   :desc "Stream View"
   :ldesc "You are standing on a path beside a gently flowing stream. The path follows the stream, which flows from west to east."
   :flags #{}  ; Underground, not lit
   :exits {:east :reservoir-south
           :west "The stream emerges from a spot too small for you to enter."}
   :globals #{:global-water}})

;; <ROOM IN-STREAM
;;       (IN ROOMS)
;;       (LDESC
;; "You are on the gently flowing stream. The upstream route is too narrow
;; to navigate, and the downstream route is invisible due to twisting
;; walls. There is a narrow beach to land on.")
;;       (DESC "Stream")
;;       (UP "The channel is too narrow.")
;;       (WEST "The channel is too narrow.")
;;       (LAND TO STREAM-VIEW)
;;       (DOWN TO RESERVOIR)
;;       (EAST TO RESERVOIR)
;;       (FLAGS NONLANDBIT)
;;       (GLOBAL GLOBAL-WATER)
;;       (PSEUDO "STREAM" STREAM-PSEUDO)>

(def in-stream
  {:id :in-stream
   :desc "Stream"
   :ldesc "You are on the gently flowing stream. The upstream route is too narrow to navigate, and the downstream route is invisible due to twisting walls. There is a narrow beach to land on."
   :flags #{:rwater}  ; NONLANDBIT = water room
   :exits {:up "The channel is too narrow."
           :west "The channel is too narrow."
           :land :stream-view
           :down :reservoir
           :east :reservoir}
   :globals #{:global-water}})

;; <ROOM DAM-ROOM	;"was DAM"
;;       (IN ROOMS)
;;       (DESC "Dam")
;;       (SOUTH TO DEEP-CANYON)
;;       (DOWN TO DAM-BASE)
;;       (EAST TO DAM-BASE)
;;       (NORTH TO DAM-LOBBY)
;;       (WEST TO RESERVOIR-SOUTH)
;;       (ACTION DAM-ROOM-FCN)
;;       (FLAGS RLANDBIT ONBIT)
;;       (GLOBAL GLOBAL-WATER)>

(def dam-room
  {:id :dam-room
   :desc "Dam"
   ;; ldesc is dynamic, handled by action function
   :flags #{:lit}  ; ONBIT = lit
   :exits {:west :reservoir-south
           :north :dam-lobby
           :down :dam-base
           :east :dam-base
           :south :deep-canyon}
   :globals #{:global-water}
   :action dam/dam-room-action})

;; <ROOM DAM-LOBBY	;"was LOBBY"
;;       (IN ROOMS)
;;       (LDESC
;; "This room appears to have been the waiting room for groups touring
;; the dam. There are open doorways here to the north and east marked
;; \"Private\", and there is a path leading south over the top of the dam.")
;;       (DESC "Dam Lobby")
;;       (SOUTH TO DAM-ROOM)
;;       (NORTH TO MAINTENANCE-ROOM)
;;       (EAST TO MAINTENANCE-ROOM)
;;       (FLAGS RLANDBIT ONBIT)>

(def dam-lobby
  {:id :dam-lobby
   :desc "Dam Lobby"
   :ldesc "This room appears to have been the waiting room for groups touring the dam. There are open doorways here to the north and east marked \"Private\", and there is a path leading south over the top of the dam."
   :flags #{:lit}  ; ONBIT = lit
   :exits {:south :dam-room
           :north :maintenance-room
           :east :maintenance-room}})

;; <ROOM MAINTENANCE-ROOM	;"was MAINT"
;;       (IN ROOMS)
;;       (LDESC
;; "This is what appears to have been the maintenance room for Flood
;; Control Dam #3. Apparently, this room has been ransacked recently, for
;; most of the valuable equipment is gone. On the wall in front of you is a
;; group of buttons colored blue, yellow, brown, and red. There are doorways to
;; the west and south.")
;;       (DESC "Maintenance Room")
;;       (SOUTH TO DAM-LOBBY)
;;       (WEST TO DAM-LOBBY)
;;       (FLAGS RLANDBIT)>

(def maintenance-room
  {:id :maintenance-room
   :desc "Maintenance Room"
   :ldesc "This is what appears to have been the maintenance room for Flood Control Dam #3. Apparently, this room has been ransacked recently, for most of the valuable equipment is gone. On the wall in front of you is a group of buttons colored blue, yellow, brown, and red. There are doorways to the west and south."
   :flags #{}  ; Underground, not lit (no ONBIT)
   :exits {:south :dam-lobby
           :west :dam-lobby}})

;; <ROOM DAM-BASE	;"was DOCK"
;;       (IN ROOMS)
;;       (LDESC
;; "You are at the base of Flood Control Dam #3, which looms above you
;; and to the north. The river Frigid is flowing by here. Along the
;; river are the White Cliffs which seem to form giant walls stretching
;; from north to south along the shores of the river as it winds its
;; way downstream.")
;;       (DESC "Dam Base")
;;       (NORTH TO DAM-ROOM)
;;       (UP TO DAM-ROOM)
;;       (FLAGS RLANDBIT ONBIT SACREDBIT)
;;       (GLOBAL GLOBAL-WATER RIVER)>

(def dam-base
  {:id :dam-base
   :desc "Dam Base"
   :ldesc "You are at the base of Flood Control Dam #3, which looms above you and to the north. The river Frigid is flowing by here. Along the river are the White Cliffs which seem to form giant walls stretching from north to south along the shores of the river as it winds its way downstream."
   :flags #{:lit :sacred}  ; ONBIT + SACREDBIT
   :exits {:north :dam-room
           :up :dam-room}
   :globals #{:global-water}})

;;; ---------------------------------------------------------------------------
;;; LOUD ROOM AREA
;;; ---------------------------------------------------------------------------

;; <ROOM LOUD-ROOM
;;       (IN ROOMS)
;;       (DESC "Loud Room")
;;       (EAST TO DAMP-CAVE)
;;       (WEST TO ROUND-ROOM)
;;       (UP TO DEEP-CANYON)
;;       (ACTION LOUD-ROOM-FCN)
;;       (FLAGS RLANDBIT)
;;       (GLOBAL STAIRS)>

(def loud-room
  {:id :loud-room
   :desc "Loud Room"
   ;; ldesc is dynamic, handled by action function
   :flags #{}  ; Underground, not lit
   :exits {:east :damp-cave
           :west :round-room
           :up :deep-canyon}
   :globals #{:stairs}
   :action loud-room/loud-room-action})

;; <ROOM DEEP-CANYON
;;       (IN ROOMS)
;;       (DESC "Deep Canyon")
;;       (NW TO RESERVOIR-SOUTH) ;COFFIN-CURE
;;       (EAST TO DAM-ROOM)
;;       (SW TO NS-PASSAGE)
;;       (DOWN TO LOUD-ROOM)
;;       (FLAGS RLANDBIT)
;;       (ACTION DEEP-CANYON-F)
;;       (GLOBAL STAIRS)>

(def deep-canyon
  {:id :deep-canyon
   :desc "Deep Canyon"
   ;; ldesc is dynamic, handled by action function
   :flags #{}  ; Underground, not lit
   :exits {:nw :reservoir-south
           :east :dam-room
           :sw :ns-passage
           :down :loud-room}
   :globals #{:stairs}
   :action loud-room/deep-canyon-action})

;; <ROOM DAMP-CAVE
;;       (IN ROOMS)
;;       (LDESC
;; "This cave has exits to the west and east, and narrows to a crack toward
;; the south. The earth is particularly damp here.")
;;       (DESC "Damp Cave")
;;       (WEST TO LOUD-ROOM)
;;       (EAST TO WHITE-CLIFFS-NORTH)
;;       (SOUTH "It is too narrow for most insects.")
;;       (FLAGS RLANDBIT)
;;       (GLOBAL CRACK)>

(def damp-cave
  {:id :damp-cave
   :desc "Damp Cave"
   :ldesc "This cave has exits to the west and east, and narrows to a crack toward the south. The earth is particularly damp here."
   :flags #{}  ; Underground, not lit
   :exits {:west :loud-room
           :east "TODO: This exit leads to WHITE-CLIFFS-NORTH."
           :south "It is too narrow for most insects."}
   :globals #{:crack}})

;;; ---------------------------------------------------------------------------
;;; NARROW PASSAGE AREA (connects to Temple/Mirror rooms)
;;; ---------------------------------------------------------------------------

;; <ROOM NARROW-PASSAGE
;;       (IN ROOMS)
;;       (LDESC
;; "This is a long and narrow corridor where a long north-south passageway
;; briefly narrows even further.")
;;       (DESC "Narrow Passage")
;;       (NORTH TO ROUND-ROOM)
;;       (SOUTH TO MIRROR-ROOM-2)
;;       (FLAGS RLANDBIT)>

(def narrow-passage
  {:id :narrow-passage
   :desc "Narrow Passage"
   :ldesc "This is a long and narrow corridor where a long north-south passageway briefly narrows even further."
   :flags #{}  ; Underground, not lit
   :exits {:north :round-room
           :south "TODO: This exit leads to MIRROR-ROOM-2."}})

;; <ROOM ENGRAVINGS-CAVE	;"was CAVE4"
;;       (IN ROOMS)
;;       (LDESC
;; "You have entered a low cave with passages leading northwest and east.")
;;       (DESC "Engravings Cave")
;;       (NW TO ROUND-ROOM)
;;       (EAST TO DOME-ROOM)
;;       (FLAGS RLANDBIT)>

(def engravings-cave
  {:id :engravings-cave
   :desc "Engravings Cave"
   :ldesc "You have entered a low cave with passages leading northwest and east."
   :flags #{}  ; Underground, not lit
   :exits {:nw :round-room
           :east "TODO: This exit leads to DOME-ROOM."}})

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
   :flags #{:lit :sacred}
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
   forest-2
   forest-3
   forest-path
   clearing
   grating-clearing
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
   round-room
   ns-passage
   chasm-room
   maze-1
   maze-2
   maze-3
   maze-4
   maze-5
   maze-6
   maze-7
   maze-8
   maze-9
   maze-10
   maze-11
   maze-12
   maze-13
   maze-14
   maze-15
   dead-end-1
   dead-end-2
   dead-end-3
   dead-end-4
   grating-room
   cyclops-room
   strange-passage
   treasure-room
   ;; Dam/Reservoir area
   reservoir-south
   reservoir
   reservoir-north
   stream-view
   in-stream
   dam-room
   dam-lobby
   maintenance-room
   dam-base
   ;; Loud Room area
   loud-room
   deep-canyon
   damp-cave
   ;; Passage rooms (toward Temple/Mirror)
   narrow-passage
   engravings-cave
   ;; Special rooms
   dark-area
   stone-barrow])
