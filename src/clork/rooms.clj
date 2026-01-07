(ns clork.rooms
  "Room definitions for Clork."
  (:require [clork.utils :as utils]
            [clork.game-state :as gs]
            [clork.thief :as thief]
            [clork.cyclops :as cyclops]
            [clork.dam :as dam]
            [clork.loud-room :as loud-room]
            [clork.death :as death]
            [clork.random :as random]
            [clork.parser.state :as parser-state]
            [clork.verbs-movement :as verbs-movement]
            [clork.forest :as forest]))

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
             ;; Only print description for :look action, not :m-beg
             (if (= rarg :look)
               (-> game-state
                   (utils/tell "You are standing in an open field west of a white house, with a boarded front door.")
                   (cond-> (:won game-state)
                     (utils/tell " A secret path leads southwest into the forest."))
                   (utils/crlf))
               game-state))})

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
           :west "You would need a machete to go further west."}
   :action forest/forest-room-action})

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
           :east :canyon-view  ; ZIL: CANYON-VIEW has (WEST TO FOREST-3)
           :south "Storm-tossed trees block your way."
           :west :forest-1
           :nw :south-of-house}
   :action forest/forest-room-action})

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
           :west :forest-1}
   :action forest/forest-room-action})

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
           :east :canyon-view  ; ZIL: (EAST TO CANYON-VIEW)
           :north :forest-2
           :south :forest-3
           :west :behind-house}
   :action forest/forest-room-action})

;; <ROOM UP-A-TREE
;;       (IN ROOMS)
;;       (DESC "Up a Tree")
;;       (DOWN TO PATH)
;;       (UP "You cannot climb any higher.")
;;       (ACTION TREE-ROOM)
;;       (FLAGS RLANDBIT ONBIT SACREDBIT)
;;       (GLOBAL TREE FOREST SONGBIRD WHITE-HOUSE)>

(defn tree-room-action
  "Room action for UP-A-TREE.
   ZIL: TREE-ROOM (1actions.zil lines 2893-2908)

   - M-ENTER: Enable forest daemon (UP-A-TREE is included in FOREST-ROOM?)
   - M-BEG: Handle CLIMB-DOWN/CLIMB-UP to navigate the tree
   - M-LOOK: Show room description (handled by ldesc)"
  [game-state rarg]
  (case rarg
    ;; M-ENTER: Enable forest daemon like other forest rooms
    :m-enter
    (forest/forest-room-action game-state rarg)

    ;; M-BEG: Handle climb commands
    :m-beg
    (let [prsa (parser-state/get-prsa game-state)
          prso (parser-state/get-prso game-state)]
      (cond
        ;; ZIL: <AND <VERB? CLIMB-DOWN> <EQUAL? ,PRSO ,TREE ,ROOMS>>
        ;; "climb down" or "climb down tree" -> go down
        (and (= prsa :climb-down)
             (or (= prso :tree) (= prso :rooms) (nil? prso)))
        (-> (verbs-movement/do-walk game-state :down)
            (assoc :command-handled true))

        ;; ZIL: <AND <VERB? CLIMB-UP CLIMB-FOO> <EQUAL? ,PRSO ,TREE>>
        ;; "climb up tree" or "climb tree" -> go up
        (and (contains? #{:climb-up :climb} prsa)
             (= prso :tree))
        (-> (verbs-movement/do-walk game-state :up)
            (assoc :command-handled true))

        :else
        (gs/use-default game-state)))

    ;; Default: use default handling
    (gs/use-default game-state)))

(def up-a-tree
  {:id :up-a-tree
   :desc "Up a Tree"
   ;; Note: The nest description comes from the nest's :fdesc, not here
   :ldesc "You are about 10 feet above the ground nestled among some large branches. The nearest branch above you is above your reach."
   :flags #{:lit :sacred}
   :globals #{:tree :white-house}  ; ZIL: (GLOBAL TREE FOREST SONGBIRD WHITE-HOUSE)
   :exits {:down :forest-path
           :up "You cannot climb any higher."}
   :action tree-room-action})

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
           :east :mountains
           :south :clearing
           :west :forest-path}
   :action forest/forest-room-action})

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
   :globals #{:kitchen-window :chimney}  ; ZIL: (GLOBAL KITCHEN-WINDOW CHIMNEY STAIRS)
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
                     ;; Paragraph break after room description
                     (utils/tell "\n\n")))
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
   :ldesc "This is the attic. The only exit is a stairway leading down."
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
   :globals #{:chimney}  ; ZIL: (GLOBAL CHIMNEY)
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
   :ldesc "This is part of a maze of twisty little passages, all alike. A skeleton, probably the remains of a luckless adventurer, lies here."
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
           :north :atlantis-room}
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
           :east :white-cliffs-north
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
           :south :mirror-room-2}})

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
           :east :dome-room}})

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
;;; CANYON / FALLS / RAINBOW AREA
;;; ---------------------------------------------------------------------------

;; <ROOM MOUNTAINS
;;       (IN ROOMS)
;;       (LDESC "The forest thins out, revealing impassable mountains.")
;;       (DESC "Forest")
;;       (UP "The mountains are impassable.")
;;       (NORTH TO FOREST-2)
;;       (EAST "The mountains are impassable.")
;;       (SOUTH TO FOREST-2)
;;       (WEST TO FOREST-2)
;;       (FLAGS RLANDBIT ONBIT SACREDBIT)
;;       (GLOBAL TREE WHITE-HOUSE)>

(def mountains
  {:id :mountains
   :desc "Forest"
   :ldesc "The forest thins out, revealing impassable mountains."
   :flags #{:lit :sacred}
   :globals #{:tree :white-house}
   :exits {:up "The mountains are impassable."
           :north :forest-2
           :east "The mountains are impassable."
           :south :forest-2
           :west :forest-2}})

;; <ROOM CANYON-VIEW
;;       (IN ROOMS)
;;       (LDESC
;; "You are at the top of the Great Canyon on its west wall. From here
;; there is a marvelous view of the canyon and parts of the Frigid River
;; upstream. Across the canyon, the walls of the White Cliffs join the
;; mighty ramparts of the Flathead Mountains to the east. Following the
;; Canyon upstream to the north, Aragain Falls may be seen, complete with
;; rainbow. The mighty Frigid River flows out from a great dark cavern. To
;; the west and south can be seen an immense forest, stretching for miles
;; around. A path leads northwest. It is possible to climb down into
;; the canyon from here.")
;;       (DESC "Canyon View")
;;       (EAST TO CLIFF-MIDDLE)
;;       (DOWN TO CLIFF-MIDDLE)
;;       (NW TO CLEARING)
;;       (WEST TO FOREST-3)
;;       (SOUTH "Storm-tossed trees block your way.")
;;       (FLAGS RLANDBIT ONBIT SACREDBIT)
;;       (GLOBAL CLIMBABLE-CLIFF RIVER RAINBOW)
;;       (ACTION CANYON-VIEW-F)>

(defn canyon-view-action
  "Handle special actions in Canyon View.
   ZIL: CANYON-VIEW-F - handles LEAP verb with death message."
  [game-state rarg]
  (cond
    ;; If player tries to jump/leap without an object, it's fatal
    (and (= rarg :m-beg)
         (= (:verb game-state) :leap)
         (nil? (:prso game-state)))
    (death/jigs-up game-state "Nice view, lousy place to jump.")

    :else
    (gs/use-default game-state)))

(def canyon-view
  {:id :canyon-view
   :desc "Canyon View"
   :ldesc "You are at the top of the Great Canyon on its west wall. From here there is a marvelous view of the canyon and parts of the Frigid River upstream. Across the canyon, the walls of the White Cliffs join the mighty ramparts of the Flathead Mountains to the east. Following the Canyon upstream to the north, Aragain Falls may be seen, complete with rainbow. The mighty Frigid River flows out from a great dark cavern. To the west and south can be seen an immense forest, stretching for miles around. A path leads northwest. It is possible to climb down into the canyon from here."
   :flags #{:lit :sacred}
   :globals #{:climbable-cliff :river :rainbow}
   :exits {:east :cliff-middle
           :down :cliff-middle
           :nw :clearing
           :west :forest-3
           :south "Storm-tossed trees block your way."}
   :action canyon-view-action})

;; <ROOM CLIFF-MIDDLE
;;       (IN ROOMS)
;;       (LDESC
;; "You are on a ledge about halfway up the wall of the river canyon.
;; You can see from here that the main flow from Aragain Falls twists
;; along a passage which it is impossible for you to enter. Below you is the
;; canyon bottom. Above you is more cliff, which appears
;; climbable.")
;;       (DESC "Rocky Ledge")
;;       (UP TO CANYON-VIEW)
;;       (DOWN TO CANYON-BOTTOM)
;;       (FLAGS RLANDBIT ONBIT SACREDBIT)
;;       (GLOBAL CLIMBABLE-CLIFF RIVER)>

(def cliff-middle
  {:id :cliff-middle
   :desc "Rocky Ledge"
   :ldesc "You are on a ledge about halfway up the wall of the river canyon. You can see from here that the main flow from Aragain Falls twists along a passage which it is impossible for you to enter. Below you is the canyon bottom. Above you is more cliff, which appears climbable."
   :flags #{:lit :sacred}
   :globals #{:climbable-cliff :river}
   :exits {:up :canyon-view
           :down :canyon-bottom}})

;; <ROOM CANYON-BOTTOM
;;       (IN ROOMS)
;;       (LDESC
;; "You are beneath the walls of the river canyon which may be climbable
;; here. The lesser part of the runoff of Aragain Falls flows by below.
;; To the north is a narrow path.")
;;       (DESC "Canyon Bottom")
;;       (UP TO CLIFF-MIDDLE)
;;       (NORTH TO END-OF-RAINBOW)
;;       (FLAGS RLANDBIT ONBIT SACREDBIT)
;;       (GLOBAL GLOBAL-WATER CLIMBABLE-CLIFF RIVER)>

(def canyon-bottom
  {:id :canyon-bottom
   :desc "Canyon Bottom"
   :ldesc "You are beneath the walls of the river canyon which may be climbable here. The lesser part of the runoff of Aragain Falls flows by below. To the north is a narrow path."
   :flags #{:lit :sacred}
   :globals #{:global-water :climbable-cliff :river}
   :exits {:up :cliff-middle
           :north :end-of-rainbow}})

;; <ROOM END-OF-RAINBOW
;;       (IN ROOMS)
;;       (LDESC
;; "You are on a small, rocky beach on the continuation of the Frigid
;; River past the Falls. The beach is narrow due to the presence of the
;; White Cliffs. The river canyon opens here and sunlight shines in
;; from above. A rainbow crosses over the falls to the east and a narrow
;; path continues to the southwest.")
;;       (DESC "End of Rainbow")
;;       (UP TO ON-RAINBOW IF RAINBOW-FLAG)
;;       (NE TO ON-RAINBOW IF RAINBOW-FLAG)
;;       (EAST TO ON-RAINBOW IF RAINBOW-FLAG)
;;       (SW TO CANYON-BOTTOM)
;;       (FLAGS RLANDBIT ONBIT)
;;       (GLOBAL GLOBAL-WATER RAINBOW RIVER)>

(def end-of-rainbow
  {:id :end-of-rainbow
   :desc "End of Rainbow"
   :ldesc "You are on a small, rocky beach on the continuation of the Frigid River past the Falls. The beach is narrow due to the presence of the White Cliffs. The river canyon opens here and sunlight shines in from above. A rainbow crosses over the falls to the east and a narrow path continues to the southwest."
   :flags #{:lit :sacred}  ; ZIL omits SACREDBIT but all nearby outdoor rooms have it - likely oversight
   :globals #{:global-water :rainbow :river}
   :exits {:up {:to :on-rainbow :if :rainbow-flag}
           :ne {:to :on-rainbow :if :rainbow-flag}
           :east {:to :on-rainbow :if :rainbow-flag}
           :sw :canyon-bottom}})

;; <ROOM ON-RAINBOW
;;       (IN ROOMS)
;;       (LDESC
;; "You are on top of a rainbow (I bet you never thought you would walk
;; on a rainbow), with a magnificent view of the Falls. The rainbow
;; travels east-west here.")
;;       (DESC "On the Rainbow")
;;       (WEST TO END-OF-RAINBOW)
;;       (EAST TO ARAGAIN-FALLS)
;;       (FLAGS RLANDBIT ONBIT SACREDBIT)
;;       (GLOBAL RAINBOW)>

(def on-rainbow
  {:id :on-rainbow
   :desc "On the Rainbow"
   :ldesc "You are on top of a rainbow (I bet you never thought you would walk on a rainbow), with a magnificent view of the Falls. The rainbow travels east-west here."
   :flags #{:lit :sacred}
   :globals #{:rainbow}
   :exits {:west :end-of-rainbow
           :east :aragain-falls}})

;; <ROOM ARAGAIN-FALLS
;;       (IN ROOMS)
;;       (DESC "Aragain Falls")
;;       (WEST TO ON-RAINBOW IF RAINBOW-FLAG)
;;       (DOWN "It's a long way...")
;;       (NORTH TO SHORE)
;;       (UP TO ON-RAINBOW IF RAINBOW-FLAG)
;;       (ACTION FALLS-ROOM)
;;       (FLAGS RLANDBIT SACREDBIT ONBIT)
;;       (GLOBAL GLOBAL-WATER RIVER RAINBOW)>

(defn aragain-falls-action
  "Handle Aragain Falls room description.
   ZIL: FALLS-ROOM - dynamic description based on RAINBOW-FLAG."
  [game-state rarg]
  (if (= rarg :look)
    (let [rainbow-solid? (get game-state :rainbow-flag false)]
      (-> game-state
          (utils/tell "You are at the top of Aragain Falls, an enormous waterfall with a drop of about 450 feet. The only path here is on the north end.")
          (utils/crlf)
          (utils/crlf)
          (utils/tell (if rainbow-solid?
                        "A solid rainbow spans the falls."
                        "A beautiful rainbow can be seen over the falls and to the west."))
          (utils/crlf)))
    (gs/use-default game-state)))

(def aragain-falls
  {:id :aragain-falls
   :desc "Aragain Falls"
   ;; ldesc handled dynamically by action function
   :flags #{:lit :sacred}
   :globals #{:global-water :river :rainbow}
   :exits {:west {:to :on-rainbow :if :rainbow-flag}
           :up {:to :on-rainbow :if :rainbow-flag}
           :down "It's a long way..."
           :north :shore}
   :action aragain-falls-action})

;;; ---------------------------------------------------------------------------
;;; FRIGID RIVER (requires boat to navigate)
;;; ---------------------------------------------------------------------------
;;; The river rooms have NONLANDBIT which means you must be in a boat to enter.
;;; Until the boat system is implemented, these rooms define the geography but
;;; won't be fully navigable.

;; <ROOM RIVER-1
;;       (IN ROOMS)
;;       (LDESC
;; "You are on the Frigid River in the vicinity of the Dam. The river
;; flows quietly here. There is a landing on the west shore.")
;;       (DESC "Frigid River")
;;       (UP "You cannot go upstream due to strong currents.")
;;       (WEST TO DAM-BASE)
;;       (LAND TO DAM-BASE)
;;       (DOWN TO RIVER-2)
;;       (EAST "The White Cliffs prevent your landing here.")
;;       (FLAGS NONLANDBIT SACREDBIT ONBIT)
;;       (GLOBAL GLOBAL-WATER RIVER)>

(def river-1
  {:id :river-1
   :desc "Frigid River"
   :ldesc "You are on the Frigid River in the vicinity of the Dam. The river flows quietly here. There is a landing on the west shore."
   :flags #{:lit :sacred :rwater}  ; NONLANDBIT = :rwater (water room, requires boat)
   :globals #{:global-water :river}
   :exits {:up "You cannot go upstream due to strong currents."
           :west :dam-base
           :land :dam-base
           :down :river-2
           :east "The White Cliffs prevent your landing here."}})

;; <ROOM RIVER-2
;;       (IN ROOMS)
;;       (LDESC
;; "The river turns a corner here making it impossible to see the
;; Dam. The White Cliffs loom on the east bank and large rocks prevent
;; landing on the west.")
;;       (DESC "Frigid River")
;;       (UP "You cannot go upstream due to strong currents.")
;;       (DOWN TO RIVER-3)
;;       (LAND "There is no safe landing spot here.")
;;       (EAST "The White Cliffs prevent your landing here.")
;;       (WEST "Just in time you steer away from the rocks.")
;;       (FLAGS NONLANDBIT SACREDBIT)
;;       (GLOBAL GLOBAL-WATER RIVER)>

(def river-2
  {:id :river-2
   :desc "Frigid River"
   :ldesc "The river turns a corner here making it impossible to see the Dam. The White Cliffs loom on the east bank and large rocks prevent landing on the west."
   :flags #{:sacred :rwater}  ; Not lit (no ONBIT), water room
   :globals #{:global-water :river}
   :exits {:up "You cannot go upstream due to strong currents."
           :down :river-3
           :land "There is no safe landing spot here."
           :east "The White Cliffs prevent your landing here."
           :west "Just in time you steer away from the rocks."}})

;; <ROOM RIVER-3
;;       (IN ROOMS)
;;       (LDESC
;; "The river descends here into a valley. There is a narrow beach on the
;; west shore below the cliffs. In the distance a faint rumbling can be
;; heard.")
;;       (DESC "Frigid River")
;;       (UP "You cannot go upstream due to strong currents.")
;;       (DOWN TO RIVER-4)
;;       (LAND TO WHITE-CLIFFS-NORTH)
;;       (WEST TO WHITE-CLIFFS-NORTH)
;;       (FLAGS NONLANDBIT SACREDBIT)
;;       (GLOBAL GLOBAL-WATER RIVER)>

(def river-3
  {:id :river-3
   :desc "Frigid River"
   :ldesc "The river descends here into a valley. There is a narrow beach on the west shore below the cliffs. In the distance a faint rumbling can be heard."
   :flags #{:sacred :rwater}  ; Water room
   :globals #{:global-water :river}
   :exits {:up "You cannot go upstream due to strong currents."
           :down :river-4
           :land :white-cliffs-north
           :west :white-cliffs-north}})

;; <ROOM RIVER-4
;;       (IN ROOMS)
;;       (LDESC
;; "The river is running faster here and the sound ahead appears to be
;; that of rushing water. On the east shore is a sandy beach. A small
;; area of beach can also be seen below the cliffs on the west shore.")
;;       (DESC "Frigid River")
;;       (UP "You cannot go upstream due to strong currents.")
;;       (DOWN TO RIVER-5)
;;       (LAND "You can land either to the east or the west.")
;;       (WEST TO WHITE-CLIFFS-SOUTH)
;;       (EAST TO SANDY-BEACH)
;;       (ACTION RIVR4-ROOM)
;;       (FLAGS NONLANDBIT SACREDBIT)
;;       (GLOBAL GLOBAL-WATER RIVER)>

(def river-4
  {:id :river-4
   :desc "Frigid River"
   :ldesc "The river is running faster here and the sound ahead appears to be that of rushing water. On the east shore is a sandy beach. A small area of beach can also be seen below the cliffs on the west shore."
   :flags #{:sacred :rwater}  ; Water room
   :globals #{:global-water :river}
   ;; Note: RIVR4-ROOM action handles warning about falls ahead - skipped for now
   :exits {:up "You cannot go upstream due to strong currents."
           :down :river-5
           :land "You can land either to the east or the west."
           :west :white-cliffs-south
           :east :sandy-beach}})

;; <ROOM RIVER-5
;;       (IN ROOMS)
;;       (LDESC
;; "The sound of rushing water is nearly unbearable here. On the east
;; shore is a large landing area.")
;;       (DESC "Frigid River")
;;       (UP "You cannot go upstream due to strong currents.")
;;       (EAST TO SHORE)
;;       (LAND TO SHORE)
;;       (FLAGS NONLANDBIT SACREDBIT ONBIT)
;;       (GLOBAL GLOBAL-WATER RIVER)>

(def river-5
  {:id :river-5
   :desc "Frigid River"
   :ldesc "The sound of rushing water is nearly unbearable here. On the east shore is a large landing area."
   :flags #{:lit :sacred :rwater}  ; Lit, water room
   :globals #{:global-water :river}
   ;; Note: Going DOWN from here without landing leads to death over the falls!
   ;; That logic will be implemented when boat system is added.
   :exits {:up "You cannot go upstream due to strong currents."
           :east :shore
           :land :shore}})

;; <ROOM SHORE
;;       (IN ROOMS)
;;       (LDESC
;; "You are on the east shore of the river. The water here seems somewhat
;; treacherous. A path travels from north to south here, the south end
;; quickly turning around a sharp corner.")
;;       (DESC "Shore")
;;       (NORTH TO SANDY-BEACH)
;;       (SOUTH TO ARAGAIN-FALLS)
;;       (FLAGS RLANDBIT SACREDBIT ONBIT)
;;       (GLOBAL GLOBAL-WATER RIVER)>

(def shore
  {:id :shore
   :desc "Shore"
   :ldesc "You are on the east shore of the river. The water here seems somewhat treacherous. A path travels from north to south here, the south end quickly turning around a sharp corner."
   :flags #{:lit :sacred}
   :globals #{:global-water :river}
   :exits {:north :sandy-beach
           :south :aragain-falls}})

;; <ROOM SANDY-BEACH
;;       (IN ROOMS)
;;       (LDESC
;; "You are on a large sandy beach on the east shore of the river, which is
;; flowing quickly by. A path runs beside the river to the south here, and
;; a passage is partially buried in sand to the northeast.")
;;       (DESC "Sandy Beach")
;;       (NE TO SANDY-CAVE)
;;       (SOUTH TO SHORE)
;;       (FLAGS RLANDBIT SACREDBIT)
;;       (GLOBAL GLOBAL-WATER RIVER)>

(def sandy-beach
  {:id :sandy-beach
   :desc "Sandy Beach"
   :ldesc "You are on a large sandy beach on the east shore of the river, which is flowing quickly by. A path runs beside the river to the south here, and a passage is partially buried in sand to the northeast."
   :flags #{:lit :sacred :rland}  ; RLANDBIT - boat can land here
   :globals #{:global-water :river}
   :exits {:ne :sandy-cave
           :south :shore}})

;; <ROOM SANDY-CAVE
;;       (IN ROOMS)
;;       (LDESC
;; "This is a sand-filled cave whose exit is to the southwest.")
;;       (DESC "Sandy Cave")
;;       (SW TO SANDY-BEACH)
;;       (FLAGS RLANDBIT)>

(def sandy-cave
  {:id :sandy-cave
   :desc "Sandy Cave"
   :ldesc "This is a sand-filled cave whose exit is to the southwest."
   :flags #{:lit}  ; Outdoor, lit
   :exits {:sw :sandy-beach}})

;; <ROOM WHITE-CLIFFS-NORTH
;;       (IN ROOMS)
;;       (LDESC
;; "You are on a narrow strip of beach which runs along the base of the
;; White Cliffs. There is a narrow path heading south along the Cliffs
;; and a tight passage leading west into the cliffs themselves.")
;;       (DESC "White Cliffs Beach")
;;       (SOUTH TO WHITE-CLIFFS-SOUTH IF DEFLATE ELSE "The path is too narrow.")
;;       (WEST TO DAMP-CAVE IF DEFLATE ELSE "The path is too narrow.")
;;       (ACTION WHITE-CLIFFS-FUNCTION)
;;       (FLAGS RLANDBIT SACREDBIT)
;;       (GLOBAL GLOBAL-WATER WHITE-CLIFF RIVER)>

(def white-cliffs-north
  {:id :white-cliffs-north
   :desc "White Cliffs Beach"
   :ldesc "You are on a narrow strip of beach which runs along the base of the White Cliffs. There is a narrow path heading south along the Cliffs and a tight passage leading west into the cliffs themselves."
   :flags #{:lit :sacred}
   :globals #{:global-water :white-cliff :river}
   ;; Note: DEFLATE flag controls whether paths are accessible (boat must be deflated)
   ;; For now, paths are open. TODO: Add conditional when boat system is implemented.
   :exits {:south :white-cliffs-south
           :west :damp-cave}})

;; <ROOM WHITE-CLIFFS-SOUTH
;;       (IN ROOMS)
;;       (LDESC
;; "You are on a rocky, narrow strip of beach beside the Cliffs. A
;; narrow path leads north along the shore.")
;;       (DESC "White Cliffs Beach")
;;       (NORTH TO WHITE-CLIFFS-NORTH
;;        IF DEFLATE ELSE "The path is too narrow.")
;;       (ACTION WHITE-CLIFFS-FUNCTION)
;;       (FLAGS RLANDBIT SACREDBIT)
;;       (GLOBAL GLOBAL-WATER WHITE-CLIFF RIVER)>

(def white-cliffs-south
  {:id :white-cliffs-south
   :desc "White Cliffs Beach"
   :ldesc "You are on a rocky, narrow strip of beach beside the Cliffs. A narrow path leads north along the shore."
   :flags #{:lit :sacred}
   :globals #{:global-water :white-cliff :river}
   ;; Note: DEFLATE flag controls path access. TODO: Add conditional when boat system is implemented.
   :exits {:north :white-cliffs-north}})

;;; ---------------------------------------------------------------------------
;;; MIRROR ROOM COMPLEX
;;; ---------------------------------------------------------------------------

;; The mirror rooms contain a large mirror that teleports objects (and the player)
;; between the two mirror rooms when rubbed. The mirror can also be broken.
;; ZIL: MIRROR-ROOM routine in 1actions.zil lines 971-1043

(defn mirror-room-action
  "Handle mirror room description and mirror interactions.
   ZIL: MIRROR-ROOM in 1actions.zil"
  [game-state rarg]
  (if (= rarg :look)
    (let [mirror-broken? (get game-state :mirror-mung false)]
      (-> game-state
          (utils/tell "You are in a large square room with tall ceilings. On the south wall is an enormous mirror which fills the entire wall. There are exits on the other three sides of the room.")
          (utils/crlf)
          (cond-> mirror-broken?
            (-> (utils/tell "Unfortunately, the mirror has been destroyed by your recklessness.")
                (utils/crlf)))))
    (gs/use-default game-state)))

;; <ROOM MIRROR-ROOM-1
;;       (IN ROOMS)
;;       (DESC "Mirror Room")
;;       (NORTH TO COLD-PASSAGE)
;;       (WEST TO TWISTING-PASSAGE)
;;       (EAST TO SMALL-CAVE)
;;       (ACTION MIRROR-ROOM)
;;       (FLAGS RLANDBIT)>

(def mirror-room-1
  {:id :mirror-room-1
   :desc "Mirror Room"
   ;; ldesc handled by action function
   :flags #{}  ; Underground, not lit
   :exits {:north :cold-passage
           :west :twisting-passage
           :east :small-cave}
   :action mirror-room-action})

;; <ROOM MIRROR-ROOM-2
;;       (IN ROOMS)
;;       (DESC "Mirror Room")
;;       (WEST TO WINDING-PASSAGE)
;;       (NORTH TO NARROW-PASSAGE)
;;       (EAST TO TINY-CAVE)
;;       (ACTION MIRROR-ROOM)
;;       (FLAGS RLANDBIT ONBIT)>

(def mirror-room-2
  {:id :mirror-room-2
   :desc "Mirror Room"
   ;; ldesc handled by action function
   :flags #{:lit}  ; ZIL ONBIT = lit
   :exits {:west :winding-passage
           :north :narrow-passage
           :east :tiny-cave}
   :action mirror-room-action})

;; <ROOM SMALL-CAVE
;;       (IN ROOMS)
;;       (LDESC
;; "This is a tiny cave with entrances west and north, and a staircase
;; leading down.")
;;       (DESC "Cave")
;;       (NORTH TO MIRROR-ROOM-1)
;;       (DOWN TO ATLANTIS-ROOM)
;;       (SOUTH TO ATLANTIS-ROOM)
;;       (WEST TO TWISTING-PASSAGE)
;;       (FLAGS RLANDBIT)
;;       (GLOBAL STAIRS)>

(def small-cave
  {:id :small-cave
   :desc "Cave"
   :ldesc "This is a tiny cave with entrances west and north, and a staircase leading down."
   :flags #{}  ; Underground, not lit
   :globals #{:stairs}
   :exits {:north :mirror-room-1
           :down :atlantis-room
           :south :atlantis-room
           :west :twisting-passage}})

;; <ROOM TINY-CAVE
;;       (IN ROOMS)
;;       (LDESC
;; "This is a tiny cave with entrances west and north, and a dark,
;; forbidding staircase leading down.")
;;       (DESC "Cave")
;;       (NORTH TO MIRROR-ROOM-2)
;;       (WEST TO WINDING-PASSAGE)
;;       (DOWN TO ENTRANCE-TO-HADES)
;;       (ACTION CAVE2-ROOM)
;;       (FLAGS RLANDBIT)
;;       (GLOBAL STAIRS)>

(defn tiny-cave-action
  "Handle Tiny Cave wind effect that can blow out candles.
   ZIL: CAVE2-ROOM in 1actions.zil lines 2429-2439
   Wind only blows when Hades gate is open (lld-flag set) - wind from below."
  [game-state rarg]
  (if (= rarg :m-end)
    (let [lld-flag (:lld-flag game-state)
          winner (:winner game-state)
          has-candles? (= winner (gs/get-thing-loc-id game-state :candles))
          candles-on? (gs/set-thing-flag? game-state :candles :on)]
      ;; Wind only comes up from Hades after the gate is opened
      (if (and lld-flag has-candles? candles-on? (< (random/rand-int* 100) 50))
        (-> game-state
            (gs/unset-thing-flag :candles :on)
            (utils/tell "A gust of wind blows out your candles!"))
        game-state))
    (gs/use-default game-state)))

(def tiny-cave
  {:id :tiny-cave
   :desc "Cave"
   :ldesc "This is a tiny cave with entrances west and north, and a dark, forbidding staircase leading down."
   :flags #{}  ; Underground, not lit
   :globals #{:stairs}
   :action tiny-cave-action
   :exits {:north :mirror-room-2
           :west :winding-passage
           :down :entrance-to-hades}})

;; <ROOM COLD-PASSAGE
;;       (IN ROOMS)
;;       (LDESC
;; "This is a cold and damp corridor where a long east-west passageway
;; turns into a southward path.")
;;       (DESC "Cold Passage")
;;       (SOUTH TO MIRROR-ROOM-1)
;;       (WEST TO SLIDE-ROOM)
;;       (FLAGS RLANDBIT)>

(def cold-passage
  {:id :cold-passage
   :desc "Cold Passage"
   :ldesc "This is a cold and damp corridor where a long east-west passageway turns into a southward path."
   :flags #{}  ; Underground, not lit
   :exits {:south :mirror-room-1
           :west :slide-room}})

;; <ROOM WINDING-PASSAGE
;;       (IN ROOMS)
;;       (LDESC
;; "This is a winding passage. It seems that there are only exits
;; on the east and north.")
;;       (DESC "Winding Passage")
;;       (NORTH TO MIRROR-ROOM-2)
;;       (EAST TO TINY-CAVE)
;;       (FLAGS RLANDBIT)>

(def winding-passage
  {:id :winding-passage
   :desc "Winding Passage"
   :ldesc "This is a winding passage. It seems that there are only exits on the east and north."
   :flags #{}  ; Underground, not lit
   :exits {:north :mirror-room-2
           :east :tiny-cave}})

;; <ROOM TWISTING-PASSAGE
;;       (IN ROOMS)
;;       (LDESC
;; "This is a winding passage. It seems that there are only exits
;; on the east and north.")
;;       (DESC "Twisting Passage")
;;       (NORTH TO MIRROR-ROOM-1)
;;       (EAST TO SMALL-CAVE)
;;       (FLAGS RLANDBIT)>

(def twisting-passage
  {:id :twisting-passage
   :desc "Twisting Passage"
   :ldesc "This is a winding passage. It seems that there are only exits on the east and north."
   :flags #{}  ; Underground, not lit
   :exits {:north :mirror-room-1
           :east :small-cave}})

;; <ROOM ATLANTIS-ROOM
;;       (IN ROOMS)
;;       (LDESC
;; "This is an ancient room, long under water. There is an exit to
;; the south and a staircase leading up.")
;;       (DESC "Atlantis Room")
;;       (UP TO SMALL-CAVE)
;;       (SOUTH TO RESERVOIR-NORTH)
;;       (FLAGS RLANDBIT)
;;       (GLOBAL STAIRS)>

(def atlantis-room
  {:id :atlantis-room
   :desc "Atlantis Room"
   :ldesc "This is an ancient room, long under water. There is an exit to the south and a staircase leading up."
   :flags #{}  ; Underground, not lit
   :globals #{:stairs}
   :exits {:up :small-cave
           :south :reservoir-north}})

;;; ---------------------------------------------------------------------------
;;; TEMPLE / HADES AREA
;;; ---------------------------------------------------------------------------

;; <ROOM DOME-ROOM
;;       (IN ROOMS)
;;       (DESC "Dome Room")
;;       (WEST TO ENGRAVINGS-CAVE)
;;       (DOWN TO TORCH-ROOM
;;        IF DOME-FLAG ELSE "You cannot go down without fracturing many bones.")
;;       (ACTION DOME-ROOM-FCN)
;;       (FLAGS RLANDBIT)
;;       (PSEUDO "DOME" DOME-PSEUDO)>

(defn dome-room-action
  "Handle Dome Room description and special behavior.
   ZIL: DOME-ROOM-FCN in 1actions.zil"
  [game-state rarg]
  (cond
    (= rarg :look)
    (let [rope-tied? (get game-state :dome-flag false)]
      (-> game-state
          (utils/tell "You are at the periphery of a large dome, which forms the ceiling of another room below. Protecting you from a precipitous drop is a wooden railing which circles the dome.")
          (utils/crlf)
          (cond-> rope-tied?
            (-> (utils/tell "Hanging down from the railing is a rope which ends about ten feet from the floor below.")
                (utils/crlf)))))

    ;; If dead (ghost), player is pulled down to torch room
    (and (= rarg :m-enter)
         (get game-state :dead false))
    (-> game-state
        (utils/tell "As you enter the dome you feel a strong pull as if from a wind drawing you over the railing and down.")
        (utils/crlf)
        (assoc :here :torch-room))

    ;; If player tries to leap, they die
    (and (= rarg :m-enter)
         (= (:verb game-state) :leap))
    (death/jigs-up game-state "I'm afraid that the leap you attempted has done you in.")

    :else
    (gs/use-default game-state)))

(def dome-room
  {:id :dome-room
   :desc "Dome Room"
   ;; ldesc handled by action function
   :flags #{}  ; Underground, not lit
   :exits {:west :engravings-cave
           :down {:to :torch-room
                  :if :dome-flag
                  :else "You cannot go down without fracturing many bones."}}
   :action dome-room-action})

;; <ROOM TORCH-ROOM
;;       (IN ROOMS)
;;       (DESC "Torch Room")
;;       (UP "You cannot reach the rope.")
;;       (SOUTH TO NORTH-TEMPLE)
;;       (DOWN TO NORTH-TEMPLE)
;;       (ACTION TORCH-ROOM-FCN)
;;       (FLAGS RLANDBIT)
;;       (GLOBAL STAIRS)
;;       (PSEUDO "DOME" DOME-PSEUDO)>

(defn torch-room-action
  "Handle Torch Room description.
   ZIL: TORCH-ROOM-FCN in 1actions.zil"
  [game-state rarg]
  (if (= rarg :look)
    (let [rope-tied? (get game-state :dome-flag false)]
      (-> game-state
          (utils/tell "This is a large room with a prominent doorway leading to a down staircase. Above you is a large dome. Up around the edge of the dome (20 feet up) is a wooden railing. In the center of the room sits a white marble pedestal.")
          ;; Paragraph break after room description
          (utils/tell "\n\n")
          (cond-> rope-tied?
            (-> (utils/tell "A piece of rope descends from the railing above, ending some five feet above your head.")
                ;; Paragraph break before objects
                (utils/tell "\n\n")))))
    (gs/use-default game-state)))

(def torch-room
  {:id :torch-room
   :desc "Torch Room"
   ;; ldesc handled by action function
   :flags #{}  ; Underground, not lit
   :globals #{:stairs}
   :exits {:up "You cannot reach the rope."
           :south :north-temple
           :down :north-temple}
   :action torch-room-action})

;; <ROOM NORTH-TEMPLE
;;       (IN ROOMS)
;;       (LDESC
;; "This is the north end of a large temple. On the east wall is an
;; ancient inscription, probably a prayer in a long-forgotten language.
;; Below the prayer is a staircase leading down. The west wall is solid
;; granite. The exit to the north end of the room is through huge
;; marble pillars.")
;;       (DESC "Temple")
;;       (DOWN TO EGYPT-ROOM)
;;       (EAST TO EGYPT-ROOM)
;;       (NORTH TO TORCH-ROOM)
;;       (OUT TO TORCH-ROOM)
;;       (UP TO TORCH-ROOM)
;;       (SOUTH TO SOUTH-TEMPLE)
;;       (FLAGS RLANDBIT ONBIT SACREDBIT)
;;       (GLOBAL STAIRS)>

(def north-temple
  {:id :north-temple
   :desc "Temple"
   :ldesc "This is the north end of a large temple. On the east wall is an ancient inscription, probably a prayer in a long-forgotten language. Below the prayer is a staircase leading down. The west wall is solid granite. The exit to the north end of the room is through huge marble pillars."
   :flags #{:lit :sacred}
   :globals #{:stairs}
   :exits {:down :egypt-room
           :east :egypt-room
           :north :torch-room
           :out :torch-room
           :up :torch-room
           :south :south-temple}})

;; <ROOM SOUTH-TEMPLE
;;       (IN ROOMS)
;;       (LDESC
;; "This is the south end of a large temple. In front of you is what
;; appears to be an altar. In one corner is a small hole in the floor
;; which leads into darkness. You probably could not get back up it.")
;;       (DESC "Altar")
;;       (NORTH TO NORTH-TEMPLE)
;;       (DOWN TO TINY-CAVE
;;        IF COFFIN-CURE
;;        ELSE "You haven't a prayer of getting the coffin down there.")
;;       (FLAGS RLANDBIT ONBIT SACREDBIT)
;;       (ACTION SOUTH-TEMPLE-FCN)>

(defn south-temple-action
  "Handle South Temple / Altar behavior.
   ZIL: SOUTH-TEMPLE-FCN - sets COFFIN-CURE based on whether player has coffin.
   The player can only go down if they're NOT carrying the coffin."
  [game-state rarg]
  (if (= rarg :m-beg)
    ;; Set coffin-cure flag: true if player does NOT have the coffin
    (let [winner (:winner game-state)
          has-coffin? (= winner (gs/get-thing-loc-id game-state :gold-coffin))]
      (-> game-state
          (assoc :coffin-cure (not has-coffin?))
          (gs/use-default)))
    (gs/use-default game-state)))

(def south-temple
  {:id :south-temple
   :desc "Altar"
   :ldesc "This is the south end of a large temple. In front of you is what appears to be an altar. In one corner is a small hole in the floor which leads into darkness. You probably could not get back up it."
   :flags #{:lit :sacred}
   :exits {:north :north-temple
           :down {:to :tiny-cave
                  :if :coffin-cure
                  :else "You haven't a prayer of getting the coffin down there."}}
   :action south-temple-action})

;; <ROOM EGYPT-ROOM
;;       (IN ROOMS)
;;       (LDESC
;; "This is a room which looks like an Egyptian tomb. There is an
;; ascending staircase to the west.")
;;       (DESC "Egyptian Room")
;;       (WEST TO NORTH-TEMPLE)
;;       (UP TO NORTH-TEMPLE)
;;       (FLAGS RLANDBIT)
;;       (GLOBAL STAIRS)>

(def egypt-room
  {:id :egypt-room
   :desc "Egyptian Room"
   :ldesc "This is a room which looks like an Egyptian tomb. There is an ascending staircase to the west."
   :flags #{}  ; Underground, not lit
   :globals #{:stairs}
   :exits {:west :north-temple
           :up :north-temple}})

;; <ROOM ENTRANCE-TO-HADES
;;       (IN ROOMS)
;;       (DESC "Entrance to Hades")
;;       (UP TO TINY-CAVE)
;;       (IN TO LAND-OF-LIVING-DEAD IF LLD-FLAG
;;        ELSE "Some invisible force prevents you from passing through the gate.")
;;       (SOUTH TO LAND-OF-LIVING-DEAD IF LLD-FLAG
;;        ELSE "Some invisible force prevents you from passing through the gate.")
;;       (ACTION LLD-ROOM)
;;       (FLAGS RLANDBIT ONBIT)
;;       (GLOBAL BODIES)
;;       (PSEUDO "GATE" GATE-PSEUDO "GATES" GATE-PSEUDO)>

(defn entrance-to-hades-action
  "Handle Entrance to Hades description and exorcism puzzle.
   ZIL: LLD-ROOM in 1actions.zil"
  [game-state rarg]
  (case rarg
    :look
    (let [gate-open? (:lld-flag game-state)
          is-dead? (:dead game-state)]
      (-> game-state
          (utils/tell "You are outside a large gateway, on which is inscribed

  Abandon every hope all ye who enter here!

The gate is open; through it you can see a desolation, with a pile of mangled bodies in one corner. Thousands of voices, lamenting some hideous fate, can be heard.")
          ;; Paragraph break after room description
          (utils/tell "\n\n")
          (cond-> (and (not gate-open?) (not is-dead?))
            (utils/tell "The way through the gate is barred by evil spirits, who jeer at your attempts to pass."))))

    ;; Ring the bell - start the exorcism sequence
    ;; ZIL: LLD-ROOM handles RING in M-BEG
    :ring-bell
    (let [here (:here game-state)
          winner (:winner game-state)
          has-candles? (= winner (gs/get-thing-loc-id game-state :candles))]
      (-> game-state
          ;; XB flag indicates exorcism started
          (assoc :xb true)
          ;; Remove bell from player, put hot-bell in room
          (assoc-in [:objects :brass-bell :in] :limbo)
          (assoc-in [:objects :hot-bell :in] here)
          (utils/tell "The bell suddenly becomes red hot and falls to the ground. The wraiths, as if paralyzed, stop their jeering and slowly turn to face you. On their ashen faces, the expression of a long-forgotten terror takes shape.")
          ;; Paragraph break before candle message
          (utils/tell "\n\n")
          ;; If player has candles, drop them
          (cond-> has-candles?
            (-> (utils/tell "In your confusion, the candles drop to the ground (and they are out).")
                (assoc-in [:objects :candles :in] here)
                (gs/unset-thing-flag :candles :on)))))

    ;; M-BEG: Check for reading the book during exorcism (prayer completion)
    ;; ZIL: LLD-ROOM checks for READ BOOK when XC is set
    :m-beg
    (let [xc (:xc game-state)
          lld-flag (:lld-flag game-state)
          prsa (parser-state/get-prsa game-state)
          prso (parser-state/get-prso game-state)]
      (if (and xc
               (not lld-flag)
               (= prsa :read)
               (= prso :black-book))
        ;; Exorcism complete! The prayer banishes the spirits
        ;; Set :command-handled so perform doesn't also run v-read
        (-> game-state
            (utils/tell "Each word of the prayer reverberates through the hall in a deafening confusion. As the last word fades, a voice, loud and commanding, speaks: \"Begone, fiends!\" A heart-stopping scream fills the cavern, and the spirits, sensing a greater power, flee through the walls.")
            (utils/crlf)
            ;; Remove ghosts and open the gate
            (assoc-in [:objects :ghosts :in] :limbo)
            (assoc :lld-flag true)
            (assoc :command-handled true))
        ;; Not the exorcism prayer - use default
        (gs/use-default game-state)))

    ;; M-END: Check for candles lit during exorcism
    ;; ZIL: LLD-ROOM M-END - spirits cower when candles lit after bell
    :m-end
    (let [xb (:xb game-state)
          xc (:xc game-state)
          winner (:winner game-state)
          has-candles? (= winner (gs/get-thing-loc-id game-state :candles))
          candles-on? (gs/set-thing-flag? game-state :candles :on)]
      (if (and xb has-candles? candles-on? (not xc))
        (-> game-state
            (assoc :xc true)
            ;; Paragraph break before flames message
            (utils/tell "\n\n")
            (utils/tell "The flames flicker wildly and appear to dance. The earth beneath your feet trembles, and your legs nearly buckle beneath you. The spirits cower at your unearthly power."))
        game-state))

    ;; Default
    (gs/use-default game-state)))

(def entrance-to-hades
  {:id :entrance-to-hades
   :desc "Entrance to Hades"
   ;; ldesc handled by action function
   :flags #{:lit}
   :globals #{:bodies}
   :exits {:up :tiny-cave
           :in {:to :land-of-living-dead
                :if :lld-flag
                :else "Some invisible force prevents you from passing through the gate."}
           :south {:to :land-of-living-dead
                   :if :lld-flag
                   :else "Some invisible force prevents you from passing through the gate."}}
   :action entrance-to-hades-action})

;; <ROOM LAND-OF-LIVING-DEAD
;;       (IN ROOMS)
;;       (LDESC
;; "You have entered the Land of the Living Dead. Thousands of lost souls
;; can be heard weeping and moaning. In the corner are stacked the remains
;; of dozens of previous adventurers less fortunate than yourself.
;; A passage exits to the north.")
;;       (DESC "Land of the Dead")
;;       (OUT TO ENTRANCE-TO-HADES)
;;       (NORTH TO ENTRANCE-TO-HADES)
;;       (FLAGS RLANDBIT ONBIT)
;;       (GLOBAL BODIES)>

(def land-of-living-dead
  {:id :land-of-living-dead
   :desc "Land of the Dead"
   :ldesc "You have entered the Land of the Living Dead. Thousands of lost souls can be heard weeping and moaning. In the corner are stacked the remains of dozens of previous adventurers less fortunate than yourself. A passage exits to the north."
   :flags #{:lit}
   :globals #{:bodies}
   :exits {:out :entrance-to-hades
           :north :entrance-to-hades}})

;;; ---------------------------------------------------------------------------
;;; COAL MINE AREA
;;; ---------------------------------------------------------------------------
;;; The coal mine is a dangerous area with gas rooms (instant death if you bring
;;; a flame), bats that randomly transport you, and the coal machine that
;;; transforms coal into diamonds.

;; BAT-DROPS: List of rooms the bat can drop you in
;; ZIL: <GLOBAL BAT-DROPS <LTABLE MINE-1 MINE-2 MINE-3 MINE-4 LADDER-TOP
;;      LADDER-BOTTOM SQUEEKY-ROOM MINE-ENTRANCE>>
(def bat-drops
  [:mine-1 :mine-2 :mine-3 :mine-4 :ladder-top :ladder-bottom
   :squeeky-room :mine-entrance])

(defn fly-me
  "Transport player to a random mine location via bat.
   ZIL: FLY-ME in 1actions.zil lines 330-337"
  [game-state]
  (let [destination (random/rand-nth* bat-drops)]
    (-> game-state
        (utils/tell "    Fweep!")
        (utils/crlf)
        (utils/tell "    Fweep!")
        (utils/crlf)
        (utils/tell "    Fweep!")
        (utils/crlf)
        (utils/tell "    Fweep!")
        (utils/crlf)
        (utils/crlf)
        (utils/tell "The bat grabs you by the scruff of your neck and lifts you away....")
        (utils/crlf)
        (utils/crlf)
        (assoc :here destination))))

(defn bats-room-action
  "Handle Bat Room description and bat transport.
   ZIL: BATS-ROOM in 1actions.zil lines 2491-2499"
  [game-state rarg]
  (cond
    (= rarg :look)
    (-> game-state
        (utils/tell "You are in a small room which has doors only to the east and south.")
        ;; Paragraph break after room description
        (utils/tell "\n\n"))

    ;; On entering, if player doesn't have garlic, bat transports them
    (and (= rarg :m-enter)
         (not (:dead game-state)))
    (let [garlic-loc (gs/get-thing-loc-id game-state :garlic)
          player-id (:winner game-state)
          here (:here game-state)
          has-garlic? (or (= garlic-loc player-id)
                          (= garlic-loc here))]
      (if has-garlic?
        (gs/use-default game-state)
        (fly-me game-state)))

    :else
    (gs/use-default game-state)))

(defn bat-ldesc
  "Get bat room long description based on garlic presence.
   ZIL: BAT-D in 1actions.zil lines 2482-2489"
  [game-state]
  (let [garlic-loc (gs/get-thing-loc-id game-state :garlic)
        player-id (:winner game-state)
        here (:here game-state)
        has-garlic? (or (= garlic-loc player-id)
                        (= garlic-loc here))]
    (if has-garlic?
      "In the corner of the room on the ceiling is a large vampire bat who is obviously deranged and holding his nose."
      "A large vampire bat, hanging from the ceiling, swoops down at you!")))

(defn boom-room-action
  "Handle Gas Room explosion if player has flame.
   ZIL: BOOM-ROOM in 1actions.zil lines 2459-2480"
  [game-state rarg]
  (if (= rarg :m-end)
    ;; Check if player is holding a lit flame source
    (let [candles-lit? (and (= (:winner game-state) (gs/get-thing-loc-id game-state :candles))
                            (gs/flag? game-state :objects :candles :on))
          torch-lit? (and (= (:winner game-state) (gs/get-thing-loc-id game-state :torch))
                          (gs/flag? game-state :objects :torch :on))
          match-lit? (and (= (:winner game-state) (gs/get-thing-loc-id game-state :match))
                          (gs/flag? game-state :objects :match :on))
          has-flame? (or candles-lit? torch-lit? match-lit?)]
      (if has-flame?
        (-> game-state
            (utils/tell "Oh dear. It appears that the smell coming from this room was coal gas. I would have thought twice about carrying flaming objects in here.")
            (utils/crlf)
            (death/jigs-up "|
      ** BOOOOOOOOOOOM **"))
        (gs/use-default game-state)))
    (gs/use-default game-state)))

(defn machine-room-action
  "Handle Machine Room description.
   ZIL: MACHINE-ROOM-FCN in 1actions.zil lines 2501-2513"
  [game-state rarg]
  (if (= rarg :look)
    (let [machine-open? (gs/flag? game-state :objects :machine :open)]
      (-> game-state
          (utils/tell "This is a large, cold room whose sole exit is to the north. In one corner there is a machine which is reminiscent of a clothes dryer. On its face is a switch which is labelled \"START\". The switch does not appear to be manipulable by any human hand (unless the fingers are about 1/16 by 1/4 inch). On the front of the machine is a large lid, which is ")
          (utils/tell (if machine-open? "open." "closed."))
          (utils/crlf)))
    (gs/use-default game-state)))

(defn no-objs-action
  "Handle rooms requiring empty hands (timber room, lower shaft).
   ZIL: NO-OBJS in 1actions.zil lines 2571-2584
   Sets EMPTY-HANDED flag based on whether player carries anything heavy."
  [game-state rarg]
  (if (= rarg :m-beg)
    ;; Check if player is carrying anything with weight > 4
    ;; For now, simplified: any carried objects make you not empty-handed
    (let [player-id (:winner game-state)
          carried-objects (->> (:objects game-state)
                               (filter (fn [[_ obj]] (= (:in obj) player-id)))
                               (map first))
          heavy-objects (filter (fn [obj-id]
                                  (let [weight (get-in game-state [:objects obj-id :weight] 0)]
                                    (> weight 4)))
                                carried-objects)
          empty-handed? (empty? heavy-objects)]
      (-> game-state
          (assoc :empty-handed empty-handed?)
          (gs/use-default)))
    (gs/use-default game-state)))

;; <ROOM SLIDE-ROOM
;;       (IN ROOMS)
;;       (LDESC
;; "This is a small chamber, which appears to have been part of a
;; coal mine. On the south wall of the chamber the letters \"Granite
;; Wall\" are etched in the rock. To the east is a long passage, and
;; there is a steep metal slide twisting downward. To the north is
;; a small opening.")
;;       (DESC "Slide Room")
;;       (EAST TO COLD-PASSAGE)
;;       (NORTH TO MINE-ENTRANCE)
;;       (DOWN TO CELLAR)
;;       (FLAGS RLANDBIT)
;;       (GLOBAL SLIDE)>

(def slide-room
  {:id :slide-room
   :desc "Slide Room"
   :ldesc "This is a small chamber, which appears to have been part of a coal mine. On the south wall of the chamber the letters \"Granite Wall\" are etched in the rock. To the east is a long passage, and there is a steep metal slide twisting downward. To the north is a small opening."
   :flags #{}  ; Underground, not lit
   :globals #{:slide}
   :exits {:east :cold-passage
           :north :mine-entrance
           :down :cellar}})

;; <ROOM MINE-ENTRANCE
;;       (IN ROOMS)
;;       (LDESC
;; "You are standing at the entrance of what might have been a coal mine.
;; The shaft enters the west wall, and there is another exit on the south
;; end of the room.")
;;       (DESC "Mine Entrance")
;;       (SOUTH TO SLIDE-ROOM)
;;       (IN TO SQUEEKY-ROOM)
;;       (WEST TO SQUEEKY-ROOM)
;;       (FLAGS RLANDBIT)>

(def mine-entrance
  {:id :mine-entrance
   :desc "Mine Entrance"
   :ldesc "You are standing at the entrance of what might have been a coal mine. The shaft enters the west wall, and there is another exit on the south end of the room."
   :flags #{}  ; Underground, not lit
   :exits {:south :slide-room
           :in :squeeky-room
           :west :squeeky-room}})

;; <ROOM SQUEEKY-ROOM
;;       (IN ROOMS)
;;       (LDESC
;; "You are in a small room. Strange squeaky sounds may be heard coming
;; from the passage at the north end. You may also escape to the east.")
;;       (DESC "Squeaky Room")
;;       (NORTH TO BAT-ROOM)
;;       (EAST TO MINE-ENTRANCE)
;;       (FLAGS RLANDBIT)>

(def squeeky-room
  {:id :squeeky-room
   :desc "Squeaky Room"
   :ldesc "You are in a small room. Strange squeaky sounds may be heard coming from the passage at the north end. You may also escape to the east."
   :flags #{}  ; Underground, not lit
   :exits {:north :bat-room
           :east :mine-entrance}})

;; <ROOM BAT-ROOM
;;       (IN ROOMS)
;;       (DESC "Bat Room")
;;       (SOUTH TO SQUEEKY-ROOM)
;;       (EAST TO SHAFT-ROOM)
;;       (ACTION BATS-ROOM)
;;       (FLAGS RLANDBIT SACREDBIT)>

(def bat-room
  {:id :bat-room
   :desc "Bat Room"
   ;; ldesc handled by action function
   :flags #{:sacred}  ; SACREDBIT - sacred room (no thief)
   :exits {:south :squeeky-room
           :east :shaft-room}
   :action bats-room-action})

;; <ROOM SHAFT-ROOM
;;       (IN ROOMS)
;;       (LDESC
;; "This is a large room, in the middle of which is a small shaft
;; descending through the floor into darkness below. To the west and
;; the north are exits from this room. Constructed over the top of the
;; shaft is a metal framework to which a heavy iron chain is attached.")
;;       (DESC "Shaft Room")
;;       (DOWN "You wouldn't fit and would die if you could.")
;;       (WEST TO BAT-ROOM)
;;       (NORTH TO SMELLY-ROOM)
;;       (FLAGS RLANDBIT)
;;       (PSEUDO "CHAIN" CHAIN-PSEUDO)>

(def shaft-room
  {:id :shaft-room
   :desc "Shaft Room"
   :ldesc "This is a large room, in the middle of which is a small shaft descending through the floor into darkness below. To the west and the north are exits from this room. Constructed over the top of the shaft is a metal framework to which a heavy iron chain is attached."
   :flags #{}  ; Underground, not lit
   :exits {:down "You wouldn't fit and would die if you could."
           :west :bat-room
           :north :smelly-room}})

;; <ROOM SMELLY-ROOM
;;       (IN ROOMS)
;;       (LDESC
;; "This is a small nondescript room. However, from the direction
;; of a small descending staircase a foul odor can be detected. To the
;; south is a narrow tunnel.")
;;       (DESC "Smelly Room")
;;       (DOWN TO GAS-ROOM)
;;       (SOUTH TO SHAFT-ROOM)
;;       (FLAGS RLANDBIT)
;;       (GLOBAL STAIRS)
;;       (PSEUDO "ODOR" GAS-PSEUDO "GAS" GAS-PSEUDO)>

(def smelly-room
  {:id :smelly-room
   :desc "Smelly Room"
   :ldesc "This is a small non-descript room. However, from the direction of a small descending staircase a foul odor can be detected. To the south is a narrow tunnel."
   :flags #{}  ; Underground, not lit
   :globals #{:stairs}
   :exits {:down :gas-room
           :south :shaft-room}})

;; <ROOM GAS-ROOM
;;       (IN ROOMS)
;;       (LDESC
;; "This is a small room which smells strongly of coal gas. There is a
;; short climb up some stairs and a narrow tunnel leading east.")
;;       (DESC "Gas Room")
;;       (UP TO SMELLY-ROOM)
;;       (EAST TO MINE-1)
;;       (ACTION BOOM-ROOM)
;;       (FLAGS RLANDBIT SACREDBIT)
;;       (GLOBAL STAIRS)
;;       (PSEUDO "GAS" GAS-PSEUDO "ODOR" GAS-PSEUDO)>

(def gas-room
  {:id :gas-room
   :desc "Gas Room"
   :ldesc "This is a small room which smells strongly of coal gas. There is a short climb up some stairs and a narrow tunnel leading east."
   :flags #{:sacred}  ; SACREDBIT - sacred room (no thief)
   :globals #{:stairs}
   :exits {:up :smelly-room
           :east :mine-1}
   :action boom-room-action})

;; <ROOM LADDER-TOP
;;       (IN ROOMS)
;;       (LDESC
;; "This is a very small room. In the corner is a rickety wooden
;; ladder, leading downward. It might be safe to descend. There is
;; also a staircase leading upward.")
;;       (DESC "Ladder Top")
;;       (DOWN TO LADDER-BOTTOM)
;;       (UP TO MINE-4)
;;       (FLAGS RLANDBIT)
;;       (GLOBAL LADDER STAIRS)>

(def ladder-top
  {:id :ladder-top
   :desc "Ladder Top"
   :ldesc "This is a very small room. In the corner is a rickety wooden ladder, leading downward. It might be safe to descend. There is also a staircase leading upward."
   :flags #{}  ; Underground, not lit
   :globals #{:ladder :stairs}
   :exits {:down :ladder-bottom
           :up :mine-4}})

;; <ROOM LADDER-BOTTOM
;;       (IN ROOMS)
;;       (LDESC
;; "This is a rather wide room. On one side is the bottom of a
;; narrow wooden ladder. To the west and the south are passages
;; leaving the room.")
;;       (DESC "Ladder Bottom")
;;       (SOUTH TO DEAD-END-5)
;;       (WEST TO TIMBER-ROOM)
;;       (UP TO LADDER-TOP)
;;       (FLAGS RLANDBIT)
;;       (GLOBAL LADDER)>

(def ladder-bottom
  {:id :ladder-bottom
   :desc "Ladder Bottom"
   :ldesc "This is a rather wide room. On one side is the bottom of a narrow wooden ladder. To the west and the south are passages leaving the room."
   :flags #{}  ; Underground, not lit
   :globals #{:ladder}
   :exits {:south :dead-end-5
           :west :timber-room
           :up :ladder-top}})

;; <ROOM DEAD-END-5
;;       (IN ROOMS)
;;       (DESC "Dead End")
;;       (LDESC "You have come to a dead end in the mine.")
;;       (NORTH TO LADDER-BOTTOM)
;;       (FLAGS RLANDBIT)>

(def dead-end-5
  {:id :dead-end-5
   :desc "Dead End"
   :ldesc "You have come to a dead end in the mine."
   :flags #{}  ; Underground, not lit
   :exits {:north :ladder-bottom}})

;; <ROOM TIMBER-ROOM
;;       (IN ROOMS)
;;       (LDESC
;; "This is a long and narrow passage, which is cluttered with broken
;; timbers. A wide passage comes from the east and turns at the
;; west end of the room into a very narrow passageway. From the west
;; comes a strong draft.")
;;       (DESC "Timber Room")
;;       (EAST TO LADDER-BOTTOM)
;;       (WEST TO LOWER-SHAFT
;;        IF EMPTY-HANDED
;;        ELSE "You cannot fit through this passage with that load.")
;;       (ACTION NO-OBJS)
;;       (FLAGS RLANDBIT SACREDBIT)>

(def timber-room
  {:id :timber-room
   :desc "Timber Room"
   :ldesc "This is a long and narrow passage, which is cluttered with broken timbers. A wide passage comes from the east and turns at the west end of the room into a very narrow passageway. From the west comes a strong draft."
   :flags #{:sacred}  ; SACREDBIT - sacred room (no thief)
   :exits {:east :ladder-bottom
           :west {:to :lower-shaft
                  :if :empty-handed
                  :else "You cannot fit through this passage with that load."}}
   :action no-objs-action})

;; <ROOM LOWER-SHAFT
;;       (IN ROOMS)
;;       (LDESC
;; "This is a small drafty room in which is the bottom of a long
;; shaft. To the south is a passageway and to the east a very narrow
;; passage. In the shaft can be seen a heavy iron chain.")
;;       (DESC "Drafty Room")
;;       (SOUTH TO MACHINE-ROOM)
;;       (OUT TO TIMBER-ROOM
;;        IF EMPTY-HANDED
;;        ELSE "You cannot fit through this passage with that load.")
;;       (EAST TO TIMBER-ROOM
;;        IF EMPTY-HANDED
;;        ELSE "You cannot fit through this passage with that load.")
;;       (ACTION NO-OBJS)
;;       (FLAGS RLANDBIT SACREDBIT)
;;       (PSEUDO "CHAIN" CHAIN-PSEUDO)>

(def lower-shaft
  {:id :lower-shaft
   :desc "Drafty Room"
   :ldesc "This is a small drafty room in which is the bottom of a long shaft. To the south is a passageway and to the east a very narrow passage. In the shaft can be seen a heavy iron chain."
   :flags #{:sacred}  ; SACREDBIT - sacred room (no thief)
   :exits {:south :machine-room
           :out {:to :timber-room
                 :if :empty-handed
                 :else "You cannot fit through this passage with that load."}
           :east {:to :timber-room
                  :if :empty-handed
                  :else "You cannot fit through this passage with that load."}}
   :action no-objs-action})

;; <ROOM MACHINE-ROOM
;;       (IN ROOMS)
;;       (DESC "Machine Room")
;;       (NORTH TO LOWER-SHAFT)
;;       (ACTION MACHINE-ROOM-FCN)
;;       (FLAGS RLANDBIT)>

(def machine-room
  {:id :machine-room
   :desc "Machine Room"
   ;; ldesc handled by action function
   :flags #{}  ; Underground, not lit
   :exits {:north :lower-shaft}
   :action machine-room-action})

;; --- COAL MINE MAZE ---

;; <ROOM MINE-1
;;       (IN ROOMS)
;;       (LDESC "This is a nondescript part of a coal mine.")
;;       (DESC "Coal Mine")
;;       (NORTH TO GAS-ROOM)
;;       (EAST TO MINE-1)
;;       (NE TO MINE-2)
;;       (FLAGS RLANDBIT)>

(def mine-1
  {:id :mine-1
   :desc "Coal Mine"
   :ldesc "This is a non-descript part of a coal mine."
   :flags #{}  ; Underground, not lit
   :exits {:north :gas-room
           :east :mine-1  ; Loop back to itself
           :ne :mine-2}})

;; <ROOM MINE-2
;;       (IN ROOMS)
;;       (LDESC "This is a nondescript part of a coal mine.")
;;       (DESC "Coal Mine")
;;       (NORTH TO MINE-2)
;;       (SOUTH TO MINE-1)
;;       (SE TO MINE-3)
;;       (FLAGS RLANDBIT)>

(def mine-2
  {:id :mine-2
   :desc "Coal Mine"
   :ldesc "This is a non-descript part of a coal mine."
   :flags #{}  ; Underground, not lit
   :exits {:north :mine-2  ; Loop back to itself
           :south :mine-1
           :se :mine-3}})

;; <ROOM MINE-3
;;       (IN ROOMS)
;;       (LDESC "This is a nondescript part of a coal mine.")
;;       (DESC "Coal Mine")
;;       (SOUTH TO MINE-3)
;;       (SW TO MINE-4)
;;       (EAST TO MINE-2)
;;       (FLAGS RLANDBIT)>

(def mine-3
  {:id :mine-3
   :desc "Coal Mine"
   :ldesc "This is a non-descript part of a coal mine."
   :flags #{}  ; Underground, not lit
   :exits {:south :mine-3  ; Loop back to itself
           :sw :mine-4
           :east :mine-2}})

;; <ROOM MINE-4
;;       (IN ROOMS)
;;       (LDESC "This is a nondescript part of a coal mine.")
;;       (DESC "Coal Mine")
;;       (NORTH TO MINE-3)
;;       (WEST TO MINE-4)
;;       (DOWN TO LADDER-TOP)
;;       (FLAGS RLANDBIT)>

(def mine-4
  {:id :mine-4
   :desc "Coal Mine"
   :ldesc "This is a non-descript part of a coal mine."
   :flags #{}  ; Underground, not lit
   :exits {:north :mine-3
           :west :mine-4  ; Loop back to itself
           :down :ladder-top}})

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
   stone-barrow
   ;; Canyon / Falls / Rainbow area
   mountains
   canyon-view
   cliff-middle
   canyon-bottom
   end-of-rainbow
   on-rainbow
   aragain-falls
   shore
   sandy-beach
   sandy-cave
   white-cliffs-north
   white-cliffs-south
   ;; Frigid River (requires boat)
   river-1
   river-2
   river-3
   river-4
   river-5
   ;; Mirror room complex
   mirror-room-1
   mirror-room-2
   small-cave
   tiny-cave
   cold-passage
   winding-passage
   twisting-passage
   atlantis-room
   ;; Temple / Hades area
   dome-room
   torch-room
   north-temple
   south-temple
   egypt-room
   entrance-to-hades
   land-of-living-dead
   ;; Coal Mine area
   slide-room
   mine-entrance
   squeeky-room
   bat-room
   shaft-room
   smelly-room
   gas-room
   ladder-top
   ladder-bottom
   dead-end-5
   timber-room
   lower-shaft
   machine-room
   mine-1
   mine-2
   mine-3
   mine-4])
