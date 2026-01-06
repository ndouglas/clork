# ZIL Implementation Inventory

This document tracks the implementation progress of Zork I features from the original ZIL source code.

**Legend:**
- [ ] Not started
- [x] Implemented
- [~] Partially implemented
- [N/A] Not applicable (ZIL-specific construct)

**Source Files:**
- `1actions.zil` - Game-specific action routines
- `1dungeon.zil` - Room and object definitions
- `gclock.zil` - Clock/daemon system
- `gglobals.zil` - Global variables and objects
- `gmacros.zil` - ZIL macros
- `gmain.zil` - Main loop
- `gparser.zil` - Parser routines
- `gsyntax.zil` - Syntax definitions
- `gverbs.zil` - Verb handler routines

---

## Phase 1: Foundation

### Game Commands (Meta-Verbs)

These are out-of-game commands that don't advance time.

| Status | Verb             | Handler       | Description                   |
| ------ | ---------------- | ------------- | ----------------------------- |
| [x]    | VERBOSE          | V-VERBOSE     | Enable full room descriptions |
| [x]    | BRIEF            | V-BRIEF       | Brief room descriptions       |
| [x]    | SUPER/SUPERBRIEF | V-SUPER-BRIEF | Minimal descriptions          |
| [x]    | INVENTORY/I      | V-INVENTORY   | Show carried items            |
| [x]    | VERSION          | V-VERSION     | Show game version             |
| [x]    | DIAGNOSE         | V-DIAGNOSE    | Show player health            |
| [x]    | SCORE            | V-SCORE       | Show current score            |
| [x]    | QUIT/Q           | V-QUIT        | Quit game                     |
| [x]    | RESTART          | V-RESTART     | Restart game                  |
| [x]    | RESTORE          | V-RESTORE     | Load saved game               |
| [x]    | SAVE             | V-SAVE        | Save game                     |
| [x]    | SCRIPT           | V-SCRIPT      | Start transcript              |
| [x]    | UNSCRIPT         | V-UNSCRIPT    | Stop transcript               |
| [x]    | $VERIFY          | V-VERIFY      | Verify game file              |

### Core Movement Verbs

| Status | Verb        | Handler      | Description                        |
| ------ | ----------- | ------------ | ---------------------------------- |
| [x]    | WALK/GO/RUN | V-WALK       | Move in direction                  |
| [x]    | ENTER       | V-ENTER      | Enter location (via :in direction) |
| [x]    | EXIT        | V-EXIT       | Exit location (via :out direction) |
| [x]    | CLIMB       | V-CLIMB-FOO  | Climb object                       |
| [x]    | CLIMB UP    | V-CLIMB-UP   | Climb up                           |
| [x]    | CLIMB DOWN  | V-CLIMB-DOWN | Climb down                         |
| [x]    | BACK        | V-BACK       | Return to previous room            |

### Core Interaction Verbs

| Status | Verb             | Handler       | Description                   |
| ------ | ---------------- | ------------- | ----------------------------- |
| [x]    | LOOK/L           | V-LOOK        | Look around room              |
| [x]    | EXAMINE/DESCRIBE | V-EXAMINE     | Examine object closely        |
| [x]    | TAKE/GET/GRAB    | V-TAKE        | Pick up object                |
| [x]    | DROP             | V-DROP        | Drop object                   |
| [x]    | PUT/INSERT       | V-PUT         | Put object in container       |
| [x]    | PUT ON           | V-PUT-ON      | Put object on surface         |
| [x]    | OPEN             | V-OPEN        | Open container/door           |
| [x]    | CLOSE            | V-CLOSE       | Close container/door          |
| [x]    | READ             | V-READ        | Read text                     |
| [x]    | SEARCH           | V-SEARCH      | Search object (v-look-inside) |
| [x]    | LOOK IN          | V-LOOK-INSIDE | Look inside container         |
| [x]    | LOOK UNDER       | V-LOOK-UNDER  | Look under object             |
| [x]    | LOOK BEHIND      | V-LOOK-BEHIND | Look behind object            |
| [x]    | LOOK ON          | V-LOOK-ON     | Look on surface               |

---

## Phase 2: Extended Verbs

### Light and Lamp

| Status | Verb                | Handler    | Description           |
| ------ | ------------------- | ---------- | --------------------- |
| [x]    | LIGHT/TURN ON       | V-LAMP-ON  | Turn on light source  |
| [x]    | EXTINGUISH/TURN OFF | V-LAMP-OFF | Turn off light source |

### Combat

| Status | Verb             | Handler  | Description                   |
| ------ | ---------------- | -------- | ----------------------------- |
| [x]    | ATTACK/FIGHT/HIT | V-ATTACK | Attack creature               |
| [x]    | KILL/MURDER      | V-ATTACK | Kill creature (synonym)       |
| [x]    | STAB             | V-STAB   | Stab without weapon specified |
| [x]    | SWING            | V-SWING  | Swing weapon                  |
| [x]    | THROW AT         | V-THROW  | Throw at target               |

### Object Manipulation

| Status | Verb       | Handler | Description        |
| ------ | ---------- | ------- | ------------------ |
| [x]    | MOVE       | V-MOVE  | Move object        |
| [x]    | PUSH       | V-PUSH  | Push object        |
| [x]    | PULL/TUG   | V-MOVE  | Pull object        |
| [x]    | RAISE/LIFT | V-RAISE | Raise object       |
| [x]    | LOWER      | V-LOWER | Lower object       |
| [x]    | TURN       | V-TURN  | Turn/rotate object |
| [x]    | SHAKE      | V-SHAKE | Shake container    |
| [x]    | WAVE       | V-WAVE  | Wave object        |
| [x]    | RUB/TOUCH  | V-RUB   | Rub/touch object   |
| [x]    | TIE        | V-TIE   | Tie object         |
| [x]    | UNTIE      | V-UNTIE | Untie object       |

### Tools and Items

| Status | Verb        | Handler   | Description     |
| ------ | ----------- | --------- | --------------- |
| [x]    | UNLOCK      | V-UNLOCK  | Unlock with key |
| [x]    | LOCK        | V-LOCK    | Lock with key   |
| [x]    | DIG         | V-DIG     | Dig with shovel |
| [x]    | CUT/SLICE   | V-CUT     | Cut with blade  |
| [x]    | BURN/IGNITE | V-BURN    | Burn with fire  |
| [x]    | FILL        | V-FILL    | Fill container  |
| [x]    | POUR        | V-DROP    | Pour liquid     |
| [x]    | POUR ON     | V-POUR-ON | Pour on object  |
| [x]    | INFLATE     | V-INFLATE | Inflate object  |
| [x]    | DEFLATE     | V-DEFLATE | Deflate object  |
| [x]    | PLUG/PATCH  | V-PLUG    | Repair object   |
| [x]    | PUMP        | V-PUMP    | Pump air        |

### Food and Drink

| Status | Verb        | Handler | Description  |
| ------ | ----------- | ------- | ------------ |
| [x]    | EAT/CONSUME | V-EAT   | Eat food     |
| [x]    | DRINK       | V-DRINK | Drink liquid |

### Communication

| Status | Verb         | Handler  | Description     |
| ------ | ------------ | -------- | --------------- |
| [x]    | SAY          | V-SAY    | Say something   |
| [x]    | TELL/ASK     | V-TELL   | Talk to NPC     |
| [x]    | ANSWER/REPLY | V-ANSWER | Answer question |
| [x]    | HELLO/HI     | V-HELLO  | Greet           |
| [x]    | YELL/SCREAM  | V-YELL   | Yell loudly     |
| [x]    | GIVE/OFFER   | V-GIVE   | Give to NPC     |

### Vehicle/Boat

| Status | Verb      | Handler     | Description   |
| ------ | --------- | ----------- | ------------- |
| [x]    | BOARD     | V-BOARD     | Board vehicle |
| [x]    | DISEMBARK | V-DISEMBARK | Leave vehicle |
| [x]    | LAUNCH    | V-LAUNCH    | Launch boat   |

### Miscellaneous Verbs

| Status | Verb        | Handler   | Description        |
| ------ | ----------- | --------- | ------------------ |
| [x]    | WAIT/Z      | V-WAIT    | Wait/pass time     |
| [x]    | JUMP/LEAP   | V-LEAP    | Jump               |
| [x]    | SWIM        | V-SWIM    | Swim               |
| [x]    | LISTEN      | V-LISTEN  | Listen to sounds   |
| [x]    | SMELL/SNIFF | V-SMELL   | Smell object       |
| [x]    | KNOCK       | V-KNOCK   | Knock on door      |
| [x]    | KICK        | V-KICK    | Kick object        |
| [x]    | KISS        | V-KISS    | Kiss creature      |
| [x]    | PRAY        | V-PRAY    | Pray               |
| [x]    | FIND/WHERE  | V-FIND    | Find object        |
| [x]    | COUNT       | V-COUNT   | Count objects      |
| [x]    | WEAR        | V-WEAR    | Wear clothing      |
| [x]    | RING        | V-RING    | Ring bell          |
| [x]    | ECHO        | V-ECHO    | Say "echo"         |
| [x]    | CROSS       | V-CROSS   | Cross bridge/chasm |
| [x]    | THROUGH     | V-THROUGH | Go through         |
| [x]    | STAND       | V-STAND   | Stand up           |

### Special/Easter Egg Verbs

| Status | Verb             | Handler    | Description         |
| ------ | ---------------- | ---------- | ------------------- |
| [x]    | ODYSSEUS/ULYSSES | V-ODYSSEUS | Cyclops puzzle      |
| [x]    | PLUGH/XYZZY      | V-ADVENT   | Adventure reference |
| [x]    | FROBOZZ          | V-FROBOZZ  | Frobozz reference   |
| [x]    | ZORK             | V-ZORK     | Meta-reference      |
| [x]    | CURSE/DAMN       | V-CURSES   | Swear words         |
| [x]    | WIN              | V-WIN      | Try to win          |
| [x]    | TREASURE/TEMPLE  | V-TREASURE | Hint command        |

### Additional Verbs (newly implemented)

| Status | Verb            | Handler       | Description                 |
| ------ | --------------- | ------------- | --------------------------- |
| [x]    | WAKE/ALARM      | V-ALARM       | Wake sleeping creature      |
| [x]    | COMMAND/ORDER   | V-COMMAND     | Command NPC                 |
| [x]    | FOLLOW          | V-FOLLOW      | Follow someone              |
| [x]    | SEND FOR        | V-SEND        | Send for someone            |
| [x]    | BLAST/BLOW UP   | V-BLAST       | Blow up something           |
| [x]    | DESTROY/MUNG    | V-MUNG        | Destroy object              |
| [x]    | STRIKE          | V-STRIKE      | Strike (attack or light)    |
| [x]    | THROW OFF/OVER  | V-THROW-OFF   | Throw over edge             |
| [x]    | TIE UP          | V-TIE-UP      | Tie up creature             |
| [x]    | INCANT/CAST     | V-INCANT      | Cast spell (Zork II stub)   |
| [x]    | ENCHANT         | V-ENCHANT     | Enchant (Zork II stub)      |
| [x]    | DISENCHANT      | V-DISENCHANT  | Disenchant (Zork II stub)   |
| [x]    | EXORCISE/BANISH | V-EXORCISE    | Exorcise spirit             |
| [x]    | BRUSH           | V-BRUSH       | Brush object                |
| [x]    | SQUEEZE         | V-SQUEEZE     | Squeeze object              |
| [x]    | SPIN/ROTATE     | V-SPIN        | Spin object                 |
| [x]    | WIND            | V-WIND        | Wind up object              |
| [x]    | PICK            | V-PICK        | Pick lock                   |
| [x]    | OIL/LUBRICATE   | V-OIL         | Lubricate object            |
| [x]    | MELT            | V-MELT        | Melt object                 |
| [x]    | SPRAY           | V-SPRAY       | Spray something             |
| [x]    | PUSH TO         | V-PUSH-TO     | Push object to location     |
| [x]    | PUT UNDER       | V-PUT-UNDER   | Put under something         |
| [x]    | PUT BEHIND      | V-PUT-BEHIND  | Put behind something        |
| [x]    | SEARCH          | V-SEARCH      | Search for hidden things    |
| [x]    | BREATHE/BLOW IN | V-BREATHE     | Blow into object            |
| [x]    | CHOMP/BITE      | V-CHOMP       | Bite something              |
| [x]    | LEAN ON         | V-LEAN-ON     | Lean on object              |
| [x]    | MAKE/BUILD      | V-MAKE        | Make something              |
| [x]    | PLAY            | V-PLAY        | Play with object            |
| [x]    | STAY            | V-STAY        | Stay put                    |
| [x]    | WISH            | V-WISH        | Make a wish                 |
| [x]    | HATCH           | V-HATCH       | Hatch something             |
| [x]    | MUMBLE/MUTTER   | V-MUMBLE      | Mumble                      |
| [x]    | REPENT          | V-REPENT      | Repent sins                 |
| [x]    | BUG             | V-BUG         | Report bug (easter egg)     |
| [x]    | REPLY           | V-REPLY       | Reply to NPC                |
| [x]    | OVERBOARD       | V-OVERBOARD   | Throw overboard from boat   |

---

## Phase 3: Rooms

### Outdoors - House Area

| Status | Room ID            | Description                       |
| ------ | ------------------ | --------------------------------- |
| [x]    | WEST-OF-HOUSE      | West of House (starting location) |
| [x]    | NORTH-OF-HOUSE     | North of House                    |
| [x]    | SOUTH-OF-HOUSE     | South of House                    |
| [x]    | EAST-OF-HOUSE      | Behind House                      |
| [x]    | CLEARING           | Forest Clearing                   |
| [x]    | FOREST-1           | Forest                            |
| [x]    | FOREST-2           | Forest                            |
| [x]    | FOREST-3           | Forest                            |
| [x]    | PATH               | Forest Path                       |
| [x]    | UP-A-TREE          | Up a Tree                         |
| [x]    | GRATING-CLEARING   | Clearing with Grating             |
| [x]    | MOUNTAINS          | Mountains                         |
| [x]    | END-OF-RAINBOW     | End of Rainbow                    |
| [x]    | ON-RAINBOW         | On the Rainbow                    |
| [x]    | ARAGAIN-FALLS      | Aragain Falls                     |
| [x]    | WHITE-CLIFFS-NORTH | White Cliffs Beach (North)        |
| [x]    | WHITE-CLIFFS-SOUTH | White Cliffs Beach (South)        |
| [x]    | SHORE              | Shore                             |
| [x]    | SANDY-BEACH        | Sandy Beach                       |

### Inside House

| Status | Room ID     | Description |
| ------ | ----------- | ----------- |
| [x]    | KITCHEN     | Kitchen     |
| [x]    | LIVING-ROOM | Living Room |
| [x]    | ATTIC       | Attic       |

### Underground - Near House

| Status | Room ID       | Description         |
| ------ | ------------- | ------------------- |
| [x]    | CELLAR        | Cellar              |
| [x]    | TROLL-ROOM    | Troll Room          |
| [x]    | EW-PASSAGE    | East-West Passage   |
| [x]    | ROUND-ROOM    | Round Room          |
| [x]    | NS-PASSAGE    | North-South Passage |
| [x]    | CHASM-ROOM    | Chasm               |
| [x]    | EAST-OF-CHASM | East of Chasm       |
| [x]    | GALLERY       | Gallery             |
| [x]    | STUDIO        | Studio              |

### Underground - Maze

| Status | Room ID      | Description |
| ------ | ------------ | ----------- |
| [x]    | MAZE-1       | Maze        |
| [x]    | MAZE-2       | Maze        |
| [x]    | MAZE-3       | Maze        |
| [x]    | MAZE-4       | Maze        |
| [x]    | MAZE-5       | Maze        |
| [x]    | MAZE-6       | Maze        |
| [x]    | MAZE-7       | Maze        |
| [x]    | MAZE-8       | Maze        |
| [x]    | MAZE-9       | Maze        |
| [x]    | MAZE-10      | Maze        |
| [x]    | MAZE-11      | Maze        |
| [x]    | MAZE-12      | Maze        |
| [x]    | MAZE-13      | Maze        |
| [x]    | MAZE-14      | Maze        |
| [x]    | MAZE-15      | Maze        |
| [x]    | DEAD-END-1   | Dead End    |
| [x]    | DEAD-END-2   | Dead End    |
| [x]    | DEAD-END-3   | Dead End    |
| [x]    | DEAD-END-4   | Dead End    |
| [x]    | DEAD-END-5   | Dead End    |
| [x]    | GRATING-ROOM | Grate Room  |

### Underground - Dam Area

| Status | Room ID          | Description      |
| ------ | ---------------- | ---------------- |
| [x]    | DAM-ROOM         | Dam              |
| [x]    | DAM-BASE         | Dam Base         |
| [x]    | DAM-LOBBY        | Dam Lobby        |
| [x]    | MAINTENANCE-ROOM | Maintenance Room |
| [x]    | RESERVOIR        | Reservoir        |
| [x]    | RESERVOIR-NORTH  | Reservoir North  |
| [x]    | RESERVOIR-SOUTH  | Reservoir South  |
| [x]    | STREAM-VIEW      | Stream View      |
| [x]    | IN-STREAM        | In Stream        |

### Underground - River

| Status | Room ID | Description  |
| ------ | ------- | ------------ |
| [x]    | RIVER-1 | Frigid River |
| [x]    | RIVER-2 | Frigid River |
| [x]    | RIVER-3 | Frigid River |
| [x]    | RIVER-4 | Frigid River |
| [x]    | RIVER-5 | Frigid River |

### Underground - Caves and Passages

| Status | Room ID          | Description      |
| ------ | ---------------- | ---------------- |
| [x]    | LOUD-ROOM        | Loud Room        |
| [x]    | DAMP-CAVE        | Damp Cave        |
| [x]    | SMALL-CAVE       | Small Cave       |
| [x]    | TINY-CAVE        | Tiny Cave        |
| [x]    | COLD-PASSAGE     | Cold Passage     |
| [x]    | NARROW-PASSAGE   | Narrow Passage   |
| [x]    | TWISTING-PASSAGE | Twisting Passage |
| [x]    | WINDING-PASSAGE  | Winding Passage  |
| [x]    | STRANGE-PASSAGE  | Strange Passage  |
| [x]    | DEEP-CANYON      | Deep Canyon      |
| [x]    | CANYON-VIEW      | Canyon View      |
| [x]    | CLIFF-MIDDLE     | Rocky Ledge      |
| [x]    | CANYON-BOTTOM    | Canyon Bottom    |
| [x]    | ENGRAVINGS-CAVE  | Engravings Cave  |
| [x]    | DOME-ROOM        | Dome Room        |
| [x]    | TORCH-ROOM       | Torch Room       |

### Underground - Mine

| Status | Room ID       | Description   |
| ------ | ------------- | ------------- |
| [x]    | MINE-ENTRANCE | Mine Entrance |
| [x]    | MINE-1        | Coal Mine     |
| [x]    | MINE-2        | Coal Mine     |
| [x]    | MINE-3        | Coal Mine     |
| [x]    | MINE-4        | Coal Mine     |
| [x]    | BAT-ROOM      | Bat Room      |
| [x]    | SHAFT-ROOM    | Shaft Room    |
| [x]    | LOWER-SHAFT   | Drafty Room   |
| [x]    | TIMBER-ROOM   | Timber Room   |
| [x]    | SLIDE-ROOM    | Slide Room    |
| [x]    | LADDER-TOP    | Ladder Top    |
| [x]    | LADDER-BOTTOM | Ladder Bottom |
| [x]    | GAS-ROOM      | Gas Room      |
| [x]    | DEAD-END-5    | Dead End      |

### Underground - Temple and Hades

| Status | Room ID             | Description             |
| ------ | ------------------- | ----------------------- |
| [x]    | NORTH-TEMPLE        | Temple                  |
| [x]    | SOUTH-TEMPLE        | Altar                   |
| [x]    | EGYPT-ROOM          | Egyptian Room           |
| [x]    | ENTRANCE-TO-HADES   | Entrance to Hades       |
| [x]    | LAND-OF-LIVING-DEAD | Land of the Living Dead |

### Underground - Thief's Lair

| Status | Room ID       | Description   |
| ------ | ------------- | ------------- |
| [x]    | CYCLOPS-ROOM  | Cyclops Room  |
| [x]    | TREASURE-ROOM | Treasure Room |
| [x]    | ATLANTIS-ROOM | Atlantis Room |

### Underground - Other

| Status | Room ID       | Description            |
| ------ | ------------- | ---------------------- |
| [x]    | MIRROR-ROOM-1 | Mirror Room            |
| [x]    | MIRROR-ROOM-2 | Mirror Room            |
| [x]    | MACHINE-ROOM  | Machine Room           |
| [x]    | SANDY-CAVE    | Sandy Cave             |
| [x]    | STONE-BARROW  | Stone Barrow (endgame) |
| [x]    | SMELLY-ROOM   | Smelly Room            |
| [x]    | SQUEEKY-ROOM  | Squeaky Room           |

---

## Phase 4: Objects

### Treasures (Scoring Items)

| Status | Object ID    | Description         | Points |
| ------ | ------------ | ------------------- | ------ |
| [x]    | BAR          | Platinum Bar        | 10     |
| [x]    | BAUBLE       | Brass Bauble        | 1      |
| [x]    | BRACELET     | Sapphire Bracelet   | 5      |
| [x]    | CHALICE      | Silver Chalice      | 10     |
| [x]    | COFFIN       | Gold Coffin         | 10     |
| [x]    | DIAMOND      | Huge Diamond        | 10     |
| [~]    | EGG          | Jewel-Encrusted Egg | 5      |
| [x]    | EMERALD      | Large Emerald       | 5      |
| [x]    | JADE         | Jade Figurine       | 5      |
| [~]    | PAINTING     | Beautiful Painting  | 4      |
| [x]    | POT-OF-GOLD  | Pot of Gold         | 10     |
| [x]    | SCARAB       | Jeweled Scarab      | 5      |
| [x]    | SCEPTRE      | Royal Sceptre       | 4      |
| [x]    | TORCH        | Ivory Torch         | 6      |
| [x]    | TRIDENT      | Crystal Trident     | 4      |
| [x]    | TRUNK        | Trunk of Jewels     | 15     |
| [x]    | CANARY       | Clockwork Canary    | 6      |
| [x]    | BAG-OF-COINS | Bag of Coins        | 5      |

### Light Sources

| Status | Object ID | Description     |
| ------ | --------- | --------------- |
| [x]    | LAMP      | Brass Lantern   |
| [x]    | CANDLES   | Pair of Candles |
| [x]    | TORCH     | Ivory Torch     |
| [x]    | MATCH     | Match           |

### Weapons

| Status | Object ID   | Description  |
| ------ | ----------- | ------------ |
| [x]    | SWORD       | Elvish Sword |
| [~]    | KNIFE       | Nasty Knife  |
| [x]    | RUSTY-KNIFE | Rusty Knife  |
| [x]    | AXE         | Bloody Axe   |
| [x]    | STILETTO    | Stiletto     |

### Tools

| Status | Object ID   | Description  |
| ------ | ----------- | ------------ |
| [ ]    | SHOVEL      | Shovel       |
| [x]    | KEYS        | Skeleton Key |
| [ ]    | SCREWDRIVER | Screwdriver  |
| [x]    | WRENCH      | Wrench       |
| [~]    | ROPE        | Rope         |
| [x]    | PUTTY       | Gunk/Putty   |

### Containers

| Status | Object ID    | Description   |
| ------ | ------------ | ------------- |
| [~]    | MAILBOX      | Small Mailbox |
| [x]    | TROPHY-CASE  | Trophy Case   |
| [ ]    | TOOL-CHEST   | Tool Chest    |
| [x]    | LARGE-BAG    | Large Bag     |
| [~]    | SANDWICH-BAG | Brown Sack    |
| [~]    | BOTTLE       | Glass Bottle  |
| [x]    | BUOY         | Buoy          |

### Boat-Related

| Status | Object ID       | Description     |
| ------ | --------------- | --------------- |
| [ ]    | INFLATABLE-BOAT | Pile of Plastic |
| [ ]    | INFLATED-BOAT   | Inflated Boat   |
| [ ]    | PUNCTURED-BOAT  | Punctured Boat  |
| [x]    | PUMP            | Hand Pump       |

### Readable Items

| Status | Object ID     | Description    |
| ------ | ------------- | -------------- |
| [x]    | ADVERTISEMENT | Leaflet        |
| [ ]    | BOOK          | Black Book     |
| [x]    | GUIDE         | Tour Guide     |
| [ ]    | MAP           | Ancient Map    |
| [ ]    | OWNERS-MANUAL | Owner's Manual |
| [ ]    | BOAT-LABEL    | Label          |
| [ ]    | ENGRAVINGS    | Engravings     |
| [ ]    | PRAYER        | Prayer         |

### Food

| Status | Object ID | Description       |
| ------ | --------- | ----------------- |
| [x]    | LUNCH     | Lunch             |
| [x]    | GARLIC    | Clove of Garlic   |
| [x]    | WATER     | Quantity of Water |

### Scenery/Fixed Objects

| Status | Object ID      | Description    |
| ------ | -------------- | -------------- |
| [ ]    | WHITE-HOUSE    | White House    |
| [ ]    | FOREST         | Forest         |
| [ ]    | TREE           | Tree           |
| [ ]    | BOARD          | Wooden Board   |
| [ ]    | FRONT-DOOR     | Front Door     |
| [~]    | KITCHEN-WINDOW | Kitchen Window |
| [~]    | TRAP-DOOR      | Trap Door      |
| [x]    | GRATE          | Grating        |
| [ ]    | CHIMNEY        | Chimney        |
| [x]    | LEAVES         | Pile of Leaves |
| [x]    | DAM            | Dam            |
| [ ]    | RAINBOW        | Rainbow        |
| [ ]    | MIRROR-1       | Mirror         |
| [ ]    | MIRROR-2       | Mirror         |
| [ ]    | MACHINE        | Machine        |
| [ ]    | SLIDE          | Slide          |
| [ ]    | CRACK          | Crack          |
| [ ]    | LEAK           | Leak           |
| [ ]    | WOODEN-DOOR    | Wooden Door    |
| [ ]    | BARROW-DOOR    | Barrow Door    |
| [~]    | RUG            | Rug            |
| [ ]    | ALTAR          | Altar          |
| [ ]    | PEDESTAL       | Pedestal       |
| [ ]    | CONTROL-PANEL  | Control Panel  |

### Basket System

| Status | Object ID      | Description      |
| ------ | -------------- | ---------------- |
| [ ]    | RAISED-BASKET  | Basket (raised)  |
| [ ]    | LOWERED-BASKET | Basket (lowered) |
| [~]    | NEST           | Bird's Nest      |

### Special Items

| Status | Object ID          | Description        |
| ------ | ------------------ | ------------------ |
| [ ]    | BELL               | Brass Bell         |
| [ ]    | HOT-BELL           | Red-hot Bell       |
| [ ]    | SKULL              | Skull              |
| [x]    | BONES              | Skeleton           |
| [ ]    | BODIES             | Bodies             |
| [ ]    | COAL               | Lump of Coal       |
| [ ]    | SAND               | Pile of Sand       |
| [ ]    | BUBBLE             | Bubble             |
| [ ]    | BOLT               | Bolt               |
| [ ]    | TUBE               | Tube               |
| [ ]    | LADDER             | Ladder             |
| [ ]    | TIMBERS            | Timbers            |
| [ ]    | BROKEN-EGG         | Broken Egg         |
| [ ]    | BROKEN-CANARY      | Broken Canary      |
| [ ]    | BROKEN-LAMP        | Broken Lamp        |
| [x]    | BURNED-OUT-LANTERN | Burned-out Lantern |

### NPCs/Actors

| Status | Object ID  | Description      |
| ------ | ---------- | ---------------- |
| [~]    | ADVENTURER | Player character |
| [x]    | TROLL      | Troll            |
| [x]    | THIEF      | Thief            |
| [x]    | CYCLOPS    | Cyclops          |
| [ ]    | BAT        | Vampire Bat      |
| [ ]    | SONGBIRD   | Songbird         |
| [ ]    | GHOSTS     | Spirits          |

### Global/Pseudo Objects

| Status | Object ID    | Description         |
| ------ | ------------ | ------------------- |
| [ ]    | GLOBAL-WATER | Global Water        |
| [ ]    | GROUND       | Ground              |
| [ ]    | STAIRS       | Stairs              |
| [x]    | GRUE         | Grue                |
| [ ]    | ME           | Self-reference      |
| [ ]    | IT           | Pronoun reference   |
| [ ]    | HANDS        | Bare Hands          |
| [ ]    | LUNGS        | Lungs/Air           |
| [ ]    | ZORKMID      | Zorkmid currency    |
| [ ]    | SAILOR       | Sailor (Easter egg) |

---

## Phase 5: Action Routines

### Room Actions

| Status | Routine           | Room          | Description               |
| ------ | ----------------- | ------------- | ------------------------- |
| [~]    | WEST-HOUSE        | WEST-OF-HOUSE | West of house description |
| [ ]    | EAST-HOUSE        | EAST-OF-HOUSE | Behind house logic        |
| [ ]    | FOREST-ROOM       | FOREST-*      | Forest room behavior      |
| [ ]    | FOREST-F          | FOREST        | Forest object             |
| [ ]    | TREE-ROOM         | UP-A-TREE     | Tree climbing             |
| [x]    | CLEARING-FCN      | CLEARING      | Clearing logic            |
| [ ]    | KITCHEN-FCN       | KITCHEN       | Kitchen logic             |
| [ ]    | LIVING-ROOM-FCN   | LIVING-ROOM   | Living room logic         |
| [ ]    | CELLAR-FCN        | CELLAR        | Cellar behavior           |
| [x]    | CYCLOPS-ROOM-FCN  | CYCLOPS-ROOM  | Cyclops room              |
| [ ]    | TROLL-ROOM-F      | TROLL-ROOM    | Troll room logic          |
| [x]    | LOUD-ROOM-FCN     | LOUD-ROOM     | Echo puzzle               |
| [x]    | DAM-ROOM-FCN      | DAM-ROOM      | Dam controls              |
| [x]    | RESERVOIR-FCN     | RESERVOIR     | Reservoir water           |
| [x]    | TREASURE-ROOM-FCN | TREASURE-ROOM | Treasure room             |
| [x]    | TORCH-ROOM-FCN    | TORCH-ROOM    | Torch room                |
| [x]    | MIRROR-ROOM       | MIRROR-ROOM-* | Mirror rooms              |
| [x]    | MACHINE-ROOM-FCN  | MACHINE-ROOM  | Coal machine              |
| [x]    | DOME-ROOM-FCN     | DOME-ROOM     | Dome room                 |
| [x]    | SOUTH-TEMPLE-FCN  | SOUTH-TEMPLE  | Temple altar              |
| [ ]    | BARROW-FCN        | STONE-BARROW  | Endgame barrow            |
| [x]    | BATS-ROOM         | BAT-ROOM      | Bat room                  |
| [x]    | FALLS-ROOM        | ARAGAIN-FALLS | Falls viewing             |
| [x]    | BOOM-ROOM         | GAS-ROOM      | Gas explosion             |
| [x]    | NO-OBJS           | TIMBER/SHAFT  | Empty-handed check        |
| [ ]    | CAVE2-ROOM        | Caves         | Various caves             |

### NPC Actions

| Status | Routine             | NPC      | Description           |
| ------ | ------------------- | -------- | --------------------- |
| [x]    | TROLL-FCN           | TROLL    | Troll combat/behavior |
| [x]    | THIEF-IN-TREASURE   | THIEF    | Thief in lair         |
| [x]    | THIEF-VS-ADVENTURER | THIEF    | Thief combat          |
| [x]    | ROBBER-FUNCTION     | THIEF    | Thief stealing        |
| [x]    | CYCLOPS-FCN         | CYCLOPS  | Cyclops behavior      |
| [ ]    | BAT-F               | BAT      | Bat behavior          |
| [ ]    | BAT-D               | BAT      | Bat daemon            |
| [ ]    | GHOSTS-F            | GHOSTS   | Spirits behavior      |
| [ ]    | SONGBIRD-F          | SONGBIRD | Bird singing          |

### Object Actions

| Status | Routine          | Object      | Description         |
| ------ | ---------------- | ----------- | ------------------- |
| [ ]    | MAILBOX-F        | MAILBOX     | Mailbox interaction |
| [ ]    | TRAP-DOOR-FCN    | TRAP-DOOR   | Trap door logic     |
| [ ]    | GRATE-FUNCTION   | GRATE       | Grating logic       |
| [x]    | TROPHY-CASE-FCN  | TROPHY-CASE | Score deposit       |
| [ ]    | BOTTLE-FUNCTION  | BOTTLE      | Bottle liquid       |
| [ ]    | ROPE-FUNCTION    | ROPE        | Rope tying          |
| [x]    | SWORD-FCN        | SWORD       | Glowing sword       |
| [x]    | LANTERN          | LAMP        | Lantern usage       |
| [x]    | CANDLES-FCN      | CANDLES     | Candle burning      |
| [x]    | MATCH-FUNCTION   | MATCH       | Match lighting      |
| [ ]    | BELL-F           | BELL        | Bell ringing        |
| [ ]    | HOT-BELL-F       | HOT-BELL    | Hot bell handling   |
| [ ]    | EGG-OBJECT       | EGG         | Egg opening         |
| [x]    | CANARY-OBJECT    | CANARY      | Clockwork canary    |
| [x]    | DAM-FUNCTION     | DAM         | Dam controls        |
| [ ]    | MACHINE-F        | MACHINE     | Coal machine        |
| [ ]    | RUG-FCN          | RUG         | Hidden trap door    |
| [ ]    | BASKET-F         | BASKET      | Shaft basket        |
| [x]    | SCEPTRE-FUNCTION | SCEPTRE     | Rainbow waving      |
| [ ]    | PAINTING-FCN     | PAINTING    | Painting handling   |
| [ ]    | RAINBOW-FCN      | RAINBOW     | Rainbow crossing    |
| [ ]    | SLIDE-FUNCTION   | SLIDE       | Cellar slide        |
| [x]    | LEAK-FUNCTION    | LEAK        | Dam leak            |
| [ ]    | LEAF-PILE        | LEAVES      | Leaf pile           |
| [ ]    | SAND-FUNCTION    | SAND        | Digging sand        |
| [x]    | TUBE-FUNCTION    | TUBE        | Tube usage          |
| [x]    | TORCH-OBJECT     | TORCH       | Torch handling      |

### Combat System

| Status | Routine          | Description          |
| ------ | ---------------- | -------------------- |
| [x]    | DO-FIGHT         | Main combat loop     |
| [x]    | I-FIGHT          | Fight interrupt      |
| [x]    | HERO-BLOW        | Player attacks       |
| [x]    | VILLAIN-BLOW     | Enemy attacks        |
| [x]    | VILLAIN-RESULT   | Combat result        |
| [x]    | WINNER-RESULT    | Combat outcome       |
| [x]    | FIGHT-STRENGTH   | Strength calculation |
| [x]    | VILLAIN-STRENGTH | Enemy strength       |
| [x]    | FIND-WEAPON      | Weapon selection     |
| [ ]    | WEAPON-FUNCTION  | Weapon handling      |
| [x]    | JIGS-UP          | Player death         |

### Daemon/Interrupt System

| Status | Routine       | Description          |
| ------ | ------------- | -------------------- |
| [x]    | I-LANTERN     | Lantern timer        |
| [x]    | I-CANDLES     | Candle timer         |
| [x]    | I-MATCH       | Match burning        |
| [x]    | I-THIEF       | Thief wandering      |
| [x]    | I-SWORD       | Sword glowing        |
| [x]    | I-CYCLOPS     | Cyclops behavior     |
| [ ]    | I-CURE        | Healing timer        |
| [ ]    | I-FOREST-ROOM | Forest wandering     |
| [ ]    | I-RIVER       | River flow           |
| [x]    | I-RFILL       | Reservoir filling    |
| [x]    | I-REMPTY      | Reservoir emptying   |
| [x]    | I-MAINT-ROOM  | Maintenance flooding |

### Utility Routines

| Status | Routine          | Description               |
| ------ | ---------------- | ------------------------- |
| [ ]    | DESCRIBE-ROOM    | Room description          |
| [ ]    | DESCRIBE-OBJECT  | Object description        |
| [ ]    | DESCRIBE-OBJECTS | List objects              |
| [ ]    | PRINT-CONT       | Print contents            |
| [ ]    | PRINT-CONTENTS   | Print container contents  |
| [ ]    | GOTO             | Room movement             |
| [ ]    | DO-WALK          | Walking logic             |
| [ ]    | NO-GO-TELL       | Blocked movement          |
| [ ]    | ITAKE            | Internal take             |
| [ ]    | IDROP            | Internal drop             |
| [x]    | SCORE-OBJ        | Score object              |
| [x]    | SCORE-UPD        | Update score              |
| [ ]    | WEIGHT           | Calculate weight          |
| [ ]    | HELD?            | Check if held             |
| [ ]    | SEE-INSIDE?      | Can see inside            |
| [ ]    | LIT?             | Check if lit              |
| [ ]    | GLOBAL-IN?       | Global object check       |
| [ ]    | THIS-IS-IT       | Set pronoun               |
| [ ]    | REMOVE-CAREFULLY | Remove without triggering |
| [ ]    | REMARK           | Random message            |
| [x]    | FINISH           | Game end                  |
| [ ]    | YES?             | Y/N prompt                |

---

## Phase 6: Parser

### Core Parser

| Status | Routine       | Description       |
| ------ | ------------- | ----------------- |
| [~]    | PARSER        | Main parser entry |
| [ ]    | CLAUSE        | Parse clause      |
| [ ]    | CLAUSE-ADD    | Add to clause     |
| [ ]    | CLAUSE-COPY   | Copy clause       |
| [ ]    | ACLAUSE-WIN   | Actor clause      |
| [ ]    | NCLAUSE-WIN   | Noun clause       |
| [ ]    | SYNTAX-CHECK  | Check syntax      |
| [ ]    | SYNTAX-FOUND  | Syntax matched    |
| [ ]    | SNARF-OBJECTS | Collect objects   |
| [ ]    | SNARFEM       | Object collection |
| [ ]    | GET-OBJECT    | Resolve object    |
| [ ]    | OBJ-FOUND     | Object found      |
| [ ]    | SEARCH-LIST   | Search for object |

### Object Resolution

| Status | Routine      | Description         |
| ------ | ------------ | ------------------- |
| [ ]    | ACCESSIBLE?  | Can reach object    |
| [ ]    | GLOBAL-CHECK | Global object       |
| [x]    | GWIM         | "Get What I Mean"   |
| [ ]    | TAKE-CHECK   | Can take object     |
| [ ]    | ITAKE-CHECK  | Internal take check |
| [ ]    | META-LOC     | Object location     |
| [x]    | THIS-IT?     | Pronoun resolution  |
| [ ]    | LIT?         | Light check         |

### Error Handling

| Status | Routine      | Description       |
| ------ | ------------ | ----------------- |
| [ ]    | CANT-USE     | Can't use object  |
| [ ]    | CANT-ORPHAN  | Orphan error      |
| [ ]    | UNKNOWN-WORD | Unknown word      |
| [ ]    | MANY-CHECK   | Ambiguity check   |
| [ ]    | WHICH-PRINT  | Which one?        |
| [ ]    | THING-PRINT  | Print object name |

### Orphan/Continuation

| Status | Routine      | Description      |
| ------ | ------------ | ---------------- |
| [ ]    | ORPHAN       | Create orphan    |
| [ ]    | ORPHAN-MERGE | Merge orphan     |
| [ ]    | BUT-MERGE    | Merge exceptions |

---

## Phase 7: Global State

### Core Globals

| Status | Global      | Description      |
| ------ | ----------- | ---------------- |
| [x]    | HERE        | Current room     |
| [x]    | WINNER      | Current actor    |
| [x]    | PLAYER      | Player object    |
| [x]    | SCORE       | Current score    |
| [x]    | SCORE-MAX   | Maximum score    |
| [x]    | MOVES       | Turn counter     |
| [x]    | DEATHS      | Death counter    |
| [x]    | VERBOSE     | Verbose mode     |
| [x]    | SUPER-BRIEF | Super-brief mode |
| [ ]    | LIT         | Is room lit      |
| [ ]    | DEAD        | Is player dead   |
| [ ]    | WON-FLAG    | Game won         |

### Parser Globals

| Status | Global      | Description         |
| ------ | ----------- | ------------------- |
| [~]    | PRSA        | Current verb/action |
| [~]    | PRSO        | Direct object       |
| [~]    | PRSI        | Indirect object     |
| [ ]    | P-IT-OBJECT | "it" reference      |
| [ ]    | P-CONT      | Continue parsing    |
| [ ]    | P-NUMBER    | Parsed number       |
| [ ]    | P-DIRECTION | Movement direction  |

### NPC State

| Status | Global          | Description       |
| ------ | --------------- | ----------------- |
| [x]    | TROLL-FLAG      | Troll state       |
| [x]    | TROLL-MELEE     | Troll in combat   |
| [x]    | THIEF-HERE      | Thief present     |
| [x]    | THIEF-ENGROSSED | Thief distracted  |
| [x]    | THIEF-MELEE     | Thief in combat   |
| [x]    | CYCLOPS-FLAG    | Cyclops state     |
| [x]    | CYCLOMAD        | Cyclops angry     |
| [x]    | CYCLOWRATH      | Cyclops attacking |
| [x]    | CYCLOPS-MELEE   | Cyclops in combat |

### Puzzle State

| Status | Global              | Description          |
| ------ | ------------------- | -------------------- |
| [ ]    | KITCHEN-WINDOW-FLAG | Window open          |
| [x]    | GRATE-REVEALED      | Grate visible        |
| [ ]    | DOME-FLAG           | Dome rope            |
| [x]    | GATE-FLAG           | Gate state           |
| [x]    | GATES-OPEN          | Gates open           |
| [ ]    | RAINBOW-FLAG        | Rainbow solid        |
| [x]    | LOW-TIDE            | Water level          |
| [x]    | WATER-LEVEL         | Reservoir level      |
| [x]    | LOUD-FLAG           | Loud room state      |
| [ ]    | RUG-MOVED           | Rug moved            |
| [ ]    | EGG-SOLVE           | Egg opened           |
| [ ]    | COFFIN-CURE         | Coffin taken         |
| [x]    | MAGIC-FLAG          | In thief's lair      |
| [ ]    | BUOY-FLAG           | Buoy opened          |
| [ ]    | LLD-FLAG            | Land of dead visited |
| [ ]    | MIRROR-MUNG         | Mirror broken        |
| [ ]    | SING-SONG           | Songbird sang        |

### Item State

| Status | Global       | Description        |
| ------ | ------------ | ------------------ |
| [ ]    | MATCH-COUNT  | Matches remaining  |
| [ ]    | LAMP-TABLE   | Lantern fuel       |
| [ ]    | CANDLE-TABLE | Candle time        |
| [ ]    | DEFLATE      | Boat deflated      |
| [ ]    | RIVER-LAUNCH | River launch state |

---

## Implementation Phases Summary

### Phase 1: Foundation (Current)
- [x] Basic game loop
- [~] Parser (basic)
- [x] Room/object framework
- [x] Meta-verbs (version, verbose, brief, inventory)
- [x] Look command
- [x] Movement commands
- [x] Core interaction (take, drop, open, close)

### Phase 2: Core Gameplay
- [~] Light system (lamp, darkness, grue)
- [~] Container system
- [x] Scoring system
- [x] Save/restore

### Phase 3: World Building
- [~] All rooms with connections
- [~] All objects placed
- [~] Room descriptions
- [~] Object descriptions

### Phase 4: NPCs and Combat
- [x] Troll encounter
- [x] Thief AI
- [x] Cyclops puzzle
- [x] Combat system

### Phase 5: Puzzles
- [x] Dam/reservoir puzzle
- [ ] Coal machine puzzle
- [x] Loud room echo
- [ ] Hades ceremony
- [ ] Mirror rooms
- [ ] Rainbow/pot of gold
- [ ] Egg/canary
- [ ] Shaft/basket

### Phase 6: Polish
- [ ] All verb handlers
- [ ] All daemon/interrupts
- [ ] Endgame (barrow)
- [ ] Edge cases and error messages

---

## Notes

### ZIL Concepts Mapping

| ZIL Concept                     | Clojure Equivalent         |
| ------------------------------- | -------------------------- |
| `<GLOBAL VAR VALUE>`            | Atom or game-state key     |
| `<OBJECT ...>`                  | Map with :id, :flags, etc. |
| `<ROOM ...>`                    | Map with :id, :exits, etc. |
| `<ROUTINE NAME (ARGS)>`         | `defn`                     |
| `<SYNTAX VERB ... = V-HANDLER>` | Parser verb table          |
| `<SYNONYM WORD ...>`            | Parser word synonyms       |
| FSET/FCLEAR                     | Flag manipulation          |
| ,GLOBAL                         | Global reference           |
| .LOCAL                          | Local binding              |
| PRSA/PRSO/PRSI                  | Parser results             |
| RARG/M-LOOK                     | Room action argument       |

### Test Strategy

For each implementation:
1. Write test with `^:pending` metadata
2. Remove `^:pending` when starting implementation
3. Ensure test passes before moving on

```clojure
(deftest ^:pending v-take-basic-test
  (testing "taking an object adds it to inventory"
    (let [state (-> (gs/initial-state)
                    (gs/place-object :lamp :west-of-house))
          result (v-take state :lamp)]
      (is (= :player (gs/object-location result :lamp))))))
```
