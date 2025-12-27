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

| Status | Verb | Handler | Description |
|--------|------|---------|-------------|
| [x] | VERBOSE | V-VERBOSE | Enable full room descriptions |
| [x] | BRIEF | V-BRIEF | Brief room descriptions |
| [x] | SUPER/SUPERBRIEF | V-SUPER-BRIEF | Minimal descriptions |
| [x] | INVENTORY/I | V-INVENTORY | Show carried items |
| [x] | VERSION | V-VERSION | Show game version |
| [x] | DIAGNOSE | V-DIAGNOSE | Show player health |
| [x] | SCORE | V-SCORE | Show current score |
| [x] | QUIT/Q | V-QUIT | Quit game |
| [ ] | RESTART | V-RESTART | Restart game |
| [ ] | RESTORE | V-RESTORE | Load saved game |
| [ ] | SAVE | V-SAVE | Save game |
| [ ] | SCRIPT | V-SCRIPT | Start transcript |
| [ ] | UNSCRIPT | V-UNSCRIPT | Stop transcript |
| [ ] | $VERIFY | V-VERIFY | Verify game file |

### Core Movement Verbs

| Status | Verb | Handler | Description |
|--------|------|---------|-------------|
| [ ] | WALK/GO/RUN | V-WALK | Move in direction |
| [ ] | ENTER | V-ENTER | Enter location |
| [ ] | EXIT | V-EXIT | Exit location |
| [ ] | CLIMB | V-CLIMB-FOO | Climb object |
| [ ] | CLIMB UP | V-CLIMB-UP | Climb up |
| [ ] | CLIMB DOWN | V-CLIMB-DOWN | Climb down |
| [ ] | BACK | V-BACK | Return to previous room |

### Core Interaction Verbs

| Status | Verb | Handler | Description |
|--------|------|---------|-------------|
| [x] | LOOK/L | V-LOOK | Look around room |
| [ ] | EXAMINE/DESCRIBE | V-EXAMINE | Examine object closely |
| [ ] | TAKE/GET/GRAB | V-TAKE | Pick up object |
| [ ] | DROP | V-DROP | Drop object |
| [ ] | PUT/INSERT | V-PUT | Put object in container |
| [ ] | PUT ON | V-PUT-ON | Put object on surface |
| [ ] | OPEN | V-OPEN | Open container/door |
| [ ] | CLOSE | V-CLOSE | Close container/door |
| [ ] | READ | V-READ | Read text |
| [ ] | SEARCH | V-SEARCH | Search object |
| [ ] | LOOK IN | V-LOOK-INSIDE | Look inside container |
| [ ] | LOOK UNDER | V-LOOK-UNDER | Look under object |
| [ ] | LOOK BEHIND | V-LOOK-BEHIND | Look behind object |
| [ ] | LOOK ON | V-LOOK-ON | Look on surface |

---

## Phase 2: Extended Verbs

### Light and Lamp

| Status | Verb | Handler | Description |
|--------|------|---------|-------------|
| [ ] | LIGHT/TURN ON | V-LAMP-ON | Turn on light source |
| [ ] | EXTINGUISH/TURN OFF | V-LAMP-OFF | Turn off light source |

### Combat

| Status | Verb | Handler | Description |
|--------|------|---------|-------------|
| [ ] | ATTACK/FIGHT/HIT | V-ATTACK | Attack creature |
| [ ] | KILL/MURDER | V-ATTACK | Kill creature (synonym) |
| [ ] | STAB | V-STAB | Stab without weapon specified |
| [ ] | SWING | V-SWING | Swing weapon |
| [ ] | THROW AT | V-THROW | Throw at target |

### Object Manipulation

| Status | Verb | Handler | Description |
|--------|------|---------|-------------|
| [ ] | MOVE | V-MOVE | Move object |
| [ ] | PUSH | V-PUSH | Push object |
| [ ] | PULL/TUG | V-MOVE | Pull object |
| [ ] | RAISE/LIFT | V-RAISE | Raise object |
| [ ] | LOWER | V-LOWER | Lower object |
| [ ] | TURN | V-TURN | Turn/rotate object |
| [ ] | SHAKE | V-SHAKE | Shake container |
| [ ] | WAVE | V-WAVE | Wave object |
| [ ] | RUB/TOUCH | V-RUB | Rub/touch object |
| [ ] | TIE | V-TIE | Tie object |
| [ ] | UNTIE | V-UNTIE | Untie object |

### Tools and Items

| Status | Verb | Handler | Description |
|--------|------|---------|-------------|
| [ ] | UNLOCK | V-UNLOCK | Unlock with key |
| [ ] | LOCK | V-LOCK | Lock with key |
| [ ] | DIG | V-DIG | Dig with shovel |
| [ ] | CUT/SLICE | V-CUT | Cut with blade |
| [ ] | BURN/IGNITE | V-BURN | Burn with fire |
| [ ] | FILL | V-FILL | Fill container |
| [ ] | POUR | V-DROP | Pour liquid |
| [ ] | POUR ON | V-POUR-ON | Pour on object |
| [ ] | INFLATE | V-INFLATE | Inflate object |
| [ ] | DEFLATE | V-DEFLATE | Deflate object |
| [ ] | PLUG/PATCH | V-PLUG | Repair object |
| [ ] | PUMP | V-PUMP | Pump air |

### Food and Drink

| Status | Verb | Handler | Description |
|--------|------|---------|-------------|
| [ ] | EAT/CONSUME | V-EAT | Eat food |
| [ ] | DRINK | V-DRINK | Drink liquid |

### Communication

| Status | Verb | Handler | Description |
|--------|------|---------|-------------|
| [ ] | SAY | V-SAY | Say something |
| [ ] | TELL/ASK | V-TELL | Talk to NPC |
| [ ] | ANSWER/REPLY | V-ANSWER | Answer question |
| [ ] | HELLO/HI | V-HELLO | Greet |
| [ ] | YELL/SCREAM | V-YELL | Yell loudly |

### Vehicle/Boat

| Status | Verb | Handler | Description |
|--------|------|---------|-------------|
| [ ] | BOARD | V-BOARD | Board vehicle |
| [ ] | DISEMBARK | V-DISEMBARK | Leave vehicle |
| [ ] | LAUNCH | V-LAUNCH | Launch boat |

### Miscellaneous Verbs

| Status | Verb | Handler | Description |
|--------|------|---------|-------------|
| [ ] | WAIT/Z | V-WAIT | Wait/pass time |
| [ ] | JUMP/LEAP | V-LEAP | Jump |
| [ ] | SWIM | V-SWIM | Swim |
| [ ] | LISTEN | V-LISTEN | Listen to sounds |
| [ ] | SMELL/SNIFF | V-SMELL | Smell object |
| [ ] | KNOCK | V-KNOCK | Knock on door |
| [ ] | KICK | V-KICK | Kick object |
| [ ] | KISS | V-KISS | Kiss creature |
| [ ] | PRAY | V-PRAY | Pray |
| [ ] | FIND/WHERE | V-FIND | Find object |
| [ ] | COUNT | V-COUNT | Count objects |
| [ ] | WEAR | V-WEAR | Wear clothing |
| [ ] | RING | V-RING | Ring bell |
| [ ] | ECHO | V-ECHO | Say "echo" |
| [ ] | CROSS | V-CROSS | Cross bridge/chasm |
| [ ] | GIVE/OFFER | V-GIVE | Give to NPC |
| [ ] | THROUGH | V-THROUGH | Go through |
| [ ] | STAND | V-STAND | Stand up |

### Special/Easter Egg Verbs

| Status | Verb | Handler | Description |
|--------|------|---------|-------------|
| [ ] | ODYSSEUS/ULYSSES | V-ODYSSEUS | Cyclops puzzle |
| [ ] | PLUGH/XYZZY | V-ADVENT | Adventure reference |
| [ ] | FROBOZZ | V-FROBOZZ | Frobozz reference |
| [ ] | ZORK | V-ZORK | Meta-reference |
| [ ] | CURSE/DAMN | V-CURSES | Swear words |
| [ ] | WIN | V-WIN | Try to win |
| [ ] | TREASURE/TEMPLE | V-TREASURE | Hint command |

---

## Phase 3: Rooms

### Outdoors - House Area

| Status | Room ID | Description |
|--------|---------|-------------|
| [~] | WEST-OF-HOUSE | West of House (starting location) |
| [ ] | NORTH-OF-HOUSE | North of House |
| [ ] | SOUTH-OF-HOUSE | South of House |
| [ ] | EAST-OF-HOUSE | Behind House |
| [ ] | CLEARING | Forest Clearing |
| [ ] | FOREST-1 | Forest |
| [ ] | FOREST-2 | Forest |
| [ ] | FOREST-3 | Forest |
| [ ] | PATH | Forest Path |
| [ ] | UP-A-TREE | Up a Tree |
| [ ] | GRATING-CLEARING | Clearing with Grating |
| [ ] | MOUNTAINS | Mountains |
| [ ] | END-OF-RAINBOW | End of Rainbow |
| [ ] | ON-RAINBOW | On the Rainbow |
| [ ] | ARAGAIN-FALLS | Aragain Falls |
| [ ] | WHITE-CLIFFS-NORTH | White Cliffs Beach (North) |
| [ ] | WHITE-CLIFFS-SOUTH | White Cliffs Beach (South) |
| [ ] | SHORE | Shore |
| [ ] | SANDY-BEACH | Sandy Beach |

### Inside House

| Status | Room ID | Description |
|--------|---------|-------------|
| [ ] | KITCHEN | Kitchen |
| [ ] | LIVING-ROOM | Living Room |
| [ ] | ATTIC | Attic |

### Underground - Near House

| Status | Room ID | Description |
|--------|---------|-------------|
| [ ] | CELLAR | Cellar |
| [ ] | TROLL-ROOM | Troll Room |
| [ ] | EW-PASSAGE | East-West Passage |
| [ ] | ROUND-ROOM | Round Room |
| [ ] | NS-PASSAGE | North-South Passage |
| [ ] | CHASM-ROOM | Chasm |
| [ ] | EAST-OF-CHASM | East of Chasm |
| [ ] | GALLERY | Gallery |
| [ ] | STUDIO | Studio |

### Underground - Maze

| Status | Room ID | Description |
|--------|---------|-------------|
| [ ] | MAZE-1 | Maze |
| [ ] | MAZE-2 | Maze |
| [ ] | MAZE-3 | Maze |
| [ ] | MAZE-4 | Maze |
| [ ] | MAZE-5 | Maze |
| [ ] | MAZE-6 | Maze |
| [ ] | MAZE-7 | Maze |
| [ ] | MAZE-8 | Maze |
| [ ] | MAZE-9 | Maze |
| [ ] | MAZE-10 | Maze |
| [ ] | MAZE-11 | Maze |
| [ ] | MAZE-12 | Maze |
| [ ] | MAZE-13 | Maze |
| [ ] | MAZE-14 | Maze |
| [ ] | MAZE-15 | Maze |
| [ ] | DEAD-END-1 | Dead End |
| [ ] | DEAD-END-2 | Dead End |
| [ ] | DEAD-END-3 | Dead End |
| [ ] | DEAD-END-4 | Dead End |
| [ ] | DEAD-END-5 | Dead End |
| [ ] | GRATING-ROOM | Grate Room |

### Underground - Dam Area

| Status | Room ID | Description |
|--------|---------|-------------|
| [ ] | DAM-ROOM | Dam |
| [ ] | DAM-BASE | Dam Base |
| [ ] | DAM-LOBBY | Dam Lobby |
| [ ] | MAINTENANCE-ROOM | Maintenance Room |
| [ ] | RESERVOIR | Reservoir |
| [ ] | RESERVOIR-NORTH | Reservoir North |
| [ ] | RESERVOIR-SOUTH | Reservoir South |
| [ ] | STREAM-VIEW | Stream View |
| [ ] | IN-STREAM | In Stream |

### Underground - River

| Status | Room ID | Description |
|--------|---------|-------------|
| [ ] | RIVER-1 | Frigid River |
| [ ] | RIVER-2 | Frigid River |
| [ ] | RIVER-3 | Frigid River |
| [ ] | RIVER-4 | Frigid River |
| [ ] | RIVER-5 | Frigid River |

### Underground - Caves and Passages

| Status | Room ID | Description |
|--------|---------|-------------|
| [ ] | LOUD-ROOM | Loud Room |
| [ ] | DAMP-CAVE | Damp Cave |
| [ ] | SMALL-CAVE | Small Cave |
| [ ] | TINY-CAVE | Tiny Cave |
| [ ] | COLD-PASSAGE | Cold Passage |
| [ ] | NARROW-PASSAGE | Narrow Passage |
| [ ] | TWISTING-PASSAGE | Twisting Passage |
| [ ] | WINDING-PASSAGE | Winding Passage |
| [ ] | STRANGE-PASSAGE | Strange Passage |
| [ ] | DEEP-CANYON | Deep Canyon |
| [ ] | CANYON-VIEW | Canyon View |
| [ ] | CANYON-BOTTOM | Canyon Bottom |
| [ ] | ENGRAVINGS-CAVE | Engravings Cave |
| [ ] | DOME-ROOM | Dome Room |
| [ ] | TORCH-ROOM | Torch Room |

### Underground - Mine

| Status | Room ID | Description |
|--------|---------|-------------|
| [ ] | MINE-ENTRANCE | Mine Entrance |
| [ ] | MINE-1 | Squeaky Room |
| [ ] | MINE-2 | Mine |
| [ ] | MINE-3 | Mine |
| [ ] | MINE-4 | Mine |
| [ ] | BAT-ROOM | Bat Room |
| [ ] | SHAFT-ROOM | Shaft Room |
| [ ] | LOWER-SHAFT | Smelly Room |
| [ ] | TIMBER-ROOM | Timber Room |
| [ ] | SLIDE-ROOM | Slide Room |
| [ ] | LADDER-TOP | Ladder Top |
| [ ] | LADDER-BOTTOM | Ladder Bottom |
| [ ] | GAS-ROOM | Gas Room |

### Underground - Temple and Hades

| Status | Room ID | Description |
|--------|---------|-------------|
| [ ] | NORTH-TEMPLE | Temple |
| [ ] | SOUTH-TEMPLE | Altar |
| [ ] | EGYPT-ROOM | Egyptian Room |
| [ ] | ENTRANCE-TO-HADES | Entrance to Hades |
| [ ] | LAND-OF-LIVING-DEAD | Land of the Living Dead |

### Underground - Thief's Lair

| Status | Room ID | Description |
|--------|---------|-------------|
| [ ] | CYCLOPS-ROOM | Cyclops Room |
| [ ] | TREASURE-ROOM | Treasure Room |
| [ ] | ATLANTIS-ROOM | Atlantis Room |

### Underground - Other

| Status | Room ID | Description |
|--------|---------|-------------|
| [ ] | MIRROR-ROOM-1 | Mirror Room |
| [ ] | MIRROR-ROOM-2 | Mirror Room |
| [ ] | MACHINE-ROOM | Machine Room |
| [ ] | SANDY-CAVE | Sandy Cave |
| [ ] | STONE-BARROW | Stone Barrow (endgame) |
| [ ] | SMELLY-ROOM | Smelly Room |
| [ ] | SQUEEKY-ROOM | Squeaky Room |

---

## Phase 4: Objects

### Treasures (Scoring Items)

| Status | Object ID | Description | Points |
|--------|-----------|-------------|--------|
| [ ] | BAR | Platinum Bar | 10 |
| [ ] | BAUBLE | Brass Bauble | 1 |
| [ ] | BRACELET | Sapphire Bracelet | 5 |
| [ ] | CHALICE | Silver Chalice | 10 |
| [ ] | COFFIN | Gold Coffin | 10 |
| [ ] | DIAMOND | Huge Diamond | 10 |
| [ ] | EGG | Jewel-Encrusted Egg | 5 |
| [ ] | EMERALD | Large Emerald | 5 |
| [ ] | JADE | Jade Figurine | 5 |
| [ ] | PAINTING | Beautiful Painting | 4 |
| [ ] | POT-OF-GOLD | Pot of Gold | 10 |
| [ ] | SCARAB | Jeweled Scarab | 5 |
| [ ] | SCEPTRE | Royal Sceptre | 4 |
| [ ] | TORCH | Ivory Torch | 6 |
| [ ] | TRIDENT | Crystal Trident | 4 |
| [ ] | TRUNK | Trunk of Jewels | 15 |
| [ ] | CANARY | Clockwork Canary | 6 |
| [ ] | BAG-OF-COINS | Bag of Coins | 5 |

### Light Sources

| Status | Object ID | Description |
|--------|-----------|-------------|
| [ ] | LAMP | Brass Lantern |
| [ ] | CANDLES | Pair of Candles |
| [ ] | TORCH | Ivory Torch |
| [ ] | MATCH | Match |

### Weapons

| Status | Object ID | Description |
|--------|-----------|-------------|
| [ ] | SWORD | Elvish Sword |
| [ ] | KNIFE | Nasty Knife |
| [ ] | RUSTY-KNIFE | Rusty Knife |
| [ ] | AXE | Bloody Axe |
| [ ] | STILETTO | Stiletto |

### Tools

| Status | Object ID | Description |
|--------|-----------|-------------|
| [ ] | SHOVEL | Shovel |
| [ ] | KEYS | Set of Keys |
| [ ] | SCREWDRIVER | Screwdriver |
| [ ] | WRENCH | Wrench |
| [ ] | ROPE | Rope |
| [ ] | PUTTY | Gunk/Putty |

### Containers

| Status | Object ID | Description |
|--------|-----------|-------------|
| [~] | MAILBOX | Small Mailbox |
| [ ] | TROPHY-CASE | Trophy Case |
| [ ] | TOOL-CHEST | Tool Chest |
| [ ] | LARGE-BAG | Large Bag |
| [ ] | SANDWICH-BAG | Brown Sack |
| [ ] | BOTTLE | Glass Bottle |
| [ ] | BUOY | Buoy |

### Boat-Related

| Status | Object ID | Description |
|--------|-----------|-------------|
| [ ] | INFLATABLE-BOAT | Pile of Plastic |
| [ ] | INFLATED-BOAT | Inflated Boat |
| [ ] | PUNCTURED-BOAT | Punctured Boat |
| [ ] | PUMP | Hand Pump |

### Readable Items

| Status | Object ID | Description |
|--------|-----------|-------------|
| [x] | ADVERTISEMENT | Leaflet |
| [ ] | BOOK | Black Book |
| [ ] | GUIDE | Tour Guide |
| [ ] | MAP | Ancient Map |
| [ ] | OWNERS-MANUAL | Owner's Manual |
| [ ] | BOAT-LABEL | Label |
| [ ] | ENGRAVINGS | Engravings |
| [ ] | PRAYER | Prayer |

### Food

| Status | Object ID | Description |
|--------|-----------|-------------|
| [ ] | LUNCH | Lunch |
| [ ] | GARLIC | Clove of Garlic |
| [ ] | WATER | Quantity of Water |

### Scenery/Fixed Objects

| Status | Object ID | Description |
|--------|-----------|-------------|
| [ ] | WHITE-HOUSE | White House |
| [ ] | FOREST | Forest |
| [ ] | TREE | Tree |
| [ ] | BOARD | Wooden Board |
| [ ] | FRONT-DOOR | Front Door |
| [ ] | KITCHEN-WINDOW | Kitchen Window |
| [ ] | TRAP-DOOR | Trap Door |
| [ ] | GRATE | Grating |
| [ ] | CHIMNEY | Chimney |
| [ ] | LEAVES | Pile of Leaves |
| [ ] | DAM | Dam |
| [ ] | RAINBOW | Rainbow |
| [ ] | MIRROR-1 | Mirror |
| [ ] | MIRROR-2 | Mirror |
| [ ] | MACHINE | Machine |
| [ ] | SLIDE | Slide |
| [ ] | CRACK | Crack |
| [ ] | LEAK | Leak |
| [ ] | WOODEN-DOOR | Wooden Door |
| [ ] | BARROW-DOOR | Barrow Door |
| [ ] | RUG | Rug |
| [ ] | ALTAR | Altar |
| [ ] | PEDESTAL | Pedestal |
| [ ] | CONTROL-PANEL | Control Panel |

### Basket System

| Status | Object ID | Description |
|--------|-----------|-------------|
| [ ] | RAISED-BASKET | Basket (raised) |
| [ ] | LOWERED-BASKET | Basket (lowered) |
| [ ] | NEST | Bird's Nest |

### Special Items

| Status | Object ID | Description |
|--------|-----------|-------------|
| [ ] | BELL | Brass Bell |
| [ ] | HOT-BELL | Red-hot Bell |
| [ ] | SKULL | Skull |
| [ ] | BONES | Pile of Bones |
| [ ] | BODIES | Bodies |
| [ ] | COAL | Lump of Coal |
| [ ] | SAND | Pile of Sand |
| [ ] | BUBBLE | Bubble |
| [ ] | BOLT | Bolt |
| [ ] | TUBE | Tube |
| [ ] | LADDER | Ladder |
| [ ] | TIMBERS | Timbers |
| [ ] | BROKEN-EGG | Broken Egg |
| [ ] | BROKEN-CANARY | Broken Canary |
| [ ] | BROKEN-LAMP | Broken Lamp |
| [ ] | BURNED-OUT-LANTERN | Burned-out Lantern |

### NPCs/Actors

| Status | Object ID | Description |
|--------|-----------|-------------|
| [~] | ADVENTURER | Player character |
| [ ] | TROLL | Troll |
| [ ] | THIEF | Thief |
| [ ] | CYCLOPS | Cyclops |
| [ ] | BAT | Vampire Bat |
| [ ] | SONGBIRD | Songbird |
| [ ] | GHOSTS | Spirits |

### Global/Pseudo Objects

| Status | Object ID | Description |
|--------|-----------|-------------|
| [ ] | GLOBAL-WATER | Global Water |
| [ ] | GROUND | Ground |
| [ ] | STAIRS | Stairs |
| [ ] | GRUE | Grue |
| [ ] | ME | Self-reference |
| [ ] | IT | Pronoun reference |
| [ ] | HANDS | Bare Hands |
| [ ] | LUNGS | Lungs/Air |
| [ ] | ZORKMID | Zorkmid currency |
| [ ] | SAILOR | Sailor (Easter egg) |

---

## Phase 5: Action Routines

### Room Actions

| Status | Routine | Room | Description |
|--------|---------|------|-------------|
| [~] | WEST-HOUSE | WEST-OF-HOUSE | West of house description |
| [ ] | EAST-HOUSE | EAST-OF-HOUSE | Behind house logic |
| [ ] | FOREST-ROOM | FOREST-* | Forest room behavior |
| [ ] | FOREST-F | FOREST | Forest object |
| [ ] | TREE-ROOM | UP-A-TREE | Tree climbing |
| [ ] | CLEARING-FCN | CLEARING | Clearing logic |
| [ ] | KITCHEN-FCN | KITCHEN | Kitchen logic |
| [ ] | LIVING-ROOM-FCN | LIVING-ROOM | Living room logic |
| [ ] | CELLAR-FCN | CELLAR | Cellar behavior |
| [ ] | TROLL-ROOM-F | TROLL-ROOM | Troll room logic |
| [ ] | LOUD-ROOM-FCN | LOUD-ROOM | Echo puzzle |
| [ ] | DAM-ROOM-FCN | DAM-ROOM | Dam controls |
| [ ] | RESERVOIR-FCN | RESERVOIR | Reservoir water |
| [ ] | CYCLOPS-ROOM-FCN | CYCLOPS-ROOM | Cyclops room |
| [ ] | TREASURE-ROOM-FCN | TREASURE-ROOM | Treasure room |
| [ ] | TORCH-ROOM-FCN | TORCH-ROOM | Torch room |
| [ ] | MIRROR-ROOM | MIRROR-ROOM-* | Mirror rooms |
| [ ] | MACHINE-ROOM-FCN | MACHINE-ROOM | Coal machine |
| [ ] | DOME-ROOM-FCN | DOME-ROOM | Dome room |
| [ ] | SOUTH-TEMPLE-FCN | SOUTH-TEMPLE | Temple altar |
| [ ] | BARROW-FCN | STONE-BARROW | Endgame barrow |
| [ ] | BATS-ROOM | BAT-ROOM | Bat room |
| [ ] | FALLS-ROOM | ARAGAIN-FALLS | Falls viewing |
| [ ] | BOOM-ROOM | GAS-ROOM | Gas explosion |
| [ ] | CAVE2-ROOM | Caves | Various caves |

### NPC Actions

| Status | Routine | NPC | Description |
|--------|---------|-----|-------------|
| [ ] | TROLL-FCN | TROLL | Troll combat/behavior |
| [ ] | THIEF-IN-TREASURE | THIEF | Thief in lair |
| [ ] | THIEF-VS-ADVENTURER | THIEF | Thief combat |
| [ ] | ROBBER-FUNCTION | THIEF | Thief stealing |
| [ ] | CYCLOPS-FCN | CYCLOPS | Cyclops behavior |
| [ ] | BAT-F | BAT | Bat behavior |
| [ ] | BAT-D | BAT | Bat daemon |
| [ ] | GHOSTS-F | GHOSTS | Spirits behavior |
| [ ] | SONGBIRD-F | SONGBIRD | Bird singing |

### Object Actions

| Status | Routine | Object | Description |
|--------|---------|--------|-------------|
| [ ] | MAILBOX-F | MAILBOX | Mailbox interaction |
| [ ] | TRAP-DOOR-FCN | TRAP-DOOR | Trap door logic |
| [ ] | GRATE-FUNCTION | GRATE | Grating logic |
| [ ] | TROPHY-CASE-FCN | TROPHY-CASE | Score deposit |
| [ ] | BOTTLE-FUNCTION | BOTTLE | Bottle liquid |
| [ ] | ROPE-FUNCTION | ROPE | Rope tying |
| [ ] | SWORD-FCN | SWORD | Glowing sword |
| [ ] | LANTERN | LAMP | Lantern usage |
| [ ] | CANDLES-FCN | CANDLES | Candle burning |
| [ ] | MATCH-FUNCTION | MATCH | Match lighting |
| [ ] | BELL-F | BELL | Bell ringing |
| [ ] | HOT-BELL-F | HOT-BELL | Hot bell handling |
| [ ] | EGG-OBJECT | EGG | Egg opening |
| [ ] | CANARY-OBJECT | CANARY | Clockwork canary |
| [ ] | DAM-FUNCTION | DAM | Dam controls |
| [ ] | MACHINE-F | MACHINE | Coal machine |
| [ ] | RUG-FCN | RUG | Hidden trap door |
| [ ] | BASKET-F | BASKET | Shaft basket |
| [ ] | SCEPTRE-FUNCTION | SCEPTRE | Rainbow waving |
| [ ] | PAINTING-FCN | PAINTING | Painting handling |
| [ ] | RAINBOW-FCN | RAINBOW | Rainbow crossing |
| [ ] | SLIDE-FUNCTION | SLIDE | Cellar slide |
| [ ] | LEAK-FUNCTION | LEAK | Dam leak |
| [ ] | LEAF-PILE | LEAVES | Leaf pile |
| [ ] | SAND-FUNCTION | SAND | Digging sand |
| [ ] | TUBE-FUNCTION | TUBE | Tube usage |
| [ ] | TORCH-OBJECT | TORCH | Torch handling |

### Combat System

| Status | Routine | Description |
|--------|---------|-------------|
| [ ] | DO-FIGHT | Main combat loop |
| [ ] | I-FIGHT | Fight interrupt |
| [ ] | HERO-BLOW | Player attacks |
| [ ] | VILLAIN-BLOW | Enemy attacks |
| [ ] | VILLAIN-RESULT | Combat result |
| [ ] | WINNER-RESULT | Combat outcome |
| [ ] | FIGHT-STRENGTH | Strength calculation |
| [ ] | VILLAIN-STRENGTH | Enemy strength |
| [ ] | FIND-WEAPON | Weapon selection |
| [ ] | WEAPON-FUNCTION | Weapon handling |
| [ ] | JIGS-UP | Player death |

### Daemon/Interrupt System

| Status | Routine | Description |
|--------|---------|-------------|
| [ ] | I-LANTERN | Lantern timer |
| [ ] | I-CANDLES | Candle timer |
| [ ] | I-MATCH | Match burning |
| [ ] | I-THIEF | Thief wandering |
| [ ] | I-SWORD | Sword glowing |
| [ ] | I-CYCLOPS | Cyclops behavior |
| [ ] | I-CURE | Healing timer |
| [ ] | I-FOREST-ROOM | Forest wandering |
| [ ] | I-RIVER | River flow |
| [ ] | I-RFILL | Reservoir filling |
| [ ] | I-REMPTY | Reservoir emptying |
| [ ] | I-MAINT-ROOM | Maintenance flooding |

### Utility Routines

| Status | Routine | Description |
|--------|---------|-------------|
| [ ] | DESCRIBE-ROOM | Room description |
| [ ] | DESCRIBE-OBJECT | Object description |
| [ ] | DESCRIBE-OBJECTS | List objects |
| [ ] | PRINT-CONT | Print contents |
| [ ] | PRINT-CONTENTS | Print container contents |
| [ ] | GOTO | Room movement |
| [ ] | DO-WALK | Walking logic |
| [ ] | NO-GO-TELL | Blocked movement |
| [ ] | ITAKE | Internal take |
| [ ] | IDROP | Internal drop |
| [x] | SCORE-OBJ | Score object |
| [x] | SCORE-UPD | Update score |
| [ ] | WEIGHT | Calculate weight |
| [ ] | HELD? | Check if held |
| [ ] | SEE-INSIDE? | Can see inside |
| [ ] | LIT? | Check if lit |
| [ ] | GLOBAL-IN? | Global object check |
| [ ] | THIS-IS-IT | Set pronoun |
| [ ] | REMOVE-CAREFULLY | Remove without triggering |
| [ ] | REMARK | Random message |
| [ ] | FINISH | Game end |
| [ ] | YES? | Y/N prompt |

---

## Phase 6: Parser

### Core Parser

| Status | Routine | Description |
|--------|---------|-------------|
| [~] | PARSER | Main parser entry |
| [ ] | CLAUSE | Parse clause |
| [ ] | CLAUSE-ADD | Add to clause |
| [ ] | CLAUSE-COPY | Copy clause |
| [ ] | ACLAUSE-WIN | Actor clause |
| [ ] | NCLAUSE-WIN | Noun clause |
| [ ] | SYNTAX-CHECK | Check syntax |
| [ ] | SYNTAX-FOUND | Syntax matched |
| [ ] | SNARF-OBJECTS | Collect objects |
| [ ] | SNARFEM | Object collection |
| [ ] | GET-OBJECT | Resolve object |
| [ ] | OBJ-FOUND | Object found |
| [ ] | SEARCH-LIST | Search for object |

### Object Resolution

| Status | Routine | Description |
|--------|---------|-------------|
| [ ] | ACCESSIBLE? | Can reach object |
| [ ] | GLOBAL-CHECK | Global object |
| [ ] | GWIM | "Get What I Mean" |
| [ ] | TAKE-CHECK | Can take object |
| [ ] | ITAKE-CHECK | Internal take check |
| [ ] | META-LOC | Object location |
| [ ] | THIS-IT? | Pronoun resolution |
| [ ] | LIT? | Light check |

### Error Handling

| Status | Routine | Description |
|--------|---------|-------------|
| [ ] | CANT-USE | Can't use object |
| [ ] | CANT-ORPHAN | Orphan error |
| [ ] | UNKNOWN-WORD | Unknown word |
| [ ] | MANY-CHECK | Ambiguity check |
| [ ] | WHICH-PRINT | Which one? |
| [ ] | THING-PRINT | Print object name |

### Orphan/Continuation

| Status | Routine | Description |
|--------|---------|-------------|
| [ ] | ORPHAN | Create orphan |
| [ ] | ORPHAN-MERGE | Merge orphan |
| [ ] | BUT-MERGE | Merge exceptions |

---

## Phase 7: Global State

### Core Globals

| Status | Global | Description |
|--------|--------|-------------|
| [x] | HERE | Current room |
| [x] | WINNER | Current actor |
| [x] | PLAYER | Player object |
| [x] | SCORE | Current score |
| [x] | SCORE-MAX | Maximum score |
| [x] | MOVES | Turn counter |
| [x] | DEATHS | Death counter |
| [x] | VERBOSE | Verbose mode |
| [x] | SUPER-BRIEF | Super-brief mode |
| [ ] | LIT | Is room lit |
| [ ] | DEAD | Is player dead |
| [ ] | WON-FLAG | Game won |

### Parser Globals

| Status | Global | Description |
|--------|--------|-------------|
| [~] | PRSA | Current verb/action |
| [~] | PRSO | Direct object |
| [~] | PRSI | Indirect object |
| [ ] | P-IT-OBJECT | "it" reference |
| [ ] | P-CONT | Continue parsing |
| [ ] | P-NUMBER | Parsed number |
| [ ] | P-DIRECTION | Movement direction |

### NPC State

| Status | Global | Description |
|--------|--------|-------------|
| [ ] | TROLL-FLAG | Troll state |
| [ ] | TROLL-MELEE | Troll in combat |
| [ ] | THIEF-HERE | Thief present |
| [ ] | THIEF-ENGROSSED | Thief distracted |
| [ ] | THIEF-MELEE | Thief in combat |
| [ ] | CYCLOPS-FLAG | Cyclops state |
| [ ] | CYCLOMAD | Cyclops angry |
| [ ] | CYCLOWRATH | Cyclops attacking |
| [ ] | CYCLOPS-MELEE | Cyclops in combat |

### Puzzle State

| Status | Global | Description |
|--------|--------|-------------|
| [ ] | KITCHEN-WINDOW-FLAG | Window open |
| [ ] | GRATE-REVEALED | Grate visible |
| [ ] | DOME-FLAG | Dome rope |
| [ ] | GATE-FLAG | Gate state |
| [ ] | GATES-OPEN | Gates open |
| [ ] | RAINBOW-FLAG | Rainbow solid |
| [ ] | LOW-TIDE | Water level |
| [ ] | WATER-LEVEL | Reservoir level |
| [ ] | LOUD-FLAG | Loud room state |
| [ ] | RUG-MOVED | Rug moved |
| [ ] | EGG-SOLVE | Egg opened |
| [ ] | COFFIN-CURE | Coffin taken |
| [ ] | MAGIC-FLAG | In thief's lair |
| [ ] | BUOY-FLAG | Buoy opened |
| [ ] | LLD-FLAG | Land of dead visited |
| [ ] | MIRROR-MUNG | Mirror broken |
| [ ] | SING-SONG | Songbird sang |

### Item State

| Status | Global | Description |
|--------|--------|-------------|
| [ ] | MATCH-COUNT | Matches remaining |
| [ ] | LAMP-TABLE | Lantern fuel |
| [ ] | CANDLE-TABLE | Candle time |
| [ ] | DEFLATE | Boat deflated |
| [ ] | RIVER-LAUNCH | River launch state |

---

## Implementation Phases Summary

### Phase 1: Foundation (Current)
- [x] Basic game loop
- [~] Parser (basic)
- [x] Room/object framework
- [x] Meta-verbs (version, verbose, brief, inventory)
- [~] Look command
- [ ] Movement commands
- [ ] Core interaction (take, drop, open, close)

### Phase 2: Core Gameplay
- [ ] Light system (lamp, darkness, grue)
- [ ] Container system
- [ ] Scoring system
- [ ] Save/restore

### Phase 3: World Building
- [ ] All rooms with connections
- [ ] All objects placed
- [ ] Room descriptions
- [ ] Object descriptions

### Phase 4: NPCs and Combat
- [ ] Troll encounter
- [ ] Thief AI
- [ ] Cyclops puzzle
- [ ] Combat system

### Phase 5: Puzzles
- [ ] Dam/reservoir puzzle
- [ ] Coal machine puzzle
- [ ] Loud room echo
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

| ZIL Concept | Clojure Equivalent |
|-------------|-------------------|
| `<GLOBAL VAR VALUE>` | Atom or game-state key |
| `<OBJECT ...>` | Map with :id, :flags, etc. |
| `<ROOM ...>` | Map with :id, :exits, etc. |
| `<ROUTINE NAME (ARGS)>` | `defn` |
| `<SYNTAX VERB ... = V-HANDLER>` | Parser verb table |
| `<SYNONYM WORD ...>` | Parser word synonyms |
| FSET/FCLEAR | Flag manipulation |
| ,GLOBAL | Global reference |
| .LOCAL | Local binding |
| PRSA/PRSO/PRSI | Parser results |
| RARG/M-LOOK | Room action argument |

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
