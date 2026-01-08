# Clork Speedrun Plan

## Overview

This plan aims to complete Zork I in minimal moves while collecting all 19 treasures
for maximum points (350). Based on A* pathfinding analysis and GOAP dependency chains.

**Target:** Complete game in ~220 moves (current record: 223 moves per walkthrough 6)

## Key Insights from Planner Analysis

### Critical Dependencies (GOAP Backward Chaining)

```
:have-skull requires:
  -> :lld-flag (exorcism complete)
     -> :have-bell, :have-book, :have-candles, :have-matches
        -> :have-torch (for temple light)
           -> :have-rope, :troll-flag
              -> :have-sword, :have-light
```

### Inventory Constraints (CRITICAL!)

- **Item limit:** ~7 items max in inventory
- **Weight limit:** Heavy items (coffin, trunk) count extra
- **Plan deposits carefully** - Multiple trips required!

### One-Way Paths (Critical Constraints)

1. **Altar hole to Tiny Cave** - Cannot return up
2. **Torch Room** - Cannot climb back to Dome Room (rope unreachable from below)
3. **Coal Mine slide** - One-way to Cellar (useful for quick deposit!)
4. **River** - Cannot go upstream

### Route Optimizations Discovered

1. **Get egg BEFORE entering house** - Saves 6 moves vs. backtracking
2. **Go to Dam BEFORE descending to Torch Room** - Must get matches first
3. **Cyclops path** - Creates shortcut from underground to Living Room
4. **Drop sword in Troll Room** - Don't carry it all the way back
5. **Skip tying rope on first Dome visit** - Go straight to dam, tie on return
6. **After Hades, rush to deposit** - Before thief steals treasures!
7. **Coal mine slide exit** - Quick route: Mine -> Slide -> Cellar -> Living Room

### Coal Mine Requirements

- **Garlic** - For bat room protection
- **TWO light sources** - Candles work, but NOT TORCH (gas room explodes!)
- **Screwdriver** - For the coal -> diamond machine
- **Candles + Matches** - Safe in bucket, need to test

---

## Phase 1: Opening (Moves 1-17)

**Goal:** Get egg, enter house, collect essential items

```
Location: West of House -> Up a Tree -> Kitchen -> Attic -> Living Room

Commands:
n                    ; North of House
n                    ; Forest Path
u                    ; Up the Tree
take egg             ; Jeweled egg (5 pts)
d                    ; Down
s                    ; North of House
e                    ; Behind House
open window          ; Open kitchen window
w                    ; Kitchen
open sack            ; Brown sack
take garlic          ; For bat room protection
u                    ; Attic
take rope            ; For dome room
d                    ; Kitchen
w                    ; Living Room
take lamp            ; Brass lantern
take sword           ; Elvish sword (for troll)
```

**Items acquired:** egg, garlic, rope, lamp, sword
**Moves:** 17

---

## Phase 2: Underground - Troll & Coins (Moves 18-36)

**Goal:** Kill troll, get coins, open cyclops path

```
Location: Living Room -> Troll Room -> Maze -> Cyclops Room -> Living Room

Commands:
move rug             ; Reveal trap door
open trap door       ; Access cellar
d                    ; Cellar
turn on lamp         ; Light source
n                    ; Troll Room

; Combat (seed 37: 2 hits to kill)
attack troll with sword
attack troll with sword
drop sword           ; Don't need anymore - drop here to save carrying!

; Navigate maze to dead adventurer (coins)
w                    ; Maze-1
s                    ; Maze-2
e                    ; Maze-3
u                    ; Dead Adventurer (maze-5)
take coins           ; Bag of coins (10 pts)

; Navigate to cyclops
sw                   ; Maze-6
e                    ; Maze-7
s                    ; Maze-15
se                   ; Cyclops Room

; Scare cyclops (opens passage to Living Room)
ulysses              ; Cyclops flees!

; Return to surface via new shortcut
e                    ; Strange Passage
e                    ; Living Room!

; Deposit first treasure
open case            ; Trophy case
put coins in case    ; +5 pts bonus
```

**Items acquired:** coins (deposited), sword dropped in troll room
**Flags set:** troll-flag, magic-flag, cyclops-flag
**Moves:** 35 (saved 1 move by dropping sword earlier)

---

## Phase 3: Dam & Dome Room (Moves 36-53)

**Goal:** Go to dam FIRST (matches, turn bolts), THEN tie rope and descend

**OPTIMIZATION:** Skip tying rope on first visit to dome - go straight to dam, tie on return!

```
Location: Living Room -> Round Room -> Dam -> Dome Room -> Torch Room

Commands:
open trap door       ; Re-open (it closed)
d                    ; Cellar
n                    ; Troll Room (cleared)
e                    ; East-West Passage
e                    ; Round Room

; Go STRAIGHT to dam (skip dome room for now!)
n                    ; NS Passage
ne                   ; Deep Canyon
e                    ; Dam Room
n                    ; Dam Lobby
take matches         ; CRITICAL for exorcism!
n                    ; Maintenance Room
turn bolt with wrench ; Bolt 1
turn bolt with wrench ; Bolt 2 - dam opening!

; Now go to dome room and tie rope
s                    ; Dam Lobby
s                    ; Dam Room
s                    ; Deep Canyon (via south)
sw                   ; NS Passage
s                    ; Round Room
se                   ; Engravings Cave
e                    ; Dome Room
tie rope to railing  ; Enable descent
d                    ; Torch Room
turn off lamp        ; Save battery
take torch           ; Ivory torch (5 pts) - now have permanent light
```

**Items acquired:** matches, torch
**Flags set:** dam-opened (draining starts)
**Moves:** 53 (saved 3 moves by not backtracking to dome!)

---

## Phase 4: Exorcism (Moves 57-80)

**Goal:** Get bell/book/candles, perform exorcism, get skull

```
Location: Torch Room -> Temple -> Entrance to Hades -> Land of Living Dead

Commands:
s                    ; North Temple
take bell            ; Brass bell
s                    ; South Temple (Altar)
take book            ; Black book
take candles         ; Candles (already lit)
d                    ; Tiny Cave (through altar hole - ONE WAY!)
d                    ; Entrance to Hades

; Exorcism ritual (bell, book, candles)
ring bell            ; Bell gets hot, drops; candles drop and go out
take candles         ; Pick up dropped candles
light candles with match  ; Relight with matchbook
read book            ; Exorcism complete!

; Enter Hades
s                    ; Land of Living Dead
take skull           ; Crystal skull (10 pts)
```

**Items acquired:** bell (dropped), book, candles, skull
**Flags set:** lld-flag (Hades open)
**Moves:** 80

---

## Phase 4b: Rush to Deposit (Moves 81-95)

**Goal:** Get treasures to trophy case ASAP before thief steals them!

**CRITICAL:** The thief is now roaming. Every turn he might steal treasures from you.
Prioritize depositing valuable items immediately.

```
Location: Land of Living Dead -> Living Room

Commands:
n                    ; Entrance to Hades
u                    ; Tiny Cave
n                    ; Mirror Room 2
n                    ; Narrow Passage
n                    ; Round Room
n                    ; NS Passage
s                    ; Round Room (backtrack - wrong way!)
; Actually need to go via cyclops path...

; Alternative: Go through maze to cyclops room
; From Round Room:
w                    ; East-West Passage
w                    ; Troll Room
w                    ; Maze-1
s                    ; Maze-2
e                    ; Maze-3
u                    ; Dead Adventurer
sw                   ; Maze-6
e                    ; Maze-7
s                    ; Maze-15
se                   ; Cyclops Room
e                    ; Strange Passage
e                    ; Living Room

; Deposit treasures
put skull in case    ; Crystal skull
put torch in case    ; Ivory torch
put egg in case      ; Jeweled egg (let thief open it later)
; Keep: lamp, garlic, matches, candles, book
```

**Items deposited:** skull, torch, egg
**Why rush:** Thief will steal treasures; torch/skull are high-value
**Moves:** ~95

---

## Phase 5: Reservoir & Atlantis (Moves 96-115)

**Goal:** Wait for reservoir to drain, get sceptre from Atlantis

```
Location: Land of Living Dead -> Reservoir -> Atlantis

Commands:
n                    ; Entrance to Hades
u                    ; Tiny Cave
n                    ; Mirror Room 2
n                    ; Narrow Passage
n                    ; Round Room
n                    ; NS Passage
ne                   ; Deep Canyon
nw                   ; Reservoir South

; Wait for reservoir to drain (need ~8 turns from dam opening)
; We've used ~24 moves since opening dam, should be drained
; If not, add: z z z z z z z z (8 waits)

n                    ; Reservoir (now drained)
n                    ; Reservoir North
n                    ; Atlantis Room
take trident         ; Crystal trident (4 pts)

; Go back south to get coffin/sceptre
s                    ; Reservoir North
s                    ; Reservoir (drained)
s                    ; Reservoir South
sw                   ; Chasm
; ... continue to Egypt room for coffin
```

**Items acquired:** trident
**Moves:** ~100

---

## Phase 6: Coffin & Sceptre (Moves 101-115)

**Goal:** Get coffin with sceptre from Egypt room

```
; Route from underground to Egypt Room
; Via Round Room -> Engravings -> Dome -> Torch -> Temple -> Egypt

Commands:
; From current location, navigate to Egypt Room
; (Route depends on current position)

take coffin          ; Gold coffin (10 pts, contains sceptre)
open coffin          ; Reveal sceptre
take sceptre         ; Sceptre (4 pts)
```

**Items acquired:** coffin, sceptre
**Moves:** ~115

---

## Phase 7: Coal Mine & Diamond (Moves 116-155)

**Goal:** Navigate coal mine, get huge diamond, use machine

**CRITICAL REQUIREMENTS:**
- **Garlic** - MUST have for bat room (bat steals lamp otherwise!)
- **Candles + Matches** - Use candles for light in gas room (NOT torch!)
- **Screwdriver** - For coal-to-diamond machine
- **Coal** - Found in mine, goes in machine

**WARNING:** Using TORCH in gas room = EXPLOSION = DEATH!

```
Location: Living Room -> Bat Room -> Coal Mine -> Machine Room -> Slide

Commands:
; From Living Room, go to coal mine entrance
open trap door       ; If closed
d                    ; Cellar
n                    ; Troll Room
e                    ; East-West Passage
n                    ; Chute (narrow passage)
n                    ; Smelly Room
d                    ; Gas Room (CANDLES ONLY - NO TORCH!)
; Turn off torch if lit, use candles

; Navigate to coal mine
e                    ; Coal Mine
take coal            ; Lump of coal

; Find the huge diamond (navigate mine maze)
; ... (specific route TBD)
take diamond         ; Huge diamond (10 pts)

; Go to machine room
; From mine, navigate to machine room
take screwdriver     ; If not already have
put coal in machine  ; Insert coal
turn switch          ; Activate machine
open lid             ; Reveal small diamond
take diamond         ; Actually get the small diamond

; Exit via slide to Cellar (quick route!)
; Navigate to slide room
d                    ; Slide to Cellar

; Return to Living Room for deposit
u                    ; Back to Living Room
```

**Items acquired:** diamond(s), coal->processed
**Moves:** ~155

**Note:** The "huge diamond" from the mine vs the machine-made diamond needs verification.

---

## Phase 8: Loud Room - Platinum Bar (Moves 141-150)

**Goal:** Get platinum bar using "echo" trick

```
Location: Round Room -> Loud Room

Commands:
e                    ; Loud Room
echo                 ; Acoustics return platinum bar!
take bar             ; Platinum bar (10 pts)
w                    ; Round Room
```

**Items acquired:** platinum bar
**Moves:** ~150

---

## Phase 9: Boat & Emerald (Moves 151-175)

**Goal:** Take boat ride, get emerald from buoy

```
Location: Dam Base -> River -> Sandy Beach

Commands:
; Get to Dam Base
; Get in boat (inflatable boat from Dam Base)
take pump            ; Hand pump
inflate boat         ; Inflate with pump
drop pump            ; Don't need anymore
get in boat
launch               ; Into river

; Float downstream
; At river sections, can't go upstream
wait                 ; Float to shore
land                 ; Sandy Beach

; Get buoy with emerald
take buoy            ; Buoy
open buoy            ; Reveal emerald
take emerald         ; Emerald (5 pts)
```

**Items acquired:** emerald
**Moves:** ~175

---

## Phase 10: Thief Confrontation (Moves 176-195)

**Goal:** Kill thief, recover stolen treasures (and opened egg with canary!)

```
Location: Treasure Room

Commands:
; Navigate to Treasure Room (above Cyclops Room)
; Thief has been collecting treasures and OPENED THE EGG

; With nasty knife:
attack thief         ; Combat (multiple rounds likely)
attack thief
attack thief         ; Eventually kills him

; Recover treasures
take all             ; Get everything thief had
; Should include: canary (from opened egg!), jade, bracelet, etc.
```

**Items acquired:** canary, jade figurine, sapphire bracelet, any stolen treasures
**Moves:** ~195

---

## Phase 11: Final Treasures & Deposits (Moves 196-215)

**Goal:** Collect remaining treasures, deposit everything in trophy case

```
; Remaining treasures to find:
; - Painting (Gallery)
; - Pot of gold (End of Rainbow - need rainbow solid)
; - Scarab (Sandy Cave)
; - Gold bar (if not gotten from Loud Room)
; - Trunk (Reservoir when drained)

; Return to Living Room and deposit all treasures
; Multiple trips may be needed due to carry limit

put egg in case      ; Egg (opened by thief = canary inside)
put skull in case
put torch in case
put coffin in case
put trident in case
put diamond in case
put bar in case
put emerald in case
put canary in case
; ... continue for all treasures
```

**Moves:** ~215

---

## Phase 12: Endgame (Moves 216-225)

**Goal:** Enter Stone Barrow with all treasures deposited

```
; With 350 points (all treasures in case):
w                    ; Through west door (now open!)
sw                   ; Stone Barrow

; VICTORY!
```

**Final moves:** ~225

---

## Treasure Checklist

| Treasure          | Points | Location               | Notes                     |
| ----------------- | ------ | ---------------------- | ------------------------- |
| Jeweled Egg       | 5+5    | Up a Tree              | Let thief open for canary |
| Clockwork Canary  | 6+4    | Inside egg             | Thief opens it            |
| Bag of Coins      | 10+5   | Maze (Dead Adventurer) |                           |
| Ivory Torch       | 5+9    | Torch Room             |                           |
| Crystal Skull     | 10+8   | Land of Living Dead    | After exorcism            |
| Gold Coffin       | 10+15  | Egypt Room             |                           |
| Sceptre           | 4+6    | Inside coffin          |                           |
| Crystal Trident   | 4+11   | Atlantis Room          | After dam drains          |
| Huge Diamond      | 10+10  | Coal Mine              |                           |
| Platinum Bar      | 10+5   | Loud Room              | Say "echo"                |
| Emerald           | 5+10   | Inside buoy            | River trip                |
| Painting          | 4+6    | Gallery                |                           |
| Pot of Gold       | 10+10  | End of Rainbow         | Wave sceptre              |
| Jade Figurine     | 5+7    | Thief's hoard          | Kill thief                |
| Sapphire Bracelet | 5+8    | Thief's hoard          | Kill thief                |
| Gold Bar          | 10+15  | Loud Room (alt)        |                           |
| Scarab            | 5+5    | Sandy Cave             |                           |
| Trunk             | 15+5   | Reservoir (drained)    |                           |
| Brass Bauble      | 1+2    | Forest                 | Wind canary in forest!    |

**Total: 350 points**

---

## Combat Notes (Seed 37)

- **Troll:** Dies in 2 sword hits
- **Thief:** Variable, ~3-5 hits with good weapon
- **Cyclops:** No combat - say "ulysses" or "odysseus"

---

## Key Optimizations

1. **Egg first** - Get before entering house (saves backtracking)
2. **Dam before torch** - Can't return from torch room
3. **Let thief take egg** - He opens it, revealing canary
4. **"Echo" in Loud Room** - Gets platinum bar without complex puzzle
5. **Garlic for bat room** - Essential for mine access
6. **Cyclops shortcut** - Major time saver for returning to surface

---

## Current Progress (80 moves, 84 points)

**Location:** Land of Living Dead
**Inventory:** skull, candles, book, torch, matchbook, lamp, garlic, egg

Completed:
- [x] Phase 1: Opening (17 moves)
- [x] Phase 2: Troll & Coins (35 moves)
- [x] Phase 3: Dome & Dam (53 moves)
- [x] Phase 4: Exorcism (80 moves)

Remaining:
- [ ] Phase 4b: Rush to Deposit
- [ ] Phase 5: Reservoir & Atlantis
- [ ] Phase 6: Coffin & Sceptre
- [ ] Phase 7: Coal Mine & Diamond
- [ ] Phase 8: Loud Room
- [ ] Phase 9: Boat & Emerald
- [ ] Phase 10: Thief
- [ ] Phase 11: Final Deposits
- [ ] Phase 12: Endgame

**Estimated total: ~225 moves**
