# Zork I Speedrun Plan for Clork

## Goal
Beat Zork I (350 points) in under 250 moves, targeting under 225 if possible.

## Reference: Best Known Speedrun
From walkthrough 6.txt, the theoretical minimum is **223 moves to 350 points** (228 to finish).
However, this uses tricks that may not work in Clork:
- The "G" (again) bug for getting stiletto
- One-hit kills via save/restore exploitation
- Bat transport shortcuts

## Strategy Overview

The optimal route focuses on:
1. **Minimize backtracking** - Visit each area as few times as possible
2. **Batch treasure deposits** - Carry max items before returning to trophy case
3. **Kill thief early** - Reduces complications from stolen treasures
4. **Use the cyclops path** - Opens direct route between underground and house
5. **Skip unnecessary items** - Don't pick up leaflet, lunch, water, etc.

## Treasures Required (19 items = 350 points)

| Treasure           | Points | Location              | Dependencies        |
| ------------------ | ------ | --------------------- | ------------------- |
| Platinum Bar       | 10     | Loud Room             | Say "echo"          |
| Brass Bauble       | 1      | Up A Tree area        | Wind canary         |
| Sapphire Bracelet  | 5      | Gas Room              | Navigate mine       |
| Silver Chalice     | 10     | Thief's Treasure Room | Kill thief          |
| Gold Coffin        | 10     | Egyptian Room         | Pray to teleport    |
| Huge Diamond       | 10     | Machine Room          | Coal machine puzzle |
| Large Emerald      | 5      | Buoy                  | Boat ride           |
| Jade Figurine      | 5      | Bat Room              | Have garlic         |
| Pot of Gold        | 10     | End of Rainbow        | Wave sceptre        |
| Jeweled Scarab     | 5      | Sandy Cave            | Dig 4x              |
| Royal Sceptre      | 4      | Gold Coffin           | Open coffin         |
| Ivory Torch        | 6      | Torch Room            | Also light source   |
| Crystal Trident    | 4      | Atlantis/Reservoir    | Drain reservoir     |
| Trunk of Jewels    | 15     | Reservoir             | Drain reservoir     |
| Clockwork Canary   | 6      | Jeweled Egg           | Thief opens egg     |
| Jeweled Egg        | 5      | Up A Tree             | Shell after opened  |
| Bag of Coins       | 5      | Dead Adventurer       | In maze             |
| Beautiful Painting | 4      | Gallery               | Easy access         |
| Crystal Skull      | 10     | Land of Living Dead   | Exorcism ritual     |

## Optimized Route

### Trip 1: Setup & Egg (Moves 1-19)
```
n                   # To Forest Path
n                   # To Clearing
u                   # Climb tree
take egg            # First treasure
d                   # Down from tree
s                   # Back to Path
e                   # Behind House
open window         # Access house
w                   # Enter Kitchen
w                   # To Living Room
take lamp           # Essential light
take sword          # For combat
move rug            # Reveal trapdoor
open case           # Ready for treasures
open trap door      # Ready to descend
```
**State: Have egg, lamp, sword. Trapdoor open.**

### Trip 2: Underground - Painting & Troll (Moves 20-35)
```
d                   # Cellar
turn on lamp        # Light up
s                   # South passage
e                   # Gallery
take painting       # Treasure #1
w                   # Back
n                   # To Cellar
n                   # Troll Room (fight begins)
kill troll with sword  # Combat (may need multiple)
[repeat as needed]
drop sword          # No longer needed
```
**State: Have egg, lamp, painting. Troll dead.**

### Trip 3: Maze & Cyclops (Moves 36-55)
```
w                   # Enter Maze
s                   # Maze navigation
e
u                   # Dead Adventurer
take coins          # Treasure
sw
e
s
se                  # Cyclops Room
ulysses             # Scare cyclops away
e                   # Through new passage
e                   # Living Room!
put painting in case
put coins in case   # Score: 9 points
```
**State: Cyclops path open. 2 treasures stored.**

### Trip 4: Thief Setup (Moves 56-70)
```
w                   # Back to Cyclops Room
u                   # Treasure Room
give egg to thief   # Let him open it
d                   # Back down
e                   # Living Room
e                   # Kitchen
open bag            # Brown sack
take garlic         # Need for bat room
w                   # Living Room
take knife          # For thief fight
```

### Trip 5: Dam & Temple Prep (Moves 71-110)
```
w                   # Cyclops Room
w                   # Round Room (via new passage)
e                   # East of Round Room
e                   # Deep Canyon area
ne                  # Reservoir South
e                   # Dam
n                   # Dam Lobby
take matches        # For candles
n                   # Maintenance Room
take wrench
take screwdriver
push yellow button  # Enable bolt
s                   # Dam Lobby
s                   # Dam
turn bolt with wrench  # Open sluices
drop wrench
```

### Trip 6: Temple & Hades (Moves 111-150)
```
s                   # Deep Canyon
d                   # Loud Room (skip bar for now)
w                   # Round Room (can't go through here right now because it's too loud)
se                  # Engravings Cave
e                   # Dome Room
tie rope to railing
d                   # Torch Room
take torch          # Treasure + light
turn off lamp       # Save battery
s                   # Temple
take bell
s                   # Altar
take book
take candles
d                   # Cave
d                   # Entrance to Hades
ring bell           # Drops & heats
take candles        # Pick up again
light match
light candles with match
read book           # Exorcism complete!
drop book
s                   # Land of Living Dead
take skull          # Treasure
n                   # Back
u                   # Cave
n                   # Mirror Room
touch mirror        # Teleport north
```

### Trip 7: Mine & Diamond (Moves 151-200)
```
n                   # Squeaky Room area
w
n
w
n                   # Bat Room (garlic protects)
take jade           # Treasure
e                   # Shaft Room
put torch in basket
put screwdriver in basket
turn on lamp
n                   # Smelly Room
d                   # Gas Room
take bracelet       # Treasure
[Navigate coal mine maze]
take coal
[Navigate back]
u                   # Gas Room
s                   # Shaft Room
put coal in basket
lower basket
[Navigate to Drafty Room via mine]
take torch
take coal
take screwdriver
s                   # Machine Room
open lid
put coal in machine
close lid
turn switch with screwdriver
open lid
take diamond        # Treasure
drop screwdriver
n                   # Drafty Room
put torch in basket
put diamond in basket
[Navigate back through mine]
s                   # Shaft Room
raise basket
take torch
take diamond
turn off lamp
```

### Trip 8: Reservoir Treasures (Moves 201-230)
```
[Navigate to Reservoir via mirror room]
n                   # Reservoir should be drained
take trunk          # Treasure
n
take trident        # Treasure
take pump
[Navigate to Dam Base]
inflate plastic with pump
drop pump
enter boat
launch
wait
wait
take buoy
e                   # Land at beach
leave boat
open buoy
take emerald        # Treasure
ne                  # Sandy Cave
dig sand
dig sand
dig sand
dig sand
take scarab         # Treasure
```

### Trip 9: Rainbow & Return (Moves 231-270)
```
sw                  # Beach
s                   # Aragain Falls
cross rainbow       # Should be solid from sceptre wave
sw                  # Canyon Bottom
[Navigate back to house]
```

### Trip 10: Kill Thief & Collect (Moves 271-300)
```
[Go to Thief's Treasure Room]
kill thief with knife
[repeat as needed]
take chalice        # Treasure
take canary         # Treasure (from opened egg)
take egg            # Treasure (empty shell)
```

### Trip 11: Canary Puzzle (Moves 301-320)
```
[Navigate to clearing/tree]
wind canary         # Songbird drops bauble
take bauble         # Treasure
```

### Trip 12: Final Deposits (Moves 321-350)
```
[Return to Living Room]
put all in case
[Get platinum bar from Loud Room]
echo
take bar            # Treasure
[Return to Living Room]
put bar in case     # 350 points!
```

### Endgame
```
read map            # Appears in case
[Go to Stone Barrow]
sw
enter barrow        # Victory!
```

## Known Issues to Watch

1. **Combat randomness** - Troll/thief fights may need multiple commands
2. **Candle timing** - Don't delay too long before exorcism
3. **Lantern battery** - Turn off when torch available
4. **Boat puncture** - Don't carry sword into boat
5. **Item limit** - Max 8 items carried at once
6. **Weight limit** - Heavy items like coffin limit what else you can carry

## Debugging Commands

Use these to debug issues:
- `$debug state` - Overview
- `$debug here` - Current room
- `$debug tree` - Object containment
- `$inspect <object>` - Deep object inspection
- `$goto <room>` - Teleport (testing only)
- `$purloin <object>` - Take any object (testing only)

## Target: Under 250 Moves

With optimal execution and some combat luck, this route should complete in approximately **220-240 moves**.
