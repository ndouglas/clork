# Automated Speedrun Route Planner

This document describes the GOAP (Goal-Oriented Action Planning) system for automatically discovering optimal speedrun routes in Clork.

## Overview

The planner works by:
1. Extracting declarative actions from game state (movement, take, deposit, puzzle)
2. Backward-chaining from the win condition to initial state
3. Handling inventory constraints, one-way paths, hazards, and dependencies

**Win Condition:** 350 points (all 19 treasures in trophy case), player enters Stone Barrow

## Architecture

```
src/clork/planner/
├── actions.clj      # Action schema, extraction, puzzle definitions
├── constraints.clj  # Inventory, light, hazards, combat, item utility
├── backward.clj     # Backward-chaining planner
└── optimizer.clj    # Route optimization, command generation

src/clork/planner.clj  # Main orchestration, debug commands
```

## Action Schema

Each action has:
```clojure
{:id          :action-id
 :type        :movement | :take | :puzzle | :combat | :deposit
 :preconditions
   {:here      :room-id        ; must be in this room
    :inventory #{:items}       ; must have these items
    :flags     #{:flags}}      ; these flags must be set
 :effects
   {:flags-set     #{:flags}   ; flags to set
    :flags-clear   #{:flags}   ; flags to clear
    :inventory-add #{:items}   ; items added
    :inventory-remove #{:items}} ; items removed
 :cost         1               ; estimated moves
 :reversible?  true            ; can action be undone?
 :commands     ["cmd" ...]}    ; actual game commands
```

## Constraint System

### Inventory
- Max weight: 100 (ZIL LOAD-ALLOWED)
- `can-carry?` checks weight limits
- `suggest-drops` recommends items to drop when full

### Light Requirements
- Dark rooms require `:brass-lantern` or `:ivory-torch`
- `requires-light?` / `has-light?` functions

### Hazards
- **Gas room**: Torch/candles/match = explosion (lantern is safe)
- **Bat room**: Need garlic or items get stolen

### Combat Score
- Player fight-strength: `2 + (score/70)`, ranges 2-7
- Thief strength: 5
- Reliable thief kill: 280+ points (strength 6 vs 5)
- Troll strength: 2 (beatable early game)

### Item Utility
- Tracks when items are no longer needed
- `droppable-items` finds items safe to discard

## Key Puzzles

### Troll
- Location: `:troll-room`
- Requires: `:sword`, light
- Effect: Sets `:troll-flag`, unlocks maze/dam areas

### Cyclops
- Location: `:cyclops-room`
- Command: "ulysses"
- Effect: Sets `:cyclops-flag`, `:magic-flag`, unlocks treasure room

### Exorcism
- Location: `:entrance-to-hades`
- Requires: `:bell`, `:book`, `:candles`, `:matchbook`
- Commands: ring bell, light candles, read book
- Effect: Sets `:lld-flag`, unlocks Land of Living Dead

### Dam
- Location: `:maintenance-room`
- Requires: `:wrench`
- Command: "turn bolt with wrench" (once)
- Effect: Sets `:dam-opened`, eventually `:low-tide`

### Loud Room
- Location: `:loud-room`
- Requires: `:low-tide` (or room ejects you)
- Command: "echo"
- Effect: Sets `:loud-flag`, reveals platinum bar

### Rainbow
- Location: `:end-of-rainbow`
- Requires: `:sceptre`
- Command: "wave sceptre"
- Effect: Sets `:rainbow-flag`, get pot of gold

### Egg (Thief Interaction)
- Player CANNOT open egg safely (any attempt breaks it)
- Must give egg to thief (or let him steal it)
- Thief deposits egg in treasure room, opens it (sets `:egg-solve`)
- Kill thief, take opened egg, canary is inside

### Thief Combat
- Location: `:treasure-room`
- Requires: Weapon (sword or nasty-knife)
- Nasty knife is more effective (thief's "best weapon")
- Score 280+ recommended for reliable success
- Drops: chalice, stiletto, stolen items

### Coal Mine (Complex!)
1. Put torch + screwdriver in basket at shaft-room
2. Lower basket (provides light at lower-shaft)
3. Navigate with LANTERN ONLY (torch explodes in gas room)
4. Get coal from coal-mine-4
5. Return to shaft-room via gas room
6. Put coal in basket, lower it
7. Navigate to ladder-bottom
8. DROP ALL to fit through narrow crack
9. In lower-shaft: take items from basket
10. Put coal in machine, close, turn switch with screwdriver
11. Open machine, take diamond
12. Put diamond in basket, raise it
13. Return through crack, collect items
14. Get diamond and torch from basket

**Critical Constraints:**
- Gas room: flame = death (use lantern only)
- Bat room: need garlic
- Narrow crack: must be empty-handed
- Lower shaft: dark unless basket has torch

## One-Way Paths

These paths cannot be traversed in reverse:
- Maze diodes: maze-2→4, maze-7→dead-end-1, maze-9→11, maze-12→5
- Slide room → cellar (collect diamond/scarab first!)
- Boat: only goes downstream

## Flag Dependencies

```
:won ← 350 points ← all treasures in case
:have-skull ← :lld-flag ← exorcism ← bell, book, candles, matches
:have-torch ← :dome-flag ← rope tied ← :troll-flag
:rainbow-flag ← wave sceptre
:low-tide ← :dam-opened ← turn bolt with wrench
:thief-dead ← combat ← :cyclops-flag ← "ulysses" ← :troll-flag
```

## Usage

### REPL
```clojure
(require '[clork.planner :as planner])
(require '[clork.planner.actions :as actions])
(require '[clork.planner.constraints :as constraints])

;; Initialize game state
(def gs (-> (gs/initial-game-state)
            (gs/add-rooms rooms/all-rooms)
            (gs/add-objects objects/all-objects)))

;; Build action registry
(def registry (actions/build-action-registry gs))
(actions/summarize-registry registry)

;; Debug specific action
(actions/print-action (get registry :kill-troll))

;; Check combat feasibility
(constraints/combat-difficulty :thief 200)  ; => :risky
(constraints/combat-difficulty :thief 280)  ; => :advantage

;; Create test goals
(constraints/minimal-goal :painting)
(constraints/flag-goal :troll-flag)
(constraints/location-goal :treasure-room :flags #{:cyclops-flag})
```

### Generate Speedrun
```clojure
(planner/generate-speedrun gs)
```

## Testing

```bash
lein test clork.planner-test
```

## Future Enhancements

1. **Score tracking** - Track accumulated score during planning for combat validation
2. **Battery optimization** - Insert lamp on/off commands
3. **Wait interleaving** - Do other tasks while waiting for dam to drain
4. **Thief randomness** - Model probabilistic thief behavior
5. **Full backward chaining** - Complete win condition planning
