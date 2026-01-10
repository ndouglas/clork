# CLORK

```
 ░▒▓██████▓▒░░▒▓█▓▒░      ░▒▓██████▓▒░░▒▓███████▓▒░░▒▓█▓▒░░▒▓█▓▒░
░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░     ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
░▒▓█▓▒░      ░▒▓█▓▒░     ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
░▒▓█▓▒░      ░▒▓█▓▒░     ░▒▓█▓▒░░▒▓█▓▒░▒▓███████▓▒░░▒▓███████▓▒░
░▒▓█▓▒░      ░▒▓█▓▒░     ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░     ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
 ░▒▓██████▓▒░░▒▓████████▓▒░▒▓██████▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
```

A faithful Clojure port of _Zork I: The Great Underground Empire_.

## Current Status

**The game is fully playable from start to finish.** All 19 treasures can be collected and deposited, and the game can be won by entering the Stone Barrow.

### What Works

- **All 110 rooms** - Complete map including the maze, coal mine, and all special areas
- **All 19 treasures** - Egg, chalice, painting, coffin, diamond, etc.
- **Combat system** - Troll and thief fights with ZIL-accurate mechanics
- **Major puzzles** - Loud room, dam, rainbow, cyclops, canary, coal mine machine
- **Thief AI** - The wandering thief who steals treasures and opens the egg
- **Light/dark mechanics** - Lantern, candles, and the dreaded grue
- **Full parser** - Multi-object commands, pronouns, disambiguation

### Running the Game

```bash
lein run
```

```
ZORK I: The Great Underground Empire
Infocom interactive fiction - a fantasy story
Copyright (c) 1981, 1982, 1983, 1984, 1985, 1986 Infocom, Inc. All rights reserved.
ZORK is a registered trademark of Infocom, Inc.
Release 1 / Serial number 1
Clojure port by Nathan Douglas

West of House
You are standing in an open field west of a white house, with a boarded front door.
There is a small mailbox here.

>
```

## Debug Commands

Clork includes a comprehensive debug system for development and testing. All commands start with `$`:

### State Inspection
- `$debug state` - High-level game state overview
- `$debug here` - Current room details
- `$inspect <object>` - Deep inspection with flag analysis
- `$where <object>` - Find where an object is located

### State Manipulation
- `$goto <room>` - Teleport to any room
- `$purloin <object>` - Take any object (bypass checks)
- `$flag <id> <flag>` - Set a flag
- `$scenario <name>` - Load test scenarios (e.g., `$scenario equipped`)

### Navigation
- `$path <from> <to>` - Find shortest path between rooms
- `$reachable <room>` - Show all rooms reachable from a location
- `$route <room1> <room2> ...` - Plan optimal route through multiple rooms

### Undo/Redo
- `$undo [n]` - Undo last command(s)
- `$redo [n]` - Redo undone command(s)
- `$history` - Show command history

## Speedrun Planner

The planner can generate complete command sequences for collecting treasures:

```
>$plan treasure egg
Planning for egg...

Plan for egg (12 total commands):
  take-egg (4 commands):
    - "ne"
    - "north"
    - "up"
    - "take egg"
  deposit-egg (8 commands):
    - "down"
    - "south"
    - "east"
    - "open window"
    - "in"
    - "west"
    - "open case"
    - "put egg in case"
```

### Planner Commands

- `$plan treasure <name>` - Plan to collect and deposit a specific treasure
- `$plan flag <flag>` - Plan to achieve a specific flag (e.g., `troll-flag`)
- `$plan kill-thief` - Plan the earliest possible thief kill
- `$plan win` - Generate complete speedrun (all 19 treasures + victory)

The planner uses:
- **Backward chaining** to find action sequences
- **TSP optimization** (Held-Karp algorithm) for efficient treasure ordering
- **Floyd-Warshall** for all-pairs shortest paths

Note: The current `$plan win` generates a working but unoptimized route (~720 commands). A well-optimized speedrun should be ~230-250 commands - optimization for batched treasure collection is planned.

## Testing

```bash
# Run all tests
lein test

# Run with pending tests (unimplemented features)
lein test :pending
```

The test suite includes 466 tests covering rooms, objects, verbs, combat, and game mechanics.

## ML Training API

Clork includes a structured API for training machine learning agents to play the game. Instead of generating free-form text, agents interact via structured actions and receive rich state observations.

### Quick Start

```bash
# Run in ML mode (JSON lines on stdin/stdout)
lein run --ml

# With reward signals for RL training
lein run --ml --ml-rewards
```

### Python Wrapper

A Python wrapper provides an OpenAI Gym-style interface:

```python
from clork_env import ClorkEnv

env = ClorkEnv(use_rewards=True)
obs = env.reset()

while not obs['game_over']:
    actions = env.valid_actions()  # No text generation needed!
    action = agent.select(actions)  # Your agent here
    obs, reward, done, info = env.step(action)

env.close()
```

### Action Format

Actions are structured data, not free-form text:

```python
{"verb": "go", "direction": "north"}
{"verb": "take", "direct-object": "lamp"}
{"verb": "put", "direct-object": "egg", "prep": "in", "indirect-object": "nest"}
```

### Reward Signals

With `--ml-rewards`, the API provides shaped rewards for training:

| Signal             | Default Weight | Description               |
| ------------------ | -------------- | ------------------------- |
| `score_delta`      | 1.0            | Change in game score      |
| `novel_room`       | 5.0            | First visit to a room     |
| `novel_message`    | 0.5            | New unique game output    |
| `object_taken`     | 2.0            | New object acquired       |
| `container_opened` | 1.5            | New container/door opened |
| `death`            | -10.0          | Player died               |

### Example: Random Agent

```bash
cd python
PYTHONPATH=. python example_random_agent.py --episodes 5 --steps 100

# Output:
# Episode 1: Reward=41.5, Rooms=8, Score=0
# Episode 2: Reward=35.0, Rooms=6, Score=0
# ...
```

See `python/clork_env.py` for the full API documentation.

## Project Structure

```
src/clork/
├── core.clj           # Main game loop and initialization
├── parser.clj         # Natural language parser
├── rooms.clj          # All 110 room definitions
├── objects.clj        # All object definitions
├── verbs.clj          # Verb handlers
├── combat.clj         # Combat mechanics
├── debug/             # Debug command system
│   ├── plan.clj       # Speedrun planner commands
│   ├── pathfind.clj   # Pathfinding commands
│   └── ...
├── planner/           # AI planning system
│   ├── actions.clj    # Action definitions and effects
│   ├── backward.clj   # Backward chaining planner
│   ├── optimizer.clj  # Route optimization
│   └── graph.clj      # Graph algorithms (Floyd-Warshall, TSP)
└── ml/                # Machine learning API

zork-i/                # Original ZIL source for reference
```

## Background

_Zork_ had a huge impact on me. I was a poor kid without a computer, but I could "check out" a computer from the school district - an Apple //e, I think - which came with a number of games and other software, including the _Zork_ trilogy.

I wasn't a terribly savvy gamer and not good at creating coherent maps (not that _Zork_ makes this easy) or recording my experiments, so a lot of my experience with the games was to load _Zork I_, play it until I was ready to throw the computer out the window, then repeat that with _Zork II_, _Zork III_, and then I was ready to go back to _Zork I_. I spent a lot of time too frustrated to do anything but rest my head on my arms while the Apple hummed steadily in the background.

I couldn't make head nor tails of a lot of the puzzles (though I muddled through many of them), but that often mattered less than you might think. I spent a lot of time just wandering around. Being young and naïve about video game tropes (so many of which this game invented!), I spent a lot of time in places like FOREST-3 - "a dimly lit forest, with large trees all around" but with "no tree... suitable for climbing," where "the rank undergrowth prevents eastward movement" and "storm-tossed trees block your way" south. I thought, perhaps, there was some way to get beyond this and go to other parts of the world, find other parts of the Great Underground Empire, etc.

I had to return that borrowed //e eventually, but there were other ways I could interact with _Zork_. I read the CYOA-esque books. I  wrote short stories that borrowed heavily from its magic system, its worldbuilding, and its unique blend of juvenile humor and wistful melancholia. And my earliest independent programming projects, once I finally got my own computer, were writing my own "interactive fiction" games in QBASIC, TADS, and Inform 5 or 6. Despite its age, substantial design issues, and various other limitations, _Zork_ is still a cornerstone of how I think about game design, storytelling, etc.

## Why Am I Doing This?

My professional experience as an engineer has almost entirely been with object-oriented programming. I've been interested for a long time in purely functional programming and functional languages, but haven't had much opportunity to engage.

At some point a few years back, I found that the original MDL and ZIL sources for _Zork_ had been released. So I did what I think any engineer with ties to _Zork_ would do - I jumped into the source to look at the parser. And recoiled in horror. The parser would be an absolutely gnarly, hairy beast in any language, but it's almost completely unreadable to me in ZIL.

Trying to read it was like trying to read Latin - I can figure out an individual word here and there, and then use those as context to figure out the meaning of another word here, and then zoom out to try to understand a sentence... but then I've forgotten what I guessed one of the words to mean, and so forth. I can get trapped in an expression, zooming in and out and moving forward and backward. And that's before trying to keep track of the nested parentheses, angle brackets, and so forth.

So, I thought, perhaps I might port it to a modern functional language with a similar Lisp-ish syntax - Clojure - that would be more likely to conform with modern programming language conventions (and that has a working compiler, syntax highlighting, automated testing tools, books I can read, etc). And so I could decode the arcane ZIL syntax and get some idea of how to transform it into (hopefully) idiomatic Clojure.

Another way to think about this, the one I prefer, is that I'm still exploring _Zork_, but in a new level. I'm once again an adventurer/archaeologist/"cretin", muddling around through an incredibly cryptic, deeply esoteric, unfair and unforgiving world. I'm taking each treasure, puzzle, room, trap, joke, and adversary, carefully extracting it from the webs and rust and impacted dirt, blowing the dust off, and installing it into a new trophy case.

Fortunately, at this point in my life, I have an excellent array of tools in my inventory.

## License

This is a fan project for educational purposes. Zork is a trademark of Activision.
