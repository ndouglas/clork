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

An attempt to remake _Zork_ in Clojure.

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

Fortunately, at this point in my life, I have an excellent array of tools in my inventory 🙂
