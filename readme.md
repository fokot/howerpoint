![](logo.png)

# Howerpoint - forget about Powerpoint!!

* **ghci** terminal-based slide-show presentations that you can control from the REPL and execute code on slides
* idea (no code) stolen from [REPLesent](https://github.com/marconilanna/REPLesent)

_It's only a single Haskell file!_

[![asciicast](https://asciinema.org/a/42096.png)](https://asciinema.org/a/42096)

To run presentation create presentation as a text file.
Run `stack ghci` and have in the in the root of the project

    $ stack ghci
    Howerpoint> :loadPresentation PATH-TO-PRESENTATION

Good stating poitn is test presentations

    $ stack ghci
    Howerpoint> :loadTestPresentation

### Navigation commands
Command | Description
--- | ---
`:loadTestPresentation` | Load test presentation
`:loadPresentation <PATH-TO-YOUR-PRESENTATION>` | Load your presentation
`help` | Help
`n` | Go to next slide
`p` | Go to previous slide
`nn i` | Advance i slides
`pp i` | Go back i slides
`g i` | Go to slide i
`:rr` | Run code from slide
`resetSize` | Set the slide size to current terminal size
`setSize x y` | Set the slide size to x y
`showSize` | Show current slide size

<!--
Command | Shortcut | Symbolic alias | Description
--- | --- | --- | ---
`next` | `n` | `>` | Go to next build/slide
`previous` | `p` | `<` | Go back to previous build/slide
`redraw` | `z` | | Redraw the current build/slide
`Next` | `N` | `>>` | Go to next slide
`Previous` | `P` | `<<` | Go back to previous slide
`i next` | `i n` | | Advance i slides
`i previous` | `i p` | | Go back i slides
`i go` | `i g` | | Go to slide i
`first` | `f` | `|<` | Go to first slide
`last` | `l` | `>|` | Go to last slide
`Last` | `L` | `>>|` | Go to last build of last slide
`run` | `r` | `!!` | Execute code that appears on slide
`blank` | `b` | | Blank screen
`help` | `h` | `?` | This help message
-->


###ANSI colors

Escape code	| Result
--- | ---
\x | Foreground color, where x is one of: blac`k`, `r`ed, `g`reen, `y`ellow, `b`lue, `m`agenta, `c`yan, `w`hite
\X | Background color, where capital X is one of the same as above
\0 | Resets foreground and background colors to terminal default

###Code

All code is run in ghci so don't forget `let`s and `:m +` as imports etc.

**\>\>\>** show code on a slide and also the same code will be run

    >>>putStrLn . show $ double 5

**L\>\>** show code on a slide and when the code is run it prepends `let`

    L>>double x = x + x

**H\>\>** doesn't show code on a slide but the code is run. It's useful for imports

    H>>:m +Data.Char
