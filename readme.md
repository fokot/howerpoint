![](logo.png)

# Howerpoint - forget about Powerpoint!!

* **ghci** based presentation engine
* idea (no code) stolen from [REPLesent](https://github.com/marconilanna/REPLesent)

_It's only a single Haskell file!_

To run presentation create presentation as a text file.
Load Howepoint.hs to ghci, enable RecordWildCards and load presentation

    $ ghci Hoverpoint.hs
    Main> :set -XRecordWildCards
    Main> Presentation {..} <- loadPresentation "PATH-TO-PRESENTATION"

### Navigation commands
Command | Description
--- | ---
`h` | Help
`n` | Go to next slide
`p` | Go to previous slide
`nn i` | Advance i slides
`pp i` | Go back i slides
`g i` | Go slide i
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
\x | Foreground color, where x is one of: `b`lack, `r`ed, `g`reen, `y`ellow, `b`lue, `m`agenta, `c`yan, `w`hite


<!-- \X | Background color, where capital X is one of the same as above -->