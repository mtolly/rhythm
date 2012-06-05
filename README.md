rhythm
======

`rhythm` is a Haskell library to read/write/convert several rhythm game (Guitar
Hero, Rock Band, etc.) file formats. It is designed to cooperate with the `midi`
library by heavily making use of the `event-list` and `non-negative` libraries.

The central file is Data.MusicTime, which defines several universal types for
timekeeping and describing rhythmic elements.

* There are three basic units for time. A rational number holding a position or
duration in `Beats` (quarter notes) is the most central. Many file formats, for
practicality, store such quarter note durations as integral numerators (called
`Ticks`) of a fixed denominator (called the `Resolution`). Finally, objects can
also be positioned in real time, independent of a tempo, by storing a rational
value of `Seconds`.

* `Tempo`s and `TimeSignature`s are defined in the obvious ways, and can be used
to convert between the various timekeeping units (for example, a `Tempo` can
convert positions between `Beats` and `Seconds`).

* Many formats read by this library share a common structure: there are events
that are "instant points" in time (with no duration), and there are events that
have a beginning and end point (stretching over some duration). The `TimeEvent`
datatype encapsulates this idea, and can be used to process such events in one
of two ways. "Duration" format means that a non-instant event is stored as a
single object, with a duration value attached. "Switch" format means that a
non-instant event is stored as two separate beginning and end events. MIDI is
inherently a "Switch" format -- notes are stored as note-on and note-off events.
FeedBack uses a "Duration" format -- a note is a single event of the form
`time = N fret len`, where `len` is the length of the note.
