rhythm
======

`rhythm` is a Haskell library to read/write/convert several rhythm game (Guitar
Hero, Rock Band, etc.) file formats. It is designed to cooperate with the `midi`
library by heavily making use of the `event-list` and `non-negative` libraries.

* `Data.Rhythm.Time` defines the three basic units for time. A rational number
holding a position or duration in `Beats` (quarter notes) is the most central.
Many file formats, for practicality, store such quarter note durations as
integral numerators (called `Ticks`) of a fixed denominator (called the
`Resolution`). Finally, objects can also be positioned in real time,
independent of a tempo, by storing a rational value of `Seconds`.

* Also in `Data.Rhythm.Time`, `Tempo`s and `TimeSignature`s are defined in the
obvious ways, and can be used to convert between the various timekeeping units
(for example, a `Tempo` can convert positions between `Beats` and `Seconds`).

* Many format share a common structure: some events are instant "points" which
have no duration, and other events have begin and end points. The `Event` type
encapsulates this idea. It can be used in one of two ways. You can have a list
of `Event Bool`, in which case the on and off events are stored as two separate
events -- this is how standard MIDI files work. Or, you can have a list of
`Event a` for some numeric type `a`, in which case you have a single event
which directly stores its length -- this is how FeedBack ".chart" files work.
