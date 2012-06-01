rhythm-game
===========

Haskell library to read/write/convert several rhythm game (Guitar Hero, Rock Band, etc.) file formats.

The event-list library is used heavily, specifically the Data.EventList.Relative.TimeBody type.

The key file is Data.MusicTime, which defines a couple of key types:

* Time is kept in three units: beats/quarter-notes (as a Rational), ticks (beats which are stored
as numerators of a fixed denominator), and real time in seconds.

* Tempos and time signatures are defined in the obvious ways.

* All the formats read by this library share a common structure: there are events that are "instant points" in time (taking up no duration), and there are events that have an inherent duration to them, a start/end point. To this end, a TimeEvent type is defined with two such constructors. This can be used in one of two ways: in "switch" format, you parametrize the TimeEvent type with Bool, and duration events are stored as two separate events, a beginning (with value True) and an end (with value False). In "duration" format, you parametrize the TimeEvent type with the numeric type used by the EventList for timekeeping, and then a duration event is stored as a single event that remembers its duration directly.
