Netwire
=======

Netwire is a functional reactive programming (FRP) library with signal
inhibition.  It implements three related concepts, *wires*, *intervals*
and *events*, the most important of which is the *wire*.  To work with
wires we will need a few imports:

    import FRP.Netwire
    import Prelude hiding ((.), id)

The `FRP.Netwire` module exports the basic types and helper functions.
It also has some convenience reexports you will pretty much always need
when working with wires, including `Control.Category`.  This is why we
need the explicit `Prelude` import.

In general wires are generalized automaton arrows, so you can express
many design patterns using them.  The `FRP.Netwire` module provides a
proper FRP framework based on them, which strictly respects continuous
time and discrete event semantics.  When developing a framework based on
Netwire, e.g. a GUI library or a game engine, you may want to import
`Control.Wire` instead.


Introduction
------------

The following type is central to the entire library:

    data Wire s e m a b

Don't worry about the large number of type arguments.  They all have
very simple meanings, which will be explained below.

A value of this type is called a *wire* and represents a *reactive*
value of type $b$, that is a value that may change over time.  It may
depend on a reactive value of type $a$.  In a sense a wire is a function
from a reactive value of type $a$ to a reactive value of type $b$, so
whenever you see something of type `Wire s e m a b` your mind should
draw an arrow from $a$ to $b$.  In FRP terminology a reactive value is
called a *behavior*.

A constant reactive value can be constructed using `pure`:

    pure 15

This wire is the reactive value 15.  It does not depend on other
reactive values and does not change over time.  This suggests that there
is an applicative interface to wires, which is indeed the case:

    liftA2 (+) (pure 15) (pure 17)

This reactive value is the sum of two reactive values, each of which is
just a constant, 15 and 17 respectively.  So this is the constant
reactive value 32.  Let's spell out its type:

    myWire :: (Monad m, Num b) => Wire s e m a b
    myWire = liftA2 (+) (pure 15) (pure 17)

This indicates that $m$ is some kind of underlying monad.  As an
application developer you don't have to concern yourself much about it.
Framework developers can use it to allow wires to access environment
values through a reader monad or to produce something (like a GUI)
through a writer monad.

The wires we have seen so far are rather boring.  Let's look at a more
interesting one:

    time :: (HasTime t s) => Wire s e m a t

This wire represents the current local time, which starts at zero when
execution begins.  It does not make any assumptions about the time type
other than that it is a numeric type with a `Real` instance.  This is
enforced implicitly by the `HasTime` constraint.

The type of this wire gives some insight into the $s$ parameter.  Wires
are generally pure and do not have access to the system clock or other
run-time information.  The timing information has to come from outside
and is passed to the wire through a value of type $s$, called the *state
delta*.  We will learn more about this in the next section about
executing wires.

Since there is an applicative interface you can also apply `fmap` to a
wire to apply a function to its value:

    fmap (2*) time

This reactive value is a clock that is twice as fast as the regular
local time clock.  If you use system time as your clock, then the time
type $t$ will most likely be `NominalDiffTime` from `Data.Time.Clock`.
However, you will usually want to have time of type `Double` or some
other floating point type.  There is a predefined wire for this:

    timeF :: (Fractional b, HasTime t s, Monad m) => Wire s e m a b
    timeF = fmap realToFrac time

If you think of reactive values as graphs with the horizontal axis
representing time, then the `time` wire is just a straight diagonal line
and constant wires (constructed by `pure`) are just horizontal lines.
You can use the applicative interface to perform arithmetic on them:

    liftA2 (\t c -> c - 2*t) time (pure 60)

This gives you a countdown clock that starts at 60 and runs twice as
fast as the regular clock.  So it after two seconds its value will be
56, decreasing by 2 each second.


Testing wires
-------------

Enough theory, we wanna see some performance now!  Let's write a simple
program to test a constant (`pure`) wire:

    import Control.Wire
    import Prelude hiding ((.), id)

    wire :: (Monad m) => Wire s () m a Integer
    wire = pure 15

    main :: IO ()
    main = testWire (pure ()) wire

This should just display the value 15.  Abort the program by pressing
Ctrl-C.  The `testWire` function is a convenience to examine wires.  It
just executes the wire and continuously prints its value to stdout:

    testWire ::
        (MonadIO m, Show b, Show e)
        => Session m s
        -> (forall a. Wire s e Identity a b)
        -> m c

The type signatures in Netwire are known to be scary. =) But like most
of the library the underlying meaning is actually very simple.
Conceptually the wire is run continuously step by step, at each step
increasing its local time slightly.  This process is traditionally
called *stepping*.

As an FRP developer you assume a continuous time model, so you don't
observe this stepping process from the point of view of your reactive
application, but it can be useful to know that wire execution is
actually a discrete process.

The first argument of `testWire` needs some explanation.  It is a recipe
for state deltas.  In the above example we have just used `pure ()`,
meaning that we don't use anything stateful from the outside world,
particularly we don't use a clock.  From the type signature it is also
clear that this sets `s = ()`.

The second argument is the wire to run.  The input type is quantified
meaning that it needs to be polymorphic in its input type.  In other
words it means that the wire does not depend on any other reactive
value.  The underlying monad is `Identity` with the obvious meaning that
this wire cannot have any monadic effects.

The following application just displays the number of seconds passed
since program start (with some subsecond precision):

    wire :: (HasTime t s) => Wire s () m a t
    wire = time

    main :: IO ()
    main = testWire clockSession_ wire

Since this time the wire actually needs a clock we use `clockSession_`
as the second argument:

    clockSession_ ::
        (Applicative m, MonadIO m)
        => Session m (Timed NominalDiffTime ())

It will instantiate $s$ to be `Timed NominalDiffTime ()`.  This type
indeed has a `HasTime` instance with $t$ being `NominalDiffTime`.  In
simpler words it provides a clock to the wire.  At first it may seem
weird to use `NominalDiffTime` instead of something like `UTCTime`, but
this is reasonable, because time is relative to the wire's start time.
Also later in the section about switching we will see that a wire does
not necessarily start when the program starts.


Constructing wires
------------------

Now that we know how to test wires we can start constructing more
complicated wires.  First of all it is handy that there are many
convenience instances, including `Num`.  Instead of `pure 15` we can
simply write `15`.  Also instead of

    liftA2 (+) time (pure 17)

we can simply write:

    time + 17

This clock starts at 17 instead of zero.  Let's make it run twice as
fast:

    2*time + 17

If you have trouble wrapping your head around such an expression it may
help to read `a*b + c` mathematically as $a(t) b(t) + c(t)$ and read
`time` as simply $t$.

So far we have seen wires that ignore their input.  The following wire
uses its input:

    integral 5

It literally integrates its input value with respect to time.  Its
argument is the integration constant, i.e. the start value.  To supply
an input simply compose it:

    integral 5 . 3

Remember that `3` really means `pure 3`, a constant wire.  The integral
of the constant 3 is $3 t + c$ and here $c = 5$.  Here is another
example:

    integral 5 . time

Since `time` denotes $t$ the integral will be $\frac{1}{2} t^2 + c$,
again with $c = 5$.  This may sound like a complicated, sophisticated
wire, but it's really not.  Surprisingly there is no crazy algebra or
complicated numerical algorithm going on under the hood.  Integrating
over time requires one addition and one division each frame.  So there
is nothing wrong with using it extensively to animate a scene or to move
objects in a game.

Sometimes categorical composition and the applicative interface can be
inconvenient, in which case you may choose to use the arrow interface.
The above integration can be expressed the following way:

    proc _ -> do
        t <- time -< ()
        integral 5 -< t

Since `time` ignores its input signal, we just give it a constant signal
with value `()`.  We name time's value $t$ and pass it as the input
signal to `integral`.


Intervals
---------

Wires may choose to produce a signal only for a limited amount of time.
We refer to those wires as intervals.  When a wire does not produce,
then it *inhibits*.  Example:

    for 3

This wire acts like the identity wire in that it passes its input signal
through unchanged:

    for 3 . "yes"

The signal of this wire will be "yes", but after three seconds it will
stop to act like the identity wire and will inhibit forever.

When you use `testWire` inhibition will be displayed as "I:" followed by
a value, the *inhibition value*.  This is what the $e$ parameter to
`Wire` is.  It's called the *inhibition monoid*:

    for :: (HasTime t s, Monoid e) => t -> Wire s e m a a

As you can see the input and output types are the same and fully
polymorphic, hinting at the identity-like behavior.  All predefined
intervals inhibit with the `mempty` value.  When the wire inhibits, you
don't get a signal of type $a$, but rather an inhibition value of type
$e$.  Netwire does not interpret this value in any way and in most cases
you would simply use `e = ()`.

Intervals give you a very elegant way to combine wires:

    for 3 . "yes" <|> "no"

This wire produces "yes" for three seconds.  Then the wire to the left
of `<|>` will stop producing, so `<|>` will use the wire to its right
instead.  You can read the operator as a left-biased "or".  The signal
of the wire `w1 <|> w2` will be the signal of the leftmost component
wire that actually produced a signal.  There are a number of predefined
interval wires.  The above signal can be written equivalently as:

    after 3 . "no" <|> "yes"

The left wire will inhibit for the first three seconds, so during that
interval the right wire is chosen.  After that, as suggested by its
name, the `after` wire starts acting like the identity wire, so the left
side takes precedence.  Once the time period has passed the `after` wire
will produce forever, leaving the "yes" wire never to be reached again.
However, you can easily combine intervals:

    after 5 . for 6 . "Blip!" <|> "Look at me..."

The left wire will produce after five seconds from the beginning for six
seconds from the beginning, so effectively it will produce for one
second.  When you animate this wire, you will see the string "Look at
me..." for five seconds, then you will see "Blip!" for one second, then
finally it will go back to "Look at me..." and display that one forever.


Events
------

Events are things that happen at certain points in time.  Examples
include button presses, network packets or even just reaching a certain
point in time.  As such they can be thought of as lists of values
together with their occurrence times.  Events are actually first class
signals of the `Event` type:

    data Event a

For example the predefined `never` event is the event that never occurs:

    never :: Wire s e m a (Event b)

As suggested by the type events contain a value.  Netwire does not
export the constructors of the `Event` type by default.  If you are a
framework developer you can import the `Control.Wire.Unsafe.Event`
module to implement your own events.  A game engine may include events
for key presses or certain things happening in the scene.  However, as
an application developer you should view this type as being opaque.
This is necessary in order to protect continuous time semantics.  You
cannot access event values directly.

There are a number of ways to respond to an event.  The primary way to
do this in Netwire is to turn events into intervals.  There are a number
of predefined wires for that purpose, for example `asSoonAs`:

    asSoonAs :: (Monoid e) => Wire s e m (Event a) a

This wire takes an event signal as its input.  Initially it inhibits,
but as soon as the event occurs for the first time, it produces the
event's last value forever.  The `at` event will occur only once after
the given time period has passed:

    at :: (HasTime t s) => t -> Wire s e m a (Event a)

Example:

    at 3 . "blubb"

This event will occur after three seconds, and the event's value will be
"blubb".  Using `asSoonAs` we can turn this into an interval:

    asSoonAs . at 3 . "blubb"

This wire will inhibit for three seconds and then start producing.  It
will produce the value "blubb" forever.  That's the event's last value
after three seconds, and it will never change, because the event does
not occur ever again.  Here is an example that may be more
representative of that property:

    asSoonAs . at 3 . time

This wire inhibits for three seconds, then it produces the value 3 (or a
value close to it) forever.  Notice that this is not a clock.  It does
not produce the current time, but the `time` at the point in time when
the event occurred.

To combine multiple events there are a number of options.  In principle
you should think of event values to form a semigroup (of your choice),
because events can occur simultaneously.  However, in many cases the
actual value of the event is not that interesting, so there is an easy
way to get a left- or right-biased combination:

    (at 2 <& at 3) . time

This event occurs two times, namely once after two seconds and once
after three seconds.  In each case the event value will be the
occurrence time.  Here is an interesting case:

    at 2 . "blah" <& at 2 . "blubb"

These events will occur simultaneously.  The value will be "blah",
because `<&` means left-biased combination.  There is also `&>` for
right-biased combination.  If event values actually form a semigroup,
then you can just use monoidal composition:

    at 2 . "blah" <> at 2 . "blubb"

Again these events occur at the same time, but this time the event value
will be "blahblubb".  Note that you are using two Monoid instances and
one Semigroup instance here.  If the signals of two wires form a monoid,
then wires themselves form a monoid:

    w1 <> w2 = liftA2 (<>) w1 w2

There are many predefined event-wires and many combinators for
manipulating events in the `Control.Wire.Event` module.  A common events
is the `now` event:

    now :: Wire s e m a (Event a)

This event occurs once at the beginning.


Switching
---------

We still lack a meaningful way to respond to events.  This is where
*switching* comes in, sometimes also called *dynamic switching*.  The
most important combinator for switching is `-->`:

    w1 --> w2

The idea is really straightforward:  This wire acts like `w1` as long as
it produces.  As soon as it stops producing it is discarded and `w2`
takes its place.  Example:

    for 3 . "yes" --> "no"

In this case the behavior will be the same as in the *intervals*
section, but with two major differences:  Firstly when the first
interval ends, it is completely discarded and garbage-collected, never
to be seen again.  Secondly and more importantly the point in time of
switching will be the beginning for the new wire.  Example:

    for 3 . time --> time

This wire will show a clock counting to three seconds, then it will
start over from zero.  This is why we usually refer to time as *local
time*.

Recursion is fully supported.  Here is a fun example:

    netwireIsCool =
        for 2 . "Once upon a time..." -->
        for 3 . "... games were completely imperative..." -->
        for 2 . "... but then..." -->
        for 10 . ("Netwire 5! " <> anim) -->
        netwireIsCool

      where
        anim =
            holdFor 0.5 . periodic 1 . "Hoo..." <|>
            "...ray!"
