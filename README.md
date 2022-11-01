# Benign

A library for benign effects in Haskell

## Philosophy

A core tenet of Haskell programming is “pure core, imperative
shell”. We write most of our programs in pure style, and a small outer
layer in imperative shell. This is great for correctness, but it comes
at a great cost for observability. Because logging, tracing, or
measuring time are all considered effects, we can't log, trace or
measure time in pure code.

This means that most of our code is traditionally unobservable. There
are some built-in facilities in GHC, such as cost-centre
profiling. But besides putting pressure on the compiler to build these
tools, these facilities are out of the language, hence not
programmable in anyway way.

The Benign library is an attempt to address this gap. A difficulty is
laziness. With laziness, things have a beginning (when the thunk is
being forced), but not a well-defined end. So what do we measure the
time of? The solution of the Benign library is to be less lazy. We
keep laziness for algorithms, but use strict (or at least stricter)
types to assemble bigger steps. It's fine to log or trace in pure
code, since we don't consider that these observations are part of the
semantics of the program.

The Benign library provides facilities to create benign effects,
including to use strict types to assemble these large steps.

The premise underlying all this, as well as the implementation of the
library, is that logging or tracing is not very fast. So we don't want
to log or trace in places where performance is really essential. This
is why at the most inner level, where tight loops and algorithms live,
laziness is not a problem: we are not going to log there, this would
cost too much performance. At a more outer level, we can use
strictness to make steps with a beginning and an end. It's ok if there
is a cost in terms of conversion, this is not where we need to
optimise too much. The library can, and does, prevent optimisation
through its functions (note that cost-centre profiling also prevents
optimisation through cost centres; it isn't surprising that we are
having a similar problem). With the optimisation not working at that
level, strictness is much less of a problem.

## Build

Build as
```shell
$ stack build
```

Format with Ormolu 0.5.0.1

You can use Nix (specifically `nix-shell`) to provide all the
necessary dependencies (including a compatible Haskell Language
Server).


