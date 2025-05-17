# Benign

A library for benign effects in Haskell

_The current state is a proof of concept, it is not meant to be used
in actual projects nor do I guarantee any stability yet_

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
keep laziness for algorithms, but use evaluate more strictly when
assembling bigger steps. It's fine to log or trace in pure code, since
we don't consider that these observations are part of the semantics of
the program.

The Benign library provides facilities to create benign effects,
including evaluation strategies to express precisely how strict we
want to be.

The premise underlying all this, as well as the implementation of the
library, is that logging or tracing is not very fast. So we don't want
to log or trace in places where performance is really essential. This
is why at the most inner level, where tight loops and algorithms live,
laziness is not a problem: we are not going to log there, this would
cost too much performance. The library can, and does, prevent
optimisation through its functions anyway (note that cost-centre
profiling also prevents optimisation through cost centres; it isn't
surprising that we are having a similar problem). So benign effects
all must happen at rather macro steps, where we don't have to worry
too much about the impact of evaluation.

## Backends

Backends are shipped as separate package (with the exception of
backends which require no additional dependencies).

I've got to confess that it's a major pain in my delicately dignified
bottom. The language server works rather poorly across packages;
duplicating the Cabal files is somewhat annoying, what with the common
stanzas being lost and the version numbers being easier to get wrong,
I'll have to publish several packages every time I want to make a
release. I'm probably forgetting other issues. But until Hackage
supports multiple-library packages, it's the only solution that I have
to avoid pulling, say, Katip, when I want to use Opentelemetry.

You are welcome to write benign effects directly in your package. If
you wish to upstream one of my backends: I'll deprecate mine.

## Build

Build as
```shell
$ stack build
```

The formatter is Ormolu 0.5.0.1. Format with
```shell
$ just format
```

You can use Nix (specifically `nix-shell`) to provide all the
necessary dependencies (including a compatible Haskell Language
Server).

For reasons unknown to me, Ormolu is not cached in the pinned version
of Nix. I haven't had time to investigate, instead I'm using
Cachix. For the `nix-shell` to load fast, [install
cachix](https://docs.cachix.org/installation) and run (once per
machine) prior to building the Nix shell.

```shell
$ cachix use aspiwack
```

## Acknowledgement

Credits go to
- Alexis King for helping me come up with the implementation strategy
  based on `async` and thread ids.
- Thomas Bagrel for suggesting (in another context) the phrase
  “lexical state” to me. I don't know whether the phrase has been used
  before, a quick googling didn't turn up anything.
