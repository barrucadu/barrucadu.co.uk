---
title: Haskell Systematic Concurrency Testing
description: What systematic concurrency testing (SCT) is, and how we can apply it to Haskell.
---

Concurrency is hard, because the execution of a concurrent program may
depend implicitly on the order in which things are scheduled on the
processor, which is---or should be---from the point of view of the
concurrent program, totally nondeterministic and unpredictable. We
call such dependencies on ordering *race conditions*, a simple example
is something like this:

~~~~{.haskell}
bad :: IO Int
bad = do
  shared <- newEmptyMVar
  forkIO $ tryPutMVar shared 1
  forkIO $ tryPutMVar shared 2
  readMVar shared
~~~~

This is obviously a trivial example, but the output will be either `1`
or `2` depending on the order of interleaving. We can't write a
function like that *not* wrapped in `IO`, or something similar,
because it would not be referentially transparent!

The problem gets even worse if we have some complex shared data
structure, and threads manipulating different parts of it. If the
structure isn't made explicitly safe for concurrent modification,
invariants may get broken, changes may be lost, nasal demons could
break free into this world.

Sadly, we can't really fix the problem with traditional testing
methods, like unit testing or property-based testing, as the result of
a concurrent program depends on something that is, traditionally,
outside of the control of the testing environment.

## *Systematic* Concurrency Testing

Systematic concurrency testing (SCT) is the process of testing a
concurrent program, whilst being explicitly aware of the scheduling
decisions made, and forcing schedules which are "interesting" in some
sense. The heart of the matter is taking control from the scheduler,
and giving it back to the testing environment.

SCT is by no means new, it's already been done for (at least) Java and
C+pthreads. I think the Java approach is very nice, so I'll expand on
it a little: because Java programs can arbitrarily rewrite their own
bytecode at runtime, and because the bytecode is at a fairly high
level of abstraction, a tool has been produced which walks through the
bytecode of a concurrent test case, forcing all threads to be run in a
co-operative multi-tasking manner, with explicit yields inserted
before operations on shared state. The test runner can then force
particular scheduling decisions to be made when the yield calls are
executed, and so explore schedules. C+pthreads programs can be
modified similarly.

An important insight mentioned briefly in that last paragraph is that
yields only happen before operations on shared state. A naive approach
might simply explore all interleavings, but this leads to a massive
state-space explosion. We can reduce this significantly by breaking
programs up at a higher level of abstraction than individual
instructions. The breaks used are *effectively-atomic blocks*, which
are a sequence of instructions which cannot be effected by the
interleaving. For example, operations on thread-local state are
effectively-atomic. Reading a shared variable is not.

Ok, so we know in principle how to do SCT, by overriding the
concurrency primitives during testing and using our own
implementations to retain control, but how do we go about doing this
systematically?

One approach is just to schedule randomly. This isn't very systematic,
but with enough runs it turns out to be surprisingly effective. A
better approach is *pre-emption bounding*.

Pre-emption bounding imposes a partial order over the set of all
schedules, which can then be used to explore them
systematically. Firstly, some terminology:

- Blocking: Waiting on some resource which will become available in
  the future.

- Pre-emption: Interrupting a thread and passing control to another,
  even if the first thread is not blocked.

- Pre-emption count: The number of pre-emptions in a particular
  schedule.

Pre-emption bounding works by, as the name would imply, bounding the
pre-emption count of a schedule. Schedules with a pre-emption bound of
0 never pre-empt, they run a thread until it is blocked, and then pick
another. There may be multiple schedules with a pre-emption count of
0, if there are multiple choices available for which thread to
run. *Iterative pre-emption bounding* is an approach for gradually
exploring all schedules: firstly, try all schedules with a pre-emption
count of 0, then with a count of 1, ..., and so on. Given enough time,
this will eventually explore all schedules, and so can guarantee that
a concurrent program is free of race conditions.

However, we don't need to explore *all* schedules to be confident
there are no errors, empirical studies have found that many race
conditions are exposed with a pre-emption bound of 2, so just trying
that may be enough. If nothing else, pre-emption bounding lets us make
statements like "the program is guaranteed free of concurrency errors,
unless the scheduler engages in pathological behaviour (>x
pre-emptions)", whereas before we could not.

## [monad-conc][MC1]: Overloadable Concurrency Primitives for SCT

I looked around for an SCT tool for Haskell, and didn't really find
anything. There's a [blog post][MC2], which ultimately ended up being
the basis for my approach, but doesn't take it very far. Ideally, it
would be possibly to take an arbitrary `IO` action which makes use of
concurrency, and systematically test it, but that would require
runtime or compiler support, so a typeclass was the way to go.

`Par` ([monad-par][MC3]) is a monad for deterministic parallel
(concurrent) programming, and there is a pair of typeclasses in
[abstract-par][MC4] defining its interface abstractly. This is
basically what we want for SCT, but it's a bit restrictive - we need
*potentially* nondeterministic concurrent computations.

Introducing `Conc` ([monad-conc][MC1])! `Conc` is like `Par`, but with
one key difference: in `Par` is is illegal to write multiple times to
the same shared variable. In `Conc` it is not. That single change is
all we need (with the semantics for what happens on writing to a
"full" value) in order to model interesting things.

Specifically, the typeclasses are these:

~~~~{.haskell}
class Monad m => ConcFuture future m | m -> future where
  spawn    :: m a -> m (future a)
  readCVar :: future a -> m a

class ConcFuture cvar m => ConcCVar cvar m | m -> cvar where
  fork         :: m () -> m ()
  newEmptyCVar :: m (cvar a)
  putCVar      :: cvar a -> a -> m ()
  tryPutCVar   :: cvar a -> a -> m Bool
  takeCVar     :: cvar a -> m a
  tryTakeCVar  :: cvar a -> m (Maybe a)
~~~~

`ConcFuture` is basically `ParFuture`, but it removes some `NFData`
constraints (`Par`'s speed-up comes from fully-evaluating things in
separate threads). The programming model enabled by this is to start
something evaluating in another thread, which returns a *future
result*, and then blocking on that future result to get the final
value.

`ConcCVar` is the powered-up `ParIVar` class, for things which support
writing to shared variables. This is where the potential
nondeterminism creeps in, and the functions should look very familiar
to anyone who has used `MVar`s before.

It's not just a pipe-dream, I have a `Conc` monad which uses a
user-supplied scheduler, and interrupts computations every time a
primitive from the typeclasses is used. On top of that I have a
testing framework which performs multiple runs of a computation,
gathering the final values into a list, along with scheduling decision
and execution traces, allowing for debugging. Currently the only
scheduler working for that is a random one, but I have an idea of how
to implement a pre-emption bounding scheduler.

The source is on github, there are a bunch of small tests, which may
be of interest, and I plan to put it up on Hackage as soon as I have a
better scheduler.

So what's the take-home message from all this? Well, I guess it's to
use `Par` if you can, because it's guaranteed to be deterministic. But
if you need multiple-write shared variables, then `Conc` is, I think,
a viable alternative with the potential for a good testing framework
to be built up around it.

[MC1]: https://github.com/barrucadu/monad-conc
[MC2]: http://kukuruku.co/hub/haskell/haskell-testing-a-multithread-application
[MC3]: https://hackage.haskell.org/package/monad-par-0.3.4.7/docs/Control-Monad-Par.html
[MC4]: https://hackage.haskell.org/package/abstract-par-0.3.3/docs/Control-Monad-Par-Class.html

## Further Reading

- [Concurrency Testing Using Schedule Bounding: an Empirical Study][F1]
- [Finding Race Conditions in Erlang with QuickCheck and PULSE][F2]

[F1]: https://wp.doc.ic.ac.uk/afd/paper_ppopp2014/
[F2]: http://www.cse.chalmers.se/~nicsma/papers/finding-race-conditions.pdf
