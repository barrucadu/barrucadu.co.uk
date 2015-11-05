Sometimes I come across something online, or in conversation, and I
think "wow, that sounds like an interesting topic, but it's a little
off the path I intend my research to take; at least in the foreseeable
future".

I have decided to amass these ideas here, on my website. If you are
working on any of these topics, I'd love to hear from you!

## QuickSpec for Concurrent APIs

[QuickSpec](https://hackage.haskell.org/package/quickspec) is a really
magical Haskell library which can generate equational laws about a
collection of functions, making use of random testing to determine if
these "laws" hold.

Given that we can now test concurrent Haskell programs, with my work
on [Déjà Fu](https://github.com/barrucadu/dejafu), it'd be cool to try
to produce a QuickSpec-like thing which can generate properties of
concurrent APIs.

The problem is that interesting properties about concurrently executed
actions tend to be much *bigger* than the little equational laws
QuickSpec generates. Perhaps some sort of templating approach where,
say, an API for a concurrent data structure is categorised into
"initialisation", "reading", and "writing" operations, which are
plugged in to a family of standard template-properties; rather than
generating properties completely from scratch.

## Seamless migration between concurrent single-machine processes and distributed systems

One week, my supervisor asked me if the concurrency abstraction I've
created for use in Déjà Fu could be applied to distributed systems.

That is, could you write a concurrent program, systematically test it,
and then be able to run it (with the same semantics) across a network
of machines *for free*? I'm pretty certain the answer is yes; the
challenge is mapping a concurrent program to an *efficient*
distributed system, which does not really correspond to any of the
three currently supported memory models.

[Alexey Gotsman](http://dblp.uni-trier.de/pers/hd/g/Gotsman:Alexey)
has done some work in the topic of modelling distributed systems with
relaxed memory models. See the POPL 2014 paper.

## Rely/Guarantee as Refinement Typing

Rely/Guarantee logic is a way of reasoning about concurrent processes
and how they deal with interference from other processes executing in
parallel. A *rely* is a predicate characterising the sorts of
interference from the environment that the process can tolerate. A
*guarantee* is a restriction on the sorts of effects that the process
can have.

Put like that, guarantees sound an awful lot like refinement types: a
restriction of what would otherwise be allowed by some predicate. I'm
sure relies could also be mapped into some sort of type-level entity.

[Ian Hayes](http://staff.itee.uq.edu.au/ianh/) has done some work on
considering relies and guarantees separately, which would probably be
of use here. See the UTP 2014 paper.
