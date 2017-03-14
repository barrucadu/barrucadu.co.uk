---
title: "ICFP: Retrospective"
description: Looking back at ICFP nearly one month later.
---

Almost a month ago now I was heading off to spend a week at ICFP and
related events, so here's a little trip report.

## Déjà Fu

My [Déjà Fu][] ([git][df-git]) talk went down pretty well, and there
is a [recording][] available. A few people asked questions or talked
to me afterwards.

One questioner seemed particularly bothered by the fact that
`modifyCRef` acts like `atomicModifyIORef`. I'm not sure what he
wanted me to say, but that's ok because [Ryan Newton][] then had very
favourable comments, and suggested that Déjà Fu could be the tool
around which all the deterministic parallelism libraries (like [Par][]
and [LVish][]) could unify around, which is awesome!

(Un?)Fortunately, doing this absolutely *requires* Déjà Fu to deal
with relaxed memory weirdness, as LVish needs
compare-and-swap. Fortunately I found a beautiful approach to solving
that problem today! Going to implement that next week.

Ryan and I have exchanged a couple of emails as we figure out the best
way to integrate the abstractions in Déjà Fu into other libraries, in
order to make them testable.

[Déjà Fu]: http://www.barrucadu.co.uk/publications/dejafu-hs15.pdf
[df-git]: https://github.com/barrucadu/dejafu
[recording]: https://youtu.be/jQCDa6WoFeY
[Ryan Newton]: http://www.cs.indiana.edu/~rrnewton/homepage.html
[Par]: https://hackage.haskell.org/package/monad-par
[LVish]: https://hackage.haskell.org/package/lvish 

## Search Party

The [Search Party][] ([git][sp-git]) poster that I submitted for the
Student Research Competition didn't go down so well. People seemed
interested that parallelising list comprehensions could speed things
up, but it seems that the judges in particular felt that there wasn't
really much novelty.

Unfortunately, there was no feedback given for entries which didn't
pass the poster round.

[Search Party]: http://www.barrucadu.co.uk/publications/searchparty-acmsrc15.pdf
[sp-git]: https://github.com/barrucadu/search-party

## Presentations I Particularly Liked

- *[Partial Aborts for Transactions via First-Class Continuations]* is
  about partially undoing transactions in STM, to avoid repeating lots
  of work. Before the talk, I had no idea how such a thing could be
  implemented soundly, so it was really enlightening.

- *[A Fast Compiler for NetKAT]* is about a compiler from a network
  control language using an abstract topology down to concrete
  programs for the actual switches to run.

- *Functional Pearl: A Smart View on Datatypes* is about extending
  data structures with additional constructors for performing
  traditionally expensive operations (such as concatenating lists) in
  constant time, and using smart patterns to rebalance the structure
  when it is deconstructed through pattern matching, giving better
  performance overall.

- *[Efficient Communication and Collection with Compact Normal Forms]*
  is about a way of storing data structures compactly in the heap to
  facilitate easy network transfer. An interesting approach as it
  doesn't implement a lot of common wisdom, and relies on virtual
  memory.

- *[Deja Fu: A Concurrency Testing Library for Haskell]* this was an
  amazing talk given by a true genius.

In addition, the [contest presentation][] was really fun.

[Partial Aborts for Transactions via First-Class Continuations]: https://youtu.be/mS5xsIdJUl8
[A Fast Compiler for NetKAT]: https://youtu.be/zn28Djdh6H8
[Efficient Communication and Collection with Compact Normal Forms]: https://youtu.be/0mYesXvsdcc
[Deja Fu: A Concurrency Testing Library for Haskell]: https://youtu.be/jQCDa6WoFeY
[contest presentation]: https://youtu.be/9hfolFJ9rlk

## Other Things

I asked both [Simon Peyton Jones][] and [Simon Marlow][] about
research internships. SPJ said he doesn't do much concurrency, but
wouldn't necessarily rule anything out. Simon Marlow said that he'll
be looking for a research intern at Facebook over the next year, and
will let me know when they start taking applications. In my follow-up
email to him I mentioned my prior work on garbage collection, as in
his talk in PLMW he said he would quite like to return the GHC's
parallel garbage collector at some point.

[Simon Peyton Jones]: http://research.microsoft.com/en-us/people/simonpj/
[Simon Marlow]: http://community.haskell.org/~simonmar/
