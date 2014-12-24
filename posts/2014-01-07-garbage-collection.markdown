---
title: Garbage Collection
description: An overview of the major different types of garbage collector.
---

My final year project is on the correctness of garbage collectors,
this post is largely derived from parts of my literature review. The
dissertation (thus far), can be seen on GitHub.

To start with, we're going to need to know what we're talking about,
so let's introduce a couple of terms:

- A *garbage collector* is an algorithm which scans the heap for
  unused memory cells, and frees them. It may also do some
  reorganisation of live cells (such as compaction) as it does so, in
  order to improve memory access characteristics of the program.

- A *mutator* is a program which has its heap garbage collected. It is
  so called because it mutates the heap, which the garbage collector
  has just (so kindly) organised for it.

Good! Now I'm going to introduce the most common types of garbage
collector, and then conclude by talking a little about what it means
for a garbage collector to be correct (which is what I'm currently
struggling with in my project).

## Types of Garbage Collector

We can break down most garbage collectors into the reference counting,
mark-sweep, copying, and mark-compact families. More advanced
collectors are typically variants on one or more of these themes,
rather than something totally new.

### Reference Counting

In a reference counted system, all cells have an integer tag
associated with them, the "reference count". This is (in the most
basic incarnation) a count of how many things point to that cell, and
the compiler or language runtime ensures that it is updated as
necessary.

As the count is updated in real-time during the execution of the
mutator, if the count falls to zero, the cell can immediately be
freed. This is a very simple approach, and does lead to problems (such
as recursive freeing), but it works.

Unfortunately, reference counting cannot reclaim cyclic structures. If
you have two cells, A and B, pointing to each other, which are
unreachable, what is the reference count of both? It is one!
Unfortunately, this cannot be avoided, and this is why people
typically don't consider reference counting a garbage collector: it
operates purely locally, and does not consider the heap as a whole, as
other types of collector do.

### Mark-Sweep

Mark-sweep dates back from the days of the early Lisp systems, and
works by giving every cell a "mark bit", which is unset. When the
system runs out of memory, the mutator is frozen and garbage
collection begins: the heap is traversed by following pointers from
the roots (the variables currently in scope), and the mark bit of
every cell is set as it goes. Then, the entire heap is traversed, and
every cell without its mark bit set is freed. Finally, the mark bits
of the remaining cells are all reset.

This is totally different from reference counting! Garbage collection
happens when it is needed, rather than throughout the computation, and
it can reclaim any shape of structure; this seems great!

Unfortunately, mark-sweep is a "stop-the-world" collector, the mutator
is halted whilst garbage collection happens, and the entire heap must
be traversed in each collection.

### Copying

Copying collectors attack the problem from a different angle than
mark-sweep or reference counting. The heap is divided into two
"semispaces", and allocation only happens in one of them.

At garbage collection time, the mutator is halted, and the heap
traversed from the roots. All live cells are copied to the other
semispace, and garbage cells are just not touched. Then, the roles of
the semispaces are swapped, and the mutator resumes.

The immediate advantage over mark-sweep here is that garbage
collection time is only proportional to the size of the live portion
of the heap, as garbage is never touched. The downside is that copying
takes longer than just setting and unsetting a bit, so if the live
portion is very large, it may be very slow. Furthermore, there is a
space overhead of half the heap size, which is very costly indeed!

There is another advantage of copying collectors, however. They
improve memory locality: things get placed close together, at the
start of a semispace, with no gaps between them. This can reduce page
faults and cache misses, and so performance of the mutator may
actually improve following a garbage collection.

### Mark-Compact

Mark-compact collectors are like a combination of the ideas behind
mark-sweep and copying. During garbage collection live cells are
marked, then live cells are moved down to one end of the heap,
overwriting garbage cells as they go, then pointers are updated, and
control is returned to the mutator.

You might wonder why you'd want to use this over copying, or vice
versa. Well, unlike copying collectors, mark-compact collectors do not
have a space overhead of half the heap, which is a huge saving!
Unfortunately, they require multiple traversals of the heap to update
pointers correctly (copying collectors can take shortcuts, such as
leaving behind a forwarding pointer, which are not possible if the
entire heap is mutated during garbage collection), and so there is a
time overhead.

### Generational

Generational garbage collectors make use of empirical observations of
cell lifetimes: old cells tend to stay around for a while, whereas
young cells tend to die quickly. We can see why this is the case by
observing that most data structures in a program are either big,
important, long-lived things like global configuration, or small
temporary things like file handles and intermediary results.

Generational collectors make use of this by dividing the heap into a
number of "generations", where allocation only happens in the younger
generation. During garbage collection, all live cells in the younger
generation get promoted to the older generation (called "tenuring"),
and there they remain; simply collecting the younger generation is
called a "minor collection".

When the old generation fills up, the entire heap needs to be
collected by something like mark-sweep, copying, or mark-compact,
which is called a "major collection".

Generational collectors tend to work well for languages where
"old-to-young" pointers are rare, such as functional languages.

## What is correctness?

Now we get to the meat of my project: what does it mean for a garbage
collector to be correct? To put it simply (because I'm still working
out the details myself), a garbage collector should not mutate data
words, should "correctly" mutate pointers, and result in all garbage
cells being deallocated.

For mark-sweep collectors, this simplifies to not mutating anything,
and deallocating all garbage cells. We can simplify this further to
obligations on the marker and the sweeper: the marker must mark
exactly those cells which are allocated and reachable; the sweeper
must deallocate exactly those cells which are unmarked, and then
unmark all marked cells; and neither must mutate anything other than
the mark bits.

I plan to attack this problem a bit more rigorously by determining
what correctness for mark-sweep and copying collectors is
independently (which I have made some progress on), and then
abstracting from that a more general notion of correctness. At the
time of posting, I have a chapter on mark-sweep correctness with a
(slightly informal and incomplete) proof of correctness for the
Armstring/Virding collector, and have started to note down the
differences required in formalism to reason about copying collectors.

## Further Reading

- Garbage Collection: Algorithms for Automatic Dynamic Memory
  Management [Jones & Lins 1996]

- Recursive Functions of Symbolic Expressions, and Their Computation
  by Machine, Part I [McCarthy 1960]

- Generation scavenging: A non-disruptive high performance storage
  reclamation algorithm [Ungar 1984]

- One pass real-time generational mark-sweep garbage collection
  [Armstrong & Virding 1995]

- An efficient, incremental, automatic garbage collector
  [Deutsch & Bobrow 1976]

- A semi-incremental garbage collection algorithm [Hughes 1982]
