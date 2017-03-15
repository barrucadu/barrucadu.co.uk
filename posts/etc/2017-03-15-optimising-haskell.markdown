---
title: Optimising Haskell
---

This is the result of profiling the `dejafu-tests` executable at the [dejafu-0.5.1.2][0.5] tag,
there are 16 properties and 126 unit tests:

```
total time  =      142.78 secs   (142778 ticks @ 1000 us, 1 processor)
total alloc = 46,909,536,504 bytes  (excludes profiling overheads)
```

...and *this* is the result at the [dejafu-0.4.0.0][0.4] tag, with 16 properties and 70 unit tests:

```
total time  =      317.12 secs   (317123 ticks @ 1000 us, 1 processor)
total alloc = 125,356,348,208 bytes  (excludes profiling overheads)
```

Not only does dejafu-0.5.1.2 have more tests, those tests run in under half the time and memory.

The performance improvements mostly come from one commit fairly innocently entitled
"[Avoid some allocation][commit]". The commit message goes into quite a lot of detail, and in this
post I'll try to give some general guidelines for optimising your Haskell.


## Profile everything!

*All* changes you make should be profiled, otherwise you're just guessing, and it's really hard to
guess correctly when something as big and complex as GHC is involved.

If you're building with `stack`, profiling is simple:

```
$ stack build --profile
$ stack exec -- my-executable-name +RTS -p
```

There are similar flags for `cabal-install`.

GHC can also produce really awesome heap profiles, which show you visually where allocation is
happening. See [the GHC users' guide][hp].


## The best optimisation is a better algorithm

There's a famous Knuth quote about program optimisation: (emphasis mine)

> There is no doubt that the grail of efficiency leads to abuse. Programmers waste enormous amounts
> of time thinking about, or worrying about, the speed of noncritical parts of their programs, and
> these attempts at efficiency actually have a strong negative impact when debugging and maintenance
> are considered. **We should forget about small efficiencies**, say about 97% of the time:
> premature optimization is the root of all evil. Yet we should not pass up our opportunities in
> that critical 3%. A good programmer will not be lulled into complacency by such reasoning, he will
> be wise to look carefully at the critical code; but only after that code has been identified.

I strongly believe that attempting to optimise a program without first thinking long and hard about
the actual *algorithms* involved is focussing on these "small efficiencies", and the root of all
evil.

Of all the tweaks that led to the great improvement in that single commit, the bulk of the change by
far was two algorithmic changes (sadly, they were initially intended as algorithmic *optimisations*,
but I didn't profile, and so was wrong):

> - The removal of the `preEmps` stuff from `findSchedulePrefix`: [...]
> - The removal of the `pruneCommits` stuff from `sctBounded`: [...]

Only once you have decided that there are no more algorithmic improvements remaining that can be
done in a reasonable amount of work should you move on to optimising the *implementations* of those
algorithms.


## The second best optimisation is reduced allocation

Idiomatic Haskell isn't really written with regard to the memory usage of your program. We allocate
data willy-nilly, and trust that GHC will do a good enough job of sorting everything out for us and
giving us a fast program.

Unfortunately, modern computers have a lot of memory: this means it's easy to write a very
space-inefficient program, and just not notice! If all your data fits into memory, then your program
is likely to be very fast. Once you have an input large enough such that your program doesn't all
fit into memory, then you have problems.

Reducing allocation helps to avoid memory-exhaustion, it also helps to reduce the rate of garbage
collection, which means that more of the run-time of your program is actually *your program*, and
not the garbage collector.

For example:

- [Here](https://github.com/barrucadu/dejafu/commit/0fd9ea7613697a2a4e7ac79f0abded7116bdf20f#diff-053889956a11db18383539d8ff547015R594)
  I replaced the repeated construction of `Maybe` values by a reference to a value in a list, and a
  second base-case in a function.
- [Here](https://github.com/barrucadu/dejafu/commit/0fd9ea7613697a2a4e7ac79f0abded7116bdf20f#diff-35f4485df5881c60045dd988103d99d3R478)
  I avoided the construction of a list of intermediary values, by using a fold.


### Inlining and higher-order functions

GHC can avoid the allocation of single-constructor data types, like tuples, by using
the [worker/wrapper transformation][ww]. Given a function:

```haskell
f (x, y) = ...
```

GHC will realise that `f` is strict in its argument, and compile it like so:

```haskell
f z = case z of (x, y) -> f' x y   -- the "wrapper"
f' x y = ...                       -- the "worker"
```

And furthermore it will inline `f` everywhere. *Unfortunately*, we can't benefit from this trick
when `f` is being passed as a parameter to another function, as the definition of `f` can't be
inlined inside the higher-order function. This was the case in two places:

- [`incorporateTrace`](https://github.com/barrucadu/dejafu/commit/0fd9ea7613697a2a4e7ac79f0abded7116bdf20f#diff-d292b30dfbac46474c19daa1498c8866R137)
- [`findBacktrackSteps`](https://github.com/barrucadu/dejafu/commit/0fd9ea7613697a2a4e7ac79f0abded7116bdf20f#diff-d292b30dfbac46474c19daa1498c8866R199)

This needless allocation of tuples was wasting space, so I split them up. This change was a great
shame: I felt (and still feel) that the tuples really clarified the intent of the function
parameter, so while this is a win, it's one I'm not very happy with.

Sadly, without something approaching whole-program optimisation (or really pervasive inlining),
higher-order functions are a bit of an obstacle to optimisations like this.


### foldr / foldl / foldl'

`foldl'` used to be my go-to fold, but now `foldr` has the place of honour. There's
an [article on the HaskellWiki][folds] about the differences, but basically:

- `foldr` is good if your function is lazy in the right argument, as it's lazier than `foldl` in that
  case.
- `foldl'` is good if your result is a strict value (such as a number).
- `foldl` is pretty much never what you want.

Additionally, [GHC is very good at optimising `foldr`][foldr-build], thanks to rewrite rules.


### Avoid concatenating lists

We often use lists as control structures, where a list contains some collection of elements to
iterate over and process in some way. This leads to very clean code, but can also lead to very
inefficient code, where we allocate a lot of lists.

In particular, be careful when concatenating lists. Try applying a form of algorithmic optimisation:
do you even *need* to concatenate the lists? Or can you operate directly on the list of lists
itself?

- [`concatPartition`](https://github.com/barrucadu/dejafu/commit/0fd9ea7613697a2a4e7ac79f0abded7116bdf20f#diff-d292b30dfbac46474c19daa1498c8866R675),
  a function to do a `partition f . concat` without actually doing the concatenation.

If you *must* concatenate, bear in mind that `(++)` is linear (in both time and space) in its first
argument. If the order of the resulting list doesn't matter, and you have a good idea which argument
will be smaller, try putting the smaller first.


### Laziness is fickle

Laziness is really great for writing composable code, and I believe it is
the [correct choice for a language's default evaluation mechanism][strict-vs-lazy]. Unfortunately,
it makes reasoning about performance (both in time and space) much trickier.

*Once*, it was a significant win to
[force some evaluation](https://github.com/barrucadu/dejafu/commit/0fd9ea7613697a2a4e7ac79f0abded7116bdf20f#diff-d292b30dfbac46474c19daa1498c8866L454),
but now that was a significant loss. Why is it now a loss? I don't know, it just is. What *seems* to
be the case is that, if you immediately process the results (like in a testsuite), it's worse to
force the evaluation there: it gets forced anyway. However, if you want to hang on to the results,
like in my [property-discovery research][spec], then you need to force it to avoid a space leak.

As dejafu is probably going to be used in testsuites, where *not* forcing appears to be the better
option, I have no qualms in making this lazier.


## Really do profile everything!

Optimisation is hard. Don't make it harder for yourself by just guessing. I really can't emphasise
enough the benefit of profiling.


[0.5]: https://github.com/barrucadu/dejafu/releases/tag/dejafu-0.5.1.2
[0.4]: https://github.com/barrucadu/dejafu/releases/tag/dejafu-0.4.0.0
[hp]: https://downloads.haskell.org/~ghc/master/users-guide/profiling.html#prof-heap
[commit]: https://github.com/barrucadu/dejafu/commit/0fd9ea7613697a2a4e7ac79f0abded7116bdf20f
[ww]: https://wiki.haskell.org/Performance/Data_types#Single-constructor_datatypes
[folds]: https://wiki.haskell.org/Foldr_Foldl_Foldl'
[foldr-build]: https://wiki.haskell.org/Correctness_of_short_cut_fusion#Short_cut_fusion
[strict-vs-lazy]: https://www.barrucadu.co.uk/posts/etc/2016-02-12-strict-vs-lazy.html
[spec]: https://github.com/barrucadu/spec
