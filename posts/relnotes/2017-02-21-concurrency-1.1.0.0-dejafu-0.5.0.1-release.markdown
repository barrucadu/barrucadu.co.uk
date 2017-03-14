---
title: concurrency-1.1.0.0 and dejafu-0.5.0.1 release
description: 35 changed files, 1660 additions, 2335 deletions, two big new features.
---

It's release time again, with major bumps in [concurrency][], [dejafu][], [hunit-dejafu][],
*and* [tasty-dejafu][] this time. Furthermore, [dpor][] has been **deprecated**, as I have merged
its functionality back into dejafu.

For the nitty-gritty, see the [comparison][]. Without further ado...

## concurrency-1.1.0.0

A couple of changes to `MonadConc`:

- `tryReadMVar` is now in the class, giving you non-blocking reads of `MVar`s

    ```haskell
    tryReadMVar :: MonadConc m => MVar m a -> m (Maybe a)
    ```

    *Issue: [#62][], commit: [12335e0][].*

- `_concMessage` is now removed.

    This was a holdover from the pre-separation days, when I added stuff to `MonadConc` based on
    what I thought would make testing more convenient. When I split the packages I removed all but
    this one which, too, is now gone. It *used* to add a dynamically-typed message to the execution
    trace. There is now no way to do this, if there's demand I can add a `Conc`-specific function to
    dejafu proper.

    *Issue: [#63][], commit: [336199e][].*

## dejafu-0.5.0.1

- You can now easily explore a fixed number of randomly-scheduled executions of your programs.

    Why might you want this over the full-blown SCT implementation dejafu normally uses? Simply put:
    it uses far less memory.

    ```haskell
    sctRandom :: (MonadRef r n, RandomGen g)
      => MemType
      -- ^ The memory model to use for non-synchronised @CRef@ operations.
      -> g
      -- ^ The random number generator.
      -> Int
      -- ^ The number of executions to perform.
      -> Conc n r a
      -- ^ The computation to run many times.
      -> n [(Either Failure a, Trace)]
    ```

    Sometimes you don't need absolute certainty, you just need probable justification. For those
    cases, randomness can be sufficient.

    *Issue: [#61][], commit: [71cb670][].*

- A new testing-mode "subconcurrency" function, which runs a concurrent computation and gives its
  result.

    See my prior [blog post][subc] on the topic. The gist of it is that you can now examine the
    effects of arbitrary functions, without needing to worry about deadlocks and the like. For
    example:

    ```haskell
    inspect :: Monad n
      => Conc n r s
      -- ^ Construct some sort of state value.
      -> (s -> Conc n r a)
      -- ^ The action to test.
      -> (s -> Conc n r b)
      -- ^ Perform an observation on the state.
      -> Conc n r (Either Failure a, b)
    inspect state action observe = do
      s <- state
      a <- subconcurrency (action s)
      b <- observe s
      pure (a, b)
    ```

    This is impossible to implement for `IO`, so the function is restricted to the `Conc` type:

    ```haskell
    subconcurrency :: Conc n r a -> Conc n r (Either Failure a)
    ```

    *Commit: [75d2b6c][].*

- A handy little data type abstracting over the choice between full SCT and random execution.

    Exposing variants of the `dejafu*` functions for every way of executing a concurrent computation
    would lead to a bit of an explosion, so I added a datatype to cover it instead:

    ```haskell
    data Way g
      = Systematically Bounds
      -- ^ Systematically explore all executions within the bounds.
      | Randomly g Int
      -- ^ Explore a fixed number of random executions, with the given PRNG.
    ```

    Now all the `dejafu'` etc functions have become `dejafuWay` etc functions. There's also a
    `runSCT` function which takes a `Way` and gives you the result/trace list; and a `resultsSet`
    function which takes a `Way` and gives you the set of results (with no traces).

    *Commit: [bc8b123][] and [a240b48][].*

- Significant performance improvements!

    Here's the result of profiling dejafu-tests at the [dejafu-0.4.0.0][0.4] tag:

    ```
    total time  =      317.12 secs   (317123 ticks @ 1000 us, 1 processor)
    total alloc = 125,356,348,208 bytes  (excludes profiling overheads)
    ```

    And here it is at the [dejafu-0.5.0.1][0.5] tag:

    ```
    total time  =      142.78 secs   (142778 ticks @ 1000 us, 1 processor)
    total alloc = 46,909,536,504 bytes  (excludes profiling overheads)
    ```

    But wait, there's more: *dejafu-tests now has 31 more test cases, and is still that much more
    efficient!*

    The details are pretty hairy, but the lion's share of the gains come down to algorithmic
    improvements, and reducing allocation. I'm going to write a blog post on what I have learned.

    This change isn't quite semantically invisible: it changes a couple of types and removes the
    `NFData` instances of everything.

    *Commit: [0fd9ea7][].*

- The dpor library has been merged back into dejafu, as it wasn't as generally useful as I expected.

    *Issue: [#60][], commit: [f48b462][].*

- `ThreadAction` and `Lookahead` now have `Eq` instances.

    This was made possible by the removal of `_concMessage`.

    *Commit: [110629f][].*

- The Var/MVar/Ref/CRef naming has been unified.

    Back when `MVar` was `CVar`, I referred to "Ref"s and "Var"s internally, which made (a very
    lazy) sense. When the name changed to `MVar` that shorthand made less sense. Now the
    one-character-longer names are used everywhere.

    *Commit: [300d1d4][].*

- Some previously-missed `CRef` action dependencies are no longer missed.

    I should have added a test for this back when I had an example failing case, because I can't
    actually remember what code I used to (inadvertently) discover this.

    *Commit: [26b5b26][].*

- I slightly broke the fair-bounding of `readMVar`, and fixed it in version 0.5.0.1[^1]. Whoops.

    *Commit: [5e00918][].*

[^1]: It's not a *real* major version bump unless there's a bugfix release almost immediately
    afterwards.

## hunit-dejafu-0.4.0.0

- The primed test functions (the ones taking a `MemType` and a `Bounds` value) have been replaced
  with functions taking a `MemType` and a `Way` value.

- The minimum compatible version of dejafu has been bumped to 0.5.0.0.

All implemented in commit [c6346b7][].

## tasty-dejafu-0.4.0.0

- The primed test functions (the ones taking a `MemType` and a `Bounds` value) have been replaced
  with functions taking a `MemType` and a `Way` value.

- The `Way` is exposed as an option, and so can be set, to some extent, on the command line:

    - **way=systematically**: use the systematic scheduler with the default bounds.
    - **way=randomly**: use the random scheduler with a fixed seed and 100 executions.

- The minimum compatible version of dejafu has been bumped to 0.5.0.0.

All implemented in commit [fdc0190][].

[concurrency]: http://hackage.haskell.org/package/concurrency
[dejafu]: http://hackage.haskell.org/package/dejafu
[hunit-dejafu]: http://hackage.haskell.org/package/hunit-dejafu
[tasty-dejafu]: http://hackage.haskell.org/package/tasty-dejafu
[dpor]: http://hackage.haskell.org/package/dpor
[comparison]: https://github.com/barrucadu/dejafu/compare/dejafu-0.4.0.0...dejafu-0.5.0.1
[0.5]: https://github.com/barrucadu/dejafu/releases/tag/dejafu-0.5.0.1
[0.4]: https://github.com/barrucadu/dejafu/releases/tag/dejafu-0.4.0.0
[subc]: https://www.barrucadu.co.uk/posts/2017-02-02-subconcurrency.html
[0fd9ea7]: https://github.com/barrucadu/dejafu/commit/0fd9ea7613697a2a4e7ac79f0abded7116bdf20f
[110629f]: https://github.com/barrucadu/dejafu/commit/110629f35356a35db349bd3effd94e2eca8bb85d
[12335e0]: https://github.com/barrucadu/dejafu/commit/12335e0090d9c079a6f397dc1371bd284d7f55bf
[26b5b26]: https://github.com/barrucadu/dejafu/commit/26b5b26900dc8110cdf339e1c0fcd95c2ebdd5d5
[300d1d4]: https://github.com/barrucadu/dejafu/commit/300d1d462a3a0836599fd3db969d231ce65b591b
[336199e]: https://github.com/barrucadu/dejafu/commit/336199e75b4bf11060266a3f1deef6ca65ba8b10
[5e00918]: https://github.com/barrucadu/dejafu/commit/5e00918e571c5a3fc17b4857c144466bbcaa1b07
[71cb670]: https://github.com/barrucadu/dejafu/commit/71cb67008320366adc55442ceac68a72d3852a9a
[75d2b6c]: https://github.com/barrucadu/dejafu/commit/75d2b6ca73d7196af10df8c697513d2e35c859b9
[a240b48]: https://github.com/barrucadu/dejafu/commit/a240b48d1f5af8cddee12b3af9265c03089e068a
[bc8b123]: https://github.com/barrucadu/dejafu/commit/bc8b123cddae49d8b23e86c006acd40599f805e5
[c6346b7]: https://github.com/barrucadu/dejafu/commit/c6346b7104cd46951ba23f4159bf7b0376bcfa78
[f48b462]: https://github.com/barrucadu/dejafu/commit/f48b462a071f49f0ae179a1668b4c841536017d6
[fdc0190]: https://github.com/barrucadu/dejafu/commit/fdc0190d254832ccf77c78e8b199f2baa4d15e8d
[#63]: https://github.com/barrucadu/dejafu/issues/63
[#62]: https://github.com/barrucadu/dejafu/issues/62
[#61]: https://github.com/barrucadu/dejafu/issues/61
[#60]: https://github.com/barrucadu/dejafu/pull/60
