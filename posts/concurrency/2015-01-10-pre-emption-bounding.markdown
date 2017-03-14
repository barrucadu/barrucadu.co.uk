---
title: Pre-emption Bounding
description: Implementing pre-emption bounding in monad-conc.
---

~~~~{.haskell}
philosophers :: ConcCVar cvar m => Int -> m ()
philosophers n = do
  forks <- replicateM n newEmptyCVar
  let phils = map (\(i,p) -> p i forks) $ zip [0..] $ replicate n philosopher
  cvars <- mapM spawn phils
  mapM_ takeCVar cvars

  where
    philosopher ident forks = do
      let leftId  = ident
      let rightId = (ident + 1) `mod` length forks
      lock $ forks !! leftId
      lock $ forks !! rightId
      unlock $ forks !! leftId
      unlock $ forks !! rightId

bad = philosophers 100
~~~~

This is a very simple [Dining Philosophers][I1] problem, but without
the loop, and with the put-down delay introduced, effectively, by the
scheduler. And with 100 philosophers, a random scheduler can't find
the deadlock. Well, actually, it probably would eventually, but I gave
up after running it for half an hour. However, with my new shiny
pre-emption bounding scheduler, with a pre-emption bound of 1, it
finds it in less than a second.

[I1]: https://en.wikipedia.org/wiki/Dining_philosophers_problem

## Pre-emption Bounding

To quickly recap from [the last post][PB1], pre-emption bounding is a
technique where we try only schedules with a given pre-emption count,
where a pre-emption is taking control from a thread which is not
blocked.

In outline, the algorithm for using iterative pre-emption bounding to
explore schedules is as follows:

1. Add an empty list of scheduling decisions to the run queue
2. While we're not done:
    1. While the run queue is not empty:
        1. Run the schedule at the head of the queue, using a non
        pre-emptive scheduler when the list of decisions runs out.
        2. Generate all new variants of this schedule which do not
        introduce an extra pre-emption.
        3. Add these variants to the run queue.
        4. Add the schedule to the done list.
        5. Add the schedule and the result of the computation to the
        output list.
    2. If this is the final pre-emption level, we're done.
    3. Otherwise, for every schedule in the done list:
        1. Generate all new variants of the schedule which introduce
        exactly one extra pre-emption.
        2. Add these variants to the run queue.
    4. Empty the done list.
3. Return the output list.

We can visualise the space of all schedules as a tree, where siblings
have the same pre-emption count, and the children of a schedule are
those schedules which can be obtained by introducing one extra
pre-emption in the parent. Iterative pre-emption bounding is then a
breadth-first traversal of this space.

Unfortunately, this uses a *lot* of memory, as we keep all the
schedules from the current pre-emption level around until we advance
to the next one, so if we have a very concurrent problem with a lot of
possible schedules, such as the Dining Philosophers, that's a lot of
memory. I was actually stuck for a few days unable to solve the
problem with 25 philosophers because the memory usage was just so
vast.

[PB1]: http://www.barrucadu.co.uk/posts/2014-12-26-haskell-systematic-concurrency-testing.html

## Laziness to the rescue!

I realised that, with a few additional assumptions, the problem could
be restructured to be much more lazy, and so cut down on the memory
usage massively in the average case:

- For many properties we want to check (such as presence of a
deadlock) we don't necessarily need to check the entire list of
possible schedules, only a prefix.
- For the purpose of proving or disproving a property we care about,
schedules containing pre-emptions are more likely to be "interesting".
- As long as we still check *all* schedules if we need to, the exact
order in which we do it does *not* need to be by pre-emption count.

I managed to restructure the algorithm to use a depth-first search
([before][L1] and [after][L2]), so my implementation is more like
this:

1. Add an empty list of scheduling decisions to the run queue
2. While the run queue is not empty:
    1. Run the schedule at the head of the queue, using a non
    pre-emptive scheduler when the list of decisions runs out.
    2. Generate all new variants of this schedule which do not
    introduce an extra pre-emption.
    3. Prepend these variants to the run queue.
    4. If this is not the final pre-emption level:
        1. Generate all new variants of the schedule which introduce
        exactly one extra pre-emption.
        2. Prepend these variants to the run queue.
    5. Add the schedule and the result of the computation to the
    output list.
3. Return the output list.

So, with a pre-emption bound of 2, this will examine one schedule with
a count of 0, one with a count of 1, a bunch with a count of 2,
another with a count of 1, and so on. This results in schedules
containing pre-emptions to be spread evenly throughout the list,
rather than clustered at the end as in the case of the breadth-first
search. Furthermore, as there's no need to keep everything in memory
until we generate the next set of schedules, things can be garbage
collected much sooner. To return to the tree analogy, we're now able
to throw away subtrees much sooner.

Even better: because this is implemented in Haskell, it simply doesn't
compute schedules unless you ask for their result, in a strict
language we'd have to implement a thunk-like structure to get the same
savings.

This is what allowed the 100 Dining Philosophers to be solved so
quickly: we know that, for that particular problem, a deadlock can
*only* arise in schedules with pre-emptions, so the breadth-first
approach resulted in exploring a huge number (100 factorial) of
schedules, which could not possibly deadlock, before even attempting
those which might. Obviously that's only one small test, but I think
bugs being exposed more commonly by pre-emptive schedules is likely to
be a reasonable assumption upon further experimentation.

[L1]: https://github.com/barrucadu/monad-conc/blob/6272abe7fdb29b1db68d7cd072f469ded7379ed9/Control/Monad/Conc/SCT.hs#L237
[L2]: https://github.com/barrucadu/monad-conc/blob/981169c25fc073367542ecb2cf2336c80307f26c/Control/Monad/Conc/SCT/PreBound.hs#L100

## What next?

Well, so far, I've only tried [monad-conc][N1] on small, simple, test
cases. I'd really like to try it on something more substantial, and
see how it holds up in finding errors. Still, this is a much better
position than I was in last week, and I'm glad to be here.

[N1]: https://github.com/barrucadu/monad-conc
