---
title: Subconcurrency in Deja Fu
description: Contemplating concurrency with continuations.
---

Let's say I give you a function that operates on `MVar`s, and I ask
you what it does. You might try something like this:

```haskell
test :: MonadConc m => Maybe Int -> m (Maybe Int)
test mint = do
  var <- maybe newEmptyMVar newMVar mint
  secretFunction var
  tryTakeMVar var
```

And then run this with dejafu with a few different parameters, to
print all the results. I do, in fact, have such a function, and here's
what I get:

```
> mapM_ (print . fst) =<< sctBound defaultMemType defaultBounds (test Nothing)
Left Deadlock
> mapM_ (print . fst) =<< sctBound defaultMemType defaultBounds (test (Just 3))
Left Deadlock
```

It always deadlocks. What a useless function! This exercise was
pointless. Well, not entirely. I shall now reveal what my function is:

```haskell
secretFunction :: MonadConc m => MVar m Int -> m ()
secretFunction var = do
  putMVar var 5
  putMVar var 5
```

Yes, it always deadlocks, but it has another effect: it sets the value
in the `MVar` to `5` if it was empty. Unfortunately, there's no good
way to observe this with dejafu currently. You could fork the call to
`secretFunction`, and do a `tryTakeMVar` after that, which would
sometimes show the `MVar` as empty and sometimes show it as holding
`5`, but that's just confusing matters: the observation of the `MVar`
should take place *after* `secretFunction` has done whatever it is
that it does, there should be no race condition there!

I've got an
[experimental branch up on github](https://github.com/barrucadu/dejafu/tree/subconcurrency)
implementing a "subconcurrency" function, with this type:

```haskell
subconcurrency :: Conc n r a -> Conc n r (Either Failure a)
```

This function:

- Can only be used in testing, not in `IO`, as it uses the `Conc`
  type.
- Can only be used in the main thread, when no other threads exist.
- Runs the provided action, returning its result *even if it
  deadlocks!* (or fails in some other way)

So now you can test it like so:

```haskell
test2 :: Monad n => Maybe Int -> Conc n r (Maybe Int)
test2 mint = do
  var <- maybe newEmptyMVar newMVar mint
  subconcurrency (secretFunction var)
  tryTakeMVar var
```

And lo and behold, `secretFunction` is not so secret any more:

```
> mapM_ (print . fst) =<< sctBound defaultMemType defaultBounds (test2 Nothing)
Right (Just 5)
> mapM_ (print . fst) =<< sctBound defaultMemType defaultBounds (test2 (Just 3))
Right (Just 3)
```

This came up in my current project,
[a tool to discover properties of concurrent data structures using observational refinement](https://github.com/barrucadu/spec),
where I was previously observing terms like so:

```haskell
newState >>= \s -> evaluate s >> observe s
```

But if the term being evaluated deadlocks, no observation gets made! I
haven't incorporated this branch yet, but the plan is to now do:

```haskell
newState >>= \s -> subconcurrency (evaluate s) >> observe s
```

This is particularly neat to me, as you *can't* do this nicely with
`IO`. To detect deadlocks with `IO`, you'd *need* to introduce a
timeout, which would render a property-discovery tool unusably
slow. But with dejafu, we can do this.
