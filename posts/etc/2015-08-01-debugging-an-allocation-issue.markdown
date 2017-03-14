---
title: Debugging an Allocation Issue
description: A seemingly-innocuous change resulted in some extra heap churn, and we went down to Core to figure out why.
---

This fortnight I've been working on [Déjà Fu][], my concurrency
testing library for Haskell[^blog]. Mostly I've been improving the
quality of code from my last overhaul. Along the way, I made this
[commit][]. Surprisingly, it caused some extra heap churn---that is,
until I added the `INLINE` pragma.

Why? What's going on? Isn't that *already* a good candidate for
inlining? Time to find out!

[^blog]: I'll write a blog post on that at some point, for now there's
a [paper][].

[Déjà Fu]: https://github.com/barrucadu/dejafu
[commit]:  https://github.com/barrucadu/dejafu/commit/9d2c3cd42e35ae79c4ac511e9a9787801294165f
[paper]:   http://www.barrucadu.co.uk/publications/dejafu-hs15.pdf

## A Minimal Example

That list comprehension is finding the latest place (up to a point) in
a list where some value has changed, and returning the index. Yes, I
know that's really un-haskelly, but it was translated from a very
imperative algorithm, forgive me.

We can throw away all the surrounding machinery and just pluck that
comprehension out to its own file:

~~~~{.haskell}
module Main where

f :: [Int] -> Int
f xs = maximum is where
  is = [ i
       | ((_,x1), (i,x2)) <- pairs $ zip [0..] xs
       , x1 /= x2
       ]

  {-# INLINE pairs #-}
  pairs = zip <*> tail

main :: IO ()
main = print $ f xs where
  xs = replicate 1000 0 ++ replicate 1000 1 ++ replicate 1000 2
~~~~

(I also threw away the "up to a point" bit while I was at it)

This allocates 785,696 bytes *with* the `INLINE`, and 1,025,656
*without* it[^compile]. Note that that memory isn't all used at once:
that's the amount of memory allocated over the lifetime of the
program.

**First question:** is that `/=` necesary? **No!**

~~~~{.haskell}
module Main where

f :: [Int] -> Int
f xs = maximum is where
  is = [ i | (_, (i,_)) <- pairs $ zip [0..] xs ]

  {-# INLINE pairs #-}
  pairs = zip <*> tail

main :: IO ()
main = print $ f xs where
  xs = replicate 1000000 0
~~~~

As I changed the input list, we're up to 264,049,640 and 336,049,624
with and without the `INLINE`, respectively. The input list and the
memory characteristics won't change much from now on so I'll leave
them out.

**Second question:** Is the the `<*>` somehow causing this? **No!**

~~~~{.haskell}
f :: [Int] -> Int
f xs = maximum is where
  is = [ i | (_, (i,_)) <- pairs $ zip [0..] xs ]

  {-# INLINE pairs #-}
  pairs xs = zip xs $ tail xs
~~~~

### The GHC Inliner

At this point it's worth quickly describing how the GHC inliner and
the `INLINE` pragma work. More details can be seen in the
[documentation][].

GHC attempts (when optimising) to inline definitions which are "small
enough". Not just everything is safe to inline, because that might
result in duplication of work and code-blowup, so it's only really
worth it when: (a) no work will be duplicated, and (b) the inlined
definition is small or the inlining would expose further
optimisations.

The `INLINE` pragma just overrides the heuristics, and marks a
definition as being really beneficial to inline. The `NOINLINE` pragma
does the opposite, unsurprisingly.

**Third question:** Does GHC think that the `zip [0..] xs` will be
  duplicated? **No!**

~~~~{.haskell}
f :: [Int] -> Int
f xs = maximum is where
  is = [ i | (_, (i,_)) <- pairs ys ]

  ys = zip [0..] xs

  {-# INLINE pairs #-}
  pairs xs = zip xs $ tail xs
~~~~

(I also tried with a `NOINLINE` pragma on `ys`)

[^compile]: Every iteration of this program was compiled with GHC
7.10.1, with `ghc -O2 -prof -fprof-auto inline.hs`

[documentation]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/pragmas.html#inline-noinline-pragma

## To the Core!

Core is one of the few intermediary languages GHC uses. It looks kinda
like regular Haskell, except application of functions to type
parameters is explicit, and things have annotations which are used
elsewhere in the compiler.

Firstly, let's make our example *really* simple. This is now the
entire contents of the file:

~~~~{.haskell}
module Foo where

f :: [Int] -> Int
f xs = length is where
  is = [ i | (_, i) <- pairs ys ] :: [(Int,Int)]

  ys = zip [0..] xs :: [(Int, Int)]

  {-# INLINE pairs #-}
  pairs xs = zip xs $ tail xs
~~~~

The `main` is gone, because that blows up the Core surprisingly much,
and the `maximum` is gone because that pulls in typeclasses, which is
another complication. Yes, the issue still remains with only `length`.

GHC can be made to tell us its internal data structures with the
various [dump flags][]. I used `-ddump-simpl` to get the Core after
it's done some transformation, but not yet gone all the way down to
code generation.

The entire Core is [available here][]. It's pretty hairy, so we used
`vimdiff` to see what's different between the two versions. The
*really interesting* change is how the desugared list comprehension
has been compiled:

~~~~{.haskell}
Rec {
Foo.f_go [Occ=LoopBreaker]
  :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
[GblId, Arity=2, Caf=NoCafRefs, Str=DmdType <S,1*U><L,1*U>]
Foo.f_go =
  \ (ds_a14a :: [(Int, Int)]) (_ys_a14b :: [(Int, Int)]) ->
    case ds_a14a of _ [Occ=Dead] {
      [] -> GHC.Types.[] @ (Int, Int);
      : ipv_a14g ipv1_a14h ->
        case _ys_a14b of _ [Occ=Dead] {
          [] -> GHC.Types.[] @ (Int, Int);
          : ipv2_a14n ipv3_a14o ->
            GHC.Types.: @ (Int, Int) ipv2_a14n (Foo.f_go ipv1_a14h ipv3_a14o)
        }
    }
end Rec }

Rec {
Foo.f_go [Occ=LoopBreaker]
  :: [((Int, Int), (Int, Int))] -> [(Int, Int)]
[GblId, Arity=1, Caf=NoCafRefs, Str=DmdType <S,1*U>]
Foo.f_go =
  \ (ds_a13o :: [((Int, Int), (Int, Int))]) ->
    case ds_a13o of _ [Occ=Dead] {
      [] -> GHC.Types.[] @ (Int, Int);
      : y_a13t ys_a13u ->
        case y_a13t of _ [Occ=Dead] { (ds1_d12V, i_an2) ->
        GHC.Types.: @ (Int, Int) i_an2 (Foo.f_go ys_a13u)
        }
    }
end Rec }
~~~~

For the `INLINE` and the no-`INLINE` cases respectively. Translating
into more familiar Haskell syntax, we get this code:

~~~~{.haskell}
f_go :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
f_go as bs = case as of
  []      -> []
  (a:as') -> case bs of
    []      -> []
    (b:bs') -> b : f_go as' bs'

f_go :: [((Int, Int), (Int, Int))] -> [(Int, Int)]
f_go as = case as of
  []      -> []
  (a:as') -> case a of
    (a1, a2) -> a2 : f_go as'
~~~~

Interestingly, `pairs` has been inlined in both cases: grep the Core
for "pairs", it's just not there! *However*, when there *is* an
explicit `INLINE` pragma, then `pairs` has been inlined *and*
deforested! The intermediary list created by `zip` is just gone.

That explains *where* the memory is going in the no-`INLINE` case, but
why's it doing that?

GHC does optimisations in phases, with some optimisations only
happening in some phases. It looks like, without the `INLINE` pragma,
the heuristics consider `pairs` as a good candidate for inlining, but
not so good that it happens before the deforestation phase. Whereas,
with the `INLINE` pragma, it gets inlined much earlier and so
deforestation also kicks in.

[dump flags]:     https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/options-debugging.html
[available here]: https://gist.github.com/barrucadu/a59df62cd16074559e35

## Heuristics?

I sent an [email][] to the ghc-devs mailing list asking about this,
and got a response from the man himself, Simon Peyton Jones:

> Hmm.  With HEAD, and without profiling, the program allocates the
> same 104M, both with and without the INLINE. The same deforestation
> happens in both cases.
>
> It's quite possible that profiling interferes with deforestation.
>
> Simon
>
> ~~~~
> c:/code/HEAD/inplace/bin/ghc-stage1 Michael.hs -O -o Michael-no-inline
> ./Michael-no-inline.exe +RTS -s
> 999999
>      104,045,052 bytes allocated in the heap
>          171,752 bytes copied during GC
>           41,756 bytes maximum residency (2 sample(s))
>           36,800 bytes maximum slop
>                1 MB total memory in use (0 MB lost due to fragmentation)
> ~~~~

And that was it. I tested, and building with RTS options but *without*
profiling had the same result. Maybe it was inserting a profiling
point and so couldn't perform deforestation there. Who knows.

It was a fun puzzle, even though I had to just ask someone for the
answer at the end.

[email]: https://mail.haskell.org/pipermail/ghc-devs/2015-July/009429.html
