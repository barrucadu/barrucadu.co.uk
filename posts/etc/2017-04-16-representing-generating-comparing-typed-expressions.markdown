---
title: Representing, Generating, and Comparing Typed Expressions
---

I have recently been writing a tool to [discover properties of concurrent programs][spec], but it
had some issues.  Mostly with variables.  The previous handling of variables was very poor: the
programmer had to specify exactly which variables may be introduced; binds and lets caused
shadowing; and no care was taken to avoid generating alpha equivalent terms.

[spec]: https://github.com/barrucadu/spec

Here are two representative problems:

1. If you have the variable `x`, and no others, a term like this would not be generated:
   `f >>= \x1 -> g x1 x`
2. If you have the variables `x` and `y` of the same type, these equivalent terms would be
   generated: `f x` and `f y`


These problems arose because I didn't anticipate how exactly I would want to use the tool when I was
first writing the core components.  My original design was short-sighted; hindsight is 20/20 as they
say.

This is a fairly long blog post, going through three problems I had to consider and solve when I was
redesigning the expression representation from the ground up.  These are:

1. How to **represent** the sort of expressions I wanted to work with.
2. How to **generate** new expressions, given smaller ones.
3. How to **compare** two expressions and determine if one refines, or is equivalent to, another.

Let's get started...


Representing and Evaluating Expressions
---------------------------------------

Or, "I tried really hard to use the [bound][] library but just couldn't get that square peg in this
round hole."

[bound]: https://hackage.haskell.org/package/bound

We're going to arrive at a simplified version of the [spec][] expression type, but to get there
we'll work through two simpler representations first and talk about their limitations.

First we're going to need some imports:

```haskell
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Expr where

import Data.Dynamic (Dynamic, dynApply, dynTypeRep)
import Data.Function (on)
import Data.List (groupBy, nub, sortOn)
import Data.Maybe (mapMaybe, maybeToList)
import Data.Ord (Down(..))
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable, TypeRep, funResultTy, splitTyConApp, typeOf, typeRep)
import Data.Void (Void, absurd)
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce) -- it's safe, I promise!
```

### Mark 1: A Simple Typed Expression Type

Our expressions are representations of Haskell code, which makes this a bit unlike most toy
expression evaluators you see in tutorials.  We want everything to be very typed, and not expose the
constructors of the expression data type, using smart constructors to ensure that only well-typed
expressions can be created.

It's this typing that makes it really difficult to use the [bound][] library here, as bound doesn't
play so nicely with typed variables.  You can infer types for variables later, but I don't really
want to implement that.

Without further ado, here is our expression type:

```haskell
-- | A type for expressions. This would not be exported by actual library code, to prevent the user
-- from mucking around with the types.
data Expr1
  = Lit1 String Dynamic
  -- ^ Literal values are just dynamically-wrapped Haskell values.
  | Hole1 TypeRep
  -- ^ "Holes" are free variables, identified by their position in the tree.
  | Named1 TypeRep String
  -- ^ Free variables which are bound by a provided environment.
  | Ap1 TypeRep Exp1 Exp1
  -- ^ The parameter is assumed to be of the correct type for the function.

instance Show Expr1 where
  show (Lit1 s _) = s
  show (Hole1 ty) = "(_ :: " ++ show ty ++ ")"
  show (Named1 ty s) = "(" ++ s ++ " :: " ++ show ty ++ ")"
  show (Ap1  _ f e) = "(" ++ show f ++ ") (" ++ show e ++ ")"
```

We can get the type of an expression, and also find all of the holes and named variables:

```haskell
-- | Get the type of an expression.
typeOf1 :: Expr1 -> TypeRep
typeOf1 (Lit1 _ dyn)  = dynTypeRep dyn
typeOf1 (Hole1  ty)   = ty
typeOf1 (Named1 ty _) = ty
typeOf1 (Ap1  ty _ _) = ty

-- | Get all the holes in an expression, identified by position.
holes1 :: Expr1 -> [(Int, TypeRep)]
holes1 = zip [0..] . go where
  go (Hole1 ty) = [ty]
  go (Ap1  _ f e) = go f ++ go e
  go _ = []

-- | Get all the named variables in an expression.
names1 :: Expr1 -> [(String, TypeRep)]
names1 = nub . go where
  go (Named1 ty s) = [(s, ty)]
  go (Ap1 _ f e) = go f ++ go e
  go _ = []
```

**Smart constructors**: we only want to be able to produce type-correct expressions, for two
reasons: the evaluator in [spec][] is more complex and does a lot of unsafe coercion, and being able
to just call `error` when the types don't work out is nicer than needing to actually handle it; and
it makes it easier to generate terms programmatically, as you simply try all possibilities and keep
the ones which succeed.

```haskell
-- | Construct a literal value.
lit1 :: String -> Dynamic -> Expr1
lit1 = Lit1

-- | Construct a typed hole.
hole1 :: TypeRep -> Expr1
hole1 = Hole1

-- | Perform a function application, if type-correct.
ap1 :: Expr1 -> Expr1 -> Maybe Expr1
ap1 f e = (\ty -> Ap1 ty f e) <$> typeOf1 f `funResultTy` typeOf1 e

-- | Give names to holes, if type-correct.
--
-- The naming is applied "atomically", in that you don't need to worry about holes disappearing and
-- so changing their position-indices while this operation happens; however the position of unnamed
-- holes may be altered in the result of this function.
name1 :: [(Int, String)] -> Expr1 -> Maybe Expr1
name1 is e0 = (\(e,_,_) -> e) <$> go [] 0 e0 where
  go env i n@(Named1 ty s) = case lookup s env of
    -- if a name gets re-used it had better be at the same type!
    Just sty
      | ty == sty -> Just (n, env, i)
      | otherwise -> Nothing
    Nothing -> Just (n, env, i)
  go env i (Hole1 ty) = case lookup i is of
    Just s -> case lookup s env of
      Just sty
        | ty == sty -> Just (Named1 ty s, env, i + 1)
        | otherwise -> Nothing
      Nothing -> Just (Named1 ty s, (s,ty):env, i + 1)
    Nothing -> Just (Hole1 ty, env, i + 1)
  go env i (Ap1 ty f e) = do
    (f', env',  i')  <- go env  i  f
    (e', env'', i'') <- go env' i' e
    Just (Ap1 ty f' e', env'', i'')
  go env i e = Just (e, env, i)
```

**Evaluation**: now we have everything in place to evaluate expressions with no unbound holes.
Everything is type-correct-by-construction, so if there are no holes (and the global environment has
everything we need) we can get out a value.

```haskell
-- | Evaluate an expression if it has no holes.
eval1 :: [(String, Dynamic)] -> Expr1 -> Maybe Dynamic
eval1 globals = go where
  go (Named1 ty s) = case lookup s globals of
    Just dyn | dynTypeRep dyn == ty -> Just dyn
    _ -> Nothing
  go (Ap1 _ f e) = do
    dynf <- go f
    dyne <- go e
    dynf `dynApply` dyne
  go (Lit1 _ dyn) = Just dyn
  go (Hole1 _) = Nothing
```

**Removing Holes**: we still have one more problem, it would be nice for holes to be given names
automatically, and not just individual holes, but groups of holes too.

For example, if we have an expression like so:

```
f (_ :: Int) (_ :: Bool) (_ :: Bool) (_ :: Int)
```

It would be nice to be able to generate these expressions automatically:

```
f (w :: Int) (x :: Bool) (y :: Bool) (z :: Int)
f (w :: Int) (x :: Bool) (y :: Bool) (w :: Int)
f (w :: Int) (x :: Bool) (x :: Bool) (z :: Int)
f (w :: Int) (x :: Bool) (x :: Bool) (w :: Int)
```

It would be particularly nice if they were generated in a list in that order, from most free to most
constrained.

```haskell
-- | From an expression that may have holes, generate a list of expressions with named variables
-- substituted instead, ordered from most free (one hole per variable) to most constrained (multiple
-- holes per variable).
--
-- This takes a function to assign a letter to each type, subsequent variables of the same type have
-- digits appended.
terms1 :: (TypeRep -> Char) -> Expr1 -> [Expr1]
terms1 nf = sortOn (Down . length . names1) . go where
  go e0 = case hs e0 of
    [] -> [e0]
    (chosen:_) -> concatMap go
      [ e | ps <- partitions chosen
          , let (((_,tyc):_):_) = ps
          , let vname i = if i == 0 then [nf tyc] else nf tyc : show i
          , let naming = concat $ zipWith (\i vs -> [(v, vname i) | (v,_) <- vs]) [0..] ps
          , e <- maybeToList (name1 naming e0)
      ]

  -- holes grouped by type
  hs = groupBy ((==) `on` snd) . sortOn snd . holes1

  -- all partitions of a list
  partitions (x:xs) =
    [[x]:p | p <- partitions xs] ++
    [(x:ys):yss | (ys:yss) <- partitions xs]
  partitions [] = [[]]
```

Here's an example from ghci:

```
λ> let intHole  = hole1 $ T.typeOf (5::Int)
λ> let boolHole = hole1 $ T.typeOf True
λ> let ibf      = lit1 "f" (D.toDyn ((\_ _ _ a -> a) :: Int -> Bool -> Bool -> Int -> Int))
λ> let ibfExp   = fromJust $ do { x <- ibf `ap1` intHole; y <- x `ap1` boolHole; z <- y `ap1` boolHole; z `ap1` intHole }
λ> mapM_ print $ terms1 (head.show) ibfExp
((((f) ((I :: Int))) ((B :: Bool))) ((B1 :: Bool))) ((I1 :: Int))
((((f) ((I :: Int))) ((B :: Bool))) ((B1 :: Bool))) ((I :: Int))
((((f) ((I :: Int))) ((B :: Bool))) ((B :: Bool))) ((I1 :: Int))
((((f) ((I :: Int))) ((B :: Bool))) ((B :: Bool))) ((I :: Int))
```

Pretty sweet!

### Mark 2: The Schema / Term Distinction

What we have now is pretty good, but it leaves a little to be desired: it would be nice to be able
to statically forbid passing expressions with holes to `eval`.  As always in Haskell, the solution
is to add another type parameter.

```haskell
data Expr2 h
  = Lit2 String Dynamic
  | Var2 TypeRep (Var2 h)
  -- ^ One constructor for holes and named variables.
  | Ap2 TypeRep (Expr2 h) (Expr2 h)

instance Show (Expr2 h) where
  show (Lit2 s _)  = s
  show (Var2 ty v) = "(" ++ show v ++ " :: " ++ show ty ++ ")"
  show (Ap2 _ f e) = "(" ++ show f ++ ") (" ++ show e ++ ")"

data Var2 h
  = Hole2 h
  -- ^ Holes get a typed tag.
  | Named2 String
  -- ^ Environment variables.

instance Show (Var2 h) where
  show (Hole2  _) = "_"
  show (Named2 s) = s
```

**Schemas and Terms**: what does this hole tag buy us? Well, actually, it lets us very simply forbid
the presence of holes! Constructing an `h` value is required to construct a hole, so if we set it to
`Void`, then no holes can be made at all! If the tag is some inhabited type, then an expression may
contain holes (but may not).

Let's introduce two type synonyms to talk about these:

```haskell
-- | A schema is an expression which may contain holes. A single schema may correspond to many
-- terms.
type Schema2 = Expr2 ()

-- | A term is an expression with no holes. Many terms may correspond to a single schema.
type Term2 = Expr2 Void

-- | Convert a Schema into a Term if there are no holes.
toTerm2 :: Schema2 -> Maybe Term2
toTerm2 (Lit2 s dyn) = Just (Lit2 s dyn)
toTerm2 (Var2 ty v) = case v of
  Hole2  _ -> Nothing
  Named2 s -> Just (Var2 ty (Named2 s))
toTerm2 (Ap2 ty f e) = Ap2 ty <$> toTerm2 f <*> toTerm2 e
```

**Evaluation & Hole Removal**: now we can evaluate _terms_ after removing holes from _schemas_.
Statically-checked guarantees that we're dealing with all of our holes properly, nice!

```haskell
-- | Evaluate a term
eval2 :: [(String, Dynamic)] -> Term2 -> Maybe Dynamic
eval2 globals = go where
  go (Var2 ty v) = env ty v
  go (Ap2 _ f e) = do
    dynf <- go f
    dyne <- go e
    dynf `dynApply` dyne
  go (Lit2 _ dyn) = Just dyn

  env ty (Named2 s) = case lookup s globals of
    Just dyn | dynTypeRep dyn == ty -> Just dyn
    _ -> Nothing
  env _ (Hole2 v) = absurd v -- this is actually unreachable now

-- | From a schema that may have holes, generate a list of terms with named variables
-- substituted instead.
terms2 :: (TypeRep -> Char) -> Schema2 -> [Term2]
terms2 nf = mapMaybe toTerm2 . sortOn (Down . length . names2) . go where
  go e0 = case hs e0 of
    [] -> [e0]
    (chosen:_) -> concatMap go
      [ e | ps <- partitions chosen
          , let (((_,tyc):_):_) = ps
          , let vname i = if i == 0 then [nf tyc] else nf tyc : show i
          , let naming = concat $ zipWith (\i vs -> [(v, vname i) | (v,_) <- vs]) [0..] ps
          , e <- maybeToList (name2 naming e0)
      ]

  -- holes grouped by type
  hs = groupBy ((==) `on` snd) . sortOn snd . holes2

  -- all partitions of a list
  partitions (x:xs) =
    [[x]:p | p <- partitions xs] ++
    [(x:ys):yss | (ys:yss) <- partitions xs]
  partitions [] = [[]]
```

The rest of the code is basically the same as in Mark 1.

### The Implementation of Data.Dynamic

In the Mark 3 (final!) evaluator, we're going to need a function of type `Monad m => m Dynamic ->
Dynamic`, which "pushes" the `m` inside the `Dynamic`, and of type `Monad m => Dynamic -> Maybe (m
Dynamic)`.  Unfortunately, Data.Dynamic doesn't provide a way to do this, for good reason: it's
impossible in general! There's no way to know what the type of the dynamic value inside the monad
is, so there's no way to do this safely.

Fortunately, we *do* know what the types will be, and implementing a Data.Dynamic-lite is pretty
simple.

```haskell
-- | A dynamic value is a pair of its type and 'Any'. Any is a magical type which is guaranteed to
-- work with 'unsafeCoerce'.
data BDynamic = BDynamic { bdynTypeRep :: TypeRep, bdynAny :: Any }

instance Show BDynamic where
  show = show . bdynTypeRep
```

(`BDynamic` for "barrucadu's dynamic")

We need to be able to construct and deconstruct dynamic values, these operations do use
`unsafeCoerce`, but are safe:

```haskell
-- | Convert an arbitrary value into a dynamic one.
toBDynamic :: Typeable a => a -> BDynamic
toBDynamic a = BDynamic (typeOf a) (unsafeCoerce a)

-- | Convert a dynamic value into an ordinary value, if the types match.
fromBDynamic :: Typeable a => BDynamic -> Maybe a
fromBDynamic (BDynamic ty v) = case unsafeCoerce v of
  -- this is a bit mind-bending, but the 'typeOf r' here is the type of the 'a', as 'unsafeCoerce
  -- v :: a' (regardless of whether it actually is an 'a' value or not). The same result could be
  -- achieved using ScopedTypeVariables and 'typeRep'.
  r | ty == typeOf r -> Just r
    | otherwise      -> Nothing
```

The final operation needed for the Marks 1 and 2 implementation is function application:

```haskell
-- | Dynamically-typed function application.
bdynApply :: BDynamic -> BDynamic -> Maybe BDynamic
bdynApply (BDynamic ty1 f) (BDynamic ty2 x) = case funResultTy ty1 ty2 of
  Just ty3 -> Just (BDynamic ty3 ((unsafeCoerce f) x))
  Nothing  -> Nothing
```

Now we can construct our strange monad-shuffling operations:

```haskell
-- | "Push" a functor inside a dynamic value, given the type of the resultant value.
--
-- This is unsafe because if the type is incorrect and the value is later used as that type, good
-- luck.
unsafeWrapFunctor :: Functor f => TypeRep -> f BDynamic -> BDynamic
unsafeWrapFunctor ty fdyn = BDynamic ty (unsafeCoerce $ fmap bdynAny fdyn)

-- | "Extract" a functor from a dynamic value.
unwrapFunctor :: forall f. (Functor f, Typeable f) => BDynamic -> Maybe (f BDynamic)
unwrapFunctor (BDynamic ty v) = case splitTyConApp ty of
    (tyCon, tyArgs)
      | tyCon == ftyCon && not (null tyArgs) && init tyArgs == ftyArgs
        -> Just $ BDynamic (last tyArgs) <$> unsafeCoerce v
    _ -> Nothing
  where
    (ftyCon, ftyArgs) = splitTyConApp (typeRep (Proxy :: Proxy f))
```

It's almost a shame that Data.Dynamic doesn't expose enough to implement this.  It has gone for a
safe but limited API.  A common Haskell "design" pattern is to have safe public APIs and unsafe but
publically-exposed "internal" APIs, but base doesn't seem to follow that.

### Mark 3: Monadic Expressions

This expression representation is pretty nice, but it's rather cumbersome to express monadic
operations for a couple of reasons:

1. Everything is monomorphic, so there would need to be a separate `lit` for `>>=` at every desired
   type.

2. Due to function application having a `Maybe` result, even at a single type writing
   `ap2 (lit2 ((>>=) :: Type)) e1 >>= \f -> ap2 f e2` is not nice.

3. The original need for this expression representation was for generating Haskell terms, and
   generating lambda terms is tricky; it would be nice to be able to bind holes directly.

This calls for a third representation of expressions.  For reasons that will become apparent when
looking at the evaluator, we'll specalise this to only working in one monad, and track the monad
type as another parameter of `Expr`:

```haskell
data Expr (m :: * -> *) (h :: *)
  = Lit  String BDynamic
  | Var  TypeRep (Var h)
  | Bind TypeRep (Expr m h) (Expr m h)
  | Ap   TypeRep (Expr m h) (Expr m h)

instance Show (Expr m h) where
  show (Lit s _) = s
  show (Var ty v) = "(" ++ show v ++ " :: " ++ show ty ++ ")"
  show (Bind _ b e) = "bind <" ++ show b ++ "> in <" ++ show e ++ ">"
  show (Ap   _ f e) = "(" ++ show f ++ ") (" ++ show e ++ ")"

data Var h
  = Hole  h
  | Named String
  | Bound Int
  -- ^ (>>=)-bound variables, with a de Bruijn index.

instance Show (Var h) where
  show (Hole  _) = "_"
  show (Named s) = s
  show (Bound i) = show i
```

As this is our final representation, I am dropping the numeric suffix.

**Construction**: type-checking a bind is similar to type-checking an allocation of names to holes.
We make sure all the types match, and replace the `Hole` values with `Bound` values.  All binds will
be to the top-level.

```haskell
-- | Monadically bind a collection of holes, if type-correct.
--
-- This has the same indexing behaviour as 'names'.
bind :: forall m. Typeable m => [Int] -> Schema m -> Schema m -> Maybe (Schema m)
bind is b e0 = case (splitTyConApp (typeOf b), splitTyConApp (typeOf e0)) of
    ((btyCon, btyArgs), (etyCon, etyArgs))
      | btyCon == mtyCon && btyCon == etyCon && not (null btyArgs) && not (null etyArgs) && mtyArgs == init btyArgs && init btyArgs == init etyArgs
        -> Bind (typeOf e0) b <$> bind (last btyArgs)
    _ -> Nothing
  where
    (mtyCon, mtyArgs) = splitTyConApp (typeRep (Proxy :: Proxy m))

    bind boundTy = fst <$> go 0 0 e0 where
      go n i (Var ty (Hole h))
        | i `elem` is = if boundTy == ty then Just (Var ty (Bound n), i + 1) else Nothing
        | otherwise   = Just (Var ty (Hole h), i + 1)
      go n i (Bind ty b e) = do
        (b', i')  <- go n     i  b
        (e', i'') <- go (n+1) i' e
        Just (Bind ty b' e', i'')
      go n i (Ap ty f e) = do
        (f', i')  <- go n i  f
        (e', i'') <- go n i' e
        Just (Ap ty f' e', i'')
      go _ i e = Just (e, i)
```

**Evaluation**: the new bind case, unfortunately, complicates things somewhat here.  It's *much*
more awkward to deal with errors during evaluation, but fortunately the only errors that can
actually arise are unbound named variables: the smart constructors ensure expressions are well-typed
and have valid de Bruijn indices.  This means we can just check the named variables for validity up
front and then use `error` once we're sure there actually are no errors.

```haskell
-- | Evaluate a term
eval :: forall m. (Monad m, Typeable m) => [(String, BDynamic)] -> Term m -> Maybe BDynamic
eval globals e0
    | all check (names e0) = Just (go [] e0)
    | otherwise = Nothing
  where
    go locals (Bind ty b e) = case (unwrapFunctor :: BDynamic -> Maybe (m BDynamic)) (go locals b) of
      Just mdyn -> unsafeWrapFunctor ty $ mdyn >>= \dyn -> case unwrapFunctor (go (dyn:locals) e) of
        Just dyn -> dyn
        Nothing -> error "type error I can't deal with here!" -- this is unreachable
      Nothing -> error "type error I can't deal with here!" -- this is unreachable
    go locals (Var ty v) = case env locals ty v of
      Just dyn -> dyn
      Nothing -> error "environment error I can't deal with here!" -- this is unreachable
    go locals (Ap _ f e) = case go locals f `bdynApply` go locals e of
      Just dyn -> dyn
      Nothing -> error "type error I can't deal with here!" -- this is unreachable
    go _ (Lit _ dyn) = dyn

    env locals _ (Bound n)
      | length locals > n = Just (locals !! n)
      | otherwise = Nothing
    env _ ty (Named s) = case lookup s globals of
      Just dyn | bdynTypeRep dyn == ty -> Just dyn
      _ -> Nothing
    env _ _ (Hole v) = absurd v

    check (s, ty) = case lookup s globals of
      Just dyn -> bdynTypeRep dyn == ty
      Nothing  -> False
```

Now it becomes apparent why the monad type parameter is needed in the expression type, the evaluator
uses `>>=`, and so it needs to know which monad to bind it as. An alternative would be to use a type
like this, but this still restricts you to using a single monad and so doesn't gain anything:

```
eval :: (Monad m, Typeable m) => proxy m -> [(String, BDynamic)] -> Term -> Maybe BDynamic
```

Here's a little example showing that side-effects do work (when I first did this, they didn't, so
it's not quite trivial to get right):

```
λ> r <- newIORef (5::Int)
λ> let intHole = hole $ T.typeOf (5::Int)
λ> let plusLit = lit "+" . toBDynamic $ ((+) :: Int -> Int -> Int)
λ> let plusTwo = fromJust $ (fromJust $ plusLit `ap` intHole) `ap` intHole
λ> let pureInt = lit "pure" . toBDynamic $ (pure :: Int -> IO Int)
λ> let plusTwoIO = fromJust $ pureInt `ap` plusTwo
λ> let intAndTimes = (lit "*2" . toBDynamic $ (modifyIORef r (*7) >> pure (7::Int))) :: Expr IO h
λ> let eval = fromJust $ (fromBDynamic :: BDynamic -> Maybe (IO Int)) =<< eval [] =<< toTerm =<< bind [0,1] intAndTimes plusTwoIO
λ> eval
14
λ> readIORef r
7
λ> eval
14
λ> readIORef r
49
```

The rest of the code is basically the same as in Mark 2.

### Limited Polymorphism

The representation so far is good, and lets us express everything we want, but it's still not very
friendly to use in one common case: polymorphic monadic functions.

There are many monadic operations of the type `Monad m => m a -> m ()`: the actual type of the first
argument is ignored.  At the moment, dealing with such terms requires either specialising that `a`
to each concrete type used, or using something like `void` and specialising *that*.

Implementing full-blown Haskell polymorphism would be a pain, but this is a small and irritating
enough case that it's worth dealing with.

Presenting (trumpets please), the "ignore" type:

```haskell
-- | A special type for enabling basic polymorphism.
--
-- A function parameter of type @m Ignore@ unifies with values of any type @m a@, where @fmap
-- (const Ignore)@ is applied to the parameter automatically. This avoids the need to clutter
-- expressions with calls to 'void', or some other such function.
data Ignore = Ignore deriving (Bounded, Enum, Eq, Ord, Read, Show)
```

`Ignore` is going to give us our limited polymorphism, by changing the typing rules for `ap` and
evaluation rules for `Ap` slightly.

**Application**: function application is as normal, with the exception that if the formal parameter
has type `m Ignore` and the actual parameter has type `m a`, for any `a`, then the application also
succeeds:

```haskell
-- | Perform a function application, if type-correct.
--
-- There is a special case, see the comment of the 'Ignore' type.
apig :: forall m h. (Applicative m, Typeable m) => Expr m h -> Expr m h -> Maybe (Expr m h)
apig f e = case (splitTyConApp (typeOf f), splitTyConApp (typeOf e)) of
    ((_, [fargTy,fresTy]), (etyCon, etyArgs))
      -- check if the formal parameter is of type @m Ignore@ and the actual parameter is of type @m a@
      | fargTy == ignoreTy && etyCon == mtyCon && not (null etyArgs) && mtyArgs == init etyArgs -> Just (Ap fresTy f e)
      -- otherwise try normal function application
      | otherwise -> (\ty -> Ap ty f e) <$> typeOf f `funResultTy` typeOf e
    _ -> Nothing
  where
    ignoreTy = typeOf (pure Ignore :: m Ignore)
    (mtyCon, mtyArgs) = splitTyConApp (typeRep (Proxy :: Proxy m))
```

**Evaluation**: evaluation of applications has an analogous case.  When applying a function, the
type of the formal parameter is checked and, if it's `m Ignore`, the argument gets `fmap (const
Ignore)` applied:

```haskell
-- | Evaluate a term
evalig :: forall m. (Monad m, Typeable m) => [(String, BDynamic)] -> Term m -> Maybe BDynamic
evalig globals e0
    | all check (names e0) = Just (go [] e0)
    | otherwise = Nothing
  where
    go locals (Bind ty b e) = case (unwrapFunctor :: BDynamic -> Maybe (m BDynamic)) (go locals b) of
      Just mdyn -> unsafeWrapFunctor ty $ mdyn >>= \dyn -> case unwrapFunctor (go (dyn:locals) e) of
        Just dyn -> dyn
        Nothing -> error "type error I can't deal with here!"
      Nothing -> error "type error I can't deal with here!"
    go locals (Var ty v) = case env locals ty v of
      Just dyn -> dyn
      Nothing -> error "environment error I can't deal with here!"
    go locals (Ap _ f e) =
      let f' = go locals f
          e' = go locals e
      in case f' `bdynApply` (if hasIgnoreArg f' then ignore e' else e') of
        Just dyn -> dyn
        Nothing -> error "type error I can't deal with here!"
    go _ (Lit _ dyn) = dyn

    env locals _ (Bound n)
      | length locals > n = Just (locals !! n)
      | otherwise = Nothing
    env _ ty (Named s) = case lookup s globals of
      Just dyn | bdynTypeRep dyn == ty -> Just dyn
      _ -> Nothing
    env _ _ (Hole v) = absurd v

    hasIgnoreArg fdyn =
      let (_, [fargTy,_]) = splitTyConApp (bdynTypeRep fdyn)
      in fargTy == ignoreTy

    ignore dyn = case (unwrapFunctor :: BDynamic -> Maybe (m BDynamic)) dyn of
      Just ma -> unsafeToBDynamic ignoreTy (const Ignore <$> ma)
      Nothing -> error "non-monadic value I can't deal with here!" -- this is unreachable

    ignoreTy = typeOf (pure Ignore :: m Ignore)

    check (s, ty) = case lookup s globals of
      Just dyn -> bdynTypeRep dyn == ty
      Nothing  -> False
```

The final piece of the puzzle is this:

```haskell
-- | Convert an arbitrary value into a dynamic value, given its type.
--
-- This is unsafe because if the type is incorrect and the value is later used as that type, good
-- luck.
unsafeToBDynamic :: TypeRep -> a -> BDynamic
unsafeToBDynamic ty = BDynamic ty . unsafeCoerce
```

And a demo:

```
λ> r <- newIORef (5::Int)
λ> let double = lit $ toBDynamic ((\x -> x >> x >> pure ()) :: IO Ignore -> IO ()) :: Expr IO h
λ> let addOne = lit $ toBDynamic (modifyIORef r (+1)) :: Expr IO h
λ> let addTwo = fromJust $ double `apig` addOne
λ> let eval = fromJust $ (fromBDynamic :: BDynamic -> Maybe (IO ())) =<< evalig [] =<< toTerm addTwo
λ> eval
λ> readIORef r
7
λ> eval
λ> readIORef r
9
```

That's part 1 of 3 done!  The actual code for this section is in the [Test.Spec.Expr][]
and [Test.Spec.Type][] modules on [GitHub][].

[Test.Spec.Expr]: https://github.com/barrucadu/spec/blob/master/Test/Spec/Expr.hs
[Test.Spec.Type]: https://github.com/barrucadu/spec/blob/master/Test/Spec/Type.hs
[GitHub]: https://github.com/barrucadu/spec


Generating Expressions
----------------------

So we can represent expressions, although it's a little cumbersome to build up complex ones by hand.
Now for part 2: generating expressions.  As in the previous section, we're going to work through a
simpler implementation before we arrive at our final goal, a `Generator` type and associated
functions to construct new expressions.

```haskell
{-# LANGUAGE StandaloneDeriving #-}

module Generate where

import Control.Monad (filterM)
import Data.Function (on)
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.Maybe (maybeToList)
import Data.Semigroup (Semigroup, (<>))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Typeable (Typeable, TypeRep)

import Expr
```

To more concretely tie down what we're doing, we're going to generate expressions in *size* order.
Expression size here corresponds roughly to the number of nodes in the tree:

```haskell
sizeOf :: Expr m h -> Int
sizeOf (Lit _ _) = 1
sizeOf (Var _ _) = 1
sizeOf (Bind _ b e) = 1 + sizeOf b + sizeOf e
sizeOf (Ap   _ f e) =     sizeOf f + sizeOf e
```

In order to avoid duplicates, we're going to want sets of expressions.  We're going to cheat a
little because we can't actually compare `BDynamic` values, so we'll just compare their types, and
hope that that plus the string representations in `Lit` will be enough to disambiguate.

```haskell
instance Eq BDynamic  where (==) = (==) `on` bdynTypeRep
instance Ord BDynamic where compare = compare `on` bdynTypeRep

deriving instance Eq  h => Eq  (Var h)
deriving instance Ord h => Ord (Var h)

deriving instance Eq  h => Eq  (Expr m h)
deriving instance Ord h => Ord (Expr m h)
```

### Mark 1: Generating Terms

We're going to have quite a simple interface for our schema generator, we shall have:

1. A type of generators, which is a map from size to generated schemas of that size.
2. A function to get all the schemas of a size.
3. A function to make a new generator from a collection of "primitive" schemas.
4. A function to generate a size, assuming all smaller sizes have been generated.

```haskell
-- | A generator of schemas, in size order.
newtype Generator1 m = Generator1 { tiers1 :: IntMap (Set (Schema m)) }

-- | Get all schemas of the given size, if generated.
schemas1 :: Generator1 m -> Int -> Set (Schema m)
schemas1 g i = M.findWithDefault (S.empty) i (tiers1 g)
```

**Creation**: to make a new generator, we'll just plug the provided schemas into the appropriate
tiers.

```haskell
-- | Create a new generator from a set of initial schemas.
create1 :: [Schema m] -> Generator1 m
create1 initial = Generator1 $ M.unionsWith S.union
  [M.singleton (sizeOf s) (S.singleton s) | s <- initial]
```

**Generation**: now we see the benefit of all the smart `Maybe`-returning constructors: we can do
the incredibly naive thing of just trying all correctly-sized combinations of already-known schemas.
The ones which return a `Just` are good and shall be kept.  To generate new `Bind`s, we want to try
all ways of binding the holes.

```haskell
-- | Generate schemas of the given size, assuming all smaller tiers have been generated.
generate1 :: (Applicative m, Typeable m) => Int -> Generator1 m -> Generator1 m
generate1 i g = Generator1 $ M.unionsWith S.union
    [ tiers1 g
    , M.singleton i aps
    , M.singleton i binds
    ]
  where
    -- sizeOf (ap f e) = 0 + sizeOf f + sizeOf e
    aps = makeTerms 0 $ \terms candidates ->
      [ new | f <- terms
            , e <- candidates
            , new <- maybeToList (ap f e)
      ]
    -- sizeOf (bind is b e) = 1 + sizeOf b + sizeOf e
    binds = makeTerms 1 $ \terms candidates ->
      [ new | b <- terms
            , e <- candidates
            , holeset <- powerset . map fst $ holes e
            , new <- maybeToList (bind holeset b e)
      ]

    makeTerms n f = M.foldMapWithKey go (tiers1 g) where
      go tier terms = S.fromList $
        let candidates = schemas1 g (i - tier - n)
        in f (S.toList terms) (S.toList candidates)

    powerset = filterM (const [False,True])
```

### Mark 2: Annotations & Pruning

The current generator is nice and simple, but is pretty limited in practice.  It happily generates
`Bind`s where the binder is a hole, which is uninteresting; and furthermore provides no way to store
additional information along with the generated schemas, which will prove useful.

```haskell
-- | A generator of schemas with metadata, in size order.
newtype Generator m ann = Generator { tiers :: IntMap (Set (Schema m, ann)) }
```

Generation is now a bit more involved:

```haskell
-- | Generate schemas of the given size, assuming all smaller tiers have been generated.
generate :: (Applicative m, Typeable m, Semigroup ann, Ord ann)
  => (ann -> ann -> Schema m -> Bool)
  -- ^ A predicate to filter generated schemas.
  -> Int
  -> Generator m ann
  -> Generator m ann
generate annp i g = Generator $ M.unionsWith S.union
    [ tiers g
    , M.singleton i aps
    , M.singleton i binds
    ]
  where
    aps = makeTerms 0 $ \terms candidates ->
      [ (new, fAnn <> eAnn) -- produce a new annotation by combining the old
        | (f, fAnn) <- terms
        , (e, eAnn) <- candidates
        , new <- maybeToList (ap f e)
        -- check the new expression and old annotations against the predicate
        , annp fAnn eAnn new
      ]

    binds = makeTerms 1 $ \terms candidates ->
      [ (new, bAnn <> eAnn) -- produce a new annotation by combining the old
        | (b, bAnn) <- terms
        -- don't allow a binder which is a hole
        , case b of Var _ (Hole _) -> False; _ -> True
        , (e, eAnn) <- candidates
        , holeset <- powerset . map fst $ holes e
        , new <- maybeToList (bind holeset b e)
        -- check the new expression and old annotations against the predicate
        , annp bAnn eAnn new
      ]

    makeTerms n f = M.foldMapWithKey go (tiers g) where
      go tier terms = S.fromList $
        let candidates = schemas g (i - tier - n)
        in f (S.toList terms) (S.toList candidates)

    powerset = filterM (const [False,True])
```

The `schemas` and `create` code remain the same.

Here is a small demo:

```
demo :: Generator IO ()
demo = create $ map (\e -> (e, ()))
  [ hole $ typeRep (Proxy :: Proxy Int)
  , lit "0" . toBDynamic $ (0 :: Int)
  , lit "1" . toBDynamic $ (1 :: Int)
  , lit "+" . toBDynamic $ ((+) :: Int -> Int -> Int)
  , lit "*" . toBDynamic $ ((*) :: Int -> Int -> Int)
  ]

λ> let upto n = mapM_ print . S.toList $ schemas (foldl (flip $ generate \_ _ _ -> True) demo [1..n]) n

λ> upto 1
((*,())
(+,())
(0,())
(1,())
((_ :: Int),())

λ> upto 2
((*) (0),())
((*) (1),())
((*) ((_ :: Int)),())
((+) (0),())
((+) (1),())
((+) ((_ :: Int)),())

λ> upto 3
(((*) (0)) (0),())
(((*) (0)) (1),())
(((*) (0)) ((_ :: Int)),())
(((*) (1)) (0),())
(((*) (1)) (1),())
(((*) (1)) ((_ :: Int)),())
(((*) ((_ :: Int))) (0),())
(((*) ((_ :: Int))) (1),())
(((*) ((_ :: Int))) ((_ :: Int)),())
(((+) (0)) (0),())
(((+) (0)) (1),())
(((+) (0)) ((_ :: Int)),())
(((+) (1)) (0),())
(((+) (1)) (1),())
(((+) (1)) ((_ :: Int)),())
(((+) ((_ :: Int))) (0),())
(((+) ((_ :: Int))) (1),())
(((+) ((_ :: Int))) ((_ :: Int)),())
```

That's part 2 of 3 done, nearly there!  The actual code for this section is in the [Test.Spec.Gen][]
module on [GitHub][].

[Test.Spec.Gen]: https://github.com/barrucadu/spec/blob/master/Test/Spec/Gen.hs


Mixing and Matching Variables
-----------------------------

To recap: we can generate *schemas* and produce a set of *terms* from a schema, and we can evaluate
these terms.  Because evaluating terms in [spec][] is expensive (we're evaluating actions with
concurrency!), we want to store the results and use these stored results to later compare schemas.

The problem is: given two terms (from different schemas) and their results, how do we compare them?
For instance, consider we have two terms:

```
term1 = f (x :: Int) (y :: Int)  (z :: Bool)
term2 = g (x :: Int) (y :: Bool) (z :: Bool)
```

Which fulfil the property that when the `z` in `term1` is equal to the `y` in `term2`, the terms
have the same result.  Each term introduces its own environment variable namespace, so what we want
is a projection into a shared namespace, in which we can reason about the equalities we desire.

We can simplify the problem a little by not changing the number of unique variables inside a term,
only restricting ourselves to identifying (or not) variables across terms.  Here are some possible
projections of those terms:

```
f x1 y1 z1 =?= g x1 y2 z2
f x1 y1 z1 =?= g x2 z1 z2
f x1 y1 z1 =?= g x2 y2 z1
f x1 y1 z1 =?= g x2 y1 z2
and so on
```

So we want to produce a *set* of all such type-correct projections.  Here we go!

```haskell
{-# LANGUAGE LambdaCase #-}

module Rename where

import Expr
```

### The `These` type

I recently read a blog post entitled [These, Align, and Crosswalk][theseblog] about safely zipping
(or merging) data structures.  The core of the blog post is the `These` type, defined as so:

```haskell
-- | The @These@ type is like 'Either', but also has the case for when we have both values.
data These a b
  = This a
  | That b
  | These a b
  deriving Show
```

This type turns out to be exactly the thing we need!

[theseblog]: http://teh.id.au/posts/2017/03/29/these-align-crosswalk
[these]: https://hackage.haskell.org/package/these

### Finding All Renamings

Our challenge is to find two functions:

```
projections :: Expr m h -> Expr m h -> [[(These String String, TypeRep)]]
renaming    :: (TypeRep -> Char) -> [(These String String, TypeRep)] -> ([(String, String)], [(String, String)])
```

Where a `These String String` value represents a type-correct renaming of variables:

- `This s` means a variable from the left term is kept distinct from all variables in the right
  term.
- `That s` means a variable from the right term is kept distinct from all variables in the left
  term.
- `These s1 s2` means a variable from the left term is identified with a variable from the right
  term.

```haskell
-- | Find all type-correct ways of associating variables.
projections :: Term m h -> Term m h -> [[(These String String, TypeRep)]]
projections t1 t2 = go (names t1) (names t2) where
  go e1 [] = [[(This v, ty) | (v, ty) <- e1]]
  go [] e2 = [[(That v, ty) | (v, ty) <- e2]]
  go ((vL, tyL):e1) e2 =
   map ((This vL, tyL) :) (go e1 e2) ++
   concat [map ((These vL vR, tyL) :) (go e1 (filter (/=x) e2)) | x@(vR, tyR) <- e2, tyL == tyR]
```

This generates projections in order from most general (most `This`/`That` usage) to least general
(most `These` usage).

Now that we have projections, we can produce consistent renamings:

```haskell
-- | Given a projection into a common namespace, produce a consistent variable renaming. Variables
-- of the same type, after the first, will have a number appended starting from 1.
renaming :: (TypeRep -> Char) -> [(These String String, TypeRep)] -> ([(String, String)], [(String, String)])
renaming varf = go [] ([], []) where
  go e x ((these, ty):rest) =
    let name = [varf ty]
    in rename e x name (maybe 0 (+1) $ lookup name e) these rest
  go e x [] = x

  rename e ~(l, r) name n = let name' = if n == 0 then name else name ++ show n in \case
    This  vL    -> go ((name, n):e) ((vL, name'):l,             r)
    That     vR -> go ((name, n):e) (            l, (vR, name'):r)
    These vL vR -> go ((name, n):e) ((vL, name'):l, (vR, name'):r)
```

The two steps can be combined:

```haskell
-- | Find all consistent renamings of a pair of terms.
renamings :: (TypeRep -> Char) -> Term m h -> Term m h -> [([(String, String)], [(String, String)])]
renamings varf t1 t2 = map (renaming varf) (projections t1 t2)
```

Here it is in action:

```
-- construct the schemas
λ> let f = lit3 "f" $ toBDynamic ((\_ _ z -> z) :: Int -> Int  -> Bool -> Bool)
λ> let g = lit3 "g" $ toBDynamic ((\_ y _ -> y) :: Int -> Bool -> Bool -> Bool)
λ> let iHole = hole3 (typeOf (5::Int))
λ> let bHole = hole3 (typeOf True)
λ> let schema1 = fromJust (fromJust (fromJust (f `ap3` iHole) `ap3` iHole) `ap3` bHole)
λ> let schema2 = fromJust (fromJust (fromJust (g `ap3` iHole) `ap3` bHole) `ap3` bHole)

-- produce the terms
λ> let varf = toLower . head . show
λ> let terms1 = terms3 varf schema1
λ> let terms2 = terms3 varf schema2

-- find the renamings of the most general terms
λ> mapM_ print (renamings varf (head terms1) (head terms2))
([("b","b"),("i1","i1"),("i","i")],[("b1","b2"),("b","b1"),("i","i2")])
([("b","b"),("i1","i1"),("i","i")],[("b1","b1"),("i","i2"),("b","b")])
([("b","b"),("i1","i1"),("i","i")],[("b","b1"),("i","i2"),("b1","b")])
([("b","b"),("i1","i1"),("i","i")],[("b1","b2"),("b","b1"),("i","i1")])
([("b","b"),("i1","i1"),("i","i")],[("b1","b1"),("b","b"),("i","i1")])
([("b","b"),("i1","i1"),("i","i")],[("b","b1"),("b1","b"),("i","i1")])
([("b","b"),("i1","i1"),("i","i")],[("b1","b2"),("b","b1"),("i","i")])
([("b","b"),("i1","i1"),("i","i")],[("b1","b1"),("b","b"),("i","i")])
([("b","b"),("i1","i1"),("i","i")],[("b","b1"),("b1","b"),("i","i")])
```

My only regret is that I found no use for the fancier functions in the [these][] package.

That's part 3 done!  The actual code for this section is in the [Test.Spec.Rename][] module
on [GitHub][].

[Test.Spec.Rename]: https://github.com/barrucadu/spec/blob/master/Test/Spec/Rename.hs


Wrap-up
-------

That's all, folks!  I am writing a paper on [spec][], which explains all this material from a
different angle (but with less code), but for now the best resource if you want to know more is
the [GitHub][] repository.
