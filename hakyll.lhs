barrucadu.co.uk Source
======================

This is the Hakyll source for barrucadu.co.uk. The directory structure is as
follows:

 - static/ - Files to be copied over, with the static/ dropped.

Preamble
--------

> {-# LANGUAGE OverloadedStrings #-}
> module Main where
> import Control.Monad (void)
> import Hakyll

Static Files
------------

> dostatic :: Pattern (Page String) -> RulesM ()
> dostatic pattern = void $ match pattern $ do
>                      route   $ dropPat "static/"
>                      compile copyFileCompiler

Main
----

The configuration for Hakyll:

> config = defaultHakyllConfiguration
>          {
>            ignoreFile = \a -> False
>          }

Now, the files are built and copied across to the appropriate locations.

> main :: IO ()
> main = hakyllWith config $ ["static/*"] --> [dostatic]

Utilities
---------

For static files, we need to drop the static/ directory name, the dropPat
route does just that. This is just a small wrapper around gsubRoute.

> dropPat :: String -> Routes
> dropPat pat = gsubRoute pat (const "")

Litle combinator for applying a list of patterns to a list of do* functions.

> (-->) :: Monad m => [a] -> [a -> m b] -> m ()
> xs --> [f]    = mapM_ f xs
> xs --> (f:fs) = mapM_ f xs
>                 >> xs --> fs