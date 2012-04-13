barrucadu.co.uk Source
======================

This is the Hakyll source for barrucadu.co.uk. The directory structure is as
follows:

 - static/ - Files to be copied over, with the static/ dropped.

Preamble
--------

> {-# LANGUAGE OverloadedStrings #-}
> module Main where
> import Control.Arrow ((>>>))
> import Control.Monad (void)
> import Hakyll

Templates
---------

All of the template files need to be read into Hakyll before they can be
referenced elsewhere.

> dotemplates :: Pattern (Identifier Template) -> RulesM ()
> dotemplates pattern = void $ match pattern $ compile templateCompiler

Static Files
------------

> dostatic :: Pattern (Page String) -> RulesM ()
> dostatic pattern = void $ match pattern $ do
>                      route   $ dropPat "static/"
>                      compile copyFileCompiler

Error Pages
-----------

Error pages use the error.hamlet template, and have absolute (not relative)
links.

> doerrors :: Pattern (Page String) -> RulesM ()
> doerrors pattern = void $ match pattern $ do
>                      route   $ setExtension ".html"
>                      compile $ pageCompiler
>                        >>> applyTemplateCompiler "template/error.hamlet"

Main
----

The configuration for Hakyll:

> config = defaultHakyllConfiguration
>          { deployCommand = "rsync -avz --checksum _site/ \
>                            \yuggoth:/srv/http/barrucadu.co.uk/www",
>            ignoreFile = \a -> False
>          }

Now, the files are built and copied across to the appropriate locations.

> main :: IO ()
> main = hakyllWith config $ do
>          dotemplates "template/*"
>          dostatic "static/**"
>          doerrors "errors/*"

Utilities
---------

For static files, we need to drop the static/ directory name, the dropPat
route does just that. This is just a small wrapper around gsubRoute.

> dropPat :: String -> Routes
> dropPat pat = gsubRoute pat (const "")
