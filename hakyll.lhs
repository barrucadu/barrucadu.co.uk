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
> import Data.Monoid (mappend)
> import Hakyll

Templates
---------

All of the template files need to be read into Hakyll before they can be
referenced elsewhere.

> dotemplates :: Pattern -> Rules ()
> dotemplates pattern = void $ match pattern $ compile templateCompiler

Static Files
------------

> dostatic :: Pattern -> Rules ()
> dostatic pattern = void $ match pattern $ do
>                      route   $ dropPat "static/"
>                      compile copyFileCompiler

Error Pages
-----------

Error pages use the error.hamlet template, and have absolute (not relative)
links.

> doerrors :: Pattern -> Rules ()
> doerrors pattern = void $ match pattern $ do
>                      route   $ setExtension ".html"
>                      compile $ pandocCompiler
>                        >>= applyTemplateCompiler "template/error.hamlet"

Pages
-----

> dopages :: Pattern -> Rules ()
> dopages pattern = void $ match pattern $ do
>                     route   $ composeRoutes (dropPat "pages/") (setExtension ".html")
>                     compile $ pandocCompiler
>                       >>= applyTemplateCompiler "template/page.hamlet"
>                       >>= relativizeUrls

Index
-----

> doindex :: Rules ()
> doindex = void $ match "index.markdown" $ do
>             route   $ setExtension ".html"
>             compile $ pandocCompiler
>               >>= applyTemplateCompiler "template/index.hamlet"
>               >>= relativizeUrls

Main
----

The configuration for Hakyll:

> config :: Configuration
> config = defaultConfiguration
>          { deployCommand = "rsync -avz --checksum _site/ \
>                            \yuggoth:/srv/http/barrucadu.co.uk/www",
>            ignoreFile = const False
>          }

Now, the files are built and copied across to the appropriate locations.

> main :: IO ()
> main = hakyllWith config $
>          dotemplates "template/*"
>          >> dostatic "static/**"
>          >> doerrors "errors/*"
>          >> dopages "pages/*.markdown"
>          >> doindex

Utilities
---------

For static files, we need to drop the static/ directory name, the dropPat
route does just that. This is just a small wrapper around gsubRoute.

> dropPat :: String -> Routes
> dropPat pat = gsubRoute pat (const "")

Apply a template with the default context

> applyTemplateCompiler :: Identifier -> Item String -> Compiler (Item String)
> applyTemplateCompiler tpl = loadAndApplyTemplate tpl defaultContext

Take a context and set a constant field in it.

> setField :: Context a -> String -> String -> Context a
> setField c f v = constField f v `mappend` c