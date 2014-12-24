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
> import Data.Monoid ((<>))
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
>   route $ dropPat "static/"
>   compile copyFileCompiler

Error Pages
-----------

Error pages use the error.hamlet template, and have absolute (not relative)
links.

> doerrors :: Pattern -> Rules ()
> doerrors pattern = void $ match pattern $ do
>   route   $ setExtension ".html"
>   compile $ pandocCompiler
>     >>= applyTemplateCompiler "template/error.hamlet"

Pages
-----

> dopages :: Pattern -> Rules ()
> dopages pattern = void $ match pattern $ do
>   route   $ composeRoutes (dropPat "pages/") (setExtension ".html")
>   compile $ pandocCompiler
>     >>= applyTemplateCompiler "template/page.hamlet"
>     >>= relativizeUrls

Blog
----

Render each post as a page, but also save a snapshot of the content
for reference when building the listing page.

> doposts :: Pattern -> Rules ()
> doposts pattern = void $ match pattern $ do
>   route   $ setExtension ".html"
>   compile $ pandocCompiler
>     >>= saveSnapshot "content"
>     >>= return . fmap demoteHeaders
>     >>= applyTemplateCompiler "template/page.hamlet"
>     >>= relativizeUrls


This is what produces the listing pages: it takes a file name to
generate, a title, and a pattern matching inline pages to include as
separate "blog posts".

> dolisting :: Pattern -> Rules ()
> dolisting pattern = void $ match "index.markdown" $ do
>   route   $ setExtension ".html"
>   compile $ pandocCompiler
>     >>= loadAndApplyTemplate "template/index.hamlet" entryCtx
>     >>= relativizeUrls
>   where
>     entries      = recentFirst =<< loadAll pattern
>     entryDateCtx = dateField "date" "%B %e, %Y"             <> defaultContext
>     entryCtx     = listField "entries" entryDateCtx entries <> defaultContext

And this ties together rendering blog posts both as separate pages,
and also as inline chunks for the listing.

> doblog :: Pattern -> Rules ()
> doblog pattern = do
>   doposts   pattern
>   dolisting pattern

Main
----

Now, the files are built and copied across to the appropriate locations.

> main :: IO ()
> main = hakyllWith defaultConfiguration $ do
>   dotemplates "template/*"
>   dostatic    "static/**"
>   doerrors    "errors/*"
>   dopages     "pages/*"
>   doblog      "posts/*"

Utilities
---------

For static files, we need to drop the static/ directory name, the dropPat
route does just that. This is just a small wrapper around gsubRoute.

> dropPat :: String -> Routes
> dropPat pat = gsubRoute pat (const "")

Apply a template with the default context

> applyTemplateCompiler :: Identifier -> Item String -> Compiler (Item String)
> applyTemplateCompiler tpl = loadAndApplyTemplate tpl defaultContext
