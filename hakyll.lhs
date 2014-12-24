barrucadu.co.uk Source
======================

This is the Hakyll source for barrucadu.co.uk. The directory structure is as
follows:

 - errors/   - Error pages
 - posts/    - Blog posts
 - static/   - Files to be copied over, with the static/ dropped.
 - template/ - Template files

Preamble
--------

> {-# LANGUAGE OverloadedStrings #-}
> module Main where
> import Data.Monoid ((<>))
> import Hakyll

> main :: IO ()
> main = hakyllWith defaultConfiguration $ do
>   match "template/*" $ compile templateCompiler
>
>   match "static/**" $ do
>     route $ dropPat "static/"
>     compile copyFileCompiler
>
>   match "errors/*" $ do
>     route $ setExtension ".html"
>     compile $ pandocCompiler
>       >>= loadAndApplyTemplate "template/error.html" defaultContext
>
>   match "posts/*" $ do
>     route   $ setExtension ".html"
>     compile $ pandocCompiler
>       >>= saveSnapshot "content"
>       >>= return . fmap demoteHeaders
>       >>= loadAndApplyTemplate "template/post.html" defaultContext
>       >>= relativizeUrls
>
>   match "index.markdown" $ do
>     let entries      = recentFirst =<< loadAll "posts/*"
>     let entryDateCtx = dateField "date" "%B %e, %Y"             <> defaultContext
>     let entryCtx     = listField "entries" entryDateCtx entries <> defaultContext
>
>     route   $ setExtension ".html"
>     compile $ pandocCompiler
>       >>= loadAndApplyTemplate "template/index.html" entryCtx
>       >>= relativizeUrls

Utilities
---------

For static files, we need to drop the static/ directory name, the dropPat
route does just that. This is just a small wrapper around gsubRoute.

> dropPat :: String -> Routes
> dropPat pat = gsubRoute pat (const "")
