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

**Inline Pages**

Inline pages are like regular pages, in that they are compiled to
HTML, but there is nothing there other than the compiled page body: no
<html>, nothing.

> doinline :: Pattern -> Rules ()
> doinline pattern = void $ match pattern $ version "inline" $ do
>                      route     idRoute
>                      compile $ pandocCompiler
>                        >>= applyTemplateCompiler "template/empty.hamlet"
>                        >>= relativizeUrls


**Listings**

This is what produces publications.html: it takes a file name to
generate, a title, and a pattern matching inline pages to include as
separate "blog posts".

> dolisting :: Identifier -> String -> Pattern -> Rules ()
> dolisting identifier title pattern = void $ create [identifier] $ do
>   route idRoute
>   compile $ do
>     let entries = recentFirst =<< loadAll (pattern .&&. hasVersion "inline")
>     let entryDateCtx = dateField "date" "%B %e, %Y" <> defaultContext
>     let entryCtx = listField "entries" entryDateCtx entries
>                    <> constField "title" title
>                    <> defaultContext
>
>     makeItem ""
>       >>= loadAndApplyTemplate "template/blog.hamlet" entryCtx
>       >>= relativizeUrls

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

Presentations and publications have two versions built: inline, and
normal. The inline versions are used in the listing page
(publications.html), and the normal pages are… normal. Currently the
normal pages are not linked to, but with a little work it would be
possible to have the normal versions contain a long description, and
the inline versions have a much shorter one, with a link to the long
one. Perhaps future work.

>          >> doinline "pubs/*.markdown"
>          >> dopages "pubs/*.markdown"
>          >> dolisting "publications.html" "Presentations and Publications" "pubs/*"
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
