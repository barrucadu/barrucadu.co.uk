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
> import Control.Monad (void, join)
> import Data.List (sort, isPrefixOf, isSuffixOf)
> import Data.Text (pack, unpack, replace)
> import System.Directory (getDirectoryContents)
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

Screenshots
-----------

For the index page, we need a list of screenshots, sorted by date and the name
of the computer which the screenshot is for.

> data ScreenshotList = Shots String [String] deriving (Eq, Ord)

We also need a function to generate the list of screenshots for a given
computer.

> scrList :: String -> String -> IO ScreenshotList
> scrList name directory = do scrs <- scrDirList
>                             return $ Shots name scrs
>     where scrDirList = do files <- getRecursiveContents False directory
>                           let imgs = map (strReplace directory "") files
>                           let pngs = filter (isSuffixOf ".png") imgs
>                           let shots = map (strReplace ".png" "") pngs
>                           return $ reverse $ sort shots

And now a convenience wrapper to get the list of screenshots for a named
computer.

> screenshotList :: String -> IO ScreenshotList
> screenshotList name = scrList name $ "static/screenshots/" ++ name ++ "/fullsize/"

Now, we have a function to get a list of screenshots for *every* computer!

> screenshots :: IO [ScreenshotList]
> screenshots = do shots <- join $ do
>                    names <- getDirectoryContents "./static/screenshots/"
>                    return $ sequence $ map screenshotList $
>                      filter (\a -> not $ isPrefixOf "." a) names
>                  return $ sort shots

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

There is no string replacement function (that I can find), however, there is a
text replacement function.

> strReplace :: String -> String -> String -> String
> strReplace old new str = unpack $ replace (pack old) (pack new) $ pack str
