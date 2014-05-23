barrucadu.co.uk Source
======================

This is the Hakyll source for barrucadu.co.uk. The directory structure is as
follows:

 - static/ - Files to be copied over, with the static/ dropped.

Preamble
--------

> {-# LANGUAGE OverloadedStrings #-}
> module Main where
> import Control.Monad (void, join, forM_)
> import Data.List (sort)
> import Data.Monoid (mappend)
> import Data.String.Utils (startswith, endswith, replace)
> import System.Directory (getDirectoryContents)
> import Text.Blaze.Html5 (Html, (!), toValue)
> import Text.Blaze.Internal (preEscapedStringValue)
> import qualified Text.Blaze.Html5 as H
> import qualified Text.Blaze.Html5.Attributes as A
> import Text.Blaze.Html.Renderer.String (renderHtml)
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
>     where scrDirList = do files <- getRecursiveContents (const $ return False) directory
>                           let imgs = map (replace directory "") files
>                           let pngs = filter (endswith ".png") imgs
>                           let shots = map (replace ".png" "") pngs
>                           return $ reverse $ sort shots

And now a convenience wrapper to get the list of screenshots for a named
computer.

> screenshotList :: String -> IO ScreenshotList
> screenshotList name = scrList name $ "static/screenshots/" ++ name ++ "/fullsize/"

Now, we have a function to get a list of screenshots for *every* computer!

> screenshots :: IO [ScreenshotList]
> screenshots = do shots <- join $ do
>                    names <- getDirectoryContents "./static/screenshots/"
>                    return $ mapM screenshotList $
>                      filter (not . startswith ".") names
>                  return $ sort shots

Now that we have out screenshot functions, we need to be able to generate
HTML from them to stick into the pages.

> scrHtml :: ScreenshotList -> Html
> scrHtml (Shots name shots) = H.ol $ forM_ shots scrHtml'
>
>     where scrHtml' s =  H.li $ H.a
>                           ! A.href  (toValue $ "/screenshots/" ++ name ++ "/thumb-big/" ++ s ++ ".png")
>                           ! A.rel   (toValue $ "lightbox-" ++ name)
>                           ! A.title (preEscapedStringValue $ name ++ ": " ++ s ++ " &lt;a href=&quot;/screenshots/" ++ name ++ "/fullsize/" ++ s ++ ".png&quot;&gt;[Click To View Full Size]&lt;/a&gt;") $
>                         H.img
>                           ! A.src (toValue $ "/screenshots/" ++ name ++ "/thumb/" ++ s ++ ".png")
>                           ! A.alt (toValue $ name ++ ": " ++ s)

And finally a function to turn the list of all screenshots into HTML.

> scrsHtml :: [ScreenshotList] -> Html
> scrsHtml shots = forM_ shots scrHtml

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

> doindex :: [ScreenshotList] -> Rules ()
> doindex shots = void $ match "index.markdown" $ do
>                   route   $ setExtension ".html"
>                   compile $ pandocCompiler
>                     >>= loadAndApplyTemplate "template/index.hamlet"
>                       (setField defaultContext "screenshots" (renderHtml $ scrsHtml shots))
>                     >>= relativizeUrls

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
> main = do shots <- screenshots
>           hakyllWith config $ do
>             dotemplates "template/*"
>             dostatic "static/**"
>             doerrors "errors/*"
>             dopages "pages/*.markdown"
>             doindex shots

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