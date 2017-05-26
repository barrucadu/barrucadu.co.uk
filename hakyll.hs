{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char (toLower)
import Data.Monoid ((<>))
import Hakyll
import Hakyll.Contrib.Hyphenation (hyphenateHtml, english_GB)
import System.Process (readProcess)
import Text.Pandoc.Definition (Pandoc, Block(..), Format(..))
import Text.Pandoc.Walk (walkM)

main :: IO ()
main = hakyllWith defaultConfiguration $ do
  -- Templates
  match "templates/*" $ compile templateCompiler

  -- Copy static files
  match "static/**" $ do
    route $ dropPat "static/"
    compile copyFileCompiler

  -- Minify CSS
  match "css/*" $ compile getResourceBody
  create ["style.css"] $ do
    route idRoute
    compile $ loadAll "css/*"
      >>= minifyCompiler compressCss

  -- Render blog posts
  match "posts/*/*.markdown" $ do
    route   $ setExtension ".html"
    compile $ pandocWithPygments
      >>= hyphenateHtml english_GB
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/page.html" pageCtx
      >>= loadAndApplyTemplate "templates/html.html" pageCtx
      >>= relativizeUrls

  -- Render HTML pages
  match "pages/*.markdown" $ do
    route $ dropPat "pages/" `composeRoutes` setExtension ".html"
    compile $ pandocWithPygments
      >>= hyphenateHtml english_GB
      >>= loadAndApplyTemplate "templates/page.html" pageCtx
      >>= loadAndApplyTemplate "templates/html.html" pageCtx
      >>= relativizeUrls

  -- Render index page
  match "index.html" $ do
    route idRoute
    compile $ do
      posts_relnotes    <- recentFirst =<< loadAll "posts/relnotes/*.markdown"
      posts_concurrency <- recentFirst =<< loadAll "posts/concurrency/*.markdown"
      posts_etc         <- recentFirst =<< loadAll "posts/etc/*.markdown"
      let ctx = constField "title" "barrucadu" <>
                listField "posts_relnotes"    pageCtx (return posts_relnotes)    <>
                listField "posts_concurrency" pageCtx (return posts_concurrency) <>
                listField "posts_etc"         pageCtx (return posts_etc)         <>
                defaultContext
      getResourceBody
        >>= applyAsTemplate ctx
        >>= hyphenateHtml english_GB
        >>= loadAndApplyTemplate "templates/html.html" ctx
        >>= relativizeUrls

  -- Create blog feed
  create ["atom.xml"] $ do
    route     idRoute
    compile $ loadAllSnapshots "posts/*/*.markdown" "content"
      >>= fmap (take 10) . recentFirst
      >>= renderAtom feedCfg feedCtx

-------------------------------------------------------------------------------
-- Contexts and Configurations

pageCtx :: Context String
pageCtx = mconcat
  [ dateField "isodate" "%Y-%m-%d"
  , dateField "ppdate"  "%d %b, %Y"
  , defaultContext
  ]

feedCfg :: FeedConfiguration
feedCfg = FeedConfiguration
  { feedTitle       = "barrucadu :: All Posts"
  , feedDescription = "Personal blog of barrucadu"
  , feedAuthorName  = "Michael Walker"
  , feedAuthorEmail = "mike@barrucadu.co.uk"
  , feedRoot        = "http://barrucadu.co.uk"
  }

feedCtx :: Context String
feedCtx = mconcat
  [ bodyField "description"
  , defaultContext
  ]

-------------------------------------------------------------------------------
-- Compilers

-- | The Pandoc compiler, but using pygments/pygmentize for syntax
-- highlighting.
pandocWithPygments :: Compiler (Item String)
pandocWithPygments = pandocCompilerWithTransformM
                       defaultHakyllReaderOptions
                       defaultHakyllWriterOptions
                       pygmentize

-- | Apply pygments/pygmentize syntax highlighting to a Pandoc
-- document.
pygmentize :: Pandoc -> Compiler Pandoc
pygmentize = unsafeCompiler . walkM highlight where
  highlight (CodeBlock opts code) = RawBlock (Format "html") <$> case opts of
    (_, lang:_, _) -> withLanguage lang code
    _ -> pure $ "<div class =\"highlight\"><pre>" ++ escapeHtml code ++ "</pre></div>"
  highlight x = pure x

  -- Apply language-specific syntax highlighting
  withLanguage lang = readProcess "pygmentize" ["-l", map toLower lang,  "-f", "html"]

-- | Concatenate and minify a collection of items.
minifyCompiler :: (String -> String) -> [Item String] -> Compiler (Item String)
minifyCompiler minify = makeItem . minify . concatMap itemBody

-------------------------------------------------------------------------------
-- Utilities

-- | Remove some portion of the route
dropPat :: String -> Routes
dropPat pat = gsubRoute pat (const "")
