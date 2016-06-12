{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char (toLower)
import Data.Monoid ((<>))
import Hakyll
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
  match "posts/*" $ do
    route   $ setExtension ".html"
    compile $ pandocWithPygments
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= loadAndApplyTemplate "templates/wrapper.html" postCtx
      >>= relativizeUrls

  -- Render all posts
  create ["posts.html"] $
    postList Nothing "posts.html" "All Posts" (makeItem "")

  -- Render index page
  match "index.markdown" $ do
    route $ setExtension ".html"
    compile $ pandocWithPygments
      >>= loadAndApplyTemplate "templates/wrapper.html"  (constField "title" "barrucadu" <> defaultContext)
      >>= relativizeUrls

  -- Create blog feed
  create ["atom.xml"] $ do
    route     idRoute
    compile $ loadAllSnapshots "posts/*" "content"
      >>= fmap (take 10) . recentFirst
      >>= renderAtom feedCfg feedCtx

-------------------------------------------------------------------------------
-- Contexts and Configurations

postCtx :: Context String
postCtx = mconcat
  [ dateField "isodate" "%Y-%m-%d"
  , dateField "ppdate"  "%B %e, %Y"
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
    _ -> pure $ "<div class =\"highlight\"><pre>" ++ code ++ "</pre></div>"
  highlight x = pure x

  -- Apply language-specific syntax highlighting
  withLanguage lang = readProcess "pygmentize" ["-l", map toLower lang,  "-f", "html"]

-- | Concatenate and minify a collection of items.
minifyCompiler :: (String -> String) -> [Item String] -> Compiler (Item String)
minifyCompiler minify = makeItem . minify . concatMap itemBody

-------------------------------------------------------------------------------
-- Utilities

-- | Render a possibly limited post list to the given path
postList :: Maybe Int -> FilePath -> String -> Compiler (Item String) -> Rules ()
postList limit fname title compiler = do
  route $ constRoute fname
  compile $ do
    entries <- fmap (maybe id take limit) . recentFirst =<< loadAll "posts/*"
    let ctx = constField "title" title <>
              listField "posts" postCtx (return entries) <>
              defaultContext

    compiler
      >>= loadAndApplyTemplate "templates/postlist.html" ctx
      >>= loadAndApplyTemplate "templates/wrapper.html"  ctx
      >>= relativizeUrls

-- | Remove some portion of the route
dropPat :: String -> Routes
dropPat pat = gsubRoute pat (const "")
