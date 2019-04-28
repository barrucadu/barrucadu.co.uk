{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (stripPrefix)
import Hakyll

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

  -- Render HTML pages
  match "pages/*.markdown" $ do
    route $ dropPat "pages/" `composeRoutes` setExtension ".html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/page.html" pageCtx
      >>= loadAndApplyTemplate "templates/html.html" pageCtx
      >>= relativizeUrls
      >>= fixHtml

  -- Render index page
  match "index.html" $ do
    route idRoute
    compile $ getResourceBody
      >>= loadAndApplyTemplate "templates/html.html" indexCtx
      >>= relativizeUrls
      >>= fixHtml

-------------------------------------------------------------------------------
-- Contexts and Configurations

indexCtx :: Context String
indexCtx = mconcat
  [ constField "title" "barrucadu"
  , constField "item_type" "Person"
  , defaultContext
  ]

pageCtx :: Context String
pageCtx = mconcat
  [ dateField "isodate" "%Y-%m-%d"
  , dateField "ppdate"  "%d %b, %Y"
  , constField "item_type" "Article"
  , defaultContext
  ]

-------------------------------------------------------------------------------
-- Compilers

-- | Concatenate and minify a collection of items.
minifyCompiler :: (String -> String) -> [Item String] -> Compiler (Item String)
minifyCompiler minify = makeItem . minify . concatMap itemBody

-- | 'relativizeUrls' (and other bits of hakyll perhaps?) uses
-- TagSoup, which is rather opinionated about how HTML should be
-- written, and is actually wrong.
fixHtml :: Item String -> Compiler (Item String)
fixHtml = pure . fmap fixup where
  fixup = replace "itemscope=\"\"" "itemscope"

  replace from to s@(x:xs) = case stripPrefix from s of
    Just rest -> to ++ replace from to rest
    Nothing -> x : replace from to xs
  replace _ _ [] = []

-------------------------------------------------------------------------------
-- Utilities

-- | Remove some portion of the route
dropPat :: String -> Routes
dropPat pat = gsubRoute pat (const "")
