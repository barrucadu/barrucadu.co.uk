{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid ((<>))
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

  -- Render index page
  match "index.html" $ do
    route idRoute
    compile $ do
      let ctx = constField "title" "barrucadu" <> defaultContext
      getResourceBody
        >>= applyAsTemplate ctx
        >>= loadAndApplyTemplate "templates/html.html" ctx
        >>= relativizeUrls

-------------------------------------------------------------------------------
-- Contexts and Configurations

pageCtx :: Context String
pageCtx = mconcat
  [ dateField "isodate" "%Y-%m-%d"
  , dateField "ppdate"  "%d %b, %Y"
  , defaultContext
  ]

-------------------------------------------------------------------------------
-- Compilers

-- | Concatenate and minify a collection of items.
minifyCompiler :: (String -> String) -> [Item String] -> Compiler (Item String)
minifyCompiler minify = makeItem . minify . concatMap itemBody

-------------------------------------------------------------------------------
-- Utilities

-- | Remove some portion of the route
dropPat :: String -> Routes
dropPat pat = gsubRoute pat (const "")
