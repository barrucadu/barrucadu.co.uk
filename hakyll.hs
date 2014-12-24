{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid ((<>))
import Hakyll

main :: IO ()
main = hakyllWith defaultConfiguration $ do
  match "templates/*" $ compile templateCompiler

  match "static/**" $ do
    route $ dropPat "static/"
    compile copyFileCompiler

  match "errors/*" $ do
    route $ setExtension ".html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/error.html" defaultContext

  match "posts/*" $ do
    route   $ setExtension ".html"
    compile $ pandocCompiler
      >>= saveSnapshot "content"
      >>= return . fmap demoteHeaders
      >>= loadAndApplyTemplate "templates/post.html" defaultContext
      >>= relativizeUrls

  match "index.markdown" $ do
    let entries      = recentFirst =<< loadAll "posts/*"
    let entryDateCtx = dateField "date" "%B %e, %Y"             <> defaultContext
    let entryCtx     = listField "entries" entryDateCtx entries <> defaultContext

    route   $ setExtension ".html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/index.html" entryCtx
      >>= relativizeUrls

-- | Remove some portion of the route
dropPat :: String -> Routes
dropPat pat = gsubRoute pat (const "")
