{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid ((<>), mconcat)
import Hakyll

main :: IO ()
main = hakyllWith defaultConfiguration $ do
  -- Templates
  match "templates/*" $ compile templateCompiler

  -- Copy static files
  match "static/**" $ do
    route $ dropPat "static/"
    compile copyFileCompiler

  -- Render error pages
  match "errors/*" $ do
    route   $ setExtension ".html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/error.html" defaultContext

  -- Render blog posts
  match "posts/*" $ do
    route   $ setExtension ".html"
    compile $ pandocCompiler
      >>= saveSnapshot "content"
      >>= return . fmap demoteHeaders
      >>= loadAndApplyTemplate "templates/post.html" defaultContext
      >>= relativizeUrls

  -- Render index page / blog post list
  match "index.markdown" $ do
    let entries = recentFirst =<< loadAll "posts/*"
    let ctx     = listField "entries" postCtx entries <> defaultContext

    route   $ setExtension ".html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/index.html" ctx
      >>= relativizeUrls

  -- Create blog feed
  create ["atom.xml"] $ do
    route     idRoute
    compile $ loadAllSnapshots "posts/*" "content"
      >>= fmap (take 10) . recentFirst
      >>= renderAtom feedCfg feedCtx

postCtx :: Context String
postCtx = mconcat
  [ dateField "date" "%B %e, %Y"
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

-- | Remove some portion of the route
dropPat :: String -> Routes
dropPat pat = gsubRoute pat (const "")
