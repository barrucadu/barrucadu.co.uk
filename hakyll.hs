{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_, void)
import Data.Monoid ((<>))
import Hakyll
import System.Directory (getCurrentDirectory)
import System.FilePath (FilePath, combine)
import System.Process (CmdSpec(ShellCommand), CreateProcess(..), StdStream(Inherit), createProcess, waitForProcess)

main :: IO ()
main = hakyllWith defaultConfiguration $ do
  -- Templates
  match "templates/*" $ compile templateCompiler

  -- Build CV
  preprocess $ do
    cwd <- getCurrentDirectory
    runCmd (cwd `combine` "cv") $ ShellCommand "make"
  match "cv/cv.pdf" $ do
    route $ dropPat "cv/"
    compile copyFileCompiler

  -- Copy static files
  forM_ ["static/**", "fontawesome/css/**", "fontawesome/fonts/**"] $ \p ->
    match p $ do
      route $ dropPat "static/"
      compile copyFileCompiler

  -- Render blog posts
  match "posts/*" $ do
    route   $ setExtension ".html"
    compile $ pandocCompiler
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= loadAndApplyTemplate "templates/wrapper.html" postCtx
      >>= relativizeUrls

  -- Render publications
  match "publications.markdown" $ do
    route $ setExtension ".html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/wrapper.html" defaultContext

  -- Render all posts
  create ["posts.html"] $
    postList Nothing "posts.html" "All Posts" (makeItem "")
            
  -- Render index page / blog post list
  match "index.markdown" $
    postList (Just 5) "index.html" "barrucadu" pandocCompiler

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

-- | Run a command in a directory.
runCmd :: FilePath -> CmdSpec -> IO ()
runCmd wd cmd = do
  (_, _, _, handle) <- createProcess process
  void $ waitForProcess handle

  where
    process = CreateProcess
      { cmdspec       = cmd
      , cwd           = Just wd
      , env           = Nothing
      , std_in        = Inherit
      , std_out       = Inherit
      , std_err       = Inherit
      , close_fds     = False
      , create_group  = False
      , delegate_ctlc = False
      }

-- | Remove some portion of the route
dropPat :: String -> Routes
dropPat pat = gsubRoute pat (const "")
