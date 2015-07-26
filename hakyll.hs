{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import Data.Monoid ((<>), mconcat)
import Hakyll
import System.Directory (getCurrentDirectory)
import System.FilePath (FilePath, combine)
import System.Process (CmdSpec(ShellCommand), CreateProcess(..), StdStream(Inherit), createProcess, waitForProcess)
import Text.Pandoc.Options (WriterOptions(..))

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
  match "static/**" $ do
    route $ dropPat "static/"
    compile copyFileCompiler

  -- Copy font-awesome files
  match "fontawesome/css/**" $ do
    route   idRoute
    compile copyFileCompiler
  match "fontawesome/fonts/**" $ do
    route   idRoute
    compile copyFileCompiler

  -- Render 404 page
  match "404.markdown" $ do
    route   $ setExtension ".html"
    compile $ myPandocCompiler
      >>= loadAndApplyTemplate "templates/error.html"   defaultContext
      >>= loadAndApplyTemplate "templates/wrapper.html" defaultContext

  -- Render blog posts
  match "posts/*" $ do
    route   $ setExtension ".html"
    compile $ myPandocCompiler
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/post.html"    defaultContext
      >>= loadAndApplyTemplate "templates/wrapper.html" defaultContext
      >>= relativizeUrls

  -- Render publications
  match "publications.markdown" $ do
    route $ setExtension ".html"
    compile $ myPandocCompiler
      >>= loadAndApplyTemplate "templates/wrapper.html" defaultContext
  match "publications/*" $ do
    route   idRoute
    compile copyFileCompiler

  -- Render index page / blog post list
  match "aboutme.markdown" $ do
    let entries = recentFirst =<< loadAll "posts/*"
    let ctx     = constField "title" "barrucadu" <> listField "entries" postCtx entries <> defaultContext

    route   $ constRoute "index.html"
    compile $ myPandocCompiler
      >>= loadAndApplyTemplate "templates/index.html"   ctx
      >>= loadAndApplyTemplate "templates/wrapper.html" ctx
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

-- | Pandoc compiler without syntax highlighting.
myPandocCompiler :: Compiler (Item String)
myPandocCompiler = pandocCompilerWith ropts wopts
  where
    ropts = defaultHakyllReaderOptions
    wopts = defaultHakyllWriterOptions { writerHighlight = False }