{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_, void)
import Data.Char (toLower)
import Data.Monoid ((<>))
import Hakyll
import System.Directory (getCurrentDirectory)
import System.FilePath (FilePath, combine)
import System.Process (CmdSpec(ShellCommand), CreateProcess(..), StdStream(Inherit), createProcess, waitForProcess, readProcess)
import Text.Pandoc.Definition (Pandoc, Block(..), Format(..))
import Text.Pandoc.Walk (walkM)

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

  -- Minify CSS
  match "css/*" $ compile getResourceBody
  create ["style.css"] $ do
    route idRoute
    compile $ loadAll "css/*"
      >>= minifyCompiler compressCss

  -- Render blog posts
  match "posts/*" $ do
    route   $ setExtension ".html"
    compile $ pandocCompiler'
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= loadAndApplyTemplate "templates/wrapper.html" postCtx
      >>= relativizeUrls

  -- Render publications
  match "publications.html" $ do
    route     idRoute
    compile $ getResourceBody
      >>= loadAndApplyTemplate "templates/wrapper.html" defaultContext
      >>= relativizeUrls

  -- Render projects
  match "projects.markdown" $ do
    route $ constRoute "projects.html"
    compile $ pandocCompiler'
      >>= loadAndApplyTemplate "templates/wrapper.html" defaultContext
      >>= relativizeUrls

  -- Render all posts
  create ["posts.html"] $
    postList Nothing "posts.html" "All Posts" (makeItem "")

  -- Render index page / blog post list
  match "index.markdown" $
    postList (Just 5) "index.html" "barrucadu" pandocCompiler'

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

--------------------------------------------------------------------------------
-- Compilers

-- | The Pandoc compiler, but using pygmentize for syntax
-- highlighting.
pandocCompiler' :: Compiler (Item String)
pandocCompiler' = pandocCompilerWithTransformM defaultHakyllReaderOptions defaultHakyllWriterOptions highlight where
  highlight :: Pandoc -> Compiler Pandoc
  highlight = unsafeCompiler . walkM pygments

  pygments :: Block -> IO Block
  pygments (CodeBlock (_, lang:_, _) code) = RawBlock (Format "html") <$> readProcess "pygmentize" ["-l", map toLower lang,  "-f", "html"] code
  pygments (CodeBlock _ code) = pure . RawBlock (Format "html") $ "<div class =\"highlight\"><pre>" ++ code ++ "</pre></div>"
  pygments x = pure x

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
