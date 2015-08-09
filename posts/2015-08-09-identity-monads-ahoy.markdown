---
title: Identity Monads Ahoy!
description: Reduce code duplication with Identity monads.
---

This is just a small note of something which finally clicked for me
today, and now I expect I'll use it all over the place.

Often, I'll have two variants of a function, one of which does IO and
the other doesn't. For example,

* [Déjà Fu][] has separate functions to run tests which do IO and to
run tests which don't do IO.

* I was working on my entry for the
[ICFP Programming Contest 2015][ICFPC], an AI for a Tetris-like
game[^private], and I wanted to be able to play the game interactively
as well as to run the AI. Interactive play requires IO, the AI
doesn't.

You can avoid duplicating the implementation of the function (as I
used to do) by writing it to be generic on the underlying monad
(passing in as parameters any specific monadic actions to perform),
and using `Identity` for the pure case:

~~~~{.haskell}
-- | Play a game with AI.
playAI :: [Phrase] -> GameState -> [Command]
playAI ps = runIdentity . play displayBrd getCommands reportError where
  displayBrd  _     = return ()
  getCommands gs cs = return $ solveUnit ps cs gs
  reportError       = return ()

-- | Play a game interactively.
playInteractive :: GameState -> IO [Command]
playInteractive = play displayBrd getCommands reportError where
  displayBrd  b   = putStrLn $ showBoard b
  getCommands _ _ = filter (`elem` allowed) <$> getLine
  reportError     = putStrLn "<<invalid>>"

  allowed = concatMap concrete [minBound..maxBound]

-- | Play a game. Returns the list of commands issued.
play :: Monad m
  => (GameState -> m ())
  -- ^ Display the game board.
  -> (GameState -> [Command] -> m [Command])
  -- ^ Get a list of commands to perform, this is given the current
  -- game state and the prior command sequence.
  -> m ()
  -- ^ Signal that an invalid move has been attempted.
  -> GameState
  -- ^ Initial game state
  -> m [Command]
play displayBrd getCommands reportError = loop [] where
  loop commands gs = case spawnUnit gs of
    Just gs' -> do
      displayBrd gs'
      cmds <- getCommands gs' commands
      case moves gs' $ map abstract cmds of
        Just moved ->
          let (ls, cleared) = clear moved
          in  loop (commands++cmds) (cleared { clearedPrev = ls })

        Nothing -> reportError >> return commands
    Nothing -> displayBrd gs >> return commands
~~~~

And [here's an example][commit] in Déjà Fu. Before, I had two
functions with the same logic, `sctBounded` and `sctBoundedIO`, now I
just have one and two tiny wrapper functions.

[^private]: In a private repository for now, sorry.

[Déjà Fu]: https://github.com/barrucadu/dejafu
[ICFPC]:   http://icfpcontest.org/
[commit]:  https://github.com/barrucadu/dejafu/commit/d32ee150bde0bb27cdab41df175bc04c61996ea9
