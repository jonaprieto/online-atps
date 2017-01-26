
-- | Monads utilities.
-- Adapted from @Apia.Utils.Monad@

{-# LANGUAGE UnicodeSyntax #-}

module OnlineATPs.Utils.Monad
  ( die
  , failureMsg
  ) where


import Control.Monad ( when )

import OnlineATPs.Options ( Options (..) )
import OnlineATPs.Utils.PrettyPrint ( Doc, prettyShow )

import System.Environment ( getProgName )
import System.Exit        ( exitFailure )
import System.IO          ( hPutStrLn, stderr )

import qualified Text.Show.Pretty as Pr

-- | Failure message.
failureMsg ∷ Doc → IO ()
failureMsg err =
  getProgName >>= \prg → hPutStrLn stderr $ prg ++ ": " ++ prettyShow err

-- | Exit with an error message.
die ∷ Doc → Options → IO a
die err opts = do
  when (optDebug opts) $ putStrLn $ Pr.ppShow opts
  failureMsg err >> exitFailure
