
-- | Monads utilities.
-- Adapted from @Apia.Utils.Monad@

{-# LANGUAGE UnicodeSyntax #-}

module OnlineATPs.Utils.Monad
  ( die
  , failureMsg
  ) where


import OnlineATPs.Utils.PrettyPrint ( Doc, prettyShow )

import System.Environment ( getProgName )
import System.Exit        ( exitFailure )
import System.IO          ( hPutStrLn, stderr )


-- | Failure message.
failureMsg ∷ Doc → IO ()
failureMsg err =
  getProgName >>= \prg → hPutStrLn stderr $ prg ++ ": " ++ prettyShow err

-- | Exit with an error message.
die ∷ Doc → IO a
die err = failureMsg err >> exitFailure
