
-- -- | Utilities related to representation of versions.
{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module OnlineATPs.Utils.Version ( progNameVersion ) where


import           Data.Char          (toUpper)
import           Data.Version       (showVersion)
import           Paths_onlineatps   (version)
import           System.Environment (getProgName)



toUpperFirst ∷ String → String
toUpperFirst []       = []
toUpperFirst (x : xs) =  toUpper x : xs

-- -- ghcid
-- progNameVersion ∷ IO String
-- progNameVersion = return "1.0.0"

-- | Return program name and version information.
progNameVersion ∷ IO String
progNameVersion = do
  progName ← getProgName
  return $ toUpperFirst progName ++ " version " ++ showVersion version
