
-- | Check the output looking for a theorem

{-# LANGUAGE UnicodeSyntax #-}

module OnlineATPs.CheckOutput
  ( checkOnlineATPOutput
  , checkTheoremSync
  , onlineATPOk
  )
  where


import Data.List (isInfixOf)
import OnlineATPs.SystemATP
  ( SystemATP
    ( NoSystemATP
    ,  SystemATP
    )
  , getNameVersion
  , msgErrorNoSystemATP
  )

import qualified Data.ByteString.Lazy as L


onlineATPOk ∷ SystemATP → String
onlineATPOk NoSystemATP = error msgErrorNoSystemATP
onlineATPOk atp = getNameVersion atp ++ " says Theorem"

checkOnlineATPOutput ∷ SystemATP → String → Bool
checkOnlineATPOutput NoSystemATP _ = False
checkOnlineATPOutput atp output    = onlineATPOk atp `isInfixOf` output

checkTheoremSync ∷ L.ByteString → [SystemATP] → String
checkTheoremSync response listATPs =
    if any (`checkOnlineATPOutput` show response) listATPs
      then "Theorem"
      else "No theorem"
