
-- | Check the output looking for a theorem

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module OnlineATPs.CheckOutput
  ( checkOnlineATPOutput
  , checkTheoremSync
  , onlineATPOk
  )
  where


import Data.List ( isInfixOf )

import OnlineATPs.Consult ( getResponseSystemOnTPTP )
import OnlineATPs.SystemATP
  ( SystemATP (..)
  , getNameVersion
  , msgErrorNoSystemATP
  )
import OnlineATPs.SystemOnTPTP ( SystemOnTPTP (..) )

import qualified Data.ByteString.Lazy as L


-- | TODO
checkOnlineATPOutput ∷ SystemATP → String → Bool
checkOnlineATPOutput NoSystemATP _ = False
checkOnlineATPOutput atp output    = onlineATPOk atp `isInfixOf` output

-- | TODO
checkTheoremSync ∷ SystemOnTPTP → IO String
checkTheoremSync spec  = do

    let atps ∷ [SystemATP]
        atps = optSystems spec

    response ∷ L.ByteString ← getResponseSystemOnTPTP $ spec { optQuietFlag = "-q01" }

    if any (`checkOnlineATPOutput` show response) atps
      then return "Theorem"
      else return "No theorem"

-- | TODO
onlineATPOk ∷ SystemATP → String
onlineATPOk NoSystemATP = error msgErrorNoSystemATP
onlineATPOk atp = getNameVersion atp ++ " says Theorem"
