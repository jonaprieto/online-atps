
-- | Check the output looking for a theorem

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OnlineATPs.CheckOutput
  ( checkOnlineATPOutput
  , checkTheoremSync
  , onlineATPOk
  )
  where


import           Data.List               (isInfixOf)
import           OnlineATPs.Consult      (getResponseSystemOnTPTP)

import           OnlineATPs.SystemATP    (SystemATP (..), getNameVersion,
                                          msgErrorNoSystemATP)

import qualified Data.ByteString.Lazy    as L
import           OnlineATPs.SystemOnTPTP (SystemOnTPTP (..))


onlineATPOk ∷ SystemATP → String
onlineATPOk NoSystemATP = error msgErrorNoSystemATP
onlineATPOk atp = getNameVersion atp ++ " says Theorem"

checkOnlineATPOutput ∷ SystemATP → String → Bool
checkOnlineATPOutput NoSystemATP _ = False
checkOnlineATPOutput atp output    = onlineATPOk atp `isInfixOf` output


checkTheoremSync ∷ SystemOnTPTP → IO String
checkTheoremSync spec  = do

    let atps ∷ [SystemATP]
        atps = optSystems spec

    response ∷ L.ByteString ← getResponseSystemOnTPTP $ spec { optQuietFlag = "-q3" }

    if any (`checkOnlineATPOutput` show response) atps
      then return "Theorem"
      else return "No theorem"
