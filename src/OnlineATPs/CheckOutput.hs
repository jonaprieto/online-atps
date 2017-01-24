
-- | Check the output looking for a theorem.

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


-- | This function tries to find a string depending on the ATP and the
-- second input, the output.
checkOnlineATPOutput ∷ SystemATP → String → Bool
checkOnlineATPOutput NoSystemATP _ = False
checkOnlineATPOutput atp output    = onlineATPOk atp `isInfixOf` output

-- | Given a specification of the form for SystemOnTPTP, the function
-- 'checkTheoremSync' performs the request and retrieves the answer.
checkTheoremSync ∷ SystemOnTPTP → IO String
checkTheoremSync spec  = do

    let atps ∷ [SystemATP]
        atps = optSystems spec

    response ∷ L.ByteString ← getResponseSystemOnTPTP $ spec { optQuietFlag = "-q01" }

    if any (`checkOnlineATPOutput` show response) atps
      then return "Theorem"
      else return "No theorem"

-- | The function 'onlineATPOk' prints the name of the ATP who proved
-- the theorem whenever ATP is different from 'NoSystemATP' value.
onlineATPOk ∷ SystemATP → String
onlineATPOk NoSystemATP = error msgErrorNoSystemATP
onlineATPOk atp = getNameVersion atp ++ " says Theorem"
