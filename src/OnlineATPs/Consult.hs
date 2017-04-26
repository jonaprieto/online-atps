
-- | Consult the TPTP World web services.

{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}
{-# LANGUAGE CPP                                    #-}
{-# LANGUAGE MultiWayIf                             #-}
{-# LANGUAGE ScopedTypeVariables                    #-}
{-# LANGUAGE UnicodeSyntax                          #-}

module OnlineATPs.Consult
  ( getOnlineATPs
  , getResponseSystemOnTPTP
  , getSystemATP
  , getSystemATPWith
  , getSystemOnTPTP
  , Msg
  ) where

#if __GLASGOW_HASKELL__ <= 708
import Control.Applicative      ( (<$>) )
#endif

import Control.Arrow            ( (***) )

#if __GLASGOW_HASKELL__ <= 710
import Control.Monad.Reader     ( MonadIO ( liftIO ) )
#else
import Control.Monad.IO.Class   ( MonadIO ( liftIO ) )
#endif



import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict  as HashMap

import Data.ByteString.Internal ( packChars )
import Data.Char                ( toLower )
import Data.List                ( isPrefixOf )
import Data.List.Split          ( splitOn )
import Data.Maybe               ( fromJust, isNothing )

import Network             ( withSocketsDo )
import Network.HTTP        ( getRequest, getResponseBody, simpleHTTP )
import Network.HTTP.Client
  ( defaultManagerSettings
  , httpLbs
  , newManager
  , parseRequest
  , responseBody
  , urlEncodedBody
  )

import OnlineATPs.Defaults      ( getDefaults )
import OnlineATPs.Options       ( getManageOpt, Options (..) )
import OnlineATPs.SystemATP     ( SystemATP (..), isFOFATP, setTimeLimit )
import OnlineATPs.SystemOnTPTP
  ( SystemOnTPTP
    ( optAutoMode
    , optAutoModeSystemsLimit
    , optAutoModeTimeLimit
    , optCompleteness
    , optCorrectness
    , optCPUPassword
    , optIDV
    , optQuietFlag
    , optReportFlag
    , optSoundness
    , optSubmitButton
    , optSystemInfo
    , optSystemOnTSTP
    , optSystems
    , optTSTPData
    , optX2TPTP
    )
  , getDataSystemOnTPTP
  , setFORMULAEProblem
  )
import OnlineATPs.Urls          ( urlSystemOnTPTP, urlSystemOnTPTPReply )

import Text.HTML.TagSoup

-- | Informative Message.
type Msg = String

getNameTag ∷ Tag String → String
getNameTag = fromAttrib "name"

prefixSystem ∷ Tag String → Bool
prefixSystem tag = isPrefixOf "System___" $ getNameTag tag

prefixTimeLimit ∷ Tag String → Bool
prefixTimeLimit tag = isPrefixOf "TimeLimit___" $ getNameTag tag

prefixTransform ∷ Tag String → Bool
prefixTransform tag = isPrefixOf "Transform___" $ getNameTag tag

prefixFormat ∷ Tag String → Bool
prefixFormat tag = isPrefixOf "Format___" $ getNameTag tag

prefixCommand ∷ Tag String → Bool
prefixCommand tag = isPrefixOf "Command___" $ getNameTag tag

matchSystem ∷ Tag String → Bool
matchSystem t = (t ~== "<input type=\"CHECKBOX\">") && prefixSystem t

matchTimeLimit ∷ Tag String → Bool
matchTimeLimit t = (t ~=="<input type=\"text\">") &&  prefixTimeLimit t

matchTransform ∷ Tag String → Bool
matchTransform t = (t ~=="<input type=\"text\">") &&  prefixTransform t

matchFormat ∷ Tag String → Bool
matchFormat t = (t ~=="<input type=\"text\">") &&  prefixFormat t

matchCommand ∷ Tag String → Bool
matchCommand t = (t ~=="<input type=\"text\">") &&  prefixCommand t

matchApplication ∷ Tag String → Bool
matchApplication = (~=="<font size=\"-1\">")

matchCellATP ∷ [Tag String → Bool]
matchCellATP = [
    matchSystem
  , matchTimeLimit
  , matchTransform
  , matchFormat
  , matchCommand
  ]

isInfoATP ∷ Tag String → Bool
isInfoATP t = isTagOpen t && any ($ t) matchCellATP

getInfoATP ∷ [Tag String] → [Tag String]
getInfoATP tags = getTags tags False
  where
    getTags ∷ [Tag String] → Bool → [Tag String]
    getTags [] _ = []
    getTags (t:ts) False
      | isTagOpen t && matchApplication t = getTags ts True
      | isInfoATP t = t : getTags ts False
      | otherwise = getTags ts False
    getTags (t:ts) True = t : getTags ts False


openURL ∷ String → IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

chucksOfSix ∷ [Tag String] → [[Tag String]]
chucksOfSix [] = []
chucksOfSix xs = take 6 xs : chucksOfSix (drop 6 xs)

renameATPs ∷ [SystemATP] → [SystemATP]
renameATPs [] = []
renameATPs atps = putVer atps 2
  where
    putVer ∷ [SystemATP] → Int → [SystemATP]
    putVer [] _ = []
    putVer [x] _ = [x]
    putVer (x:y:ys) v
      | sysName x == sysName y = x: y { sysKey = sysKey y ++ show v} : putVer ys (v+1)
      | otherwise              = x: y : putVer ys 2

getVal ∷ Tag String → String
getVal = fromAttrib "value"

tagsToSystemATP ∷ [Tag String] → SystemATP
tagsToSystemATP [tSys, tTime, tTrans, tFormat, tCmd, tApp] = newATP
  where
    info ∷ [String]
    info  = splitOn "---" $ getVal tSys

    name, version ∷ String
    name = head info
    version = last info

    newATP ∷ SystemATP
    newATP = SystemATP
      { sysName = name
      , sysKey = "online-" ++ map toLower name
      , sysVersion = version
      , sysTimeLimit = getVal tTime
      , sysFormat = getVal tFormat
      , sysTransform = getVal tTrans
      , sysCommand = getVal tCmd
      , sysApplication = fromTagText tApp
      }
tagsToSystemATP _  = NoSystemATP

-- | The function 'getOnlineATPs' returns a list of ATPs given some options
-- using 'Options' type, and return the list using the 'SystemATP' type.
getOnlineATPs ∷ Options → IO [SystemATP]
getOnlineATPs opts = do
  tags ← canonicalizeTags . parseTags <$> openURL urlSystemOnTPTP

  let systems ∷ [SystemATP]
      systems = renameATPs $ map tagsToSystemATP $ chucksOfSix $ getInfoATP tags

  if optFOF opts
    then return $ filter isFOFATP systems
    else return systems

-- | The function 'getSystemATPWith' choose from a list the ATP most accurate
-- according to the name given in the second input. If there is not such a ATP,
-- this method returns 'NoSystemATP'.
getSystemATPWith ∷ [SystemATP] → String → SystemATP
getSystemATPWith _ "" = NoSystemATP
getSystemATPWith atps name =
  if not $ "online-" `isPrefixOf` name then
    getSystemATPWith atps $ "online-" ++ name
    else
      case lookup name (zip (map sysKey atps) atps) of
        Just atp → atp
        _        → NoSystemATP

-- | The function 'getSystemATP' tries to find an ATP given the specification
-- from its input.
getSystemATP ∷ Options → IO SystemATP
getSystemATP opts =
  let name = optVersionATP opts in
    if | null name → return NoSystemATP

       | not $ "online-" `isPrefixOf` name →

          getSystemATP $ opts { optVersionATP = "online-" ++ name }

       | otherwise → do

          atps ∷ [SystemATP] ← getOnlineATPs opts

          let namesATPs ∷ [String]
              namesATPs = map sysKey atps

          let mapATP = HashMap.fromList $ zip namesATPs atps
          -- Future:
          -- The idea is when the name is not valid, we'll try to find
          -- the most similar ATP. We can do this using Levenstein
          -- The HashMap is not  necessary yet. Anyway, I'll use it.

          return $ HashMap.lookupDefault NoSystemATP name mapATP

-- | The function 'getResponseSystemOnTPTP' performs a request to the TPTP
-- World based on the form information of the input 'SystemOnTPTP'.
getResponseSystemOnTPTP ∷ SystemOnTPTP → IO L.ByteString
getResponseSystemOnTPTP spec = withSocketsDo $ do
  initReq ← parseRequest urlSystemOnTPTPReply

  let dataForm ∷ [(String, String)]
      dataForm = getDataSystemOnTPTP spec

  let form = map (packChars *** packChars) dataForm
  let request = urlEncodedBody form initReq
  manager ← newManager defaultManagerSettings
  res ← httpLbs request manager
  liftIO $ do
    let response = responseBody res
    return response

-- | The function 'getSystemOnTPTP' reads the options given by the user
-- including the problem file and return a SystemOnTPTP data type
-- with all settings.
getSystemOnTPTP ∷ Options → IO (Either Msg SystemOnTPTP)
getSystemOnTPTP opts = do

  atps ∷ [SystemATP]  ← getOnlineATPs opts

  let listATPs ∷ [SystemATP]
      listATPs =
        if optWithAll opts
          then atps
          else map (getSystemATPWith atps) (getManageOpt (optATP opts))

  let time ∷ String
      time = show $ optTime opts

  let setATPs ∷ [SystemATP]
      setATPs = filter (NoSystemATP/=) (map (`setTimeLimit` time) listATPs)

  if null setATPs &&
    optSubmitButton (optSystemOnTPTP opts) /= "RecommendSystems"
    then return $ Left "Check the ATPs names using --list-atps"
    else do
      defaults ∷ SystemOnTPTP ← getDefaults

      -- -----------------------------------------------------------------------
      -- Update defaults info for SystemOnTPTP based on arguments
      -- given on the command-line (They have priority)

      let finals ∷ SystemOnTPTP
          finals = let system = optSystemOnTPTP opts in
            defaults {
              optAutoMode = case optAutoMode system of
                []   → optAutoMode defaults
                mode → mode
            , optAutoModeSystemsLimit = case optAutoModeSystemsLimit system of
                []   → optAutoModeSystemsLimit defaults
                mode → mode
            , optAutoModeTimeLimit    = case optAutoModeTimeLimit system of
                []   → optAutoModeTimeLimit defaults
                mode → mode
            ,  optCPUPassword  = case optCPUPassword system of
                []   → optCPUPassword defaults
                pass → pass
            , optQuietFlag     = case optQuietFlag system of
                []   → optQuietFlag defaults
                flag → flag
            , optCompleteness = optCompleteness system || optCompleteness defaults
            , optCorrectness  = optCorrectness system  || optCorrectness defaults
            , optIDV          = optIDV system          || optIDV defaults
            , optReportFlag   = case optReportFlag system of
                []   → optReportFlag defaults
                mode → mode
            , optSoundness    = optSoundness system    || optSoundness defaults
            , optSubmitButton = case optSubmitButton system of
                []     → optSubmitButton defaults
                action → action
            , optSystemInfo   = optSystemInfo system   || optSystemInfo defaults
            , optSystemOnTSTP = optSystemOnTSTP system || optSystemOnTSTP defaults
            , optSystems      = setATPs
            , optTSTPData     = optTSTPData system     || optTSTPData defaults
            , optX2TPTP       = optX2TPTP system       || optX2TPTP defaults
          }

      -- ---------------------------------------------------------------------------

      let file ∷ Maybe FilePath
          file = optInputFile opts

      if isNothing file
        then return $ Left "Missing input file"
        else do

          contentFile ∷ String ← readFile $ fromJust file

          let form ∷ SystemOnTPTP
              form  = setFORMULAEProblem finals contentFile

          return $ Right form
