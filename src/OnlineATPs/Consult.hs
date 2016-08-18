{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module OnlineATPs.Consult
  ( getOnlineATPs
  , getResponseSystemOnTPTP
  , getSystemATP
  , getSystemATPWith
  ) where

import           Control.Arrow            ((***))
import           Control.Monad.IO.Class   (liftIO)
import           Data.ByteString.Internal (packChars)
import           Data.Char                (toLower)
import qualified Data.HashMap.Strict      as HashMap
import           Data.List                (isPrefixOf)
import           Data.List.Split          (splitOn)
import           Network                  (withSocketsDo)
import           Network.HTTP             (getRequest, getResponseBody,
                                           simpleHTTP)
import           Network.HTTP.Client      (defaultManagerSettings, httpLbs,
                                           newManager, parseRequest,
                                           responseBody, urlEncodedBody)
import           OnlineATPs.SystemATP     (SystemATP (..), isFOFATP)
import           OnlineATPs.SystemOnTPTP  (SystemOnTPTP, getDataSystemOnTPTP)
import           OnlineATPs.Urls          (urlSystemOnTPTP,
                                           urlSystemOnTPTPReply)
import           Text.HTML.TagSoup


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


groupEachSix ∷ [Tag String] → [[Tag String]]
groupEachSix [] = []
groupEachSix xs = take 6 xs : groupEachSix (drop 6 xs)

checkNameVersion ∷ [SystemATP] → [SystemATP]
checkNameVersion [] = []
checkNameVersion atps = putVer atps 2
  where
    putVer ∷ [SystemATP] → Int → [SystemATP]
    putVer [] _ = []
    putVer [x] _ = [x]
    putVer (x:y:ys) v
      | sysName x == sysName y = x: y { sysKey = sysKey y ++ show v} : putVer ys (v+1)
      | otherwise              = x: y : putVer ys 2

getVal ∷ Tag String → String
getVal = fromAttrib "value"

buildSystemATP ∷ [Tag String] → SystemATP
buildSystemATP [tSys, tTime, tTrans, tFormat, tCmd, tApp] = newATP
  where
    name, version ∷ String
    [name, version] = splitOn "---" $ getVal tSys

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
buildSystemATP _  = NoSystemATP

getOnlineATPs ∷ IO [SystemATP]
getOnlineATPs = do
  tags ← canonicalizeTags . parseTags <$> openURL urlSystemOnTPTP

  let systems ∷ [SystemATP]
      systems = map buildSystemATP $ groupEachSix $ getInfoATP tags

  let fofSystems ∷ [SystemATP]
      fofSystems = checkNameVersion $ filter isFOFATP systems

  return fofSystems


getSystemATPWith ∷ [SystemATP] → String → SystemATP
getSystemATPWith _ "" = NoSystemATP
getSystemATPWith atps name =
  if not $ "online-" `isPrefixOf` name then
    getSystemATPWith atps $ "online-" ++ name
    else
      case lookup name (zip (map sysKey atps) atps) of
        Just atp → atp
        _        → NoSystemATP


getSystemATP ∷ String → IO SystemATP
getSystemATP "" = return NoSystemATP
getSystemATP name =
  if not $ "online-" `isPrefixOf` name then
    getSystemATP $ "online-" ++ name
    else do

      atps ∷ [SystemATP] ← getOnlineATPs

      let namesATPs ∷ [String]
          namesATPs = map sysKey atps

      let mapATP = HashMap.fromList $ zip namesATPs atps
      -- Future:
      -- The idea is when the name is not valid, we'll try to find
      -- the most similar ATP. We can do this using Levenstein
      -- The HashMap is not  necesary yet. Anyway, I'll use it.

      return $ HashMap.lookupDefault NoSystemATP name mapATP

getResponseSystemOnTPTP ∷ SystemOnTPTP → IO String
getResponseSystemOnTPTP spec = withSocketsDo $ do
  initReq ← parseRequest urlSystemOnTPTPReply

  let dataForm ∷ [(String, String)]
      dataForm = getDataSystemOnTPTP spec

  -- putStrLn $ show dataForm

  let form = map (packChars *** packChars) dataForm
  let request = urlEncodedBody form initReq
  manager ← newManager defaultManagerSettings
  res ← httpLbs request manager
  liftIO $ do
    let response = responseBody res
    return $ show  response
