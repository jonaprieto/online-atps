
-- | Set the defaults fot the package

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module OnlineATPs.Defaults
  ( defaultSystemATP
  , defaultSystemOnTPTP
  , getDefaults
  )
  where

import           Control.Applicative     ((<|>))
import           Control.Monad           (filterM)
import           Data.Aeson              (withObject)
import           Data.Aeson.Types
import qualified Data.HashMap.Strict     as HashMap
import           Data.Maybe              (catMaybes, fromJust, fromMaybe, maybe)
import qualified Data.Text               as T
import           Data.Yaml               (FromJSON (parseJSON), Object,
                                          decodeFile, decodeFileEither,
                                          parseMaybe, (.!=), (.:), (.:?))
import qualified Data.Yaml.Include       as YamlInclude
import           OnlineATPs.SystemATP    (SystemATP (..))
import           OnlineATPs.SystemOnTPTP (SystemOnTPTP (..))
import           Paths_onlineatps        (getDataFileName)
import           Prelude                 hiding (lookup)
import           System.Directory        (doesFileExist, getCurrentDirectory,
                                          getHomeDirectory)
import           System.FilePath.Posix   ((</>))


underscore ∷ T.Text → T.Text
underscore field = T.pack $ camelTo2 '_' $ T.unpack field

hypen ∷ T.Text → T.Text
hypen field = T.pack $ camelTo2 '-' $T.unpack field

lower ∷ T.Text → T.Text
lower = T.toLower

upper ∷ T.Text → T.Text
upper = T.toUpper

(.?.) :: FromJSON a => Object  → T.Text → Parser (Maybe a)
x .?. field = x .:? field
  <|> x .:? underscore field
  <|> x .:? hypen field
  <|> x .:? lower field
  <|> x .:? upper field

(.:.) :: FromJSON a => Object  → T.Text → Parser a
x .:. field = x .: field
  <|> x .: underscore field
  <|> x .: hypen field
  <|> x .: lower field
  <|> x .: upper field


(.@.) ∷ FromJSON a ⇒ [Object] → T.Text → Parser a
[]  .@. field = fail  "failed. Expected at least one key-value"
[x] .@. field = x .:. field
(x:xs) .@. field = do
  value ← x .?. field
  maybe (xs .@. field) return value

defaultSystemATP ∷ SystemATP
defaultSystemATP = SystemATP
  { sysApplication = "Prover and model finder, for FOF CNF"
  , sysCommand     = "eprover -s --cpu-limit=%d %s"
  , sysFormat      = "tptp:raw"
  , sysKey         = "online-e"
  , sysName        = "E---2.0"
  , sysTimeLimit   = "60"
  , sysTransform   = "none"
  , sysVersion     = "2.0"
}

instance FromJSON SystemATP where
  parseJSON = withObject "atp" $ \o → do
    sysApplication ← o .?. "Application" .!= ""
    sysCommand     ← o .?. "Command" .!= "default"
    sysFormat      ← o .?. "Format" .!= "tptp:raw"
    sysKey         ← o .:. "Key" <|> o .:. "ATP"
    sysName        ← o .?. "Name" .!= "default"
    sysTimeLimit   ← o .?. "TimeLimit" .!= "60"
    sysTransform   ← o .?. "Transform" .!= "none"
    sysVersion     ← o .?. "Version" .!= "default"
    return SystemATP{..}


defaultSystemOnTPTP ∷ SystemOnTPTP
defaultSystemOnTPTP = SystemOnTPTP
  {
    optAutoMode              = "-cE"
  , optAutoModeSystemsLimit  = "3"
  , optAutoModeTimeLimit     = "300"
  , optCompleteness          = False
  , optCorrectness           = False
  , optCPUPassword           = ""
  , optFORMULAEProblem       = ""
  , optFormulaURL            = ""
  , optIDV                   = False
  , optNoHTML                = "1"
  , optProblemSource         = "FORMULAE" --"TPTP"
  , optQuietFlag             = "-q01" --q2
  , optReportFlag            = "-q0"
  , optSoundness             = False
  , optSubmitButton          = "RunSelectedSystems"
  , optSystemInfo            = False
  , optSystemOnTSTP          = False
  , optSystems               = []
  , optTPTPProblem           = "" --AGT001+1"
  , optTSTPData              = False
  , optUPLOADProblem         = ""
  , optX2TPTP                = False
}

loadYAML ∷ FilePath → IO (Maybe Object)
loadYAML path = do
  decoded ← YamlInclude.decodeFileEither path
  case decoded of
    Right o   → return $ Just o
    Left msg  → do
      print msg
      return Nothing

onlineatpsNameFile ∷ FilePath
onlineatpsNameFile = ".onlineatps"

onlineatpsTemplate ∷ FilePath
onlineatpsTemplate = "onlineatps.yml"

-- -- Uncomment this to use ghcid
-- getDataFileName ∷ FilePath → IO FilePath
-- getDataFileName path = return $ "./data" </> path

combineConfigs ∷ [Object] → Parser SystemOnTPTP
combineConfigs configs  =  do
  optAutoMode               ← configs .@. "AutoMode"
  optAutoModeSystemsLimit   ← configs .@. "AutoModeSystemsLimit"
  optAutoModeTimeLimit      ← configs .@. "AutoModeTimeLimit"
  optCompleteness           ← configs .@. "Completeness"
  optCorrectness            ← configs .@. "Correctness"
  optCPUPassword            ← configs .@. "CPUPassword"
  optFORMULAEProblem        ← configs .@. "FORMULAEProblem"
  optFormulaURL             ← configs .@. "FormulaURL"
  optIDV                    ← configs .@. "IDV"
  optNoHTML                 ← configs .@. "NoHTML"
  optProblemSource          ← configs .@. "ProblemSource"
  optQuietFlag              ← configs .@. "QuietFlag"
  optReportFlag             ← configs .@. "ReportFlag"
  optSoundness              ← configs .@. "Soundness"
  optSubmitButton           ← configs .@. "SubmitButton"
  optSystemInfo             ← configs .@. "SystemInfo"
  optSystemOnTSTP           ← configs .@. "SystemOnTSTP"
  optSystems                ← configs .@. "Systems"
  optTPTPProblem            ← configs .@. "TPTPProblem"
  optTSTPData               ← configs .@. "TSTPData"
  optUPLOADProblem          ← configs .@. "UPLOADProblem"
  optX2TPTP                 ← configs .@. "X2TPTP"
  return SystemOnTPTP{..}

-- | Get the default values for the command-line 'SystemOnTPTP'.
getDefaults ∷ IO SystemOnTPTP
getDefaults = do
  paths ← sequence [getCurrentDirectory, getHomeDirectory]
  userFiles ← filterM doesFileExist $ map (</>onlineatpsNameFile) paths
  defaultConfig ← getDataFileName onlineatpsTemplate

  let allFiles ∷ [FilePath]
      allFiles = userFiles ++ [defaultConfig]

  loaded ∷ [Maybe Object] ← mapM loadYAML allFiles

  let combined ∷ Either String SystemOnTPTP
      combined = parseEither combineConfigs $ catMaybes loaded

  case combined of
    Left  msg → do
      putStrLn $ "Error: " ++ msg
      return defaultSystemOnTPTP
    Right o   → return o
