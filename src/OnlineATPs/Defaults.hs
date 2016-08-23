
-- | Set the defaults fot the package

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE UnicodeSyntax     #-}

module OnlineATPs.Defaults
  ( defaultSystemATP
  , defaultSystemOnTPTP
  , getDefaults
  )
  where

import           OnlineATPs.SystemATP    (SystemATP (..))
import           OnlineATPs.SystemOnTPTP (SystemOnTPTP (..))

import           Control.DeepSeq         (NFData (rnf))

import qualified Data.HashMap.Strict     as HashMap
import qualified Data.Text               as T

import           Data.Yaml               (FromJSON (parseJSON), Object,
                                          parseMaybe, (.!=), (.:), (.:?))
import qualified Data.Yaml.Include       as YamlInclude

import           Control.Monad           (filterM)
import           Data.Maybe              (fromMaybe)
import           System.FilePath.Posix   ((</>))

import           Paths_onlineatps        (getDataFileName)

import           Prelude                 hiding (lookup)

import           System.Directory        (doesFileExist, getCurrentDirectory,
                                          getHomeDirectory)

import           Control.Applicative     ((<$>), (<*>), (<|>))
import           Data.Aeson              (withObject)

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
    sysApplication ← o .:? "Application" .!= ""
    sysCommand     ← o .:? "Command" .!= "default"
    sysFormat      ← o .:? "Format" .!= "tptp:raw"
    sysKey         ← o .: "Key" <|> o .: "key" <|> o .: "ATP"
    sysName        ← o .:? "Name" .!= "default"
    sysTimeLimit   ← o .:? "TimeLimit" .!= "60"
    sysTransform   ← o .:? "Transform" .!= "none"
    sysVersion     ← o .:? "Version" .!= "default"
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

type Key = T.Text

data Config = Config Object
  deriving (Eq, Show)

getObject ∷ Config → Object
getObject (Config o) = o


instance NFData Config where
  rnf (Config o) = rnf o `seq` ()

lookConfig ∷ FromJSON b ⇒ Key → Config → Maybe b
lookConfig k (Config o) = HashMap.lookup k o >>= parseMaybe parseJSON

setAutoMode ∷ Config → SystemOnTPTP → SystemOnTPTP
setAutoMode config spec = newSpec
  where
    newSpec ∷ SystemOnTPTP
    newSpec = case lookConfig "AutoMode" config of
      Just val → spec { optAutoMode = val }
      _        → spec

setAutoModeSystemsLimit ∷ Config → SystemOnTPTP → SystemOnTPTP
setAutoModeSystemsLimit config spec = newSpec
  where
    newSpec ∷ SystemOnTPTP
    newSpec = case lookConfig "AutoModeSystemsLimit" config of
      Just val → spec { optAutoModeSystemsLimit = val }
      _        → spec

setAutoModeTimeLimit ∷ Config → SystemOnTPTP → SystemOnTPTP
setAutoModeTimeLimit config spec = newSpec
  where
    newSpec ∷ SystemOnTPTP
    newSpec = case lookConfig "AutoModeTimeLimit" config of
      Just val → spec { optAutoModeTimeLimit = val }
      _        → spec

setCompleteness ∷ Config → SystemOnTPTP → SystemOnTPTP
setCompleteness config spec = newSpec
  where
    newSpec ∷ SystemOnTPTP
    newSpec = case lookConfig "Completeness" config of
      Just val → spec { optCompleteness = val }
      _        → spec

setCorrectness ∷ Config → SystemOnTPTP → SystemOnTPTP
setCorrectness config spec = newSpec
  where
    newSpec ∷ SystemOnTPTP
    newSpec = case lookConfig "Correctness" config of
      Just val → spec { optCorrectness = val }
      _        → spec

setCPUPassword ∷ Config → SystemOnTPTP → SystemOnTPTP
setCPUPassword config spec = newSpec
  where
    newSpec ∷ SystemOnTPTP
    newSpec = case lookConfig "CPUPassword" config of
      Just val → spec { optCPUPassword = val }
      _        → spec

setFORMULAEProblem ∷ Config → SystemOnTPTP → SystemOnTPTP
setFORMULAEProblem config spec = newSpec
  where
    newSpec ∷ SystemOnTPTP
    newSpec = case lookConfig "FORMULAEProblem" config of
      Just val → spec { optFORMULAEProblem = val }
      _        → spec

setFormulaURL ∷ Config → SystemOnTPTP → SystemOnTPTP
setFormulaURL config spec = newSpec
  where
    newSpec ∷ SystemOnTPTP
    newSpec = case lookConfig "FormulaURL" config of
      Just val → spec { optFormulaURL = val }
      _        → spec

setIDV ∷ Config → SystemOnTPTP → SystemOnTPTP
setIDV config spec = newSpec
  where
    newSpec ∷ SystemOnTPTP
    newSpec = case lookConfig "IDV" config of
      Just val → spec { optIDV = val }
      _        → spec

setNoHTML ∷ Config → SystemOnTPTP → SystemOnTPTP
setNoHTML config spec = newSpec
  where
    newSpec ∷ SystemOnTPTP
    newSpec = case lookConfig "NoHTML" config of
      Just val → spec { optNoHTML = val }
      _        → spec

setProblemSource ∷ Config → SystemOnTPTP → SystemOnTPTP
setProblemSource config spec = newSpec
  where
    newSpec ∷ SystemOnTPTP
    newSpec = case lookConfig "ProblemSource" config of
      Just val → spec { optProblemSource = val }
      _        → spec

setQuietFlag ∷ Config → SystemOnTPTP → SystemOnTPTP
setQuietFlag config spec = newSpec
  where
    newSpec ∷ SystemOnTPTP
    newSpec = case lookConfig "QuietFlag" config of
      Just val → spec { optQuietFlag = val }
      _        → spec

setReportFlag ∷ Config → SystemOnTPTP → SystemOnTPTP
setReportFlag config spec = newSpec
  where
    newSpec ∷ SystemOnTPTP
    newSpec = case lookConfig "ReportFlag" config of
      Just val → spec { optReportFlag = val }
      _        → spec

setSoundness ∷ Config → SystemOnTPTP → SystemOnTPTP
setSoundness config spec = newSpec
  where
    newSpec ∷ SystemOnTPTP
    newSpec = case lookConfig "Soundness" config of
      Just val → spec { optSoundness = val }
      _        → spec

setSubmitButton ∷ Config → SystemOnTPTP → SystemOnTPTP
setSubmitButton config spec = newSpec
  where
    newSpec ∷ SystemOnTPTP
    newSpec = case lookConfig "SubmitButton" config of
      Just val → spec { optSubmitButton = val }
      _        → spec

setSystemInfo ∷ Config → SystemOnTPTP → SystemOnTPTP
setSystemInfo config spec = newSpec
  where
    newSpec ∷ SystemOnTPTP
    newSpec = case lookConfig "SystemInfo" config of
      Just val → spec { optSystemInfo = val }
      _        → spec

setSystemOnTSTP ∷ Config → SystemOnTPTP → SystemOnTPTP
setSystemOnTSTP config spec = newSpec
  where
    newSpec ∷ SystemOnTPTP
    newSpec = case lookConfig "SystemOnTSTP" config of
      Just val → spec { optSystemOnTSTP = val }
      _        → spec

setSystems ∷ Config → SystemOnTPTP → SystemOnTPTP
setSystems config spec = newSpec
  where
    newSpec = case lookConfig "Systems" config of
      Just val → spec { optSystems = val }
      _        → spec

setTPTPProblem ∷ Config → SystemOnTPTP → SystemOnTPTP
setTPTPProblem config spec = newSpec
  where
    newSpec ∷ SystemOnTPTP
    newSpec = case lookConfig "TPTPProblem" config of
      Just val → spec { optTPTPProblem = val }
      _        → spec

setTSTPData ∷ Config → SystemOnTPTP → SystemOnTPTP
setTSTPData config spec = newSpec
  where
    newSpec ∷ SystemOnTPTP
    newSpec = case lookConfig "TSTPData" config of
      Just val → spec { optTSTPData = val }
      _        → spec

setUPLOADProblem ∷ Config → SystemOnTPTP → SystemOnTPTP
setUPLOADProblem config spec = newSpec
  where
    newSpec ∷ SystemOnTPTP
    newSpec = case lookConfig "UPLOADProblem" config of
      Just val → spec { optUPLOADProblem = val }
      _        → spec

setX2TPTP ∷ Config → SystemOnTPTP → SystemOnTPTP
setX2TPTP config spec = newSpec
  where
    newSpec ∷ SystemOnTPTP
    newSpec = case lookConfig "X2TPTP" config of
      Just val → spec { optX2TPTP = val }
      _        → spec

setters ∷ [Config → SystemOnTPTP → SystemOnTPTP]
setters = [
    setAutoMode
  , setAutoModeSystemsLimit
  , setAutoModeTimeLimit
  , setCompleteness
  , setCorrectness
  , setCPUPassword
  , setFORMULAEProblem
  , setFormulaURL
  , setIDV
  , setNoHTML
  , setProblemSource
  , setQuietFlag
  , setReportFlag
  , setSoundness
  , setSubmitButton
  , setSystemInfo
  , setSystemOnTSTP
  , setSystems
  , setTPTPProblem
  , setTSTPData
  , setUPLOADProblem
  , setX2TPTP
  ]

combineSystemOnTPTP ∷ Config → SystemOnTPTP
combineSystemOnTPTP config = setVals defaultSystemOnTPTP
  where
    setVals ∷ SystemOnTPTP → SystemOnTPTP
    setVals = foldl (flip (.)) id g

    g ∷ [SystemOnTPTP → SystemOnTPTP]
    g = map (\f → f config) setters


finalConfig ∷ [Config] → SystemOnTPTP
finalConfig []  = defaultSystemOnTPTP
finalConfig cfs = combineSystemOnTPTP $ Config $ HashMap.unions hs
  where
    hs ∷ [Object]
    hs = map getHashMap cfs

    getHashMap ∷ Config → Object
    getHashMap (Config o) = o

loadYAML ∷ FilePath → IO Config
loadYAML dotOnlineatps = do
  decoded ← YamlInclude.decodeFile dotOnlineatps
  return $ Config $ fromMaybe HashMap.empty decoded

onlineatpsNameFile ∷ FilePath
onlineatpsNameFile = ".onlineatps"

onlineatpsTemplate :: FilePath
onlineatpsTemplate = "onlineatps.yml"

-- Uncomment this to use ghcid
-- getDataFileName ∷ FilePath → IO FilePath
-- getDataFileName path = return $ "./data" </> path


-- | Get the default values for the command-line 'SystemOnTPTP'.
getDefaults ∷ IO SystemOnTPTP
getDefaults = do
  paths ← sequence [getCurrentDirectory, getHomeDirectory]
  userFiles ← filterM doesFileExist $ map (</>onlineatpsNameFile) paths
  defaultConfig ← getDataFileName onlineatpsTemplate

  let allFiles ∷ [FilePath]
      allFiles = userFiles ++ [defaultConfig]

  loaded ← mapM loadYAML allFiles
  return $ finalConfig loaded
