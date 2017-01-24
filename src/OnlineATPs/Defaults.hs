
-- | Default values for all options of the package.

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

import Control.Monad           ( filterM )
import Data.Maybe

import OnlineATPs.SystemATP    ( SystemATP (..) )
import OnlineATPs.SystemOnTPTP ( SystemOnTPTP (..) )
import OnlineATPs.Utils.Yaml

import Paths_online_atps        ( getDataFileName )
import Prelude hiding ( lookup )

import System.Directory
  ( doesFileExist
  , getCurrentDirectory
  , getHomeDirectory
  )
import System.FilePath.Posix   ( (</>) )

-- | 'defaultSystemATP' provides the default settings for the ATP by
-- default, Eprover 2.0.
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

-- | 'defaultSystemATP' stores the defaults options in the TPTP World.
defaultSystemOnTPTP ∷ SystemOnTPTP
defaultSystemOnTPTP = SystemOnTPTP
  { optAutoMode              = "-cE"
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


configFileName ∷ FilePath
configFileName = ".online-atps"

configTemplate ∷ FilePath
configTemplate = "online-atps.yml"

combineConfigs ∷ [Object] → Parser SystemOnTPTP
combineConfigs configs  =  do
  optAutoMode             ← configs .@. "AutoMode"
  optAutoModeSystemsLimit ← configs .@. "AutoModeSystemsLimit"
  optAutoModeTimeLimit    ← configs .@. "AutoModeTimeLimit"
  optCompleteness         ← configs .@. "Completeness"
  optCorrectness          ← configs .@. "Correctness"
  optCPUPassword          ← configs .@. "CPUPassword"
  optFORMULAEProblem      ← configs .@. "FORMULAEProblem"
  optFormulaURL           ← configs .@. "FormulaURL"
  optIDV                  ← configs .@. "IDV"
  optNoHTML               ← configs .@. "NoHTML"
  optProblemSource        ← configs .@. "ProblemSource"
  optQuietFlag            ← configs .@. "QuietFlag"
  optReportFlag           ← configs .@. "ReportFlag"
  optSoundness            ← configs .@. "Soundness"
  optSubmitButton         ← configs .@. "SubmitButton"
  optSystemInfo           ← configs .@. "SystemInfo"
  optSystemOnTSTP         ← configs .@. "SystemOnTSTP"
  optSystems              ← configs .@. "Systems"
  optTPTPProblem          ← configs .@. "TPTPProblem"
  optTSTPData             ← configs .@. "TSTPData"
  optUPLOADProblem        ← configs .@. "UPLOADProblem"
  optX2TPTP               ← configs .@. "X2TPTP"
  return SystemOnTPTP{..}

-- | Get the default values for the command-line 'SystemOnTPTP'.
getDefaults ∷ IO SystemOnTPTP
getDefaults = do
  paths ← sequence [getCurrentDirectory, getHomeDirectory]
  userFiles ← filterM doesFileExist $ map (</>configFileName) paths
  defaultConfig ← getDataFileName configTemplate

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
