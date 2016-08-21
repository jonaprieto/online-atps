
-- | Set the defaults fot the package

{-# LANGUAGE UnicodeSyntax #-}

module OnlineATPs.Defaults
  ( defaultOnlineATP
  , defaultSystemOnTPTP
  , getDefaults
  ) where

import           OnlineATPs.SystemATP    (SystemATP (..))
import           OnlineATPs.SystemOnTPTP (SystemOnTPTP (..))

defaultOnlineATP ∷ SystemATP
defaultOnlineATP = SystemATP
  { sysKey         = "online-e"
  , sysName        = "E---2.0"
  , sysVersion     = "2.0"
  , sysTimeLimit   = "60"
  , sysTransform   = "none"
  , sysFormat      = "tptp:raw"
  , sysCommand     = "eprover -s --cpu-limit=%d %s"
  , sysApplication = "Prover and model finder, for FOF CNF"
}

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
  , optSystems               = [ defaultOnlineATP ]
  , optTPTPProblem           = "" --AGT001+1"
  , optTSTPData              = False
  , optUPLOADProblem         = ""
  , optX2TPTP                = False
}

getDefaults ∷ IO SystemOnTPTP
getDefaults = return defaultSystemOnTPTP
