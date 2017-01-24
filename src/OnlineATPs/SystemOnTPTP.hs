
-- | SystemOnTPTP data type.

{-# LANGUAGE UnicodeSyntax #-}

module OnlineATPs.SystemOnTPTP
  ( SystemOnTPTP
    ( SystemOnTPTP
    , optAutoMode
    , optAutoModeSystemsLimit
    , optAutoModeTimeLimit
    , optCompleteness
    , optCorrectness
    , optCPUPassword
    , optFORMULAEProblem
    , optFormulaURL
    , optIDV
    , optNoHTML
    , optProblemSource
    , optQuietFlag
    , optReportFlag
    , optSoundness
    , optSubmitButton
    , optSystemInfo
    , optSystemOnTSTP
    , optSystems
    , optTPTPProblem
    , optTSTPData
    , optUPLOADProblem
    , optX2TPTP
    )
  , getDataSystemOnTPTP
  , setFORMULAEProblem
  , setSystems
  ) where


import OnlineATPs.SystemATP ( SystemATP, getDataSystemATP )


-- | The 'SystemOnTPTP' data type handles the option that a request can
-- have to get a response in the TPTP World.
data SystemOnTPTP = SystemOnTPTP
  { optAutoMode             ∷ String
  , optAutoModeSystemsLimit ∷ String
  , optAutoModeTimeLimit    ∷ String
  , optCompleteness         ∷ Bool
  , optCorrectness          ∷ Bool
  , optCPUPassword          ∷ String
  , optFORMULAEProblem      ∷ String
  , optFormulaURL           ∷ String
  , optIDV                  ∷ Bool
  , optNoHTML               ∷ String
  , optProblemSource        ∷ String
  , optQuietFlag            ∷ String
  , optReportFlag           ∷ String
  , optSoundness            ∷ Bool
  , optSubmitButton         ∷ String
  , optSystemInfo           ∷ Bool
  , optSystemOnTSTP         ∷ Bool
  , optSystems              ∷ [SystemATP]
  , optTPTPProblem          ∷ String
  , optTSTPData             ∷ Bool
  , optUPLOADProblem        ∷ String
  , optX2TPTP               ∷ Bool
  } deriving Show

getDataAutoMode ∷ SystemOnTPTP → [(String, String)]
getDataAutoMode spec
  | not $ null $ optAutoMode spec = val
  | otherwise = []
  where
    val ∷ [(String, String)]
    val = [("AutoMode", optAutoMode spec )]

getDataAutoModeSystemsLimit ∷ SystemOnTPTP → [(String, String)]
getDataAutoModeSystemsLimit spec
  | not $ null $ optAutoModeSystemsLimit spec = val
  | otherwise = []
  where
    val ∷ [(String, String)]
    val = [("AutoModeSystemsLimit", optAutoModeSystemsLimit spec )]

getDataAutoModeTimeLimit ∷ SystemOnTPTP → [(String, String)]
getDataAutoModeTimeLimit spec
  | not $ null $ optAutoModeTimeLimit spec = val
  | otherwise = []
  where
    val ∷ [(String, String)]
    val = [("AutoModeTimeLimit", optAutoModeTimeLimit spec )]

getDataCompleteness ∷ SystemOnTPTP → [(String, String)]
getDataCompleteness spec
  | optCompleteness spec  = [("Completeness", "Completeness")]
  | otherwise             = []

getDataCorrectness ∷ SystemOnTPTP → [(String, String)]
getDataCorrectness spec
  | optCorrectness spec = [("Correctness", "Correctness")]
  | otherwise           = []

getDataCPUPassword ∷ SystemOnTPTP → [(String, String)]
getDataCPUPassword spec
  | not $ null $ optCPUPassword spec = val
  | otherwise = []
  where
    val ∷ [(String, String)]
    val = [("CPUPassword", optCPUPassword spec )]

getDataFORMULAEProblem ∷ SystemOnTPTP → [(String, String)]
getDataFORMULAEProblem spec
  | not $ null $ optFORMULAEProblem spec = val
  | otherwise = []
  where
    val ∷ [(String, String)]
    val = [("FORMULAEProblem", optFORMULAEProblem spec )]

getDataFormulaURL ∷ SystemOnTPTP → [(String, String)]
getDataFormulaURL spec
  | not $ null $ optFormulaURL spec = val
  | otherwise = []
  where
    val ∷ [(String, String)]
    val = [("FormulaURL", optFormulaURL spec )]

getDataIDV ∷ SystemOnTPTP → [(String, String)]
getDataIDV spec
  | optIDV spec   = [("IDV", "-T")]
  | otherwise     = []

getDataNoHTML ∷ SystemOnTPTP → [(String, String)]
getDataNoHTML spec
  | not $ null $ optNoHTML spec = val
  | otherwise = []
  where
    val ∷ [(String, String)]
    val = [("NoHTML", optNoHTML spec )]

getDataProblemSource ∷ SystemOnTPTP → [(String, String)]
getDataProblemSource spec
  | not $ null $ optProblemSource spec = val
  | otherwise = []
  where
    val ∷ [(String, String)]
    val = [("ProblemSource", optProblemSource spec )]

getDataQuietFlag ∷ SystemOnTPTP → [(String, String)]
getDataQuietFlag spec
  | not $ null $ optQuietFlag spec = val
  | otherwise = []
  where
    val ∷ [(String, String)]
    val = [("QuietFlag", optQuietFlag spec )]

getDataReportFlag ∷ SystemOnTPTP → [(String, String)]
getDataReportFlag spec
  | not $ null $ optReportFlag spec = val
  | otherwise = []
  where
    val ∷ [(String, String)]
    val = [("ReportFlag", optReportFlag spec )]

getDataSoundness ∷ SystemOnTPTP → [(String, String)]
getDataSoundness spec
  | optSoundness spec   = [("Soundness", "Soundness")]
  | otherwise           = []

getDataSubmitButton ∷ SystemOnTPTP → [(String, String)]
getDataSubmitButton spec
  | not $ null $ optSubmitButton spec = val
  | otherwise = []
  where
    val ∷ [(String, String)]
    val = [("SubmitButton", optSubmitButton spec )]

getDataSystemInfo ∷ SystemOnTPTP → [(String, String)]
getDataSystemInfo spec
  | optSystemInfo spec  = [("SystemInfo", "SystemInfo")]
  | otherwise           = []

getDataSystemOnTSTP ∷ SystemOnTPTP → [(String, String)]
getDataSystemOnTSTP spec
  | optSystemOnTSTP spec = [("SystemOnTSTP", "-S")]
  | otherwise            = []

getDataSystems ∷ SystemOnTPTP → [(String, String)]
getDataSystems spec = concatMap getDataSystemATP $ optSystems spec

getDataTPTPProblem ∷ SystemOnTPTP → [(String, String)]
getDataTPTPProblem spec
  | not $ null $ optTPTPProblem spec = val
  | otherwise = []
  where
    val ∷ [(String, String)]
    val = [("TPTPProblem", optTPTPProblem spec )]

getDataTSTPData ∷ SystemOnTPTP → [(String, String)]
getDataTSTPData spec
  | optTSTPData spec  = [("TSTPData", "TSTPData")]
  | otherwise         = []

getDataUPLOADProblem ∷ SystemOnTPTP → [(String, String)]
getDataUPLOADProblem spec
  | not $ null $ optUPLOADProblem spec = val
  | otherwise = []
  where
    val ∷ [(String, String)]
    val = [("UPLOADProblem", optUPLOADProblem spec )]

getDataX2TPTP ∷ SystemOnTPTP → [(String, String)]
getDataX2TPTP spec
  | optX2TPTP spec  = [("X2TPTP", "-S")]
  | otherwise       = []

getters ∷ [ SystemOnTPTP → [(String, String)] ]
getters = [
    getDataAutoMode
  , getDataAutoModeSystemsLimit
  , getDataAutoModeTimeLimit
  , getDataCompleteness
  , getDataCorrectness
  , getDataCPUPassword
  , getDataFORMULAEProblem
  , getDataFormulaURL
  , getDataIDV
  , getDataNoHTML
  , getDataProblemSource
  , getDataQuietFlag
  , getDataReportFlag
  , getDataSoundness
  , getDataSubmitButton
  , getDataSystemInfo
  , getDataSystemOnTSTP
  , getDataSystems
  , getDataTPTPProblem
  , getDataTSTPData
  , getDataUPLOADProblem
  , getDataX2TPTP
  ]

-- | The function 'getDataSystemOnTPTP' returns a list of tuples
-- about (field, value).
getDataSystemOnTPTP ∷ SystemOnTPTP → [(String, String)]
getDataSystemOnTPTP spec = concatMap ($ spec) getters

-- | The function 'setFORMULAEProblem' sets the problem in the format TSTP.
setFORMULAEProblem ∷ SystemOnTPTP → String → SystemOnTPTP
setFORMULAEProblem spec problemText = spec { optFORMULAEProblem = problemText }

-- | The function 'setSystems' sets a list of ATPs to try against the problem.
setSystems ∷ SystemOnTPTP → [SystemATP] → SystemOnTPTP
setSystems spec atps = spec { optSystems = atps }
