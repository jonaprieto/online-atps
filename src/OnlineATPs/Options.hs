
-- | Process the command-line arguments.

{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module OnlineATPs.Options
  ( defaultOptions
  , getManageOpt
  , options
  , ManageOption
  , MOptions  -- Required by Haddock.
  , Options
    ( Options --Improve Haddock information.
    , optATP
    , optATPList
    , optDebug
    , optFOF
    , optHelp
    , optInputFile
    , optSystemOnTPTP
    , optOnlyCheck
    , optTime
    , optVersion
    , optVersionATP
    , optWithAll -- Test against all ATPs
    )
  , printUsage
  , processOptions
  ) where

import Data.Char ( isDigit )
import Data.List ( foldl', nub )

import OnlineATPs.Defaults ( defaultSystemOnTPTP )
import OnlineATPs.SystemOnTPTP
  ( SystemOnTPTP
    ( optAutoMode
    , optAutoModeSystemsLimit
    , optAutoModeTimeLimit
    , optCompleteness
    , optCorrectness
    , optCPUPassword
    -- , optFORMULAEProblem
    -- , optFormulaURL
    , optIDV
    -- , optNoHTML
    -- , optProblemSource
    , optQuietFlag
    , optReportFlag
    , optSoundness
    , optSubmitButton
    , optSystemInfo
    , optSystemOnTSTP
    -- , optTPTPProblem
    , optTSTPData
    -- , optUPLOADProblem
    , optX2TPTP
    )
  )
import OnlineATPs.Utils.PrettyPrint
  ( Doc
  , Pretty ( pretty )
  , squotes
  , (<>)
  )

import System.Console.GetOpt
  (
    ArgDescr ( NoArg, ReqArg )
  , ArgOrder ( ReturnInOrder )
  , getOpt
  , OptDescr ( Option )
  , usageInfo
  )
import System.Environment ( getProgName )

#if MIN_VERSION_base(4,11,0)
import Prelude hiding ( (<>) )
#endif

-- | 'ManageOption' handles the options from the defaults and the command
-- line.
data ManageOption a = DefaultOpt a | CommandOpt a
                    deriving Show

-- | The function 'getManageOpt' extracts the value from the
-- 'Options.ManageOption' data type.
getManageOpt ∷ ManageOption a → a
getManageOpt (DefaultOpt val) = val
getManageOpt (CommandOpt val) = val


-- | Program command-line options.
data Options = Options
  { optATP            ∷ ManageOption [String]
  , optATPList        ∷ Bool
  , optDebug          ∷ Bool
  , optFOF            ∷ Bool
  , optHelp           ∷ Bool
  , optInputFile      ∷ Maybe FilePath
  , optSystemOnTPTP   ∷ SystemOnTPTP
  , optOnlyCheck      ∷ Bool
  , optTime           ∷ Int
  , optVersion        ∷ Bool
  , optVersionATP     ∷ String
  , optWithAll        ∷ Bool
  }
  deriving Show

-- | Default record values for 'Options'.
defaultOptions ∷ Options
defaultOptions = Options
  { optATP            = DefaultOpt []
  , optATPList        = False
  , optDebug          = False
  , optFOF            = False
  , optHelp           = False
  , optInputFile      = Nothing
  , optSystemOnTPTP   = defaultSystemOnTPTP
  , optOnlyCheck      = False
  , optTime           = 240
  , optVersion        = False
  , optVersionATP     = ""
  , optWithAll        = False
  }


-- | 'Options' monad.
type MOptions = Options → Either Doc Options

atpOpt ∷ String → MOptions
atpOpt [] _ = Left $
  pretty "option " <> squotes "--atp" <> pretty " requires an argument NAME"
atpOpt name opts = Right opts { optATP = CommandOpt atps }
  where
    atps ∷ [String]
    atps = case optATP opts of
      CommandOpt old → nub $ old ++ [name]
      DefaultOpt _   → [name]

atpListOpt ∷ MOptions
atpListOpt opts = Right opts { optATPList = True }

autoModeOpt ∷ String → MOptions
autoModeOpt [] _ = Left $
  pretty "option " <> squotes "--auto-mode" <> pretty " requires an argument MODE"
autoModeOpt val opts = let system = optSystemOnTPTP opts in
  Right opts { optSystemOnTPTP = system { optAutoMode = val } }

autoModeSystemsLimitOpt ∷ String → MOptions
autoModeSystemsLimitOpt [] _ = Left $
  pretty "option " <> squotes "--auto-mode-timelimit" <> pretty " requires an argument TIME"
autoModeSystemsLimitOpt val opts = let system = optSystemOnTPTP opts in
  Right opts { optSystemOnTPTP = system { optAutoModeSystemsLimit = val } }

autoModeTimeLimitOpt ∷ String → MOptions
autoModeTimeLimitOpt [] _ = Left $
  pretty "option " <> squotes "--auto-mode-timelimit" <> pretty " requires an argument TIME"
autoModeTimeLimitOpt val opts = let system = optSystemOnTPTP opts in
  Right opts { optSystemOnTPTP = system { optAutoModeTimeLimit = val } }

completenessOpt ∷ MOptions
completenessOpt opts = let system = optSystemOnTPTP opts in
  Right opts { optSystemOnTPTP = system { optCompleteness = True } }

correctenessOpt ∷ MOptions
correctenessOpt opts = let system = optSystemOnTPTP opts in
  Right opts { optSystemOnTPTP = system { optCorrectness = True } }

cpuPasswordOpt ∷ String → MOptions
cpuPasswordOpt [] _ = Left $
  pretty "option " <> squotes "--cpu-password" <> pretty " requires an argument KEY"
cpuPasswordOpt pass opts = let system = optSystemOnTPTP opts in
  Right opts { optSystemOnTPTP = system { optCPUPassword = pass } }

debugOpt ∷ MOptions
debugOpt opts = Right opts { optDebug = True }

fofOpt ∷ MOptions
fofOpt opts = Right opts { optFOF = True }

helpOpt ∷ MOptions
helpOpt opts = Right opts { optHelp = True }

idvOpt ∷ MOptions
idvOpt opts = let system = optSystemOnTPTP opts in
  Right opts { optSystemOnTPTP = system { optIDV = True } }

inputFileOpt ∷ FilePath → MOptions
inputFileOpt file opts =
  case optInputFile opts of
    Nothing → Right opts { optInputFile = Just file }
    Just _  → Left $ pretty "only one input file allowed"

onlyCheckOpt ∷ MOptions
onlyCheckOpt opts = Right opts { optOnlyCheck = True }

quietOpt ∷ String → MOptions
quietOpt [] _ = Left $
  pretty "option " <> squotes "--quiet-mode" <> pretty " requires an argument MODE"
quietOpt mode opts = let system = optSystemOnTPTP opts in
  Right opts { optSystemOnTPTP = system { optQuietFlag = mode } }

reportFlagOpt ∷ String → MOptions
reportFlagOpt [] _ = Left $
  pretty "option " <> squotes "--report-flag" <> pretty " requires an argument MODE"
reportFlagOpt mode opts = let system = optSystemOnTPTP opts in
  Right opts { optSystemOnTPTP = system { optReportFlag = mode } }

systemInfoOpt ∷ MOptions
systemInfoOpt opts = let system = optSystemOnTPTP opts in
  Right opts { optSystemOnTPTP = system { optSystemInfo = True } }

systemOnTSTPOpt ∷ MOptions
systemOnTSTPOpt opts = let system = optSystemOnTPTP opts in
  Right opts { optSystemOnTPTP = system { optSystemOnTSTP = True } }

soudnessOpt ∷ MOptions
soudnessOpt opts = let system = optSystemOnTPTP opts in
  Right opts { optSystemOnTPTP = system { optSoundness = True } }

submitButtonOpt ∷ String → MOptions
submitButtonOpt [] _ = Left $ pretty "option " <> squotes "--action" <> pretty " requires an argument MODE"
submitButtonOpt mode opts = let system = optSystemOnTPTP opts in
  Right opts { optSystemOnTPTP = system { optSubmitButton = mode } }

timeOpt ∷ String → MOptions
timeOpt [] _ = Left $
  pretty "option " <> squotes "--time" <> pretty " requires an argument NUM"
timeOpt secs opts =
  if all isDigit secs
  then Right opts { optTime = read secs }
  else Left $ pretty "option " <> squotes "--time"
              <> pretty " requires a non-negative integer argument"

tstpDataOpt ∷ MOptions
tstpDataOpt opts = let system = optSystemOnTPTP opts in
  Right opts { optSystemOnTPTP = system { optTSTPData = True } }

versionOpt ∷ MOptions
versionOpt opts = Right opts { optVersion = True }

versionATPOpt ∷ String → MOptions
versionATPOpt [] _ = Left $
  pretty "option " <> squotes "--version-atp" <> pretty " requires an argument NAME"
versionATPOpt name opts = Right opts { optVersionATP = name }

withAllOpt ∷ MOptions
withAllOpt opts = Right opts { optWithAll = True }

x2tptpOpt ∷ MOptions
x2tptpOpt opts = let system = optSystemOnTPTP opts in
  Right opts { optSystemOnTPTP = system { optX2TPTP = True } }


-- | Description of the command-line 'Options'.
options ∷ [OptDescr MOptions]
options =
  [ Option []  ["action"] (ReqArg submitButtonOpt "MODE") $
                "Action to submit (\"RunSelectedSystems\", \"RunParallel\",\n"
                ++ "\"RecommendSystems\" or \"ReportSelectedSystems\")."
  , Option []  ["atp"] (ReqArg atpOpt "NAME") $
               "Set the ATP (e.g. \"e\" or \"online-e\", \"vampire\" or "
               ++ "\"online-vampire\")"
  , Option []  ["auto-mode"] (ReqArg autoModeOpt "MODE") $
                "Parellel Mode (\"Selected\", \"Naive\", \"SSCPA\""
                ++ "and \"Eager SSCPA\")"
  , Option []  ["auto-mode-system-limit"] (ReqArg autoModeSystemsLimitOpt "TIME")
                "System time limet in the parallel mode"
  , Option []  ["auto-mode-time-limit"] (ReqArg autoModeTimeLimitOpt "TIME")
                "Time limit in the parallel mode"
  , Option []  ["completeness"] (NoArg completenessOpt)
                "Turn on the option completess"
  , Option []  ["correcteness"] (NoArg correctenessOpt)
                "Turn on the option correctness"
  , Option []  ["cpu-password"] (ReqArg cpuPasswordOpt "KEY")
                "Set the CPU Password on SystemOnTPTP\n"
  , Option []  ["debug"] (NoArg debugOpt)
                "Prints out the given options values"
  , Option []  ["fof"] (NoArg fofOpt)
               "Only use ATP for FOF"
  , Option []  ["help"] (NoArg helpOpt)
               "Show this help"
  , Option []  ["idv"] (NoArg idvOpt)
               "Turn on the option IDV"
  , Option []  ["list-atps"] (NoArg atpListOpt)
               "Consult all ATPs available in TPTP World"
  , Option []  ["only-check"] (NoArg onlyCheckOpt)
               "Only checks the output looking for a theorem."
  , Option []  ["quiet-mode"] (ReqArg quietOpt "MODE") $
                "Set the Output Mode (\"-q0\" : Everything, \"-q01\" : System,\n"
                ++ "-q2\" Progress, \"-q3\" : Result, and \"-q4\": Nothing)"
  , Option []  ["report"] (ReqArg reportFlagOpt "MODE") $
                "Report flag for the \"Recommend Systems\""
                ++ "section (\"-q2\": Summary, \"-q0\": Full)"
  , Option []  ["soudness"] (NoArg soudnessOpt)
               "Turn on the option Soudness"
  , Option []  ["system-info"] (NoArg systemInfoOpt)
               "Turn on the option System Information"
  , Option []  ["system-on-tstp"] (NoArg systemOnTSTPOpt)
               "Turn on the option SystemOnTSTP"
  , Option []  ["time"] (ReqArg timeOpt "NUM")
               "Set timeout for the ATPs in seconds (default: 300)"
  , Option []  ["tstp-data"] (NoArg tstpDataOpt)
               "Turn on the option TSTP Data"
  , Option []  ["version"] (NoArg versionOpt)
               "Show version number"
  , Option []  ["version-atp"] (ReqArg versionATPOpt "NAME")
               "Show version of the atp NAME"
  , Option []  ["with-all"] (NoArg withAllOpt)
               "Use all ATPs available"
  , Option []  ["x2tptp"] (NoArg x2tptpOpt)
               "Turn on the option X2TPTP"
  ]

usageHeader ∷ String → String
usageHeader prgName = "Usage: " ++ prgName ++ " [OPTIONS] FILE\n"

-- | Print usage information.
printUsage ∷ IO ()
printUsage = do
  progName ← getProgName
  putStrLn $ usageInfo (usageHeader progName) options

processOptionsHelper ∷ [String] → (FilePath → MOptions) → MOptions
processOptionsHelper argv f defaults =
  case getOpt (ReturnInOrder f) options argv of
    (o, _, [])   → foldl' (>>=) (return defaults) o
    (_, _, errs) → Left $ pretty $ unlines errs

-- | Processing the command-line 'Options'.
processOptions ∷ [String] → Either Doc Options
processOptions argv = processOptionsHelper argv inputFileOpt defaultOptions
