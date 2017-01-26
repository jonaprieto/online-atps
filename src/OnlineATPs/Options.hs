
-- | Process the command-line arguments.

{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module OnlineATPs.Options
  ( getManageOpt
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

completenessOpt ∷ MOptions
completenessOpt opts = Right opts { optSystemOnTPTP = newTPTP }
  where
    sTPTP ∷ SystemOnTPTP
    sTPTP = optSystemOnTPTP opts

    newTPTP ∷ SystemOnTPTP
    newTPTP = sTPTP { optCompleteness = True }

correctenessOpt ∷ MOptions
correctenessOpt opts = Right opts { optSystemOnTPTP = newTPTP }
  where
    sTPTP ∷ SystemOnTPTP
    sTPTP = optSystemOnTPTP opts

    newTPTP ∷ SystemOnTPTP
    newTPTP = sTPTP { optCorrectness = True }

cpuPasswordOpt ∷ String → MOptions
cpuPasswordOpt [] _ = Left $
  pretty "option " <> squotes "--cpu-password" <> pretty " requires an argument KEY"
cpuPasswordOpt pass opts = Right opts { optSystemOnTPTP = newTPTP }
  where
    sTPTP ∷ SystemOnTPTP
    sTPTP = optSystemOnTPTP opts

    newTPTP ∷ SystemOnTPTP
    newTPTP = sTPTP { optCPUPassword = pass }

debugOpt ∷ MOptions
debugOpt opts = Right opts { optDebug = True }

fofOpt ∷ MOptions
fofOpt opts = Right opts { optFOF = True }

helpOpt ∷ MOptions
helpOpt opts = Right opts { optHelp = True }

idvOpt ∷ MOptions
idvOpt opts = Right opts { optSystemOnTPTP = newTPTP }
  where
    sTPTP ∷ SystemOnTPTP
    sTPTP = optSystemOnTPTP opts

    newTPTP ∷ SystemOnTPTP
    newTPTP = sTPTP { optIDV = True }

inputFileOpt ∷ FilePath → MOptions
inputFileOpt file opts =
  case optInputFile opts of
    Nothing → Right opts { optInputFile = Just file }
    Just _  → Left $ pretty "only one input file allowed"

onlyCheckOpt ∷ MOptions
onlyCheckOpt opts = Right opts { optOnlyCheck = True }


systemInfoOpt ∷ MOptions
systemInfoOpt opts = Right opts { optSystemOnTPTP = newTPTP }
  where
    sTPTP ∷ SystemOnTPTP
    sTPTP = optSystemOnTPTP opts

    newTPTP ∷ SystemOnTPTP
    newTPTP = sTPTP { optSystemInfo = True }

systemOnTSTPOpt ∷ MOptions
systemOnTSTPOpt opts = Right opts { optSystemOnTPTP = newTPTP }
  where
    sTPTP ∷ SystemOnTPTP
    sTPTP = optSystemOnTPTP opts

    newTPTP ∷ SystemOnTPTP
    newTPTP = sTPTP { optSystemOnTSTP = True }

soudnessOpt ∷ MOptions
soudnessOpt opts = Right opts { optSystemOnTPTP = newTPTP }
  where
    sTPTP ∷ SystemOnTPTP
    sTPTP = optSystemOnTPTP opts

    newTPTP ∷ SystemOnTPTP
    newTPTP = sTPTP { optSoundness = True }

timeOpt ∷ String → MOptions
timeOpt [] _ = Left $
  pretty "option " <> squotes "--time" <> pretty " requires an argument NUM"
timeOpt secs opts =
  if all isDigit secs
  then Right opts { optTime = read secs }
  else Left $ pretty "option " <> squotes "--time"
              <> pretty " requires a non-negative integer argument"

tstpDataOpt ∷ MOptions
tstpDataOpt opts = Right opts { optSystemOnTPTP = newTPTP }
  where
    sTPTP ∷ SystemOnTPTP
    sTPTP = optSystemOnTPTP opts

    newTPTP ∷ SystemOnTPTP
    newTPTP = sTPTP { optTSTPData = True }

versionOpt ∷ MOptions
versionOpt opts = Right opts { optVersion = True }

versionATPOpt ∷ String → MOptions
versionATPOpt [] _ = Left $
  pretty "option " <> squotes "--version-atp" <> pretty " requires an argument NAME"
versionATPOpt name opts = Right opts { optVersionATP = name }

withAllOpt ∷ MOptions
withAllOpt opts = Right opts { optWithAll = True }

x2tptpOpt ∷ MOptions
x2tptpOpt opts = Right opts { optSystemOnTPTP = newTPTP }
  where
    sTPTP ∷ SystemOnTPTP
    sTPTP = optSystemOnTPTP opts

    newTPTP ∷ SystemOnTPTP
    newTPTP = sTPTP { optX2TPTP = True }

-- | Description of the command-line 'Options'.
options ∷ [OptDescr MOptions]
options =
  [ Option []  ["atp"] (ReqArg atpOpt "NAME")
               "Set the ATP (online-e, online-vampire, online-z3, ...)\n"
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
