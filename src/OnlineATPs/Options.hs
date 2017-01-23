
-- | Process the command-line arguments.

{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module OnlineATPs.Options
  ( getManageOpt
  , options
  , MOptions  -- Required by Haddock.
  , Options
    ( Options --Improve Haddock information.
    , optATP
    , optATPList
    , optFOF
    , optHelp
    , optInputFile
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


data ManageOption a = DefaultOpt a | CommandOpt a

-- | TODO
getManageOpt ∷ ManageOption a → a
getManageOpt (DefaultOpt val) = val
getManageOpt (CommandOpt val) = val


-- | Program command-line options.
data Options = Options
  { optATP            ∷ ManageOption [String]
  , optATPList        ∷ Bool
  , optFOF            ∷ Bool
  , optHelp           ∷ Bool
  , optInputFile      ∷ Maybe FilePath
  , optOnlyCheck      ∷ Bool
  , optTime           ∷ Int
  , optVersion        ∷ Bool
  , optVersionATP     ∷ String
  , optWithAll        ∷ Bool
  }


defaultOptions ∷ Options
defaultOptions = Options
  { optATP            = DefaultOpt []
  , optATPList        = False
  , optFOF            = False
  , optHelp           = False
  , optInputFile      = Nothing
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

fofOpt ∷ MOptions
fofOpt opts = Right opts { optFOF = True }

helpOpt ∷ MOptions
helpOpt opts = Right opts { optHelp = True }

inputFileOpt ∷ FilePath → MOptions
inputFileOpt file opts =
  case optInputFile opts of
    Nothing → Right opts { optInputFile = Just file }
    Just _  → Left $ pretty "only one input file allowed"


onlyCheckOpt ∷ MOptions
onlyCheckOpt opts = Right opts { optOnlyCheck = True }

timeOpt ∷ String → MOptions
timeOpt [] _ = Left $
  pretty "option " <> squotes "--time" <> pretty " requires an argument NUM"
timeOpt secs opts =
  if all isDigit secs
  then Right opts { optTime = read secs }
  else Left $ pretty "option " <> squotes "--time"
              <> pretty " requires a non-negative integer argument"

versionOpt ∷ MOptions
versionOpt opts = Right opts { optVersion = True }

versionATPOpt ∷ String → MOptions
versionATPOpt [] _ = Left $
  pretty "option " <> squotes "--version-atp" <> pretty " requires an argument NAME"
versionATPOpt name opts = Right opts { optVersionATP = name }

withAllOpt ∷ MOptions
withAllOpt opts = Right opts { optWithAll = True }

-- -- | Description of the command-line 'Options'.
options ∷ [OptDescr MOptions]
options =
  [ Option []  ["atp"] (ReqArg atpOpt "NAME")
               "Set the ATP (online-e, online-vampire, online-z3, ...)\n"
  , Option []  ["fof"] (NoArg fofOpt)
               "Only use ATP for FOF"
  , Option []  ["help"] (NoArg helpOpt)
               "Show this help"
  , Option []  ["list-atps"] (NoArg atpListOpt)
               "Consult all ATPs available in TPTP World"
  , Option []  ["only-check"] (NoArg onlyCheckOpt)
               "Only checks the output looking for a theorem."
  , Option []  ["time"] (ReqArg timeOpt "NUM")
               "Set timeout for the ATPs in seconds (default: 300)"
  , Option []  ["version"] (NoArg versionOpt)
               "Show version number"
  , Option []  ["version-atp"] (ReqArg versionATPOpt "NAME")
               "Show version of the atp NAME"
  , Option []  ["with-all"] (NoArg withAllOpt)
               "Use all ATPs available"
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
