
-- -- | Process the command-line arguments.
-- Adapted from @Apia.Options

{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnicodeSyntax #-}

module OnlineATPs.Options
  (
    options
  , MOptions  -- Required by Haddock.
  , Options
    ( Options --Improve Haddock information.
    , optATP
    , optHelp
    , optInputFile
    , optATPList
    , optTime
    , optVersion
    )
  , printUsage
  , processOptions
  ) where

import           Data.Char                    (isDigit)
import           Data.List                    (foldl', nub)
import qualified Data.Text                    as T (pack)
import           OnlineATPs.SystemOnTPTP      (SystemOnTPTP)
import           OnlineATPs.Utils.PrettyPrint (Doc, Pretty (pretty), squotes,
                                               (<>))
-- import Safe ( initDef )
import System.Console.GetOpt
  (
    ArgDescr(NoArg, ReqArg)
  , ArgOrder(ReturnInOrder)
  , getOpt
  , OptDescr(Option)
  , usageInfo
  )

import           System.Environment           (getProgName)


-- #include "undefined.h"


-- -- | Program command-line options.
data Options = Options
  { optATP        ∷ [String]
  , optHelp       ∷ Bool
  , optInputFile  ∷ Maybe FilePath
  , optATPList    ∷ Bool
  , optTime       ∷ Int
  , optVersion    ∷ Bool
  }
  deriving Show


defaultOptions ∷ Options
defaultOptions = Options
  { optATP        =  []
  , optHelp       =  False
  , optInputFile  =  Nothing
  , optATPList    =  False
  , optTime       =  240
  , optVersion    =  False
  }


-- | 'Options' monad.
type MOptions = Options → Either Doc Options

atpOpt ∷ String → MOptions
atpOpt [] _ = Left $
  pretty "option " <> squotes "--atp" <> pretty " requires an argument NAME"
atpOpt name opts = Right opts { optATP = nub $ optATP opts ++ [name] }

helpOpt ∷ MOptions
helpOpt opts = Right opts { optHelp = True }

inputFileOpt ∷ FilePath → MOptions
inputFileOpt file opts =
  case optInputFile opts of
    Nothing → Right opts { optInputFile = Just file }
    Just _  → Left $ pretty "only one input file allowed"

atpListOpt ∷ MOptions
atpListOpt opts = Right opts { optATPList = True }

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

-- -- | Description of the command-line 'Options'.
options ∷ [OptDescr MOptions]
options =
  [ Option []  ["atp"] (ReqArg atpOpt "NAME")
               "Set the ATP (online-e, online-vampire, online-z3, ...)\n"
  , Option []  ["help"] (NoArg helpOpt)
               "Show this help"
  , Option []  ["time"] (ReqArg timeOpt "NUM")
               "Set timeout for the ATPs in seconds (default: 240)"
  , Option []  ["list-atps"] (NoArg atpListOpt)
               "Consult all FOF ATPs available in TPTP World"
  , Option []  ["version"] (NoArg versionOpt)
               "Show version number"
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

-- -- | Processing the command-line 'Options'.
processOptions ∷ [String] → Either Doc Options
processOptions argv = processOptionsHelper argv inputFileOpt defaultOptions
