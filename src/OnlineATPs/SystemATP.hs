
-- | SystemOnATP data type

{-# LANGUAGE UnicodeSyntax #-}

module OnlineATPs.SystemATP
  ( SystemATP
    ( SystemATP
    , NoSystemATP
    , sysApplication
    , sysCommand
    , sysFormat
    , sysKey
    , sysName
    , sysTimeLimit
    , sysTransform
    , sysVersion
    )
  , getDataSystemATP
  , getNameVersion
  , isFOFATP
  , onlineATPVersion
  , printListOnlineATPs
  , msgErrorNoSystemATP
  ) where

import           Data.List             (intercalate, isInfixOf)
import           OnlineATPs.Utils.Show (showListLn)
import Control.Monad (mapM_)

data SystemATP = SystemATP
  { sysApplication ∷ String
  , sysCommand     ∷ String
  , sysFormat      ∷ String
  , sysKey         ∷ String
  , sysName        ∷ String
  , sysTimeLimit   ∷ String
  , sysTransform   ∷ String
  , sysVersion     ∷ String
  } | NoSystemATP
  deriving Eq

msgErrorNoSystemATP ∷ String
msgErrorNoSystemATP = "The system is not a valid ATP."

instance Show SystemATP where
  show NoSystemATP = msgErrorNoSystemATP
  show atp = intercalate "\n" $
    [ "[" ++ sysName atp ++ "]"
    , "  application: " ++ sysApplication atp
    , "  key: " ++ sysKey atp
    , "  version: " ++ sysVersion atp
    , "\n"
    ]

getNameVersion ∷ SystemATP → String
getNameVersion atp = sysName atp ++ "---" ++ sysVersion atp

getDataSystemATP ∷ SystemATP → [(String, String)]
getDataSystemATP NoSystemATP = []
getDataSystemATP atp = [
     ( "Command___"    ++ label, sysCommand atp )
  ,  ( "Format___"     ++ label, sysFormat atp )
  ,  ( "System___"     ++ label, label )
  ,  ( "TimeLimit___"  ++ label, sysTimeLimit atp )
  ,  ( "Transform___"  ++ label, sysTransform atp )
  ]
  where
    label ∷ String
    label = getNameVersion atp

isFOFATP ∷ SystemATP → Bool
isFOFATP NoSystemATP = False
isFOFATP atp         = isInfixOf "FOF" $ sysApplication atp

onlineATPVersion ∷ SystemATP → String
onlineATPVersion NoSystemATP = error msgErrorNoSystemATP
onlineATPVersion atp         = sysVersion atp

printListOnlineATPs ∷ [SystemATP] → IO ()
printListOnlineATPs atps = do
  putStr $ showListLn atps
  putStrLn $ "(" ++ show (length atps) ++ ") ATPs available"
