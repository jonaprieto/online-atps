{-# LANGUAGE UnicodeSyntax #-}

module OnlineATPs.SystemATP
  ( SystemATP
    ( SystemATP
    , sysApplication
    , sysCommand
    , sysFormat
    , sysKey
    , sysName
    , sysTimeLimit
    , sysTransform
    , sysVersion
    , NoSystemATP
    )
  , checkOnlineATPOutput
  , getDataSystemATP
  , isFOFATP
  , onlineATPOk
  , onlineATPVersion
  , printListOnlineATPs
  ) where

import           Data.List (intercalate, isInfixOf)

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
  show atp = sysKey atp ++ ": "
    ++ "\n name: " ++ sysName atp
    ++ "\n version: " ++ sysVersion atp
    ++ "\n application: " ++ sysApplication atp

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
    label = sysName atp

isFOFATP ∷ SystemATP → Bool
isFOFATP NoSystemATP = False
isFOFATP atp         = isInfixOf "FOF" $ sysApplication atp

onlineATPOk ∷ SystemATP → String
onlineATPOk NoSystemATP = error msgErrorNoSystemATP
onlineATPOk atp = sysName atp ++ "---" ++ sysVersion atp ++ " says Theorem"

onlineATPVersion ∷ SystemATP → String
onlineATPVersion NoSystemATP = error msgErrorNoSystemATP
onlineATPVersion atp         = sysVersion atp

checkOnlineATPOutput ∷ SystemATP → String → Bool
checkOnlineATPOutput NoSystemATP _ = False
checkOnlineATPOutput atp output    = onlineATPOk atp `isInfixOf` output

printListOnlineATPs ∷ [SystemATP] → IO ()
printListOnlineATPs atps = putStrLn $ intercalate "\n\n" $ map show atps
