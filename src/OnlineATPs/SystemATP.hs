{-# LANGUAGE UnicodeSyntax #-}

module OnlineATPs.SystemATP
  ( SystemATP
    ( SystemATP
    , sysApplication
    , sysCommand
    , sysFormat
    , sysName
    , sysKey
    , sysVersion
    , sysTimeLimit
    , sysTransform
    )
  , getDataSystemATP
  , isFOFATP
  , printListOnlineATPs
  ) where

import           Data.List (intercalate, isInfixOf)

data SystemATP = SystemATP
  { sysApplication ∷ String
  , sysCommand     ∷ String
  , sysFormat      ∷ String
  , sysName        ∷ String
  , sysKey         ∷ String
  , sysVersion     ∷ String
  , sysTimeLimit   ∷ String
  , sysTransform   ∷ String
  }

instance Show SystemATP where
  show atp = sysKey atp ++ ": "
    ++ "\n name: " ++ sysName atp
    ++ "\n version: " ++ sysVersion atp
    ++ "\n application: " ++ sysApplication atp

getDataSystemATP ∷ SystemATP → [(String, String)]
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
isFOFATP atp = isInfixOf "FOF" $ sysApplication atp

printListOnlineATPs ∷ [SystemATP] → IO ()
printListOnlineATPs atps = putStrLn $ intercalate "\n\n" $ map show atps

