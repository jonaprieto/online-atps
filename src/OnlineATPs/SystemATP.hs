
-- | SystemOnATP data type

{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}
{-# LANGUAGE OverloadedStrings                      #-}
{-# LANGUAGE RecordWildCards                        #-}
{-# LANGUAGE ScopedTypeVariables                    #-}
{-# LANGUAGE UnicodeSyntax                          #-}

module OnlineATPs.SystemATP
  ( getDataSystemATP
  , getNameVersion
  , isFOFATP
  , msgErrorNoSystemATP
  , onlineATPVersion
  , printListOnlineATPs
  , setTimeLimit
  , SystemATP
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
  ) where

import Control.Applicative   ( (<|>) )
import Data.List             ( intercalate, isInfixOf )

import OnlineATPs.Utils.Show ( showListLn )
import OnlineATPs.Utils.Yaml


-- | TODO
data SystemATP = NoSystemATP | SystemATP
  { sysApplication ∷ String
  , sysCommand     ∷ String
  , sysFormat      ∷ String
  , sysKey         ∷ String
  , sysName        ∷ String
  , sysTimeLimit   ∷ String
  , sysTransform   ∷ String
  , sysVersion     ∷ String
  }
  deriving Eq

instance FromJSON SystemATP where
  parseJSON = withObject "atp" $ \o → do
    sysApplication ← o .?. "Application" .!= ""
    sysCommand     ← o .?. "Command" .!= "default"
    sysFormat      ← o .?. "Format" .!= "tptp:raw"
    sysKey         ← o .:. "Key" <|> o .:. "ATP"
    sysName        ← o .?. "Name" .!= "default"
    sysTimeLimit   ← o .?. "TimeLimit" .!= "60"
    sysTransform   ← o .?. "Transform" .!= "none"
    sysVersion     ← o .?. "Version" .!= "default"
    return SystemATP{..}


-- | TODO
msgErrorNoSystemATP ∷ String
msgErrorNoSystemATP = "The system is not a valid ATP."

instance Show SystemATP where
  show NoSystemATP = msgErrorNoSystemATP
  show atp = intercalate "\n"
    [ "[" ++ sysName atp ++ "]"
    , "  application: " ++ sysApplication atp
    , "  key: " ++ sysKey atp
    , "  version: " ++ sysVersion atp
    , "\n"
    ]


-- | TODO
getNameVersion ∷ SystemATP → String
getNameVersion atp = sysName atp ++ "---" ++ sysVersion atp


-- | TODO
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

-- | TODO
isFOFATP ∷ SystemATP → Bool
isFOFATP NoSystemATP = False
isFOFATP atp         = isInfixOf "FOF" $ sysApplication atp

-- | TODO
onlineATPVersion ∷ SystemATP → String
onlineATPVersion NoSystemATP = error msgErrorNoSystemATP
onlineATPVersion atp         = sysVersion atp

-- | TODO
printListOnlineATPs ∷ [SystemATP] → IO ()
printListOnlineATPs atps = do
  putStr $ showListLn atps
  putStrLn $ "(" ++ show (length atps) ++ ") ATPs available"


-- | TODO
setTimeLimit ∷ SystemATP → String → SystemATP
setTimeLimit NoSystemATP _ = NoSystemATP
setTimeLimit atp time = atp { sysTimeLimit = time }
