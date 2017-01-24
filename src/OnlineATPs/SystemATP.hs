
-- | SystemOnATP data type.

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


-- | The 'SystemATP' data type handle all information about one ATP.
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


-- | A error message when the ATP specified by the user is not correct.
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


-- | Prints out the ATP jointly with its version in a standard format.
getNameVersion ∷ SystemATP → String
getNameVersion atp = sysName atp ++ "---" ++ sysVersion atp


-- | The function 'getDataSystemATP' returns a list of tuples providing
-- the information need to send a request to TPTP World.
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

-- | Check for ATP with First-Order Formulation capability.
isFOFATP ∷ SystemATP → Bool
isFOFATP NoSystemATP = False
isFOFATP atp         = isInfixOf "FOF" $ sysApplication atp

-- | The method 'onlineATPVersion' outputs only the number or the string
-- from the version of the ATP.
onlineATPVersion ∷ SystemATP → String
onlineATPVersion NoSystemATP = error msgErrorNoSystemATP
onlineATPVersion atp         = sysVersion atp

-- | The function 'printListOnlineATPs' prints out a list with all ATPs
-- available at the moment of the request.
printListOnlineATPs ∷ [SystemATP] → IO ()
printListOnlineATPs atps = do
  putStr $ showListLn atps
  putStrLn $ "(" ++ show (length atps) ++ ") ATPs available"

-- | Set up the time limit to get a response in the TPTP World.
setTimeLimit ∷ SystemATP → String → SystemATP
setTimeLimit NoSystemATP _ = NoSystemATP
setTimeLimit atp time = atp { sysTimeLimit = time }
