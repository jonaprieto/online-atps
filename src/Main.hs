
-- | OnlineATPs: A program for proving first-order theorems written in the
-- | TPTP format using SystemOnTPTP

{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}


module Main
  ( main  -- Required by Haddock.
  ) where

import OnlineATPs.Consult
  ( getOnlineATPs
  , getResponseSystemOnTPTP
  , getSystemATP
  , getSystemATPWith
  , getSystemOnTPTP
  , Msg
  )

import OnlineATPs.CheckOutput (checkTheoremSync)
import OnlineATPs.Options
  ( Options
    ( optATP
    , optATPList
    , optHelp
    , optTime
    , optInputFile
    , optOnlyCheck
    , optVersion
    , optVersionATP
    )
  , printUsage
  , processOptions
  )

import OnlineATPs.SystemATP
  ( SystemATP (NoSystemATP, sysTimeLimit)
  , printListOnlineATPs
  , getNameVersion
  )


import           OnlineATPs.SystemOnTPTP    (SystemOnTPTP)

import           Control.Monad              (unless)
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as C
import           OnlineATPs.Utils.Monad     (die)
import           OnlineATPs.Utils.Version   (progNameVersion)
import           System.Directory           (doesFileExist)
import           System.Environment         (getArgs)
import           System.Exit                (exitFailure, exitSuccess)

main ∷ IO ()
main = do
  args ← getArgs
  opts ← case processOptions args of
    Left err → die err
    Right o  → return o

  if  | optHelp opts → printUsage >> exitSuccess

      | optVersion opts → do
        v ← progNameVersion
        putStrLn v  >> exitSuccess

      | optATPList opts → do
        atps ← getOnlineATPs opts
        printListOnlineATPs atps  >> exitSuccess

      | not (null $ optVersionATP opts) → do
          atps ← getOnlineATPs opts
          atp ∷ SystemATP  ← getSystemATP opts
          case atp of
            NoSystemATP → putStrLn "Unknown ATP name. Check --list-atps" >> exitFailure
            _           → putStrLn ( getNameVersion atp ) >> exitSuccess

      | otherwise → do

          file ← case optInputFile opts of
            Nothing → putStrLn "Missing input file (try --help)" >> exitFailure
            Just f  → return f

          isFile ← doesFileExist file
          unless isFile $ putStrLn "The file doesn't exist" >> exitFailure

          case optATP opts of
            [] → putStrLn "Missing --atp=NAME (try --help)" >> exitFailure
            o  → return o

          form ∷ Either Msg SystemOnTPTP ← getSystemOnTPTP opts

          case form of
            Left msg   → putStrLn msg >> exitFailure

            Right spec →
              if optOnlyCheck opts
                then do

                  answer ∷ String ← checkTheoremSync spec
                  putStrLn answer >> exitSuccess

                else do

                  response ∷ L.ByteString ← getResponseSystemOnTPTP spec
                  C.putStrLn response >> exitSuccess
