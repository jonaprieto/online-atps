
-- | OnlineATPs: A program for proving first-order theorems written in the
-- TPTP format using SystemOnTPTP

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
  , getSystemATPWith
  )

import OnlineATPs.Defaults    ( getDefaults )
import OnlineATPs.Options
  ( Options
    ( optATP
    , optHelp
    , optATPList
    , optTime
    , optVersion
    , optInputFile
    )
  , processOptions
  , printUsage
  )

import OnlineATPs.SystemATP
  ( SystemATP ( sysTimeLimit )
  , checkOnlineATPOutput
  , printListOnlineATPs
  )
import OnlineATPs.SystemOnTPTP
  ( SystemOnTPTP
  , setFORMULAEProblem
  , setSystems
  )

import           OnlineATPs.Utils.Monad   (die)
import           OnlineATPs.Utils.Version (progNameVersion)
import           System.Environment       (getArgs)
import           System.Exit              (exitFailure, exitSuccess)
import           System.IO                (readFile)

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
        atps ← getOnlineATPs
        printListOnlineATPs atps  >> exitSuccess

      | otherwise → do

          file ← case optInputFile opts of
            Nothing → putStrLn "missing input file (try --help)" >> exitFailure
            Just f  → return f

          requiredATPs ← case optATP opts of
            [] → putStrLn "missing --atp=NAME (try --help)" >> exitFailure
            o  → return o

          atps ← getOnlineATPs

          let listATPs ∷ [SystemATP]
              listATPs = map (getSystemATPWith atps) requiredATPs

          let time ∷ String
              time = show $ optTime opts

          let setATPs ∷ [SystemATP]
              setATPs = map (\p → p { sysTimeLimit = time }) listATPs

          defaults ∷ SystemOnTPTP ← getDefaults

          contentFile ∷ String ← readFile file

          -- putStrLn contentFile

          let formData ∷ SystemOnTPTP
              formData  = setFORMULAEProblem (setSystems defaults setATPs) contentFile

          -- putStrLn "Waiting for TPTP World ..."
          response ∷ String  ← getResponseSystemOnTPTP formData

          -- putStrLn response

          if any (`checkOnlineATPOutput` response) listATPs
            then putStrLn "Theorem" >> exitSuccess
            else putStrLn "No theorem" >> exitSuccess

          return ()
