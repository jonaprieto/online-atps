
-- | This module exports the main functions of the package.

{-# LANGUAGE UnicodeSyntax #-}

module OnlineATPs
  ( getOnlineATPs
  , getSystemATP
  , onlineATPOk
  , printListOnlineATPs
  , getResponseSystemOnTPTP
  ) where


import OnlineATPs.Consult
  ( getOnlineATPs
  , getSystemATP
  , getResponseSystemOnTPTP
  )
import OnlineATPs.Defaults  ( defaultSystemATP )
import OnlineATPs.SystemATP
  ( onlineATPOk
  , printListOnlineATPs
  )
