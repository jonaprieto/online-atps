{-# LANGUAGE UnicodeSyntax #-}

module OnlineATPs
  ( getOnlineATPs
  , getSystemATP
  , onlineATPOk
  , printListOnlineATPs
  , getResponseSystemOnTPTP
  ) where

----------------------------------------------------------------------

import  OnlineATPs.Consult
  ( getOnlineATPs
  , getSystemATP
  , getResponseSystemOnTPTP
  )
import  OnlineATPs.Defaults  (defaultOnlineATP)
import  OnlineATPs.SystemATP
  ( onlineATPOk
  , printListOnlineATPs
  )
