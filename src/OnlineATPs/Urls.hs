
-- | URL addresses of the TPTP website.

{-# LANGUAGE UnicodeSyntax #-}

module OnlineATPs.Urls
  ( urlSystemOnTPTP
  , urlSystemOnTPTPReply
  ) where


-- | This is the url address to send a form with the parameters
-- and the problem to TPTP World.
urlSystemOnTPTP ∷ String
urlSystemOnTPTP = "http://www.tptp.org/cgi-bin/SystemOnTPTP"


-- | This is the url address that serves the response of a form sended
-- before to the web form specified by the url address 'urlSystemOnTPTP'.
urlSystemOnTPTPReply ∷ String
urlSystemOnTPTPReply = "http://www.tptp.org/cgi-bin/SystemOnTPTPFormReply"
