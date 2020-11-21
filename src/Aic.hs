module Aic where

import Network.Wai


type Handler = (String, Request -> IO Response)



doAic :: String
doAic = "AIC"
