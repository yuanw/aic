{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as Bytes
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Network.HTTP.Types.Status as HTTP

type Handler = (String, Request -> IO Response)

server :: Application
server req respCC =
  let content = Bytes.fromStrict $ "You requested " <>
        requestMethod req <> " " <> rawPathInfo req
      headers = [("Content-Type", "text/plain")]
      response = responseLBS HTTP.ok200 headers content
  in respCC response

main :: IO ()
main = do
  putStrLn "starting server on port 8080..."
  run 8080 server
