module Main (main) where

import qualified Network.WebSockets as WS
import Server (handleConnection, newServer)

main :: IO ()
main = do
  server <- newServer

  putStrLn $ "listening on port " <> show serverPort

  WS.runServer serverUrl serverPort $ \pendingConnection -> do
    conn <- WS.acceptRequest pendingConnection
    handleConnection server conn
  where
    serverUrl :: String
    serverUrl = "127.0.0.1"

    serverPort :: Int
    serverPort = 1234
