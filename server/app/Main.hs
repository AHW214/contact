module Main (main) where

import qualified Control.Concurrent.STM as STM
import qualified Network.WebSockets as WS
import Server (handleConnection, newServer)

main :: IO ()
main = do
  server <- newServer
  broadcastChannelIn <- STM.newBroadcastTChanIO

  putStrLn $ "listening on port " <> show serverPort

  WS.runServer serverUrl serverPort $ \pendingConnection -> do
    conn <- WS.acceptRequest pendingConnection
    handleConnection server broadcastChannelIn conn
  where
    serverUrl :: String
    serverUrl = "127.0.0.1"

    serverPort :: Int
    serverPort = 1234
