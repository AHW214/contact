module Main (main) where

import qualified Control.Concurrent.STM as STM
import Data.Function ((&))
import Network.Wai.Handler.Warp (Port, setBeforeMainLoop, setPort)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS
import Server (mkHttpApp, mkWsApp, newServer)

main :: IO ()
main = do
  broadcastChanIn <- STM.newBroadcastTChanIO
  server <- newServer broadcastChanIn

  let wsApp = mkWsApp server
  httpApp <- mkHttpApp server

  Warp.runSettings warpSettings $
    websocketsOr WS.defaultConnectionOptions wsApp httpApp
  where
    warpSettings :: Warp.Settings
    warpSettings =
      Warp.defaultSettings
        & setPort serverPort
        & setBeforeMainLoop (putStrLn $ "listening on port " <> show serverPort)

    serverPort :: Port
    serverPort = 1234
