module Server
  ( Client (..),
    Message (..),
    Server (..),
    handleConnection,
    newServer,
  )
where

import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM (STM, TBQueue, TVar)
import qualified Control.Concurrent.STM as STM
import Control.Exception (finally)
import Control.Monad (forever, join, when)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Network.WebSockets as WS
import Numeric.Natural (Natural)

newtype Server = Server
  { serverClients :: TVar (Map Text Client)
  }

data Client = Client
  { clientConnection :: WS.Connection,
    clientName :: Text,
    clientSendQueue :: TBQueue Message
  }

data Message
  = Hi
  | Inbound Text

handleConnection :: Server -> WS.Connection -> IO ()
handleConnection server@Server {serverClients} conn =
  WS.withPingThread conn pingMillis onPing $ do
    name <- WS.receiveData conn

    maybeClient <-
      STM.atomically $ do
        clients <- STM.readTVar serverClients
        if Map.member name clients
          then pure Nothing
          else do
            client <- newClient conn name
            STM.writeTVar serverClients $ Map.insert name client clients
            pure $ Just client

    case maybeClient of
      Nothing ->
        let byebye :: Text
            byebye = "name already in use"
         in WS.sendClose conn byebye
      Just client@Client {clientName} ->
        handleClient client `finally` removeClient server clientName
  where
    onPing :: IO ()
    onPing = pure ()

    pingMillis :: Int
    pingMillis = 30

handleClient :: Client -> IO ()
handleClient client@Client {clientConnection, clientSendQueue} = do
  _ <- Async.race receive serve
  pure ()
  where
    receive :: IO ()
    receive = forever $ do
      msg <- WS.receiveData clientConnection
      STM.atomically $ sendMessage client $ Inbound msg

    serve :: IO ()
    serve = join $ STM.atomically $ do
      msg <- STM.readTBQueue clientSendQueue
      pure $ do
        continue <- handleMessage client msg
        when continue serve

handleMessage :: Client -> Message -> IO Bool
handleMessage Client {clientConnection} message =
  case message of
    Hi ->
      tellClient "hiiii~"
    Inbound "byebye" ->
      pure False
    Inbound msg ->
      tellClient $ "you said: \"" <> msg <> "\""
  where
    tellClient :: Text -> IO Bool
    tellClient msg = do
      _ <- WS.sendTextData clientConnection msg
      pure True

removeClient :: Server -> Text -> IO ()
removeClient Server {serverClients} clientName = STM.atomically $ do
  STM.modifyTVar' serverClients $ Map.delete clientName

newServer :: IO Server
newServer = do
  clients <- STM.newTVarIO Map.empty
  pure Server {serverClients = clients}

clientSendQueueCapacity :: Natural
clientSendQueueCapacity = 128

newClient :: WS.Connection -> Text -> STM Client
newClient conn name = do
  sendQueue <- STM.newTBQueue clientSendQueueCapacity
  pure
    Client
      { clientConnection = conn,
        clientName = name,
        clientSendQueue = sendQueue
      }

sendMessage :: Client -> Message -> STM ()
sendMessage Client {clientSendQueue} =
  STM.writeTBQueue clientSendQueue
