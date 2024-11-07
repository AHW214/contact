{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module Server
  ( Client (..),
    Message (..),
    Server (..),
    mkHttpApp,
    mkWsApp,
    newServer,
  )
where

import Control.Concurrent.Async (race_)
import Control.Concurrent.STM (STM, TBQueue, TChan, TVar)
import qualified Control.Concurrent.STM as STM
import Control.Exception (finally)
import Control.Monad (forever, join, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Message.Client (ClientMessage (..), ContactMessage (..), HintMessage (..))
import Message.Server (DeclareContactMessage (..), ServerMessage (..), ShareHintMessage (..))
import qualified Network.Wai as Wai
import qualified Network.WebSockets as WS
import Numeric.Natural (Natural)
import Web.Scotty (get, scottyApp)
import qualified Web.Scotty as Scotty

newtype Server = Server
  { serverClients :: TVar (Map Text Client)
  }

data Client = Client
  { -- TODO - different message type just for broadcasts
    -- TODO - !! broadcast chan is unbounded !!
    clientBroadcastChanIn :: TChan ServerMessage,
    clientBroadcastChanOut :: TChan ServerMessage,
    clientConnection :: WS.Connection,
    clientName :: Text,
    clientSendQueue :: TBQueue Message
  }

data Message
  = Broadcast ServerMessage
  | Hi
  | Inbound ClientMessage

newtype RoomResponse = RoomResponse
  { players :: [Text]
  }
  deriving (Generic)

instance ToJSON RoomResponse

mkHttpApp :: Server -> IO Wai.Application
mkHttpApp Server {serverClients} = scottyApp $ do
  get "/room/:roomId" $ do
    -- TODO - use when rooms are implemented server-side
    -- roomId <- Scotty.queryParam "roomId"

    players <- liftIO $ STM.atomically $ do
      clients <- STM.readTVar serverClients
      pure $ Map.keys clients

    Scotty.json $ RoomResponse {players}

mkWsApp :: Server -> TChan ServerMessage -> WS.ServerApp
mkWsApp server broadcastChannelIn pendingConnection = do
  conn <- WS.acceptRequest pendingConnection
  handleConnection server broadcastChannelIn conn

handleConnection :: Server -> TChan ServerMessage -> WS.Connection -> IO ()
handleConnection server@Server {serverClients} broadcastChannelIn conn =
  WS.withPingThread conn pingMillis onPing $ do
    name <- Text.strip <$> WS.receiveData conn

    maybeClient <-
      STM.atomically $ do
        clients <- STM.readTVar serverClients
        if Map.member name clients
          then pure Nothing
          else do
            client <- newClient broadcastChannelIn conn name
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
handleClient client@Client {clientBroadcastChanOut, clientSendQueue} = do
  STM.atomically $ sendMessage client Hi
  -- TODO: racing multiple threads this way seems jank
  receive `race_` serve `race_` broadcast
  pure ()
  where
    receive :: IO ()
    receive = forever $ do
      msg <- receiveMessage client
      case Aeson.eitherDecodeStrict msg of
        Left err ->
          putStrLn $ "could not decode client message: " <> err
        Right clientMessage ->
          STM.atomically $ sendMessage client $ Inbound clientMessage

    serve :: IO ()
    serve = join $ STM.atomically $ do
      msg <- STM.readTBQueue clientSendQueue
      pure $ do
        continue <- handleMessage client msg
        when continue serve

    broadcast :: IO ()
    broadcast = forever $
      STM.atomically $ do
        msg <- STM.readTChan clientBroadcastChanOut
        sendMessage client $ Broadcast msg

handleMessage :: Client -> Message -> IO Bool
handleMessage Client {clientBroadcastChanIn, clientConnection, clientName} message =
  case message of
    Broadcast msg -> do
      WS.sendTextData clientConnection $ Aeson.encode msg
      pure True
    Hi -> do
      putStrLn $ "hi hi " <> Text.unpack clientName <> "! welcome to the server :^)"
      pure True
    Inbound msg -> do
      putStrLn $ "received message: " <> show msg

      let msgOut =
            case msg of
              Contact (ContactMessage {playerId}) ->
                DeclareContact (DeclareContactMessage {fromPlayerId = clientName, toPlayerId = playerId})
              Hint (HintMessage {description}) ->
                ShareHint (ShareHintMessage {description, playerId = clientName})

      STM.atomically $ STM.writeTChan clientBroadcastChanIn msgOut
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

newClient :: TChan ServerMessage -> WS.Connection -> Text -> STM Client
newClient broadcastChanIn conn name = do
  broadCastChanOut <- STM.dupTChan broadcastChanIn
  sendQueue <- STM.newTBQueue clientSendQueueCapacity
  pure
    Client
      { clientBroadcastChanIn = broadcastChanIn,
        clientBroadcastChanOut = broadCastChanOut,
        clientConnection = conn,
        clientName = name,
        clientSendQueue = sendQueue
      }

receiveMessage :: Client -> IO ByteString
receiveMessage Client {clientConnection} =
  WS.receiveData clientConnection

sendMessage :: Client -> Message -> STM ()
sendMessage Client {clientSendQueue} =
  STM.writeTBQueue clientSendQueue
