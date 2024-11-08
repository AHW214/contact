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
import qualified Control.Concurrent.Async as Async
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
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import GHC.Generics (Generic)
import Message.Client
import Message.Server
import qualified Network.Wai as Wai
import qualified Network.WebSockets as WS
import Numeric.Natural (Natural)
import Web.Scotty (get, scottyApp)
import qualified Web.Scotty as Scotty

data Server = Server
  { serverBroadcastChanIn :: TChan ServerMessage,
    serverClients :: TVar (Map Text Client),
    serverLobby :: TVar (Map UUID WS.Connection)
  }

data Client = Client
  { -- TODO - different message type just for broadcasts
    -- TODO - !! broadcast chan is unbounded !!
    clientBroadcastChanOut :: TChan ServerMessage,
    clientConnection :: WS.Connection,
    clientName :: Text,
    clientSendQueue :: TBQueue Message
  }

data Message
  = Broadcast ServerMessage
  | Inbound ClientMessage
  | Sync

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

mkWsApp :: Server -> WS.ServerApp
mkWsApp server pendingConnection = do
  conn <- WS.acceptRequest pendingConnection
  handleConnection server conn

handleConnection :: Server -> WS.Connection -> IO ()
handleConnection server@Server {serverBroadcastChanIn, serverClients, serverLobby} conn =
  WS.withPingThread conn pingMillis onPing $ do
    sessionId <- UUID.nextRandom
    STM.atomically $ STM.modifyTVar serverLobby (Map.insert sessionId conn)

    broadcastChannelOut <- STM.atomically $ STM.dupTChan serverBroadcastChanIn
    result <- Async.race (broadcast broadcastChannelOut) waitForPlayerName

    case result of
      Left _ ->
        removeFromLobby server sessionId
      Right client@Client {clientName} -> do
        -- TODO - group with other STM computations in waitForPlayerName ?
        removeFromLobby server sessionId

        STM.atomically $
          STM.writeTChan serverBroadcastChanIn $
            JoinedGame JoinedGameMessage {playerName = clientName}

        handleClient server client `finally` removeClient server clientName
  where
    waitForPlayerName :: IO Client
    waitForPlayerName = do
      msg <- WS.receiveData conn
      case Aeson.eitherDecodeStrict msg of
        Left err -> do
          putStrLn $ "could not decode client message: " <> err
          print msg
          waitForPlayerName
        Right (ChooseName ChooseNameMessage {name}) -> do
          join $ STM.atomically $ do
            clients <- STM.readTVar serverClients
            if Map.member name clients
              then pure waitForPlayerName
              else do
                client <- newClient serverBroadcastChanIn conn name
                STM.writeTVar serverClients $ Map.insert name client clients
                pure $ pure client

    broadcast :: TChan ServerMessage -> IO ()
    broadcast broadcastChannelOut = forever $ do
      -- TODO - use internal type for message? then convert before sending
      msg <- STM.atomically $ STM.readTChan broadcastChannelOut
      case msg of
        LeftGame _ ->
          WS.sendTextData conn $ Aeson.encode msg
        JoinedGame _ ->
          WS.sendTextData conn $ Aeson.encode msg
        _ ->
          pure ()

    onPing :: IO ()
    onPing = pure ()

    pingMillis :: Int
    pingMillis = 30

handleClient :: Server -> Client -> IO ()
handleClient server client@Client {clientBroadcastChanOut, clientSendQueue} = do
  STM.atomically $ sendMessage client Sync
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
        continue <- handleMessage server client msg
        when continue serve

    broadcast :: IO ()
    broadcast = forever $
      STM.atomically $ do
        msg <- STM.readTChan clientBroadcastChanOut
        sendMessage client $ Broadcast msg

handleMessage :: Server -> Client -> Message -> IO Bool
handleMessage Server {serverBroadcastChanIn, serverClients} Client {clientConnection, clientName} message =
  case message of
    Broadcast _msg -> do
      -- WS.sendTextData clientConnection $ Aeson.encode msg
      pure True
    Inbound msg -> do
      putStrLn $ "received message: " <> show msg

      let msgOut =
            case msg of
              Contact (ContactMessage {playerId}) ->
                DeclareContact (DeclareContactMessage {fromPlayerId = clientName, toPlayerId = playerId})
              Hint (HintMessage {description}) ->
                ShareHint (ShareHintMessage {description, playerId = clientName})

      STM.atomically $ STM.writeTChan serverBroadcastChanIn msgOut
      pure True
    Sync -> do
      clients <- STM.atomically $ STM.readTVar serverClients

      WS.sendTextData clientConnection $
        Aeson.encode $
          SyncGame SyncGameMessage {myPlayerName = clientName, players = Map.keys clients}

      pure True

removeFromLobby :: Server -> UUID -> IO ()
removeFromLobby Server {serverLobby} sessionId = STM.atomically $ do
  STM.modifyTVar' serverLobby $ Map.delete sessionId

removeClient :: Server -> Text -> IO ()
removeClient Server {serverBroadcastChanIn, serverClients} clientName = STM.atomically $ do
  STM.modifyTVar' serverClients $ Map.delete clientName
  STM.writeTChan serverBroadcastChanIn $ LeftGame LeftGameMessage {playerName = clientName}

newServer :: TChan ServerMessage -> IO Server
newServer broadcastChanIn = do
  clients <- STM.newTVarIO Map.empty
  lobby <- STM.newTVarIO Map.empty

  pure
    Server
      { serverBroadcastChanIn = broadcastChanIn,
        serverClients = clients,
        serverLobby = lobby
      }

clientSendQueueCapacity :: Natural
clientSendQueueCapacity = 128

newClient :: TChan ServerMessage -> WS.Connection -> Text -> STM Client
newClient broadcastChanIn conn name = do
  broadCastChanOut <- STM.dupTChan broadcastChanIn
  sendQueue <- STM.newTBQueue clientSendQueueCapacity
  pure
    Client
      { clientBroadcastChanOut = broadCastChanOut,
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
