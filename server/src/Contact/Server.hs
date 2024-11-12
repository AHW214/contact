{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module Contact.Server
  ( Message (..),
    Player (..),
    Server (..),
    mkHttpApp,
    mkWsApp,
    newServer,
  )
where

import Contact.Message.Client
import Contact.Message.Server
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_, withAsync)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM (STM, TBQueue, TChan, TVar)
import qualified Control.Concurrent.STM as STM
import Control.Exception (catch, finally, throwIO)
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
import qualified Network.Wai as Wai
import qualified Network.WebSockets as WS
import Numeric.Natural (Natural)
import Web.Scotty (get, scottyApp)
import qualified Web.Scotty as Scotty

data Player = Player
  { -- TODO - different message type just for broadcasts
    -- TODO - !! broadcast chan is unbounded !!
    playerBroadcastChanOut :: TChan ServerMessage,
    playerConnection :: WS.Connection,
    playerMessage :: Text,
    playerName :: Text,
    playerSendQueue :: TBQueue Message
  }

data Server = Server
  { serverBroadcastChanIn :: TChan ServerMessage,
    serverGame :: TVar (Map Text Player),
    serverLobby :: TVar (Map UUID WS.Connection)
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
mkHttpApp Server {serverGame} = scottyApp $ do
  get "/room/:roomId" $ do
    -- TODO - use when rooms are implemented server-side
    -- roomId <- Scotty.queryParam "roomId"

    players <- liftIO $ STM.atomically $ do
      game <- STM.readTVar serverGame
      pure $ Map.keys game

    liftIO $ putStrLn $ "current players: " <> show players

    Scotty.json $ RoomResponse {players}

mkWsApp :: Server -> WS.ServerApp
mkWsApp server pendingConnection = do
  conn <- WS.acceptRequest pendingConnection
  handleConnection server conn

handleConnection :: Server -> WS.Connection -> IO ()
handleConnection server@Server {serverBroadcastChanIn, serverGame, serverLobby} conn =
  WS.withPingThread conn pingMillis onPing $ do
    sessionId <- UUID.nextRandom
    STM.atomically $ STM.modifyTVar serverLobby (Map.insert sessionId conn)

    broadcastChannelOut <- STM.atomically $ STM.dupTChan serverBroadcastChanIn
    result <- Async.race (broadcast broadcastChannelOut) waitForPlayerName

    case result of
      Left _ ->
        removeFromLobby server sessionId
      Right player@Player {playerName} -> do
        -- TODO - group with other STM computations in waitForPlayerName ?
        removeFromLobby server sessionId

        -- TODO - this sends redundant message to client who has just now joined too
        STM.atomically $
          STM.writeTChan serverBroadcastChanIn $
            JoinedGame JoinedGameMessage {playerName}

        handlePlayer server player `finally` do
          putStrLn $ "removing player " <> show playerName
          removeFromGame server playerName
  where
    waitForPlayerName :: IO Player
    waitForPlayerName = do
      msg <- WS.receiveData conn
      case Aeson.eitherDecodeStrict msg of
        Left err -> do
          putStrLn $ "could not decode client message: " <> err
          print msg
          waitForPlayerName
        Right (ChooseName ChooseNameMessage {name}) -> do
          join $ STM.atomically $ do
            players <- STM.readTVar serverGame
            if Map.member name players
              then pure waitForPlayerName
              else do
                player <- newPlayer serverBroadcastChanIn conn name
                STM.writeTVar serverGame $ Map.insert name player players
                pure $ pure player

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

handlePlayer :: Server -> Player -> IO ()
handlePlayer server player@Player {playerBroadcastChanOut, playerName, playerSendQueue} = do
  STM.atomically $ sendMessage player Sync
  -- TODO: racing multiple threads this way seems jank
  receive `race_` serve `race_` broadcast
  pure ()
  where
    receive :: IO ()
    receive = forever $ do
      msg <- receiveMessage player `catch` onDisconnect
      case Aeson.eitherDecodeStrict msg of
        Left err ->
          putStrLn $ "could not decode client message: " <> err
        Right clientMessage ->
          STM.atomically $ sendMessage player $ Inbound clientMessage

    onDisconnect :: WS.ConnectionException -> IO a
    onDisconnect ex = do
      putStrLn $ "player " <> show playerName <> " disconnected: " <> show ex
      throwIO ex

    serve :: IO ()
    serve = join $ STM.atomically $ do
      msg <- STM.readTBQueue playerSendQueue
      pure $ do
        continue <- handleMessage server player msg
        when continue serve

    broadcast :: IO ()
    broadcast = forever $
      STM.atomically $ do
        msg <- STM.readTChan playerBroadcastChanOut
        sendMessage player $ Broadcast msg

handleMessage :: Server -> Player -> Message -> IO Bool
handleMessage server player@Player {playerName} message =
  case message of
    Broadcast msg -> do
      sendPlayerWS player msg
      pure True
    Inbound msg -> do
      putStrLn $ "received message: " <> show msg

      case msg of
        ClearHint -> do
          let msgOut = ClearedHint $ ClearedHintMessage {playerName}
          STM.atomically $ do
            modifyPlayer server player $ \p -> p {playerMessage = ""}
            broadcastMessage server msgOut
          pure True
        Contact (ContactMessage {player, word}) -> do
          let msgOut =
                DeclaredContact $
                  DeclaredContactMessage
                    { fromPlayer = playerName,
                      toPlayer = player
                    }

          STM.atomically $ broadcastMessage server msgOut

          withAsync (threadDelayMillis 1500) $ \async -> do
            Async.wait async

            STM.atomically $ do
              players <- readPlayers server
              let hintedWord =
                    case Map.lookup player players of
                      -- TODO
                      Nothing -> undefined
                      Just Player {playerMessage} -> playerMessage

                  success =
                    word == hintedWord

                  -- TODO - name
                  msgOut2 =
                    RevealedContact $
                      RevealedContactMessage
                        { guessedWord = word,
                          guessingPlayer = playerName,
                          hintedWord,
                          hintingPlayer = player,
                          success
                        }

              broadcastMessage server msgOut2

          pure True
        Disconnect -> do
          putStrLn $ "player " <> show playerName <> " disconnected"
          pure False
        Hint (HintMessage {description}) -> do
          let msgOut = SharedHint $ SharedHintMessage {description, player = playerName}
          STM.atomically $ do
            modifyPlayer server player $ \p -> p {playerMessage = description}
            broadcastMessage server msgOut
          pure True
    Sync -> do
      players <- STM.atomically $ readPlayers server
      let syncGameMsg = messageFromGame players playerName

      sendPlayerWS player $ SyncGame syncGameMsg
      pure True

broadcastMessage :: Server -> ServerMessage -> STM ()
broadcastMessage =
  STM.writeTChan . serverBroadcastChanIn

readPlayers :: Server -> STM (Map Text Player)
readPlayers Server {serverGame} =
  STM.readTVar serverGame

modifyPlayer :: Server -> Player -> (Player -> Player) -> STM ()
modifyPlayer Server {serverGame} Player {playerName} withPlayer =
  STM.modifyTVar' serverGame $ Map.adjust withPlayer playerName

removeFromLobby :: Server -> UUID -> IO ()
removeFromLobby Server {serverLobby} sessionId = STM.atomically $ do
  STM.modifyTVar' serverLobby $ Map.delete sessionId

removeFromGame :: Server -> Text -> IO ()
removeFromGame Server {serverBroadcastChanIn, serverGame} playerName = STM.atomically $ do
  STM.modifyTVar' serverGame $ Map.delete playerName
  STM.writeTChan serverBroadcastChanIn $ LeftGame LeftGameMessage {playerName}

newServer :: TChan ServerMessage -> IO Server
newServer broadcastChanIn = do
  game <- STM.newTVarIO Map.empty
  lobby <- STM.newTVarIO Map.empty

  pure
    Server
      { serverBroadcastChanIn = broadcastChanIn,
        serverGame = game,
        serverLobby = lobby
      }

playerSendQueueCapacity :: Natural
playerSendQueueCapacity = 128

newPlayer :: TChan ServerMessage -> WS.Connection -> Text -> STM Player
newPlayer broadcastChanIn conn name = do
  broadCastChanOut <- STM.dupTChan broadcastChanIn
  sendQueue <- STM.newTBQueue playerSendQueueCapacity
  pure
    Player
      { playerBroadcastChanOut = broadCastChanOut,
        playerConnection = conn,
        playerMessage = "",
        playerName = name,
        playerSendQueue = sendQueue
      }

sendPlayerWS :: Player -> ServerMessage -> IO ()
sendPlayerWS Player {playerConnection} =
  WS.sendTextData playerConnection . Aeson.encode

receiveMessage :: Player -> IO ByteString
receiveMessage =
  WS.receiveData . playerConnection

sendMessage :: Player -> Message -> STM ()
sendMessage =
  STM.writeTBQueue . playerSendQueue

messageFromGame :: Map Text Player -> Text -> SyncGameMessage
messageFromGame players myPlayerName =
  SyncGameMessage
    { myPlayerName,
      players = messageFromPlayer <$> players
    }

messageFromPlayer :: Player -> SyncGamePlayer
messageFromPlayer Player {playerName, playerMessage} =
  SyncGamePlayer
    { name = playerName,
      message = playerMessage
    }

threadDelayMillis :: Int -> IO ()
threadDelayMillis millis =
  threadDelay $ 1000 * millis
