{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module Contact.Server
  ( Player (..),
    Server (..),
    mkHttpApp,
    mkWsApp,
    newServer,
  )
where

import Contact.Data.Game
  ( Contact
      ( contactGuessingPlayer,
        contactGuessingWord,
        contactHintingPlayer,
        contactHintingWord
      ),
    Game (..),
    newGame,
  )
import qualified Contact.Data.Game as Game
import Contact.Data.Player (Event (..), Player (..), newPlayer)
import qualified Contact.Data.Player as Player
import Contact.Message.Client
import qualified Contact.Message.Client as Client
import Contact.Message.Server
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Concurrently (..), async)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM (STM, TChan, TVar)
import qualified Control.Concurrent.STM as STM
import Control.Exception (catch, finally, throwIO)
import Control.Monad (forever, join, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import qualified Data.Aeson as Aeson
import Data.Foldable (asum)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import GHC.Generics (Generic)
import qualified Network.Wai as Wai
import qualified Network.WebSockets as WS
import Web.Scotty (get, scottyApp)
import qualified Web.Scotty as Scotty

data Server = Server
  { serverBroadcastChanIn :: TChan ServerMessage,
    serverGame :: TVar Game,
    serverLobby :: TVar (Map UUID WS.Connection)
  }

data RoomResponse = RoomResponse
  { players :: [Text],
    secretWordRevealed :: Text
  }
  deriving (Generic)

instance ToJSON RoomResponse

mkHttpApp :: Server -> IO Wai.Application
mkHttpApp server = scottyApp $ do
  get "/room/:roomId" $ do
    -- TODO - use when rooms are implemented server-side
    -- roomId <- Scotty.queryParam "roomId"

    (players, secretWordRevealed) <- liftIO $ STM.atomically $ do
      Game {gamePlayers, gameSecretWordRevealed} <- readGame server
      pure (Map.keys gamePlayers, gameSecretWordRevealed)

    liftIO $ putStrLn $ "current players: " <> show players

    Scotty.json $ RoomResponse {players, secretWordRevealed}

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
          removeFromGame server player
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
            game <- readGame server
            if Game.hasPlayer game name
              then pure waitForPlayerName
              else do
                player <- newPlayer serverBroadcastChanIn conn name
                STM.writeTVar serverGame $ Game.addPlayer game player
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
handlePlayer server player@Player {playerName} = do
  STM.atomically $ Player.dispatchEvent player Sync
  raceAll [receive, serve, broadcast]
  where
    receive :: IO ()
    receive = forever $ do
      msg <- Player.receiveWebSocket player `catch` onDisconnect
      case Aeson.eitherDecodeStrict msg of
        Left err ->
          putStrLn $ "could not decode client message: " <> err
        Right clientMessage ->
          STM.atomically $ Player.dispatchEvent player $ Inbound clientMessage

    onDisconnect :: WS.ConnectionException -> IO a
    onDisconnect ex = do
      putStrLn $ "player " <> show playerName <> " disconnected: " <> show ex
      throwIO ex

    serve :: IO ()
    serve = join $ STM.atomically $ do
      msg <- Player.receiveEvent player
      pure $ do
        continue <- handleMessage server player msg
        when continue serve

    broadcast :: IO ()
    broadcast = forever $
      STM.atomically $ do
        msg <- Player.receiveBroadcast player
        Player.dispatchEvent player $ Broadcast msg

handleMessage :: Server -> Player -> Event -> IO Bool
handleMessage server@Server {serverGame} player@Player {playerName} message =
  case message of
    Broadcast msg -> do
      Player.sendWebSocket player msg
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
        ConfirmContact (ConfirmContactMessage {maybeWord}) -> do
          -- TODO - VERIFY WORD PREFIXED BY KNOWN LETTERS
          STM.atomically $
            modifyGame server $
              \game -> Game.updateContact game player maybeWord

          Player.sendWebSocket player ConfirmedContact
          pure True

        -- TODO - this branch v messy, can refactor
        Client.DeclareContact (DeclareContactMessage {player = hintingPlayer}) -> do
          STM.atomically $ do
            let contact =
                  Game.Contact
                    { contactGuessingPlayer = playerName,
                      contactGuessingWord = Nothing,
                      contactHintingPlayer = hintingPlayer,
                      contactHintingWord = Nothing
                    }

            modifyGame server $ \game -> Game.setContact game contact

            let msgOut =
                  DeclaredContact $
                    DeclaredContactMessage
                      { fromPlayer = playerName,
                        toPlayer = hintingPlayer
                      }

            broadcastMessage server msgOut

          -- TODO - make sure players have time to confirm on client side
          -- TODO - magic numbers
          runAfterDelay 4500 $ do
            STM.atomically $ do
              game@Game {gameContact} <- readGame server

              case gameContact of
                Nothing ->
                  -- TODO - error case
                  pure ()
                Just
                  Game.Contact
                    { contactGuessingPlayer,
                      contactGuessingWord,
                      contactHintingPlayer,
                      contactHintingWord
                    } -> do
                    -- TODO - CHECK FOR WIN CONDITIONS AND INFORM PLAYERS IF
                    -- GAME OVER
                    let result =
                          case (contactGuessingWord, contactHintingWord) of
                            (Just guessingWord, Just hintingWord)
                              | guessingWord == hintingWord ->
                                  Just $ Game.revealSecretLetter game
                            _ -> Nothing

                        msgOut =
                          RevealedContact $
                            RevealedContactMessage
                              { guessedWord = contactGuessingWord,
                                guessingPlayer = contactGuessingPlayer,
                                hintedWord = contactHintingWord,
                                hintingPlayer = contactHintingPlayer,
                                maybeRevealedLetter = fmap snd result
                              }

                    case result of
                      Just (game', _) ->
                        STM.writeTVar serverGame game'
                      Nothing ->
                        pure ()

                    broadcastMessage server msgOut

            -- TODO - magic numbers
            runAfterDelay 3000 $
              STM.atomically $ do
                modifyGame server Game.clearContact
                broadcastMessage server EndContact

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
      Game {gamePlayers} <- STM.atomically $ readGame server
      let syncGameMsg = messageFromGame gamePlayers playerName

      Player.sendWebSocket player $ SyncGame syncGameMsg
      pure True

broadcastMessage :: Server -> ServerMessage -> STM ()
broadcastMessage =
  STM.writeTChan . serverBroadcastChanIn

readGame :: Server -> STM Game
readGame =
  STM.readTVar . serverGame

modifyPlayer :: Server -> Player -> (Player -> Player) -> STM ()
modifyPlayer server player withPlayer =
  modifyGame server $ \game -> Game.updatePlayer game player withPlayer

modifyGame :: Server -> (Game -> Game) -> STM ()
modifyGame Server {serverGame} =
  STM.modifyTVar' serverGame

removeFromLobby :: Server -> UUID -> IO ()
removeFromLobby Server {serverLobby} sessionId = STM.atomically $ do
  STM.modifyTVar' serverLobby $ Map.delete sessionId

removeFromGame :: Server -> Player -> IO ()
removeFromGame
  Server {serverBroadcastChanIn, serverGame}
  player@Player {playerName} = STM.atomically $ do
    STM.modifyTVar' serverGame $ \game -> Game.removePlayer game player
    STM.writeTChan serverBroadcastChanIn $ LeftGame LeftGameMessage {playerName}

newServer :: TChan ServerMessage -> IO Server
newServer broadcastChanIn = do
  game <- STM.newTVarIO newGame
  lobby <- STM.newTVarIO Map.empty

  pure
    Server
      { serverBroadcastChanIn = broadcastChanIn,
        serverGame = game,
        serverLobby = lobby
      }

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

runAfterDelay :: Int -> IO () -> IO ()
runAfterDelay millis action =
  -- TODO - possible to write this using Async.withAsync ?
  void $ async $ do
    threadDelay $ 1000 * millis
    action

-- https://stackoverflow.com/a/66591096
raceAll :: [IO a] -> IO a
raceAll = runConcurrently . asum . fmap Concurrently
