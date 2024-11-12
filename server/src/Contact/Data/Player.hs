module Contact.Data.Player
  ( Event (..),
    Player (..),
    dispatchEvent,
    newPlayer,
    receiveBroadcast,
    receiveEvent,
    receiveWebSocket,
    sendWebSocket,
  )
where

import Contact.Message.Client (ClientMessage)
import Contact.Message.Server (ServerMessage)
import Control.Concurrent.STM (STM, TBQueue, TChan)
import qualified Control.Concurrent.STM as STM
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Network.WebSockets as WS
import Numeric.Natural (Natural)

data Event
  = Broadcast ServerMessage
  | Inbound ClientMessage
  | Sync

data Player = Player
  { -- TODO - different message type just for broadcasts
    -- TODO - !! broadcast chan is unbounded !!
    playerBroadcastChanOut :: TChan ServerMessage,
    playerConnection :: WS.Connection,
    playerMessage :: Text,
    playerName :: Text,
    playerSendQueue :: TBQueue Event
  }

sendWebSocket :: Player -> ServerMessage -> IO ()
sendWebSocket Player {playerConnection} =
  WS.sendTextData playerConnection . Aeson.encode

receiveWebSocket :: Player -> IO ByteString
receiveWebSocket =
  WS.receiveData . playerConnection

receiveBroadcast :: Player -> STM ServerMessage
receiveBroadcast =
  STM.readTChan . playerBroadcastChanOut

receiveEvent :: Player -> STM Event
receiveEvent =
  STM.readTBQueue . playerSendQueue

dispatchEvent :: Player -> Event -> STM ()
dispatchEvent =
  STM.writeTBQueue . playerSendQueue

newPlayer :: TChan ServerMessage -> WS.Connection -> Text -> STM Player
newPlayer broadcastChanIn conn name = do
  broadCastChanOut <- STM.dupTChan broadcastChanIn
  sendQueue <- STM.newTBQueue sendQueueCapacity
  pure
    Player
      { playerBroadcastChanOut = broadCastChanOut,
        playerConnection = conn,
        playerMessage = "",
        playerName = name,
        playerSendQueue = sendQueue
      }
  where
    sendQueueCapacity :: Natural
    sendQueueCapacity = 128
