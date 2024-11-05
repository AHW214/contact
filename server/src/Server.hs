module Server
  ( someFunc,
  )
where

import Control.Concurrent.STM (STM, TBQueue, TVar)
import qualified Control.Concurrent.STM as STM
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Network.WebSockets as WS
import Numeric.Natural (Natural)

newtype Server = Server
  { clients :: TVar (Map Text Client)
  }

data Client = Client
  { clientConnection :: WS.Connection,
    clientName :: Text,
    clientSendQueue :: TBQueue Message
  }

data Message
  = Hi
  | Bye

newServer :: IO Server
newServer = do
  clients <- STM.newTVarIO Map.empty
  pure Server {clients}

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

someFunc :: IO ()
someFunc = putStrLn "someFunc"
