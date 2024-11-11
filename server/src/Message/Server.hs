{-# LANGUAGE DuplicateRecordFields #-}

module Message.Server
  ( DeclaredContactMessage (..),
    JoinedGameMessage (..),
    LeftGameMessage (..),
    ServerMessage (..),
    SharedHintMessage (..),
    SyncGameMessage (..),
    SyncGamePlayer (..),
  )
where

import Data.Aeson
  ( Options (constructorTagModifier, sumEncoding),
    SumEncoding (contentsFieldName),
    ToJSON (toJSON),
  )
import qualified Data.Aeson as Aeson
import Data.Aeson.Casing (camelCase)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

data ServerMessage
  = DeclaredContact DeclaredContactMessage
  | JoinedGame JoinedGameMessage
  | LeftGame LeftGameMessage
  | RevealedContact
  | SharedHint SharedHintMessage
  | SyncGame SyncGameMessage
  deriving (Generic, Show)

instance ToJSON ServerMessage where
  toJSON =
    Aeson.genericToJSON $
      Aeson.defaultOptions
        { constructorTagModifier = camelCase,
          sumEncoding = sumEncodingOptions
        }
    where
      sumEncodingOptions :: SumEncoding
      sumEncodingOptions =
        Aeson.defaultTaggedObject {contentsFieldName = "data"}

data DeclaredContactMessage = DeclaredContactMessage
  { fromPlayer :: Text,
    toPlayer :: Text
  }
  deriving (Generic, Show)

instance ToJSON DeclaredContactMessage

newtype LeftGameMessage = LeftGameMessage
  { playerName :: Text
  }
  deriving (Generic, Show)

instance ToJSON LeftGameMessage

newtype JoinedGameMessage = JoinedGameMessage
  { playerName :: Text
  }
  deriving (Generic, Show)

instance ToJSON JoinedGameMessage

data SharedHintMessage = SharedHintMessage
  { description :: Text,
    player :: Text
  }
  deriving (Generic, Show)

instance ToJSON SharedHintMessage

data SyncGamePlayer = SyncGamePlayer
  { name :: Text,
    message :: Text
  }
  deriving (Generic, Show)

instance ToJSON SyncGamePlayer

data SyncGameMessage = SyncGameMessage
  { myPlayerName :: Text,
    players :: Map Text SyncGamePlayer
  }
  deriving (Generic, Show)

instance ToJSON SyncGameMessage
