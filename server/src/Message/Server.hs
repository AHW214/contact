{-# LANGUAGE DuplicateRecordFields #-}

module Message.Server
  ( DeclaredContactMessage (..),
    JoinedGameMessage (..),
    LeftGameMessage (..),
    ServerMessage (..),
    SharedHintMessage (..),
    SyncGameMessage (..),
  )
where

import Data.Aeson
  ( Options (constructorTagModifier, sumEncoding),
    SumEncoding (contentsFieldName),
    ToJSON (toJSON),
  )
import qualified Data.Aeson as Aeson
import Data.Aeson.Casing (camelCase)
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

data SyncGameMessage = SyncGameMessage
  { myPlayerName :: Text,
    players :: [Text]
  }
  deriving (Generic, Show)

instance ToJSON SyncGameMessage
