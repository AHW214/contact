{-# LANGUAGE DuplicateRecordFields #-}

module Contact.Message.Server
  ( ClearedHintMessage (..),
    DeclaredContactMessage (..),
    JoinedGameMessage (..),
    LeftGameMessage (..),
    RevealedContactMessage (..),
    RevealedContactResult (..),
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
  = ClearedHint ClearedHintMessage
  | ConfirmedContact
  | DeclaredContact DeclaredContactMessage
  | EndContact
  | JoinedGame JoinedGameMessage
  | LeftGame LeftGameMessage
  | RevealedContact RevealedContactMessage
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

newtype ClearedHintMessage = ClearedHintMessage
  { playerName :: Text
  }
  deriving (Generic, Show)

instance ToJSON ClearedHintMessage

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

data RevealedContactMessage = RevealedContactMessage
  { guessedWord :: Maybe Text,
    guessingPlayer :: Text,
    hintedWord :: Maybe Text,
    hintingPlayer :: Text,
    maybeResult :: Maybe RevealedContactResult
  }
  deriving (Generic, Show)

instance ToJSON RevealedContactMessage

data RevealedContactResult = RevealedContactResult
  { isGameOver :: Bool,
    revealedLetter :: Char
  }
  deriving (Generic, Show)

instance ToJSON RevealedContactResult

data SharedHintMessage = SharedHintMessage
  { description :: Text,
    player :: Text
  }
  deriving (Generic, Show)

instance ToJSON SharedHintMessage

data SyncGameMessage = SyncGameMessage
  { myPlayerName :: Text,
    players :: Map Text SyncGamePlayer
  }
  deriving (Generic, Show)

instance ToJSON SyncGameMessage

data SyncGamePlayer = SyncGamePlayer
  { name :: Text,
    message :: Text
  }
  deriving (Generic, Show)

instance ToJSON SyncGamePlayer
