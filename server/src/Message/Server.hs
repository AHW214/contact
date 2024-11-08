{-# LANGUAGE DuplicateRecordFields #-}

module Message.Server
  ( DeclareContactMessage (..),
    JoinedGameMessage (..),
    LeftGameMessage (..),
    ServerMessage (..),
    ShareHintMessage (..),
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
  = DeclareContact DeclareContactMessage
  | JoinedGame JoinedGameMessage
  | LeftGame LeftGameMessage
  | RevealContact
  | ShareHint ShareHintMessage
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

data DeclareContactMessage = DeclareContactMessage
  { fromPlayerId :: Text,
    toPlayerId :: Text
  }
  deriving (Generic, Show)

instance ToJSON DeclareContactMessage

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

data ShareHintMessage = ShareHintMessage
  { description :: Text,
    playerId :: Text
  }
  deriving (Generic, Show)

instance ToJSON ShareHintMessage
