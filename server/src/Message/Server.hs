module Message.Server
  ( DeclareContactMessage (..),
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

data ShareHintMessage = ShareHintMessage
  { description :: Text,
    playerId :: Text
  }
  deriving (Generic, Show)

instance ToJSON ShareHintMessage
