module Message.Client
  ( ClientMessage (..),
    ContactMessage (..),
    HintMessage (..),
  )
where

import Data.Aeson
  ( FromJSON (parseJSON),
    Options (constructorTagModifier, sumEncoding),
    SumEncoding (contentsFieldName),
  )
import qualified Data.Aeson as Aeson
import Data.Aeson.Casing (camelCase)
import Data.Text (Text)
import GHC.Generics (Generic)

data ClientMessage
  = Contact ContactMessage
  | Hint HintMessage
  deriving (Generic, Show)

instance FromJSON ClientMessage where
  parseJSON =
    Aeson.genericParseJSON $
      Aeson.defaultOptions
        { constructorTagModifier = camelCase,
          sumEncoding = sumEncodingOptions
        }
    where
      sumEncodingOptions :: SumEncoding
      sumEncodingOptions =
        Aeson.defaultTaggedObject {contentsFieldName = "data"}

data ContactMessage = ContactMessage
  { playerId :: Text,
    word :: Text
  }
  deriving (Generic, Show)

instance FromJSON ContactMessage

newtype HintMessage = HintMessage
  { description :: Text
  }
  deriving (Generic, Show)

instance FromJSON HintMessage
