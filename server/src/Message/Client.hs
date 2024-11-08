{-# LANGUAGE FlexibleContexts #-}

module Message.Client
  ( ChooseNameMessage (..),
    ClientMessage (..),
    ContactMessage (..),
    HintMessage (..),
    LobbyClientMessage (..),
  )
where

import Data.Aeson
  ( FromJSON (parseJSON),
    GFromJSON,
    Options (constructorTagModifier, sumEncoding, tagSingleConstructors),
    SumEncoding (contentsFieldName),
    Value,
    Zero,
  )
import qualified Data.Aeson as Aeson
import Data.Aeson.Casing (camelCase)
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import GHC.Generics (Generic, Rep)

data ClientMessage
  = Contact ContactMessage
  | Hint HintMessage
  deriving (Generic, Show)

instance FromJSON ClientMessage where
  parseJSON = parseTaggedJSON

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

newtype LobbyClientMessage
  = ChooseName ChooseNameMessage
  deriving (Generic, Show)

instance FromJSON LobbyClientMessage where
  parseJSON = parseTaggedJSON

newtype ChooseNameMessage = ChooseNameMessage
  { name :: Text
  }
  deriving (Generic, Show)

instance FromJSON ChooseNameMessage

parseTaggedJSON :: (Generic a, GFromJSON Zero (Rep a)) => Value -> Parser a
parseTaggedJSON =
  Aeson.genericParseJSON $
    Aeson.defaultOptions
      { constructorTagModifier = camelCase,
        sumEncoding = sumEncodingOptions,
        tagSingleConstructors = True
      }
  where
    sumEncodingOptions :: SumEncoding
    sumEncodingOptions =
      Aeson.defaultTaggedObject {contentsFieldName = "data"}
