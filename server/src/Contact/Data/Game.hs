module Data.Game
  (
  )
where

import Data.Map (Map)
import Data.Text (Text)

data Contact = Contact2
  { contactGuessingPlayer :: Text,
    contactGuessingWord :: Text,
    contactHintingPlayer :: Text,
    contactHintingWord :: Text
  }

data Game = Game
  { gameContact :: Maybe Contact,
    gamePlayers :: Map Text Player
  }