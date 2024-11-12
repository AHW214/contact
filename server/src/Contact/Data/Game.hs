module Contact.Data.Game
  ( Contact (..),
    Game (..),
    addPlayer,
    getPlayers,
    hasPlayer,
    newGame,
    removePlayer,
    updatePlayer,
  )
where

import Contact.Data.Player (Player (Player, playerName))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

data Contact = Contact
  { contactGuessingPlayer :: Text,
    contactGuessingWord :: Text,
    contactHintingPlayer :: Text,
    contactHintingWord :: Text
  }

data Game = Game
  { gameContact :: Maybe Contact,
    gamePlayers :: Map Text Player
  }

hasPlayer :: Game -> Text -> Bool
hasPlayer Game {gamePlayers} name =
  Map.member name gamePlayers

getPlayers :: Game -> Map Text Player
getPlayers = gamePlayers

addPlayer :: Game -> Player -> Game
addPlayer game@Game {gamePlayers} player@Player {playerName} =
  game {gamePlayers = Map.insert playerName player gamePlayers}

updatePlayer :: Game -> Player -> (Player -> Player) -> Game
updatePlayer game@Game {gamePlayers} Player {playerName} withPlayer =
  game {gamePlayers = Map.adjust withPlayer playerName gamePlayers}

removePlayer :: Game -> Player -> Game
removePlayer game@Game {gamePlayers} Player {playerName} =
  game {gamePlayers = Map.delete playerName gamePlayers}

newGame :: Game
newGame =
  Game
    { gameContact = Nothing,
      gamePlayers = Map.empty
    }
