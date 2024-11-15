module Contact.Data.Game
  ( Contact (..),
    Game (..),
    addPlayer,
    clearContact,
    getPlayers,
    hasPlayer,
    newGame,
    removePlayer,
    setContact,
    updatePlayer,
  )
where

import Contact.Data.Player (Player (Player, playerName))
import qualified Contact.Data.Player as Player
import Data.Foldable (foldl')
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

clearContact :: Game -> Game
clearContact game@Game {gameContact, gamePlayers} =
  case gameContact of
    Nothing ->
      game
    Just contact ->
      let contactingPlayers = getContactingPlayers contact
       in Game
            { gameContact = Nothing,
              gamePlayers = adjustMany Player.clearMessage contactingPlayers gamePlayers
            }
  where
    adjustMany :: (Ord k, Foldable t) => (a -> a) -> t k -> Map k a -> Map k a
    adjustMany f keys mp = foldl' (flip $ Map.adjust f) mp keys

setContact :: Game -> Contact -> Game
setContact game contact =
  game {gameContact = Just contact}

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

getContactingPlayers :: Contact -> (Text, Text)
getContactingPlayers Contact {contactGuessingPlayer, contactHintingPlayer} =
  (contactGuessingPlayer, contactHintingPlayer)
