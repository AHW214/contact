module Contact.Data.Game
  ( Contact (..),
    ContactResult (..),
    Game (..),
    addPlayer,
    checkContact,
    clearContact,
    getPlayers,
    hasPlayer,
    isGameOver,
    newGame,
    removePlayer,
    revealSecretLetter,
    setContact,
    updateContact,
    updatePlayer,
  )
where

import Contact.Data.Player (Player (Player, playerName))
import qualified Contact.Data.Player as Player
import Data.Foldable (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text

data Game = Game
  { gameContact :: Maybe Contact,
    gamePlayers :: Map Text Player,
    gameSecretWord :: Text,
    gameSecretWordRevealed :: Text
  }

data Contact = Contact
  { contactGuessingPlayer :: Text,
    contactGuessingWord :: Maybe Text,
    contactHintingPlayer :: Text,
    contactHintingWord :: Maybe Text
  }

data ContactResult
  = Contacted Game Char
  | Missed

checkContact :: Game -> Maybe (Contact, ContactResult)
checkContact game@Game {gameContact} =
  case gameContact of
    Nothing -> Nothing
    Just contact@Contact {contactGuessingWord, contactHintingWord} ->
      let result =
            case (contactGuessingWord, contactHintingWord) of
              (Just guessingWord, Just hintingWord)
                | guessingWord == hintingWord ->
                    let (game', nextLetter) = revealSecretLetter game
                     in Contacted game' nextLetter
              _ -> Missed
       in Just (contact, result)

clearContact :: Game -> Game
clearContact game@Game {gameContact, gamePlayers} =
  case gameContact of
    Nothing ->
      game
    Just contact ->
      let contactingPlayers = getContactingPlayers contact
       in game
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

revealSecretLetter :: Game -> (Game, Char)
revealSecretLetter game@Game {gameSecretWord, gameSecretWordRevealed} =
  (game {gameSecretWordRevealed = secretWordRevealed}, letterRevealed)
  where
    letterRevealed :: Char
    letterRevealed =
      -- TODO - feels jank
      -- TODO - maybe use NonEmpty Char instead of Text ?
      maybe ' ' snd $ Text.unsnoc secretWordRevealed

    secretWordRevealed :: Text
    secretWordRevealed =
      Text.take (1 + Text.length gameSecretWordRevealed) gameSecretWord

isGameOver :: Game -> Bool
isGameOver Game {gameSecretWord, gameSecretWordRevealed} =
  gameSecretWordRevealed == gameSecretWord

newGame :: Game
newGame =
  Game
    { gameContact = Nothing,
      gamePlayers = Map.empty,
      gameSecretWord = "evangelion",
      gameSecretWordRevealed = "evang"
    }

getContactingPlayers :: Contact -> (Text, Text)
getContactingPlayers Contact {contactGuessingPlayer, contactHintingPlayer} =
  (contactGuessingPlayer, contactHintingPlayer)

updateContact :: Game -> Player -> Maybe Text -> Game
updateContact game@Game {gameContact} player maybeWord =
  let update contact = updateContactWord contact player maybeWord
   in game {gameContact = update <$> gameContact}

updateContactWord :: Contact -> Player -> Maybe Text -> Contact
updateContactWord contact player maybeWord
  | isPlayerGuessing = contact {contactGuessingWord = maybeWord}
  | isPlayerHinting = contact {contactHintingWord = maybeWord}
  | otherwise = contact
  where
    isPlayerHinting =
      playerName player == contactHintingPlayer contact

    isPlayerGuessing =
      playerName player == contactGuessingPlayer contact
