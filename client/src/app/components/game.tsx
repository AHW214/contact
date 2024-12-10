"use client";

import {
  type MutableRefObject,
  type Ref,
  useEffect,
  useReducer,
  useRef,
} from "react";

import PlayerInput from "contact/app/components/player-input";
import PlayerView, { type PlayerState } from "contact/app/components/player";
import WordDisplay, {
  type SecretWord,
  updateSecretWord,
} from "contact/app/components/word-display";
import Wordmaster from "contact/app/components/wordmaster";
import type { Player, PlayerAction, PlayerId } from "contact/app/data/player";
import type { ClientMessage, ServerMessage } from "contact/app/network/message";
import * as Record from "contact/app/util/record";

type Contact = {
  guessingPlayer: PlayerId;
  hintingPlayer: PlayerId;
  result:
    | {
        guessingWord: string | undefined;
        hintingWord: string | undefined;
        revealedLetter: string | undefined;
      }
    | undefined;
};

type Model = {
  contact: Contact | undefined;
  // TODO - merge model.countdown into model.contact
  countdown: number | undefined;
  currentAction: PlayerAction;
  currentInput: string;
  myPlayerName: PlayerId;
  players: Record<PlayerId, Player>;
  secretWord: SecretWord;
};

type Msg =
  | { tag: "changedInput"; value: string }
  | { tag: "clickedCancel" }
  | { tag: "clickedContact"; player: PlayerId }
  | { tag: "clickedEscape" }
  | { tag: "sharedHint" }
  | { tag: "tickCountdown"; millis: number }
  | { tag: "receivedServerMessage"; message: ServerMessage };

type GameProps = {
  lastServerMessage: ServerMessage | undefined;
  myPlayerName: PlayerId;
  // TODO - currently duplicated between Room and Game components
  players: Record<PlayerId, Player>;
  secretWordRevealed: string;
  sendServer: (msg: ClientMessage) => void;
};

const COUNTDOWN_TIME_MILLIS = 5000;
const COUNTDOWN_TICK_MILLIS = 1000;

const isPlayerContacting = (contact: Contact, player: PlayerId): boolean => {
  const { guessingPlayer, hintingPlayer } = contact;
  return [guessingPlayer, hintingPlayer].includes(player);
};

const handleServerMessage = (model: Model, msg: ServerMessage): Model => {
  switch (msg.tag) {
    case "clearedHint": {
      const { playerName } = msg.data;

      return {
        ...model,
        players: Record.update(model.players, playerName, (player) => {
          return {
            ...player,
            hintState: {
              tag: "thinking",
            },
          };
        }),
      };
    }

    case "sharedHint": {
      const { description, player } = msg.data;

      return {
        ...model,
        players: Record.update(model.players, player, (player) => {
          return {
            ...player,
            hintState: {
              tag: "sharing",
              word: description,
            },
          };
        }),
      };
    }

    case "confirmedContact": {
      return {
        ...model,
        currentAction: { tag: "contact", confirmed: true },
      };
    }

    case "declaredContact": {
      const { fromPlayer, toPlayer } = msg.data;

      const contact = {
        guessingPlayer: fromPlayer,
        hintingPlayer: toPlayer,
        result: undefined,
      };

      const isMyPlayerContacting = isPlayerContacting(
        contact,
        model.myPlayerName
      );

      const { currentAction, currentInput } = isMyPlayerContacting
        ? {
            currentAction: { tag: "contact" as const, confirmed: false },
            currentInput: "",
          }
        : model;

      return {
        ...model,
        contact,
        countdown: COUNTDOWN_TIME_MILLIS,
        currentAction,
        currentInput,
      };
    }

    case "endContact": {
      if (model.contact === undefined || model.contact.result === undefined) {
        // TODO - handle error case
        return model;
      }

      const { contact, myPlayerName } = model;
      const wasMyPlayerContacting = isPlayerContacting(contact, myPlayerName);

      const { currentAction, currentInput } = wasMyPlayerContacting
        ? { currentAction: { tag: "thinking" as const }, currentInput: "" }
        : model;

      const { revealedLetter } = model.contact.result;

      const secretWord =
        revealedLetter !== undefined
          ? updateSecretWord(model.secretWord, revealedLetter)
          : model.secretWord;

      const { guessingPlayer, hintingPlayer } = contact;

      return {
        ...model,
        contact: undefined,
        currentAction,
        currentInput,
        players: Record.updateMany(
          model.players,
          [guessingPlayer, hintingPlayer],
          (player) => {
            return {
              ...player,
              hintState: { tag: "thinking" },
            };
          }
        ),
        secretWord,
      };
    }

    case "revealedContact": {
      if (model.contact === undefined) {
        // TODO - handle error case
        return model;
      }

      const { guessingPlayer, hintingPlayer } = model.contact;

      // TODO - can remove above Player fields from model
      // TODO - make message field names consistent (e.g. "guessed" -> "guessing")
      const { guessedWord, hintedWord, maybeResult } = msg.data;

      return {
        ...model,
        contact: {
          ...model.contact,
          result: {
            guessingWord: guessedWord ?? undefined,
            hintingWord: hintedWord ?? undefined,
            revealedLetter: maybeResult?.revealedLetter ?? undefined,
          },
        },
      };
    }

    case "joinedGame": {
      const { playerName } = msg.data;

      const MOCK_PLAYER: Player = {
        hintState: { tag: "thinking" },
        isTyping: false,
        name: playerName,
      };

      return {
        ...model,
        players: { ...model.players, [playerName]: MOCK_PLAYER },
      };
    }

    case "leftGame": {
      const { playerName } = msg.data;
      const { [playerName]: _, ...restPlayers } = model.players;

      return {
        ...model,
        players: restPlayers,
      };
    }

    default: {
      console.log(`useReducer(): ignoring message '${msg.tag}'`);
      return model;
    }
  }
};

const update = (model: Model, msg: Msg): Model => {
  switch (msg.tag) {
    case "changedInput": {
      const { currentAction, secretWord } = model;

      switch (currentAction.tag) {
        case "contact": {
          const guessingWord = msg.value;

          const canPlayerStillGuess = !currentAction.confirmed;

          // TODO - seems silly / redundant / bad
          const isGuessAllowed =
            guessingWord.length <= secretWord.word.length
              ? secretWord.word.startsWith(guessingWord)
              : guessingWord.startsWith(secretWord.word);

          return canPlayerStillGuess && isGuessAllowed
            ? { ...model, currentInput: guessingWord }
            : model;
        }

        case "hinting": {
          return model;
        }

        case "thinking":
        default: {
          return { ...model, currentInput: msg.value };
        }
      }
    }

    case "clickedCancel": {
      return { ...model, currentAction: { tag: "thinking" }, currentInput: "" };
    }

    case "clickedContact": {
      return model;
    }

    case "clickedEscape": {
      return model.currentAction.tag === "hinting"
        ? {
            ...model,
            currentAction: { tag: "thinking" },
            currentInput: "",
          }
        : model;
    }

    case "sharedHint": {
      return {
        ...model,
        currentAction: { tag: "hinting" },
      };
    }

    case "tickCountdown": {
      if (model.countdown !== undefined) {
        const newCountdown = model.countdown - msg.millis;

        return {
          ...model,
          countdown: newCountdown > 0 ? newCountdown : undefined,
        };
      }

      return model;
    }

    case "receivedServerMessage": {
      return handleServerMessage(model, msg.message);
    }

    default: {
      return model;
    }
  }
};

const playerContactState = (model: Model, player: Player): PlayerState => {
  if (model.contact === undefined) {
    return { tag: "hintingWord", hint: player.hintState };
  }

  const { guessingPlayer, hintingPlayer, result } = model.contact;

  const isGuessing = player.name === guessingPlayer;
  const isHinting = player.name === hintingPlayer;
  const isContacting = isGuessing || isHinting;

  if (!isContacting) {
    return { tag: "spectatingContact" };
  }

  if (result === undefined) {
    return {
      tag: "performingContact",
      contact: { tag: "declared" },
    };
  }

  const { guessingWord, hintingWord, revealedLetter } = result;

  return {
    tag: "performingContact",
    contact: {
      tag: "revealed",
      success: revealedLetter !== undefined,
      word: isGuessing ? guessingWord : hintingWord,
    },
  };
};

const inputHeaderText = (model: Model): string => {
  const { contact, countdown, currentAction, myPlayerName, secretWord } = model;

  if (contact !== undefined) {
    const { guessingPlayer, hintingPlayer, result } = contact;

    const isResultUnknown = result === undefined;
    const isResultHidden = countdown !== undefined && countdown > 0;

    if (isResultUnknown || isResultHidden) {
      if (isPlayerContacting(contact, myPlayerName)) {
        const otherPlayer =
          myPlayerName === guessingPlayer ? hintingPlayer : guessingPlayer;

        const isContactConfirmed =
          currentAction.tag === "contact" && currentAction.confirmed;

        return isContactConfirmed
          ? `you are about to contact with ${otherPlayer}`
          : model.currentInput === ""
          ? `guess ${otherPlayer}'s word!`
          : // TODO - dont want this (have ghost characters that fill in instead)
          secretWord.status === "guessing" &&
            !model.currentInput.startsWith(model.secretWord.word)
          ? "BAD BAD BAD"
          : `press enter to send your guess`;
      }

      return `${guessingPlayer} and ${hintingPlayer} are about to contact`;
    }

    const isContactSuccessful = result.revealedLetter !== undefined;
    return isContactSuccessful ? "success!" : "failure...";
  }

  switch (currentAction.tag) {
    case "hinting": {
      return "press escape to stop sharing your hint";
    }
    case "thinking": {
      return model.currentInput === ""
        ? "words, words, words..."
        : "press enter to share your hint with everyone";
    }
    default: {
      // IMPOSSIBLE CASE
      return "";
    }
  }
};

export default function Game({
  lastServerMessage,
  myPlayerName,
  players,
  secretWordRevealed,
  sendServer,
}: GameProps) {
  const MOCK_WORDMASTER = "Shinji";

  const inputRef: Ref<HTMLInputElement> = useRef(null);
  const intervalRef: MutableRefObject<number | undefined> = useRef(undefined);

  const initModel: Model = {
    contact: undefined,
    countdown: undefined,
    currentAction: { tag: "thinking" },
    currentInput: "",
    myPlayerName,
    players,
    secretWord: { status: "guessing", word: secretWordRevealed },
  };

  const [model, dispatch] = useReducer(update, initModel);

  useEffect(() => {
    if (lastServerMessage !== undefined) {
      console.log(
        `GAME: dispatching server message: ${JSON.stringify(lastServerMessage)}`
      );
      dispatch({
        tag: "receivedServerMessage",
        message: lastServerMessage,
      });
    }
  }, [lastServerMessage]);

  useEffect(() => {
    const onKeyup = (ev: KeyboardEvent) => {
      if (ev.key === "Escape") {
        dispatch({ tag: "clickedEscape" });

        sendServer({
          tag: "clearHint",
        });
      }
    };

    document.addEventListener("keyup", onKeyup);

    return () => document.removeEventListener("keyup", onKeyup);
  }, []);

  useEffect(() => {
    const cleanup = () => {
      if (intervalRef.current !== null && intervalRef.current !== undefined) {
        clearInterval(intervalRef.current);
        intervalRef.current = undefined;
      }
    };

    if (model.countdown !== undefined) {
      if (intervalRef.current !== undefined) {
        clearInterval(intervalRef.current);
      }

      intervalRef.current = window.setInterval(() => {
        dispatch({ tag: "tickCountdown", millis: COUNTDOWN_TICK_MILLIS });
      }, 1000);
    } else {
      cleanup();
    }

    return cleanup;
  }, [model.countdown === undefined]);

  useEffect(() => {
    const onBeforeUnload = () => {
      sendServer({ tag: "disconnect" });
    };

    window.addEventListener("beforeunload", onBeforeUnload);

    return () => {
      window.removeEventListener("beforeunload", onBeforeUnload);
    };
  }, []);

  const { [myPlayerName]: myPlayer, ...restPlayers } = model.players;

  if (myPlayer === undefined) {
    return <div>no player???</div>;
  }

  return (
    <div className="flex flex-col gap-8 items-center">
      <WordDisplay secretWord={model.secretWord} />
      <Wordmaster id="0" name={MOCK_WORDMASTER} />
      <div className="flex gap-2">
        {Object.values(restPlayers).map((player) => (
          <PlayerView
            countdownMillis={model.countdown}
            isTyping={player.isTyping}
            key={player.name}
            name={player.name}
            onClickContact={() => {
              dispatch({
                tag: "clickedContact",
                player: player.name,
              });

              inputRef.current?.focus();
              inputRef.current?.select();

              sendServer({
                tag: "declareContact",
                data: { player: player.name },
              });
            }}
            state={playerContactState(model, player)}
          />
        ))}
      </div>
      <div className="flex flex-col gap-1">
        <h3 className="text-zinc-400 text-sm">{inputHeaderText(model)}</h3>
        <PlayerInput
          currentAction={model.currentAction}
          hideContactResult={
            model.countdown !== undefined && model.countdown > 0
          }
          ref={inputRef}
          secretWordPrefix={model.secretWord.word}
          state={playerContactState(model, myPlayer)}
          onChange={(ev) => {
            dispatch({ tag: "changedInput", value: ev.target.value });
          }}
          onEnter={() => {
            if (model.currentAction.tag === "contact") {
              if (model.currentInput.startsWith(model.secretWord.word)) {
                sendServer({
                  tag: "confirmContact",
                  data: {
                    maybeWord: model.currentInput || null,
                  },
                });
              }
            } else if (model.currentAction.tag === "thinking") {
              dispatch({ tag: "sharedHint" });

              sendServer({
                tag: "hint",
                data: {
                  description: model.currentInput,
                },
              });
            }
          }}
          value={model.currentInput}
          misclick={false}
        />
      </div>
    </div>
  );
}
