"use client";

import {
  type MutableRefObject,
  type Ref,
  useEffect,
  useReducer,
  useRef,
} from "react";

import PlayerInput from "contact/app/components/player-input";
import PlayerView, { type ContactState } from "contact/app/components/player";
import WordDisplay, {
  type TargetWord,
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
        guessingWord: string;
        hintingWord: string;
        success: boolean;
      }
    | undefined;
};

type Model = {
  contact: Contact | undefined;
  countdown: number | undefined;
  currentAction: PlayerAction;
  currentInput: string;
  myPlayerName: PlayerId;
  players: Record<PlayerId, Player>;
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
  sendServer: (msg: ClientMessage) => void;
};

const COUNTDOWN_TIME_MILLIS = 5000;
const COUNTDOWN_TICK_MILLIS = 1000;

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

    case "declaredContact": {
      const { fromPlayer, toPlayer } = msg.data;

      return {
        ...model,
        contact: {
          guessingPlayer: fromPlayer,
          hintingPlayer: toPlayer,
          result: undefined,
        },
        countdown: COUNTDOWN_TIME_MILLIS,
      };
    }

    case "endContact": {
      if (model.contact === undefined) {
        // TODO - handle error case
        return model;
      }

      const { guessingPlayer, hintingPlayer } = model.contact;
      const contactingPlayers = [guessingPlayer, hintingPlayer];

      const wasMyPlayerContacting = contactingPlayers.includes(
        model.myPlayerName
      );

      const { currentAction, currentInput } = wasMyPlayerContacting
        ? { currentAction: { tag: "thinking" as const }, currentInput: "" }
        : model;

      return {
        ...model,
        contact: undefined,
        currentAction,
        currentInput,
        players: Record.updateMany(
          model.players,
          contactingPlayers,
          (player) => {
            return {
              ...player,
              hintState: { tag: "thinking" },
            };
          }
        ),
      };
    }

    case "revealedContact": {
      if (model.contact === undefined) {
        // TODO - handle error case
        return model;
      }

      const { guessingPlayer, hintingPlayer } = model.contact;

      // TODO - can remove above Player fields from message
      // TODO - make message field names consistent
      const {
        guessedWord: guessingWord,
        hintedWord: hintingWord,
        success,
      } = msg.data;

      return {
        ...model,
        contact: {
          ...model.contact,
          result: { guessingWord, hintingWord, success },
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
    case "changedInput":
      return { ...model, currentInput: msg.value };

    case "clickedCancel":
      return { ...model, currentAction: { tag: "thinking" }, currentInput: "" };

    case "clickedContact":
      return {
        ...model,
        currentAction: { tag: "contact", player: msg.player },
      };

    case "clickedEscape":
      return model.currentAction.tag === "hinting"
        ? {
            ...model,
            currentAction: { tag: "thinking" },
            currentInput: "",
          }
        : model;

    case "sharedHint":
      return {
        ...model,
        currentAction: { tag: "hinting" },
      };

    case "tickCountdown":
      if (model.countdown !== undefined) {
        const newCountdown = model.countdown - msg.millis;

        return {
          ...model,
          countdown: newCountdown > 0 ? newCountdown : undefined,
        };
      }

      return model;

    case "receivedServerMessage": {
      return handleServerMessage(model, msg.message);
    }

    default:
      return model;
  }
};

const playerContactState = (
  model: Model,
  player: PlayerId
): ContactState | undefined => {
  if (model.contact === undefined) {
    return undefined;
  }

  const { guessingPlayer, hintingPlayer, result } = model.contact;

  const isGuessing = player === guessingPlayer;
  const isHinting = player === hintingPlayer;
  const isContacting = isGuessing || isHinting;

  if (!isContacting) {
    return undefined;
  }

  if (result === undefined) {
    return {
      tag: "declared",
    };
  }

  const { guessingWord, hintingWord, success } = result;

  return {
    tag: "revealed",
    success,
    word: isGuessing ? guessingWord : hintingWord,
  };
};

export default function Game({
  lastServerMessage,
  myPlayerName,
  players,
  sendServer,
}: GameProps) {
  const MOCK_TARGET_WORD: TargetWord = {
    status: "guessing",
    word: "evang",
  };

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

  const isAnyoneContacting = model.contact !== undefined;

  if (myPlayer === undefined) {
    return <div>no player???</div>;
  }

  return (
    <div className="flex flex-col gap-8 items-center">
      <WordDisplay target={MOCK_TARGET_WORD} />
      <Wordmaster id="0" name={MOCK_WORDMASTER} />
      <div className="flex gap-2">
        {Object.values(restPlayers).map((player) => (
          <PlayerView
            countdownMillis={model.countdown}
            inputRef={inputRef}
            isTyping={player.isTyping}
            key={player.name}
            name={player.name}
            onClickCancel={() => dispatch({ tag: "clickedCancel" })}
            onClickContact={() =>
              dispatch({
                tag: "clickedContact",
                player: player.name,
              })
            }
            state={playerContactState(model, player.name) ?? player.hintState}
          />
        ))}
      </div>
      <div className="flex flex-col gap-1">
        <h3 className="text-zinc-400 text-sm">
          {model.contact !== undefined
            ? model.contact.result !== undefined &&
              model.countdown === undefined
              ? model.contact.result.success
                ? "success!"
                : "failure..."
              : `${model.contact.guessingPlayer} and ${model.contact.hintingPlayer} are about to contact`
            : model.currentInput === ""
            ? "words, words, words..."
            : model.currentAction.tag === "contact"
            ? `press enter to contact with ${model.currentAction.player}`
            : model.currentAction.tag === "hinting"
            ? "press escape to stop sharing your hint"
            : "press enter to share your hint with everyone"}
        </h3>
        <PlayerInput
          contactState={playerContactState(model, model.myPlayerName)}
          currentAction={model.currentAction}
          hideContactResult={
            model.countdown !== undefined && model.countdown > 0
          }
          isAnyoneContacting={isAnyoneContacting}
          ref={inputRef}
          onChange={(ev) => {
            if (model.currentAction.tag !== "hinting") {
              dispatch({ tag: "changedInput", value: ev.target.value });
            }
          }}
          onEnter={() => {
            if (model.currentAction.tag === "contact") {
              sendServer({
                tag: "contact",
                data: {
                  player: model.currentAction.player,
                  word: model.currentInput,
                },
              });
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
        />
      </div>
    </div>
  );
}
