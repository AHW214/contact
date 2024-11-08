"use client";

import {
  type MutableRefObject,
  type Ref,
  useEffect,
  useReducer,
  useRef,
} from "react";

import Input from "contact/app/components/input";
import PlayerView from "contact/app/components/player";
import WordDisplay, {
  type TargetWord,
} from "contact/app/components/word-display";
import Wordmaster from "contact/app/components/wordmaster";
import type { Player, PlayerId } from "contact/app/data/player";
import type { ClientMessage, ServerMessage } from "contact/app/network/message";
import * as Record from "contact/app/util/record";

type Action =
  | { tag: "contact"; player: { id: PlayerId; name: string } }
  | { tag: "hinting" }
  | { tag: "thinking" };

type Model = {
  countdown: number | undefined;
  currentAction: Action;
  currentInput: string;
  myPlayerName: PlayerId;
  players: Record<PlayerId, Player>;
};

type Msg =
  | { tag: "changedInput"; value: string }
  | { tag: "clickedCancel" }
  | { tag: "clickedContact"; player: { id: PlayerId; name: string } }
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

const handleServerMessage = (model: Model, msg: ServerMessage): Model => {
  switch (msg.tag) {
    case "sharedHint": {
      return {
        ...model,
        players: Record.update(model.players, [msg.data.player], (player) => {
          return {
            ...player,
            hintState: {
              tag: "sharing",
              word: msg.data.description,
            },
          };
        }),
      };
    }

    case "declaredContact": {
      return {
        ...model,
        countdown: 5000,
        players: Record.update(
          model.players,
          [msg.data.fromPlayer, msg.data.toPlayer],
          (player): Player => {
            return {
              ...player,
              contactState: { tag: "declared" },
            };
          }
        ),
      };
    }

    case "revealedContact": {
      const contactStatus = msg.data.success ? "succeeded" : "failed";

      return {
        ...model,
        players: Record.updateWithData(
          model.players,
          [
            [msg.data.from.player, msg.data.from.word],
            [msg.data.to.player, msg.data.to.word],
          ],
          (player, word): Player => {
            return {
              ...player,
              contactState: { tag: contactStatus, word },
            };
          }
        ),
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

    case "receivedServerMessage":
      return handleServerMessage(model, msg.message);

    default:
      return model;
  }
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
    countdown: undefined,
    currentAction: { tag: "thinking" },
    currentInput: "",
    myPlayerName,
    players,
  };

  const [model, dispatch] = useReducer(update, initModel);

  useEffect(() => {
    if (lastServerMessage !== undefined) {
      dispatch({ tag: "receivedServerMessage", message: lastServerMessage });
    }
  }, [lastServerMessage]);

  useEffect(() => {
    const onKeyup = (ev: KeyboardEvent) => {
      if (ev.key === "Escape") {
        dispatch({ tag: "clickedEscape" });
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
        dispatch({ tag: "tickCountdown", millis: 1000 });
      }, 1000);
    } else {
      cleanup();
    }

    return cleanup;
  }, [model.countdown === undefined]);

  const { [myPlayerName]: myPlayer, ...restPlayers } = model.players;

  const contactingPlayers = Object.values(players)
    .filter(({ contactState }) => contactState !== undefined)
    .slice(0, 2);

  const isAnyoneContacting = contactingPlayers.length === 2;

  if (myPlayer === undefined) {
    return <div>no player???</div>;
  }

  return (
    <div className="flex flex-col gap-8 items-center">
      <WordDisplay target={MOCK_TARGET_WORD} />
      <Wordmaster id="0" name={MOCK_WORDMASTER} />
      <div className="flex gap-2">
        {Object.values(players).map((player) => (
          <PlayerView
            key={player.id}
            inputRef={inputRef}
            countdownMillis={model.countdown}
            onClickCancel={() => dispatch({ tag: "clickedCancel" })}
            onClickContact={() =>
              dispatch({
                tag: "clickedContact",
                player,
              })
            }
            {...player}
          />
        ))}
      </div>
      <div className="flex flex-col gap-1">
        <h3 className="text-zinc-400 text-sm">
          {isAnyoneContacting
            ? `${contactingPlayers[0].name} and ${contactingPlayers[1].name} are about to contact`
            : model.currentInput === ""
            ? "words, words, words..."
            : model.currentAction.tag === "contact"
            ? `press enter to contact with ${model.currentAction.player.name}`
            : model.currentAction.tag === "hinting"
            ? "press escape to stop sharing your hint"
            : "press enter to share your hint with everyone"}
        </h3>
        <Input
          className={`${
            model.currentAction.tag === "hinting" && !isAnyoneContacting
              ? "font-bold caret-transparent border-zinc-800"
              : "font-normal caret-inherit border-inherit"
          } ${
            myPlayer.contactState === undefined
              ? "border-zinc-300"
              : myPlayer.contactState.tag === "declared"
              ? "border-blue-800"
              : myPlayer.contactState.tag === "failed"
              ? "border-red-800"
              : "border-green-800"
          } ${isAnyoneContacting ? "cursor-not-allowed" : "cursor-auto"}`}
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
                  player: model.currentAction.player.id,
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
          placeholder={
            isAnyoneContacting
              ? "...suspense..."
              : model.currentAction.tag === "contact"
              ? "type your guess here..."
              : "type your hint here..."
          }
          value={isAnyoneContacting ? "" : model.currentInput}
          disabled={isAnyoneContacting}
        />
      </div>
    </div>
  );
}
