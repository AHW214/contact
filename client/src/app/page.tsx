// TODO
"use client";

import { type Either, Left, Right } from "purify-ts";
import { Codec } from "purify-ts/Codec";
import * as C from "purify-ts/Codec";
import { type Ref, useEffect, useReducer, useRef, useState } from "react";
import useWebSocket, { ReadyState } from "react-use-websocket";

import Input from "contact/app/components/input";
// TODO - import naming conflict things
import PlayerView from "contact/app/components/player";
import WordDisplay, {
  type TargetWord,
} from "contact/app/components/word-display";
import Wordmaster from "contact/app/components/wordmaster";
import {
  type Player,
  type PlayerId,
  playerIdCodec,
} from "contact/app/data/player";
import * as Record from "contact/app/util/record";

type Action =
  | { tag: "contact"; player: { id: PlayerId; name: string } }
  | { tag: "hint" };

type Model = {
  currentAction: Action;
  currentInput: string;
  players: Record<PlayerId, Player>;
};

type Msg =
  | { tag: "changedInput"; value: string }
  | { tag: "clickedCancel" }
  | { tag: "clickedContact"; player: { id: PlayerId; name: string } }
  | { tag: "webSocketMessage"; data: unknown };

type InboundMessage =
  | { tag: "hint"; description: string; playerId: PlayerId }
  | { tag: "declareContact"; players: { fromId: PlayerId; toId: PlayerId } }
  | {
      // TODO - separate messages for failed and successful contacts?
      tag: "revealContact";
      players: {
        from: { id: PlayerId; word: string };
        to: { id: PlayerId; word: string };
      };
      success: boolean;
    };

type OutboundMessage =
  | { tag: "contact"; playerId: PlayerId; word: string }
  | { tag: "hint"; description: string };

type MockPlayerParams = {
  id: string;
  name: string;
};

const mockPlayer = ({ id, name }: MockPlayerParams): Player => ({
  contactState: undefined,
  hint: undefined,
  id: id as PlayerId,
  isTyping: false,
  name,
});

const mockPlayerMap = (names: string[]): Record<PlayerId, Player> => {
  const players = names.map((name, ix) => mockPlayer({ id: `${ix}`, name }));
  return Object.fromEntries(players.map((player) => [player.id, player]));
};

const MOCK_TARGET_WORD: TargetWord = { status: "guessing", word: "evange" };

const MOCK_WORDMASTER: string = "Shinji Ikari";

const MOCK_PLAYERS: Record<PlayerId, Player> = mockPlayerMap([
  "Bob",
  "Alice",
  "Gandalf",
]);

const MOCK_MODEL: Model = {
  currentAction: { tag: "hint" },
  currentInput: "",
  players: MOCK_PLAYERS,
};

// TODO - refactor duplicate codec parts
const inboundMessageCodec: Codec<InboundMessage> = C.oneOf([
  Codec.interface({
    tag: C.exactly("hint"),
    description: C.string,
    playerId: playerIdCodec,
  }),
  Codec.interface({
    tag: C.exactly("declareContact"),
    players: Codec.interface({ fromId: playerIdCodec, toId: playerIdCodec }),
  }),
  Codec.interface({
    tag: C.exactly("revealContact"),
    players: Codec.interface({
      from: Codec.interface({ id: playerIdCodec, word: C.string }),
      to: Codec.interface({ id: playerIdCodec, word: C.string }),
    }),
    success: C.boolean,
  }),
]);

const showWebSocketState = (readyState: ReadyState): string => {
  switch (readyState) {
    case ReadyState.CLOSED:
      return "closed";
    case ReadyState.CLOSING:
      return "closing";
    case ReadyState.CONNECTING:
      return "connecting";
    case ReadyState.OPEN:
      return "open";
    case ReadyState.UNINSTANTIATED:
      return "uninstantiated";
    default:
      return "unknown";
  }
};

const parseWebSocketData = (data: unknown): Either<string, InboundMessage> => {
  if (typeof data !== "string") {
    return Left("websocket data not string");
  }

  try {
    const dataJson = JSON.parse(data);
    return inboundMessageCodec.decode(dataJson);
  } catch (err) {
    return Left(`failed to parse websocket data: ${err}`);
  }
};

const update = (model: Model, msg: Msg): Model => {
  switch (msg.tag) {
    case "changedInput":
      return { ...model, currentInput: msg.value };

    case "clickedCancel":
      return { ...model, currentAction: { tag: "hint" }, currentInput: "" };

    case "clickedContact":
      return {
        ...model,
        currentAction: { tag: "contact", player: msg.player },
      };

    case "webSocketMessage":
      return parseWebSocketData(msg.data).caseOf({
        Left: (err) => {
          console.log(`bad websocket message: ${err}`);
          return model;
        },

        Right: (message) => {
          switch (message.tag) {
            case "hint":
              return {
                ...model,
                players: Record.update(
                  model.players,
                  [message.playerId],
                  (player) => {
                    return { ...player, hint: message.description };
                  }
                ),
              };

            case "declareContact":
              return {
                ...model,
                players: Record.update(
                  model.players,
                  [message.players.fromId, message.players.toId],
                  (player): Player => {
                    return { ...player, contactState: "declared" };
                  }
                ),
              };

            case "revealContact":
              const contactState = message.success ? "succeeded" : "failed";

              return {
                ...model,
                players: Record.updateWithData(
                  model.players,
                  [
                    [message.players.from.id, message.players.from.word],
                    [message.players.to.id, message.players.to.word],
                  ],
                  (player, word): Player => {
                    return { ...player, hint: word, contactState };
                  }
                ),
              };

            default:
              console.error(`useReducer(): UNKNOWN MESSAGE '${msg.tag}'`);
              return model;
          }
        },
      });

    default:
      return model;
  }
};

export default function Home() {
  const inputRef: Ref<HTMLInputElement> = useRef(null);

  const [model, dispatch] = useReducer(update, MOCK_MODEL);

  const WEB_SOCKET_URL = "ws://localhost:1234";

  // TODO: check readystate and display loading screen if not yet connected etc
  const { sendMessage, lastMessage, readyState } = useWebSocket(WEB_SOCKET_URL);

  const sendServer = (message: OutboundMessage): void =>
    sendMessage(JSON.stringify(message));

  useEffect(() => {
    if (lastMessage !== null) {
      console.log(`last message: ${lastMessage.data}`);
      dispatch({ tag: "webSocketMessage", data: lastMessage.data });
    }
  }, [lastMessage]);

  return (
    <div className="grid grid-rows-[20px_1fr_20px] items-center justify-items-center min-h-screen p-8 pb-20 gap-16 sm:p-20 font-[family-name:var(--font-geist-sans)]">
      <main className="flex flex-col gap-8 row-start-2 items-center sm:items-start">
        {readyState !== ReadyState.OPEN ? (
          <div>websocket state: {showWebSocketState(readyState)}</div>
        ) : (
          <div className="flex flex-col gap-8 items-center">
            <WordDisplay target={MOCK_TARGET_WORD} />
            <Wordmaster id="0" name={MOCK_WORDMASTER} />
            <div className="flex gap-2">
              {Object.values(model.players).map((player) => (
                <PlayerView
                  key={player.id}
                  inputRef={inputRef}
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
                {model.currentInput === ""
                  ? "words, words, words..."
                  : model.currentAction.tag === "contact"
                  ? `press enter to contact with ${model.currentAction.player.name}`
                  : "press enter to share your hint with everyone"}
              </h3>
              <Input
                ref={inputRef}
                onChange={(ev) =>
                  dispatch({ tag: "changedInput", value: ev.target.value })
                }
                onEnter={() => {
                  const message: OutboundMessage =
                    model.currentAction.tag === "contact"
                      ? {
                          tag: "contact",
                          playerId: model.currentAction.player.id,
                          word: model.currentInput,
                        }
                      : { tag: "hint", description: model.currentInput };

                  sendServer(message);
                }}
                placeholder={
                  model.currentAction.tag === "contact"
                    ? "type your guess here..."
                    : "type your hint here..."
                }
                value={model.currentInput}
              />
            </div>
          </div>
        )}
      </main>
      <footer className="row-start-3 flex gap-6 flex-wrap items-center justify-center">
        footer
      </footer>
    </div>
  );
}
