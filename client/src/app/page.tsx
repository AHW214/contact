// TODO
"use client";

import type { WithBrand } from "@coderspirit/nominal";
import { type Either, Left, Right } from "purify-ts";
import { Codec } from "purify-ts/Codec";
import * as C from "purify-ts/Codec";
import { type Ref, useEffect, useReducer, useRef, useState } from "react";
import useWebSocket, { ReadyState } from "react-use-websocket";

import Input from "./components/input";
import Player, { type ContactState } from "./components/player";
import WordDisplay, { type TargetWord } from "./components/word-display";
import Wordmaster from "./components/wordmaster";

namespace Player {
  export type Id = WithBrand<string, "Player">;
}

type Player = {
  contactState: ContactState | undefined;
  hint: string | undefined;
  id: Player.Id;
  isTyping: boolean;
  name: string;
};

type Action =
  | { tag: "contact"; player: { id: string; name: string } }
  | { tag: "hint" };

type Model = {
  currentAction: Action;
  currentInput: string;
  players: Record<Player.Id, Player>;
};

type Msg =
  | { tag: "clickedCancel" }
  | { tag: "clickedContact"; player: { id: string; name: string } }
  | { tag: "webSocketMessage"; data: unknown };

type InboundMessage =
  | { tag: "hint"; description: string; playerId: string }
  | { tag: "declareContact"; players: { fromId: string; toId: string } }
  | {
      // TODO - separate messages for failed and successful contacts?
      tag: "revealContact";
      players: {
        from: { id: string; word: string };
        to: { id: string; word: string };
      };
      success: boolean;
    };

type OutboundMessage =
  | { tag: "contact"; playerId: string; word: string }
  | { tag: "hint"; description: string };

type MockPlayerParams = {
  id: string;
  name: string;
};

const mockPlayer = ({ id, name }: MockPlayerParams): Player => ({
  contactState: undefined,
  hint: undefined,
  id: id as Player.Id,
  isTyping: false,
  name,
});

const mockPlayerMap = (names: string[]): Record<Player.Id, Player> => {
  const players = names.map((name, ix) => mockPlayer({ id: `${ix}`, name }));
  return Object.fromEntries(players.map((player) => [player.id, player]));
};

const MOCK_TARGET_WORD: TargetWord = { status: "guessing", word: "evange" };

const MOCK_WORDMASTER: string = "Shinji Ikari";

const MOCK_PLAYERS: Record<Player.Id, Player> = mockPlayerMap([
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
    playerId: C.string,
  }),
  Codec.interface({
    tag: C.exactly("declareContact"),
    players: Codec.interface({ fromId: C.string, toId: C.string }),
  }),
  Codec.interface({
    tag: C.exactly("revealContact"),
    players: Codec.interface({
      from: Codec.interface({ id: C.string, word: C.string }),
      to: Codec.interface({ id: C.string, word: C.string }),
    }),
    success: C.boolean,
  }),
]);

const tryParseJSON = (value: any): string | undefined => {
  try {
    return JSON.parse(value);
  } catch {
    return undefined;
  }
};

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

namespace Record {
  export const updateWithData = <K extends string | number | symbol, V, T>(
    record: Record<K, V>,
    keysAndData: [K, T][],
    updateFn: (value: V, data: T) => V
  ): Record<K, V> => {
    const entries = keysAndData.reduce<[K, [V, T]][]>((acc, [key, data]) => {
      const value = record[key];
      const entry: [K, [V, T]] = [key, [value, data]];
      return value !== undefined ? [...acc, entry] : acc;
    }, []);

    if (entries.length !== keysAndData.length) {
      return record;
    }

    return entries.reduce(
      (acc, [key, [value, data]]) => ({ ...acc, [key]: updateFn(value, data) }),
      record
    );
  };

  export const update = <K extends string | number | symbol, V>(
    record: Record<K, V>,
    keys: K[],
    updateFn: (value: V) => V
  ): Record<K, V> => {
    const keysAndData: [K, undefined][] = keys.map((key) => [key, undefined]);
    return updateWithData(record, keysAndData, updateFn);
  };
}

const update = (model: Model, msg: Msg): Model => {
  switch (msg.tag) {
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
              const newPlayers = Record.update(
                model.players,
                [message.playerId] as Player.Id[],
                (player) => {
                  return { ...player, hint: message.description };
                }
              );

              return { ...model, players: newPlayers };

            case "declareContact":
              const newPlayers2 = Record.update(
                model.players,
                [message.players.fromId, message.players.toId] as Player.Id[],
                (player): Player => {
                  return { ...player, contactState: "declared" };
                }
              );

              return { ...model, players: newPlayers2 };

            case "revealContact":
              const contactState = message.success ? "succeeded" : "failed";

              const newPlayers3 = Record.updateWithData(
                model.players,
                [
                  [
                    message.players.from.id as Player.Id,
                    message.players.from.word,
                  ],
                  [message.players.to.id as Player.Id, message.players.to.word],
                ],
                (player, word): Player => {
                  return { ...player, hint: word, contactState };
                }
              );

              return { ...model, players: newPlayers3 };

            default:
              console.error("UNKNOWN MESSAGE");
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

  const [currentInput, setCurrentInput] = useState<string>("");
  const [action, setAction] = useState<Action>({ tag: "hint" });
  const [players, setPlayers] = useState<Map<string, Player>>(new Map());

  const [model, dispatch] = useReducer(update, MOCK_MODEL);

  const handleInboundMessage = (message: InboundMessage): void => {
    // TODO - much redundant code

    if (message.tag === "hint") {
      const player = players.get(message.playerId);

      if (player !== undefined) {
        const updatedPlayer = { ...player, hint: message.description };
        setPlayers(new Map([...players, [player.id, updatedPlayer]]));
      }
    } else if (message.tag === "declareContact") {
      const fromPlayer = players.get(message.players.fromId);
      const toPlayer = players.get(message.players.toId);

      if (fromPlayer !== undefined && toPlayer !== undefined) {
        const updatedPlayers: Player[] = [fromPlayer, toPlayer].map(
          (player) => ({
            ...player,
            contactState: "declared",
          })
        );

        setPlayers(
          new Map([
            ...players,
            ...updatedPlayers.map<[string, Player]>((p) => [p.id, p]),
          ])
        );
      }
    } else if (message.tag === "revealContact") {
      const fromPlayer = players.get(message.players.from.id);
      const toPlayer = players.get(message.players.to.id);

      if (fromPlayer !== undefined && toPlayer !== undefined) {
        const contactState = message.success ? "succeeded" : "failed";

        const updatedPlayers: Player[] = [
          { player: fromPlayer, msg: message.players.from },
          { player: toPlayer, msg: message.players.to },
        ].map(({ player, msg }) => ({
          ...player,
          hint: msg.word,
          contactState,
        }));

        setPlayers(
          new Map([
            ...players,
            ...updatedPlayers.map<[string, Player]>((p) => [p.id, p]),
          ])
        );
      }
    }
  };

  const WEB_SOCKET_URL = "ws://localhost:1234";

  // TODO: check readystate and display loading screen if not yet connected etc
  const { sendMessage, lastMessage, readyState } = useWebSocket(WEB_SOCKET_URL);

  const sendServer = (message: OutboundMessage): void =>
    sendMessage(JSON.stringify(message));

  useEffect(() => {
    if (lastMessage !== null) {
      console.log(`last message: ${lastMessage.data}`);

      const messageJson = tryParseJSON(lastMessage.data);

      inboundMessageCodec.decode(messageJson).caseOf({
        Left: (err) => {
          console.log(`bad inbound message: ${err}`);
        },

        Right: (msg) => {
          handleInboundMessage(msg);
        },
      });
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
              {[...players].map(([, player]) => (
                <Player
                  key={player.id}
                  inputRef={inputRef}
                  onClickCancel={() => {
                    setAction({ tag: "hint" });
                    setCurrentInput("");
                  }}
                  onClickContact={() =>
                    setAction({
                      tag: "contact",
                      player: { id: player.id, name: player.name },
                    })
                  }
                  {...player}
                />
              ))}
            </div>
            <div className="flex flex-col gap-1">
              <h3 className="text-zinc-400 text-sm">
                {currentInput === ""
                  ? "words, words, words..."
                  : action.tag === "contact"
                  ? `press enter to contact with ${action.player.name}`
                  : "press enter to share your hint with everyone"}
              </h3>
              <Input
                ref={inputRef}
                onChange={(ev) => setCurrentInput(ev.target.value)}
                onEnter={() => {
                  const message: OutboundMessage =
                    action.tag === "contact"
                      ? {
                          tag: "contact",
                          playerId: action.player.id,
                          word: currentInput,
                        }
                      : { tag: "hint", description: currentInput };

                  sendServer(message);
                }}
                placeholder={
                  action.tag === "contact"
                    ? "type your guess here..."
                    : "type your hint here..."
                }
                value={currentInput}
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
