// TODO
"use client";

import { type Either, Left, Right } from "purify-ts";
import { Codec } from "purify-ts/Codec";
import * as C from "purify-ts/Codec";
import {
  type MutableRefObject,
  type Ref,
  useEffect,
  useReducer,
  useRef,
} from "react";
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
  | { tag: "hinting" }
  | { tag: "thinking" };

type Model = {
  countdown: number | undefined;
  currentAction: Action;
  currentInput: string;
  players: Record<PlayerId, Player>;
};

type Msg =
  | { tag: "changedInput"; value: string }
  | { tag: "clickedCancel" }
  | { tag: "clickedContact"; player: { id: PlayerId; name: string } }
  | { tag: "clickedEscape" }
  | { tag: "sharedHint" }
  | { tag: "tickCountdown"; millis: number }
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
  hintState: { tag: "thinking" },
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
  "Sei",
  "Bob",
  "Alice",
  "Gandalf",
]);

const MOCK_MY_PLAYER_ID = "0" as PlayerId;

const MOCK_MODEL: Model = {
  countdown: undefined,
  currentAction: { tag: "thinking" },
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

    case "webSocketMessage":
      return parseWebSocketData(msg.data).caseOf({
        Left: (err) => {
          console.log(`bad websocket message: ${err}`);
          return model;
        },

        Right: (message) => {
          switch (message.tag) {
            case "hint": {
              return {
                ...model,
                players: Record.update(
                  model.players,
                  [message.playerId],
                  (player) => {
                    return {
                      ...player,
                      hintState: { tag: "sharing", word: message.description },
                    };
                  }
                ),
              };
            }

            case "declareContact": {
              return {
                ...model,
                countdown: 5000,
                players: Record.update(
                  model.players,
                  [message.players.fromId, message.players.toId],
                  (player): Player => {
                    return {
                      ...player,
                      contactState: { tag: "declared" },
                    };
                  }
                ),
              };
            }

            case "revealContact": {
              const contactStatus = message.success ? "succeeded" : "failed";

              return {
                ...model,
                players: Record.updateWithData(
                  model.players,
                  [
                    [message.players.from.id, message.players.from.word],
                    [message.players.to.id, message.players.to.word],
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
              console.error(`useReducer(): UNKNOWN MESSAGE '${msg.tag}'`);
              return model;
            }
          }
        },
      });

    default:
      return model;
  }
};

export default function Home() {
  const inputRef: Ref<HTMLInputElement> = useRef(null);
  const intervalRef: MutableRefObject<number | undefined> = useRef(undefined);

  const [model, dispatch] = useReducer(update, MOCK_MODEL);

  const WEB_SOCKET_URL = "ws://localhost:1234";

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

  const { [MOCK_MY_PLAYER_ID]: myPlayer, ...players } = model.players;

  const contactingPlayers = Object.values(players)
    .filter(({ contactState }) => contactState !== undefined)
    .slice(0, 2);

  const isAnyoneContacting = contactingPlayers.length === 2;

  return (
    <div className="grid grid-rows-[20px_1fr_20px] items-center justify-items-center min-h-screen p-8 pb-20 gap-16 sm:p-20 font-[family-name:var(--font-geist-sans)]">
      <main className="flex flex-col gap-8 row-start-2 items-center sm:items-start">
        {readyState !== ReadyState.OPEN ? (
          <div>websocket state: {showWebSocketState(readyState)}</div>
        ) : myPlayer === undefined ? (
          <div>... who am i ...</div>
        ) : (
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
                      playerId: model.currentAction.player.id,
                      word: model.currentInput,
                    });
                  } else if (model.currentAction.tag === "thinking") {
                    dispatch({ tag: "sharedHint" });

                    sendServer({
                      tag: "hint",
                      description: model.currentInput,
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
        )}
      </main>
      <footer className="row-start-3 flex gap-6 flex-wrap items-center justify-center">
        footer
      </footer>
    </div>
  );
}
