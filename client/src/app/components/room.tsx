"use client";

import { Codec, type Either, Left } from "purify-ts";
import * as C from "purify-ts/Codec";
import { useEffect, useReducer } from "react";
import useWebSocket from "react-use-websocket";

import Input from "contact/app/components/input";

namespace Codec_ {
  export const tagged = <T extends string, U extends { [x: string]: any }>(
    tag: T,
    data: U
  ) =>
    Codec.interface({
      tag: C.exactly(tag),
      data: Codec.interface(data),
    });
}

namespace Network {
  export type ClientMessage = { tag: "chooseName"; data: { name: string } };

  export type ServerMessage =
    | { tag: "playerJoined"; data: { name: string } }
    | { tag: "playerLeft"; data: { name: string } };

  export const clientMessageCodec: Codec<ClientMessage> = C.oneOf([
    Codec_.tagged("chooseName", { name: C.string }),
  ]);

  export const serverMessageCodec: Codec<ServerMessage> = C.oneOf([
    Codec_.tagged("playerJoined", { name: C.string }),
    Codec_.tagged("playerLeft", { name: C.string }),
  ]);
}

type State =
  | { tag: "playing"; playerName: string }
  | { tag: "waiting"; currentInput: string; players: string[] };

type Model = { state: State };

type Msg =
  | { tag: "changedInput"; value: string }
  | { tag: "receivedServerMessage"; data: unknown };

type RoomProps = {
  players: string[];
  roomId: string;
  webSocketUrl: string;
};

const parseWebSocketData = (
  data: unknown
): Either<string, Network.ServerMessage> => {
  if (typeof data !== "string") {
    return Left("websocket data not string");
  }

  try {
    const dataJson = JSON.parse(data);
    return Network.serverMessageCodec.decode(dataJson);
  } catch (err) {
    return Left(`failed to parse websocket data: ${err}`);
  }
};

const handleServerMessage = (
  model: Model,
  msg: Network.ServerMessage
): Model => {
  switch (msg.tag) {
    case "playerJoined":
      return model;
    case "playerLeft":
      return model;
    default:
      // IMPOSSIBLE CASE
      return model;
  }
};

const update = (model: Model, msg: Msg): Model => {
  if (msg.tag === "receivedServerMessage") {
    return parseWebSocketData(msg.data).caseOf({
      Left: (err) => {
        console.log(`failed to parse server websocket message: ${err}`);
        return model;
      },

      Right: (message) => {
        return handleServerMessage(model, message);
      },
    });
  } else if (msg.tag === "changedInput" && model.state.tag === "waiting") {
    return { ...model, state: { ...model.state, currentInput: msg.value } };
  } else {
    return model;
  }
};

export default function Room({ players, roomId, webSocketUrl }: RoomProps) {
  const initModel: Model = {
    state: { tag: "waiting", currentInput: "", players },
  };

  const [model, dispatch] = useReducer(update, initModel);

  // TODO - update player list when players join / leave game
  const { lastJsonMessage, readyState, sendJsonMessage } = useWebSocket(
    webSocketUrl,
    {
      queryParams: { roomId },
    }
  );

  const sendServer = (msg: Network.ClientMessage): void => {
    sendJsonMessage(msg);
  };

  useEffect(() => {
    if (lastJsonMessage !== null) {
      dispatch({ tag: "receivedServerMessage", data: lastJsonMessage });
    }
  }, [lastJsonMessage]);

  switch (model.state.tag) {
    case "waiting": {
      const chosenName = model.state.currentInput;

      return (
        <div>
          <div>
            {players.map((name) => (
              <div key={name}>{name}</div>
            ))}
          </div>
          <h1>choose a name</h1>
          <Input
            placeholder="怎么称呼你哦～"
            onChange={(ev) => {
              dispatch({ tag: "changedInput", value: ev.target.value });
            }}
            onEnter={() => {
              sendServer({
                tag: "chooseName",
                data: { name: chosenName },
              });
            }}
            value={chosenName}
          />
        </div>
      );
    }

    case "playing": {
      return <div>playing</div>;
    }

    default: {
      // IMPOSSIBLE CASE
      return <div></div>;
    }
  }
}
