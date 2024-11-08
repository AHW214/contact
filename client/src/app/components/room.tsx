"use client";

import { Codec, type Either, Left } from "purify-ts";
import * as C from "purify-ts/Codec";
import { useEffect, useReducer } from "react";
import useWebSocket from "react-use-websocket";

import Input from "contact/app/components/input";
import {
  type Player,
  type PlayerId,
  playerIdCodec,
} from "contact/app/data/player";

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
    | { tag: "joinedGame"; data: { playerName: PlayerId } }
    | { tag: "leftGame"; data: { playerName: PlayerId } }
    | {
        tag: "syncGame";
        data: { myPlayerName: PlayerId; players: PlayerId[] };
      };

  export const clientMessageCodec: Codec<ClientMessage> = C.oneOf([
    Codec_.tagged("chooseName", { name: C.string }),
  ]);

  export const serverMessageCodec: Codec<ServerMessage> = C.oneOf([
    Codec_.tagged("joinedGame", { playerName: playerIdCodec }),
    Codec_.tagged("leftGame", { playerName: playerIdCodec }),
    Codec_.tagged("syncGame", {
      myPlayerName: playerIdCodec,
      players: C.array(playerIdCodec),
    }),
  ]);
}

type State =
  | { tag: "playing"; playerName: PlayerId; players: Record<PlayerId, Player> }
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
  console.log(data);

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
  if (msg.tag === "joinedGame" && model.state.tag === "waiting") {
    return {
      ...model,
      state: {
        ...model.state,
        players: [...model.state.players, msg.data.playerName],
      },
    };
  } else if (msg.tag === "leftGame" && model.state.tag === "waiting") {
    return {
      ...model,
      state: {
        ...model.state,
        players: model.state.players.filter((p) => p !== msg.data.playerName),
      },
    };
  } else if (msg.tag === "syncGame" && model.state.tag === "waiting") {
    const players = msg.data.players.reduce<Record<PlayerId, Player>>(
      (acc, playerId) => {
        const MOCK_PLAYER: Player = {
          contactState: undefined,
          hintState: { tag: "thinking" },
          id: playerId,
          isTyping: false,
          name: playerId,
        };

        return {
          ...acc,
          [playerId]: MOCK_PLAYER,
        };
      },
      {}
    );

    return {
      ...model,
      state: {
        tag: "playing",
        playerName: msg.data.myPlayerName,
        players,
      },
    };
  } else {
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
  const { lastMessage, readyState, sendJsonMessage } = useWebSocket(
    webSocketUrl,
    {
      queryParams: { roomId },
    }
  );

  const sendServer = (msg: Network.ClientMessage): void => {
    sendJsonMessage(msg);
  };

  useEffect(() => {
    if (lastMessage !== null) {
      dispatch({ tag: "receivedServerMessage", data: lastMessage.data });
    }
  }, [lastMessage]);

  switch (model.state.tag) {
    case "waiting": {
      const chosenName = model.state.currentInput;

      return (
        <div>
          <div>
            {model.state.players.map((name) => (
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
      return <div>{JSON.stringify(model.state)}</div>;
    }

    default: {
      // IMPOSSIBLE CASE
      return <div></div>;
    }
  }
}
