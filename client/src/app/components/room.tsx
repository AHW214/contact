"use client";

import { useEffect, useReducer, useState } from "react";
import useWebSocket from "react-use-websocket";

import Game from "contact/app/components/game";
import Input from "contact/app/components/input";
import type { Player, PlayerId } from "contact/app/data/player";
import {
  type ClientMessage,
  type ServerMessage,
  serverMessageCodec,
} from "contact/app/network/message";

type State =
  | { tag: "playing"; playerName: PlayerId; players: Record<PlayerId, Player> }
  | { tag: "waiting"; currentInput: string; players: string[] };

type Model = { state: State };

type Msg =
  | { tag: "changedInput"; value: string }
  | { tag: "receivedServerMessage"; message: ServerMessage };

type RoomProps = {
  players: string[];
  roomId: string;
  webSocketUrl: string;
};

const parseWebSocketData = (data: unknown): ServerMessage | undefined => {
  console.log(data);

  if (typeof data !== "string") {
    console.log("websocket data is not a string");
    return undefined;
  }

  try {
    const dataJson = JSON.parse(data);
    return serverMessageCodec.decode(dataJson).caseOf({
      Left: (err) => {
        console.log(`failed to parse server websocket message: ${err}`);
        return undefined;
      },
      Right: (message) => {
        return message;
      },
    });
  } catch (err) {
    console.log(`failed to parse websocket data: ${err}`);
    return undefined;
  }
};

const handleServerMessage = (model: Model, msg: ServerMessage): Model => {
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
    return handleServerMessage(model, msg.message);
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

  const [lastServerMessage, setLastServerMessage] = useState<
    ServerMessage | undefined
  >(undefined);

  // TODO - update player list when players join / leave game
  const { lastMessage, readyState, sendJsonMessage } = useWebSocket(
    webSocketUrl,
    {
      queryParams: { roomId },
    }
  );

  const sendServer = (msg: ClientMessage): void => {
    sendJsonMessage(msg);
  };

  useEffect(() => {
    if (lastMessage !== null) {
      const serverMessage = parseWebSocketData(lastMessage.data);
      if (serverMessage !== undefined) {
        setLastServerMessage(serverMessage);
      }
    }
  }, [lastMessage]);

  useEffect(() => {
    if (lastServerMessage !== undefined) {
      console.log(
        `ROOM: dispatching server message: ${JSON.stringify(lastServerMessage)}`
      );
      dispatch({ tag: "receivedServerMessage", message: lastServerMessage });
    }
  }, [lastServerMessage]);

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
      return (
        <Game
          // TODO - debugging why message dispatched many times
          lastServerMessage={lastServerMessage}
          myPlayerName={model.state.playerName}
          players={model.state.players}
          sendServer={sendServer}
        />
      );
    }

    default: {
      // IMPOSSIBLE CASE
      return <div></div>;
    }
  }
}
