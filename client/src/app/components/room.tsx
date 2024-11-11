"use client";

import { useEffect, useMemo, useReducer } from "react";
import useWebSocket from "react-use-websocket";

import Game from "contact/app/components/game";
import Lobby from "contact/app/components/lobby";
import type { Player, PlayerId } from "contact/app/data/player";
import {
  type ClientMessage,
  type ServerMessage,
  serverMessageCodec,
} from "contact/app/network/message";

type State =
  | {
      tag: "playing";
      myPlayerName: PlayerId;
      players: Record<PlayerId, Player>;
    }
  | { tag: "waiting"; playersInGame: string[] };

type Model = { state: State };

type Msg = { tag: "receivedServerMessage"; message: ServerMessage };

type RoomProps = {
  playersInGame: string[];
  roomId: string;
  webSocketUrl: string;
};

const parseWebSocketData = (data: unknown): ServerMessage | undefined => {
  console.log(`received websocket message: "${data}"`);

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
  if (msg.tag === "syncGame" && model.state.tag === "waiting") {
    const { players, myPlayerName } = msg.data;

    const mockPlayers = Object.values(players).reduce<Record<PlayerId, Player>>(
      (acc, { name, message }) => {
        const MOCK_PLAYER: Player = {
          contactState: undefined,
          hintState:
            message === ""
              ? { tag: "thinking" }
              : { tag: "sharing", word: message },
          id: name,
          isTyping: false,
          name,
        };

        return {
          ...acc,
          [name]: MOCK_PLAYER,
        };
      },
      {}
    );

    return {
      ...model,
      state: {
        tag: "playing",
        myPlayerName,
        players: mockPlayers,
      },
    };
  } else {
    return model;
  }
};

const update = (model: Model, msg: Msg): Model => {
  if (msg.tag === "receivedServerMessage") {
    return handleServerMessage(model, msg.message);
  } else {
    return model;
  }
};

export default function Room({
  playersInGame,
  roomId,
  webSocketUrl,
}: RoomProps) {
  const initModel: Model = {
    state: { tag: "waiting", playersInGame },
  };

  const [model, dispatch] = useReducer(update, initModel);

  // TODO - handle websocket readyState
  const { lastMessage, readyState, sendJsonMessage } = useWebSocket(
    webSocketUrl,
    {
      queryParams: { roomId },
    }
  );

  const sendServer = (msg: ClientMessage): void => {
    sendJsonMessage(msg);
  };

  const lastServerMessage: ServerMessage | undefined = useMemo(() => {
    return lastMessage ? parseWebSocketData(lastMessage.data) : undefined;
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
      const { playersInGame } = model.state;

      return (
        <Lobby
          lastServerMessage={lastServerMessage}
          playersInGame={playersInGame}
          sendServer={sendServer}
        />
      );
    }

    case "playing": {
      const { myPlayerName: playerName, players } = model.state;

      return (
        <Game
          // TODO - debugging why message dispatched many times
          lastServerMessage={lastServerMessage}
          myPlayerName={playerName}
          players={players}
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
