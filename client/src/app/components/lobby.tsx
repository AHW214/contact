"use client";

import { useEffect, useReducer } from "react";

import Input from "contact/app/components/input";
import type { ClientMessage, ServerMessage } from "contact/app/network/message";

type Model = {
  currentInput: string;
  playersInGame: string[];
};

type Msg =
  | { tag: "changedInput"; value: string }
  | { tag: "receivedServerMessage"; message: ServerMessage };

type LobbyProps = {
  lastServerMessage: ServerMessage | undefined;
  playersInGame: string[];
  sendServer: (msg: ClientMessage) => void;
};

const handleServerMessage = (model: Model, msg: ServerMessage): Model => {
  if (msg.tag === "joinedGame") {
    const playersInGame = [...model.playersInGame, msg.data.playerName];

    return {
      ...model,
      playersInGame,
    };
  } else if (msg.tag === "leftGame") {
    const playersInGame = model.playersInGame.filter(
      (name) => name !== msg.data.playerName
    );

    return {
      ...model,
      playersInGame,
    };
  } else {
    // ignore all other messages from server
    return model;
  }
};

const update = (model: Model, msg: Msg): Model => {
  if (msg.tag === "changedInput") {
    return {
      ...model,
      currentInput: msg.value,
    };
  } else if (msg.tag === "receivedServerMessage") {
    return handleServerMessage(model, msg.message);
  } else {
    // IMPOSSIBLE CASE
    return model;
  }
};

export default function Lobby({
  lastServerMessage,
  playersInGame,
  sendServer,
}: LobbyProps) {
  const initModel: Model = {
    currentInput: "",
    playersInGame,
  };

  const [model, dispatch] = useReducer(update, initModel);

  useEffect(() => {
    if (lastServerMessage !== undefined) {
      dispatch({ tag: "receivedServerMessage", message: lastServerMessage });
    }
  }, [lastServerMessage]);

  return (
    <div>
      <div>
        {model.playersInGame.map((name) => (
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
          const chosenName = model.currentInput;

          sendServer({
            tag: "chooseName",
            data: { name: chosenName },
          });
        }}
        value={model.currentInput}
      />
    </div>
  );
}
