"use client";

import { useReducer } from "react";
import useWebSocket from "react-use-websocket";

type State =
  | { tag: "playing"; playerName: string }
  | { tag: "waiting"; players: string[] };

type Model = { state: State };

type Msg = { tag: "noop" };

type RoomProps = {
  players: string[];
  roomId: string;
  webSocketUrl: string;
};

const update = (model: Model, msg: Msg): Model => {
  switch (msg.tag) {
    case "noop":
      return model;

    default:
      return model;
  }
};

export default function Room({ players, roomId, webSocketUrl }: RoomProps) {
  const initModel: Model = {
    state: { tag: "waiting", players },
  };

  const [model, _dispatch] = useReducer(update, initModel);

  const _ = useWebSocket(webSocketUrl, {
    queryParams: { roomId },
  });

  switch (model.state.tag) {
    case "waiting":
      return (
        <div>
          <div>
            {players.map((name) => (
              <div key={name}>{name}</div>
            ))}
          </div>
          <h1>choose a name</h1>
          <input placeholder="怎么称呼你哦～"></input>
        </div>
      );

    case "playing":
      return <div>playing</div>;

    default:
      // IMPOSSIBLE CASE
      return <div></div>;
  }
}
