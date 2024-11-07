"use client";

import { useReducer } from "react";
import useWebSocket, { ReadyState } from "react-use-websocket";

type State =
  | { tag: "playing"; playerName: string }
  | { tag: "waiting"; players: string[] };

type Model = { state: State };

type Msg = { tag: "noop" };

type RoomProps = {
  players: string[];
  roomId: string;
};

const REST_URL = "http://localhost:1234";
const WEB_SOCKET_URL = "ws://localhost:1234";

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

const update = (model: Model, msg: Msg): Model => {
  switch (msg.tag) {
    case "noop":
      return model;

    default:
      return model;
  }
};

function Room({ players, roomId }: RoomProps) {
  const initModel: Model = {
    state: { tag: "waiting", players },
  };

  const [model, dispatch] = useReducer(update, initModel);

  const { sendJsonMessage, sendMessage, lastMessage, readyState } =
    useWebSocket(WEB_SOCKET_URL, {
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

export default async function Page({
  params,
}: {
  params: Promise<{ roomId: string }>;
}) {
  const { roomId } = await params;

  const res = await fetch(`${REST_URL}/room/${roomId}`);
  const { players } = await res.json();

  return (
    <div className="grid grid-rows-[20px_1fr_20px] items-center justify-items-center min-h-screen p-8 pb-20 gap-16 sm:p-20 font-[family-name:var(--font-geist-sans)]">
      <main className="flex flex-col gap-8 row-start-2 items-center sm:items-start">
        {Room({ players, roomId })}
      </main>
    </div>
  );
}
