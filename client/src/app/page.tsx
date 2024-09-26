// TODO
"use client";

import { Codec } from "purify-ts/Codec";
import * as C from "purify-ts/Codec";
import { type Ref, useEffect, useRef, useState } from "react";
import useWebSocket from "react-use-websocket";

import Input from "./components/input";
import Player from "./components/player";
import WordDisplay, { type TargetWord } from "./components/word-display";
import Wordmaster from "./components/wordmaster";

type Action = { tag: "contact"; player: string } | { tag: "hint" };

type InboundMessage =
  | { tag: "hint"; description: string; player: string }
  | { tag: "contact"; player: string };

type OutboundMessage =
  | { tag: "contact"; player: string; word: string }
  | { tag: "hint"; description: string };

const MOCK_TARGET_WORD: TargetWord = { status: "guessing", word: "evange" };

const MOCK_WORDMASTER: string = "Shinji Ikari";

const MOCK_PLAYERS = [
  { hint: undefined, id: "1", isTyping: true, name: "Bob" },
  {
    hint: undefined,
    id: "2",
    isTyping: false,
    name: "Alice",
  },
  {
    hint: "they wish to introduce you to the lord and savior",
    id: "3",
    isTyping: false,
    name: "Gandalf",
  },
];

const inboundMessageCodec: Codec<InboundMessage> = C.oneOf([
  Codec.interface({
    tag: C.exactly("hint"),
    description: C.string,
    player: C.string,
  }),
  Codec.interface({ tag: C.exactly("contact"), player: C.string }),
]);

export default function Home() {
  const inputRef: Ref<HTMLInputElement> = useRef(null);

  const [currentInput, setCurrentInput] = useState<string>("");
  const [action, setAction] = useState<Action>({ tag: "hint" });

  const WEB_SOCKET_URL = "ws://localhost:1234";

  // TODO: check readystate and display loading screen if not yet connected etc
  const {
    sendMessage,
    lastMessage,
    readyState: _readyState,
  } = useWebSocket(WEB_SOCKET_URL);

  const sendServer = (message: OutboundMessage): void =>
    sendMessage(JSON.stringify(message));

  useEffect(() => {
    if (lastMessage !== null) {
      console.log(`last message: ${lastMessage.data}`);

      const messageJson = JSON.parse(lastMessage.data);

      const toLog = inboundMessageCodec.decode(messageJson).caseOf({
        Left: (err) => `bad inbound message: ${err}`,
        Right: (msg) => `good inbound message!: ${msg}`,
      });

      console.log(toLog);
    }
  }, [lastMessage]);

  return (
    <div className="grid grid-rows-[20px_1fr_20px] items-center justify-items-center min-h-screen p-8 pb-20 gap-16 sm:p-20 font-[family-name:var(--font-geist-sans)]">
      <main className="flex flex-col gap-8 row-start-2 items-center sm:items-start">
        <div className="flex flex-col gap-8 items-center">
          <WordDisplay target={MOCK_TARGET_WORD} />
          <Wordmaster id="0" name={MOCK_WORDMASTER} />
          <div className="flex gap-2">
            {MOCK_PLAYERS.map((props) => (
              <Player
                key={props.id}
                inputRef={inputRef}
                guess={currentInput}
                onClickCancel={() => {
                  setAction({ tag: "hint" });
                  setCurrentInput("");
                }}
                onClickContact={() =>
                  setAction({ tag: "contact", player: props.name })
                }
                {...props}
              />
            ))}
          </div>
          <div className="flex flex-col gap-1">
            <h3 className="text-zinc-400 text-sm">
              {currentInput === ""
                ? "words, words, words..."
                : action.tag === "contact"
                ? `press enter to contact with ${action.player}`
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
                        player: action.player,
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
      </main>
      <footer className="row-start-3 flex gap-6 flex-wrap items-center justify-center">
        footer
      </footer>
    </div>
  );
}
