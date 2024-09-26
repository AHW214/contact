// TODO
"use client";

import { type Ref, useRef, useState } from "react";

import Input from "./components/input";
import Player from "./components/player";
import WordDisplay, { type TargetWord } from "./components/word-display";
import Wordmaster from "./components/wordmaster";

type Action = { tag: "contact"; player: string } | { tag: "hint" };

type WordHintMessage = {
  playerId: string;
  hint: string;
};

type WordGuessMessage = {
  playerId: string;
  guess: string;
};

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

export default function Home() {
  const inputRef: Ref<HTMLInputElement> = useRef(null);
  const [currentInput, setCurrentInput] = useState<string>("");
  const [action, setAction] = useState<Action>({ tag: "hint" });

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
                const thing = `${action.tag}: ${currentInput}`;
                alert(thing);
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
