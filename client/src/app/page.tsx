import Player, * as P from "./components/Player";

type TargetWord = { status: "guessing" | "unveiled"; word: string };

type WordHintMessage = {
  playerId: string;
  hint: string;
};

type WordGuessMessage = {
  playerId: string;
  guess: string;
};

type WordDisplayProps = {
  target: TargetWord;
};

type WordmasterProps = {
  id: string;
  name: string;
};

const MOCK_TARGET_WORD: TargetWord = { status: "guessing", word: "evange" };

const MOCK_WORDMASTER: string = "Shinji Ikari";

const MOCK_PLAYERS: P.Props[] = [
  { guess: undefined, hint: undefined, id: "1", isTyping: true, name: "Bob" },
  {
    guess: undefined,
    hint: undefined,
    id: "2",
    isTyping: false,
    name: "Alice",
  },
  {
    guess: undefined,
    hint: "they wish to introduce you to the lord and savior",
    id: "3",
    isTyping: false,
    name: "Gandalf",
  },
];

export default function Home() {
  return (
    <div className="grid grid-rows-[20px_1fr_20px] items-center justify-items-center min-h-screen p-8 pb-20 gap-16 sm:p-20 font-[family-name:var(--font-geist-sans)]">
      <main className="flex flex-col gap-8 row-start-2 items-center sm:items-start">
        <div className="flex flex-col gap-8 items-center">
          <WordDisplay target={MOCK_TARGET_WORD} />
          <Wordmaster id="0" name={MOCK_WORDMASTER} />
          <div className="flex gap-2">
            {MOCK_PLAYERS.map((props) => (
              <Player key={props.id} {...props} />
            ))}
          </div>
          <input
            className="p-1 min-w-96 border-2 border-zinc-300 rounded-lg focus:outline-zinc-800"
            placeholder="guess here..."
          ></input>
        </div>
      </main>
      <footer className="row-start-3 flex gap-6 flex-wrap items-center justify-center">
        footer
      </footer>
    </div>
  );
}

function WordDisplay({ target }: WordDisplayProps) {
  return (
    <div className="flex gap-2">
      {target.word.split("").map((letter, ix) => (
        <div
          className="p-2 w-16 border-2 border-zinc-800 rounded-lg bg-zinc-800 text-6xl text-zinc-100 font-light uppercase text-center"
          key={ix}
        >
          {letter}
        </div>
      ))}
      {target.status === "guessing" ? (
        <div className="p-2 text-6xl font-light tracking-[1.5rem]">...</div>
      ) : undefined}
    </div>
  );
}

function Wordmaster({ name }: WordmasterProps) {
  return (
    <div className="px-0 pt-0 w-40 h-16 p-2 border-2 border-zinc-800 rounded-lg bg-zinc-800 text-zinc-100">
      <div className="w-fit -mx-[.125rem] -mt-[.125rem] px-2 border-2 border-zinc-600 border-l-zinc-800 border-t-zinc-800 rounded-tl-lg rounded-br-lg ">
        <h3>{name}</h3>
      </div>
      <p className="ml-2">placeholder</p>
    </div>
  );
}
