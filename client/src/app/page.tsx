type WordDisplayProps = {
  unveiled: string;
};

type WordmasterProps = {
  id: string;
  name: string;
};

type PlayerProps = {
  id: string;
  name: string;
};

const MOCK_WORD_UNVEILED: string = "evange";

const MOCK_WORDMASTER: string = "Shinji Ikari";

const MOCK_PLAYERS: PlayerProps[] = [
  { id: "1", name: "Bob" },
  { id: "2", name: "Alice" },
  { id: "3", name: "Gandalf" },
];

export default function Home() {
  return (
    <div className="grid grid-rows-[20px_1fr_20px] items-center justify-items-center min-h-screen p-8 pb-20 gap-16 sm:p-20 font-[family-name:var(--font-geist-sans)]">
      <main className="flex flex-col gap-8 row-start-2 items-center sm:items-start">
        <div className="flex flex-col gap-8 items-center">
          <WordDisplay unveiled={MOCK_WORD_UNVEILED} />
          <Wordmaster id="0" name={MOCK_WORDMASTER} />
          <div className="flex gap-2">{MOCK_PLAYERS.map(Player)}</div>
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

function WordDisplay({ unveiled }: WordDisplayProps) {
  return (
    <div className="flex gap-2">
      {unveiled.split("").map((letter, ix) => (
        <div
          className="p-2 w-16 border-2 border-zinc-800 rounded-lg bg-zinc-800 text-6xl text-zinc-100 font-light uppercase text-center"
          key={ix}
        >
          {letter}
        </div>
      ))}
      <div className="p-2 text-6xl font-light tracking-[1.5rem]">...</div>
    </div>
  );
}

function Wordmaster({ name }: WordmasterProps) {
  return (
    <div className="p-2 border-2 border-zinc-800 rounded-lg bg-zinc-800 text-zinc-100">
      <h3>{name}</h3>
      <p>--- placeholder guess ---</p>
    </div>
  );
}

function Player({ id, name }: PlayerProps) {
  return (
    <div key={id} className="p-2 border-2 border-zinc-300 rounded-lg">
      <h3>{name}</h3>
      <p>--- placeholder hint ---</p>
    </div>
  );
}
