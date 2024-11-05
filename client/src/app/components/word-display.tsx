export type TargetWord = { status: "guessing" | "unveiled"; word: string };

export type Props = {
  target: TargetWord;
};

export default function WordDisplay({ target }: Props) {
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
