export type SecretWord = { status: "guessing" | "unveiled"; word: string };

export type Props = {
  secretWord: SecretWord;
};

export const updateSecretWord = (
  secretWord: SecretWord,
  newLetter: string
): SecretWord => {
  return secretWord.status === "guessing"
    ? { ...secretWord, word: secretWord.word + newLetter }
    : secretWord;
};

export default function WordDisplay({ secretWord }: Props) {
  return (
    <div className="flex gap-2">
      {secretWord.word.split("").map((letter, ix) => (
        <div
          className="p-2 w-16 border-2 border-zinc-800 rounded-lg bg-zinc-800 text-6xl text-zinc-100 font-light uppercase text-center"
          key={ix}
        >
          {letter}
        </div>
      ))}
      {secretWord.status === "guessing" ? (
        <div className="p-2 text-6xl font-light tracking-[1.5rem]">...</div>
      ) : undefined}
    </div>
  );
}
