"use client";

export type Props = {
  guess: string | undefined;
  hint: string | undefined;
  id: string;
  isTyping: boolean;
  name: string;
};

export default function Player({ guess, hint, id, isTyping, name }: Props) {
  const { classes, coverText, onClick } =
    hint !== undefined && hint !== ""
      ? {
          classes: { cursor: "cursor-pointer", visibility: "visible" },
          coverText: guess !== undefined && guess !== "" ? "contact" : "guess",
          onClick: () => alert("contact!"),
        }
      : {
          classes: { cursor: "auto", visibility: "invisible" },
          coverText: "",
          onClick: () => {},
        };

  return (
    <div key={id} className="relative group">
      <div
        className={`absolute top-0 left-0 w-full h-full flex items-center justify-center rounded-lg bg-zinc-800 invisible group-hover:${classes.visibility}`}
      >
        <h3 className="text-zinc-100 text-center text-4xl uppercase">
          {coverText}
        </h3>
      </div>
      <div
        className={`pl-0 pt-0 p-2 min-w-40 max-w-72 min-h-16 max-h-48 border-2 border-zinc-300 rounded-lg ${classes.cursor}`}
        onClick={onClick}
      >
        <div className="w-fit -mx-[.125rem] -mt-[.125rem] px-2 border-2 rounded-tl-lg rounded-br-lg border-zinc-300">
          <h3>{name}</h3>
        </div>
        {hint !== undefined && hint !== "" ? (
          <p className="ml-2">{hint}</p>
        ) : isTyping ? (
          <p className="ml-2 tracking-widest">...</p>
        ) : undefined}
      </div>
    </div>
  );
}
