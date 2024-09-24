"use client";

export type Props = {
  hint: string | undefined;
  id: string;
  isTyping: boolean;
  name: string;
};

export default function Player({ hint, id, isTyping, name }: Props) {
  return (
    <div
      key={id}
      className="pl-0 pt-0 min-w-40 max-w-72 min-h-16 max-h-48 p-2 border-2 border-zinc-300 rounded-lg"
    >
      <div className="w-fit -mx-[.125rem] -mt-[.125rem] px-2 border-2 rounded-tl-lg rounded-br-lg border-zinc-300">
        <h3>{name}</h3>
      </div>
      {hint !== undefined && hint !== "" ? (
        <p className="ml-2" onClick={() => alert("contact!")}>
          {hint}
        </p>
      ) : isTyping ? (
        <p className="ml-2 tracking-widest">...</p>
      ) : undefined}
    </div>
  );
}
