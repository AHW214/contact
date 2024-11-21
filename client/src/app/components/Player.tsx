import { type MouseEventHandler } from "react";

import type { HintState } from "contact/app/data/player";

export type ContactState =
  | { tag: "declared" }
  | { tag: "revealed"; success: boolean; word: string | undefined };

export type PlayerState =
  | { tag: "hintingWord"; hint: HintState }
  | { tag: "performingContact"; contact: ContactState }
  | { tag: "spectatingContact" };

export type Props = {
  countdownMillis: number | undefined;
  isTyping: boolean;
  name: string;
  onClickContact: MouseEventHandler<HTMLDivElement>;
  state: PlayerState;
};

const playerStyles = (
  state: PlayerState,
  countdownMillis: number | undefined
) => {
  switch (state.tag) {
    case "hintingWord": {
      const { hint } = state;

      return {
        classes: {
          borderColor: "border-zinc-300",
          coverHoverVisibility:
            hint.tag === "sharing"
              ? "group-hover:visible"
              : "group-hover:invisible",
          coverVisibility: "invisible",
          cursor: hint.tag === "sharing" ? "cursor-pointer" : "auto",
        },
        coverText: "contact",
      };
    }

    case "performingContact": {
      const { contact } = state;

      return {
        classes: {
          borderColor:
            contact.tag === "declared"
              ? "border-blue-800"
              : contact.success
              ? "border-green-800"
              : "border-red-800",
          coverHoverVisibility:
            countdownMillis !== undefined
              ? "group-hover:visible"
              : "group-hover:invisible",
          coverVisibility:
            countdownMillis !== undefined ? "visible" : "invisible",
          cursor: "auto",
        },
        coverText:
          countdownMillis !== undefined
            ? `${countdownMillis / 1000}`
            : undefined,
      };
    }

    case "spectatingContact":
    default: {
      return {
        classes: {
          borderColor: "border-zinc-300",
          coverHoverVisibility: "group-hover:invisible",
          coverVisibility: "invisible",
          cursor: "cursor-not-allowed",
        },
        coverText: undefined,
      };
    }
  }
};

export default function Player({
  countdownMillis,
  isTyping,
  name,
  onClickContact,
  state,
}: Props) {
  const { classes, coverText } = playerStyles(state, countdownMillis);

  const onClick: MouseEventHandler<HTMLDivElement> = (ev) => {
    onClickContact(ev);
  };

  // TODO - use clsx for long tailwind class organization?
  // https://shnoman97.medium.com/simplify-your-tailwind-css-class-management-with-merge-and-clsx-42f1e2458fd8

  return (
    <div className="relative group">
      <div
        className={`absolute top-0 left-0 w-full h-full flex items-center justify-center rounded-lg bg-zinc-800 ${classes.coverVisibility} ${classes.coverHoverVisibility}`}
        onClick={onClick}
      >
        <h3 className="text-zinc-100 text-center text-3xl uppercase">
          {coverText}
        </h3>
      </div>
      <div
        className={`pl-0 pt-0 p-2 min-w-40 max-w-72 min-h-16 max-h-48 border-2 ${classes.borderColor} rounded-lg ${classes.cursor}`}
      >
        <div
          className={`w-fit -mx-[.125rem] -mt-[.125rem] px-2 border-2 rounded-tl-lg rounded-br-lg ${classes.borderColor}`}
        >
          <h3>{name}</h3>
        </div>
        {
          // TODO - getting a bit verbose with the nested state properties
          // maybe refactor out into separate function
          state.tag === "performingContact" &&
          state.contact.tag === "revealed" ? (
            <p className="ml-2">{state.contact.word ?? "..."}</p>
          ) : state.tag === "hintingWord" &&
            state.hint.tag === "sharing" &&
            state.hint.word !== "" ? (
            <p className="ml-2">{state.hint.word}</p>
          ) : isTyping ? (
            <p className="ml-2 tracking-widest">...</p>
          ) : undefined
        }
      </div>
    </div>
  );
}
