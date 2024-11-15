"use client";

import { type MouseEventHandler, type RefObject, useState } from "react";

import type { HintState } from "contact/app/data/player";

export type ContactState =
  | { tag: "declared" }
  | { tag: "revealed"; success: boolean; word: string };

export type PlayerState = ContactState | HintState;

export type Props = {
  countdownMillis: number | undefined;
  inputRef: RefObject<HTMLInputElement>;
  isTyping: boolean;
  name: string;
  onClickCancel: MouseEventHandler<HTMLDivElement>;
  onClickContact: MouseEventHandler<HTMLDivElement>;
  state: PlayerState;
};

const playerStyles = (
  state: PlayerState,
  countdownMillis: number | undefined,
  isSelected: boolean
) => {
  if (state.tag === "thinking" || state.tag === "sharing") {
    // is hinting

    return {
      classes: {
        borderColor: isSelected ? "border-zinc-800" : "border-zinc-300",
        coverHoverVisibility: "group-hover:visible",
        coverVisibility: "invisible",
        cursor: "cursor-pointer",
      },
      coverText: isSelected ? "cancel" : "contact",
    };
  }

  // is contacting

  return {
    classes: {
      borderColor:
        state.tag === "declared"
          ? "border-blue-800"
          : state.success
          ? "border-green-800"
          : "border-red-800",
      coverHoverVisibility:
        countdownMillis !== undefined
          ? "group-hover:visible"
          : "group-hover:invisible",
      coverVisibility: countdownMillis !== undefined ? "visible" : "invisible",
      cursor: "auto",
    },
    coverText:
      countdownMillis !== undefined ? `${countdownMillis / 1000}` : undefined,
  };
};

export default function Player({
  countdownMillis,
  inputRef,
  isTyping,
  name,
  onClickCancel,
  onClickContact,
  state,
}: Props) {
  const [isSelected, setIsSelected] = useState<boolean>(false);

  const { classes, coverText } = playerStyles(
    state,
    countdownMillis,
    isSelected
  );

  const onClick: MouseEventHandler<HTMLDivElement> = (ev) => {
    if (isSelected) {
      setIsSelected(false);
      onClickCancel(ev);
      return;
    }

    setIsSelected(true);

    inputRef.current?.focus();
    inputRef.current?.select();

    onClickContact(ev);
  };

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
        {state.tag === "revealed" ? (
          <p className="ml-2">{state.word}</p>
        ) : state.tag === "sharing" && state.word !== "" ? (
          <p className="ml-2">{state.word}</p>
        ) : isTyping ? (
          <p className="ml-2 tracking-widest">...</p>
        ) : undefined}
      </div>
    </div>
  );
}
