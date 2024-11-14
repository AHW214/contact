"use client";

import { type MouseEventHandler, type RefObject, useState } from "react";

import type { HintState } from "contact/app/data/player";

// TODO - merge with HintState
export type ContactState =
  | { tag: "declared" }
  | { tag: "revealed"; success: boolean; word: string };

export type Props = {
  contactState: ContactState | undefined;
  countdownMillis: number | undefined;
  hintState: HintState;
  id: string;
  inputRef: RefObject<HTMLInputElement>;
  isTyping: boolean;
  name: string;
  onClickCancel: MouseEventHandler<HTMLDivElement>;
  onClickContact: MouseEventHandler<HTMLDivElement>;
};

export default function Player({
  contactState,
  countdownMillis,
  hintState,
  id,
  inputRef,
  isTyping,
  name,
  onClickCancel,
  onClickContact,
}: Props) {
  const [isSelected, setIsSelected] = useState<boolean>(false);
  const borderColor =
    contactState === undefined
      ? isSelected
        ? "border-zinc-800"
        : "border-zinc-300"
      : contactState.tag === "declared"
      ? "border-blue-800"
      : contactState.success
      ? "border-green-800"
      : "border-red-800";

  // TODO - clean up
  const classes =
    contactState !== undefined
      ? {
          borderColor,
          cursor: "auto",
          hoverVisibility:
            countdownMillis !== undefined
              ? "group-hover:visible"
              : "group-hover:invisible",
          visibility: countdownMillis !== undefined ? "visible" : "invisible",
        }
      : {
          borderColor,
          cursor: "cursor-pointer",
          hoverVisibility: "group-hover:visible",
          visibility: "invisible",
        };

  const coverText =
    contactState !== undefined && countdownMillis !== undefined
      ? `${countdownMillis / 1000}`
      : isSelected
      ? "cancel"
      : "contact";

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
    <div key={id} className="relative group">
      <div
        className={`absolute top-0 left-0 w-full h-full flex items-center justify-center rounded-lg bg-zinc-800 ${classes.visibility} ${classes.hoverVisibility}`}
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
        {contactState !== undefined && contactState.tag === "revealed" ? (
          <p className="ml-2">{contactState.word}</p>
        ) : hintState.tag === "sharing" && hintState.word !== "" ? (
          <p className="ml-2">{hintState.word}</p>
        ) : isTyping ? (
          <p className="ml-2 tracking-widest">...</p>
        ) : undefined}
      </div>
    </div>
  );
}
