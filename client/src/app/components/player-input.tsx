import type { Ref } from "react";

import Input, { type InputProps } from "contact/app/components/input";
// TODO - lift into own module?
import type { PlayerState } from "contact/app/components/player";
import type { PlayerAction } from "contact/app/data/player";

export interface PlayerInputProps extends InputProps {
  currentAction: PlayerAction;
  hideContactResult: boolean;
  ref: Ref<HTMLInputElement>;
  state: PlayerState;
  value: string;
}

export default function PlayerInput({
  currentAction,
  hideContactResult,
  ref,
  state,
  value,
  ...restProps
}: PlayerInputProps) {
  const isSpectating = state.tag === "spectatingContact";
  const isInputEmboldened =
    (currentAction.tag === "hinting" && !isSpectating) ||
    (currentAction.tag === "contact" && currentAction.confirmed);

  return (
    <Input
      {...restProps}
      className={`${
        isInputEmboldened
          ? "font-bold caret-transparent border-zinc-800"
          : "font-normal caret-inherit border-inherit"
      } ${
        state.tag === "performingContact" && !hideContactResult
          ? state.contact.tag === "declared"
            ? "border-blue-800"
            : state.contact.success
            ? "border-green-800"
            : "border-red-800"
          : "border-zinc-300"
      } ${isSpectating ? "cursor-not-allowed" : "cursor-auto"}`}
      ref={ref}
      placeholder={
        isSpectating
          ? "...suspense..."
          : state.tag === "performingContact"
          ? "type your guess here..."
          : "type your hint here..."
      }
      value={isSpectating ? "" : value}
      disabled={isSpectating}
    />
  );
}
