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

const PlayerInput = ({
  currentAction,
  hideContactResult,
  ref,
  state,
  value,
  ...restProps
}: PlayerInputProps) => {
  const isSpectating = state.tag === "spectatingContact";

  return (
    <Input
      {...restProps}
      className={`${
        currentAction.tag === "hinting" && !isSpectating
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
          : currentAction.tag === "contact"
          ? "type your guess here..."
          : "type your hint here..."
      }
      value={isSpectating ? "" : value}
      disabled={isSpectating}
    />
  );
};

export default PlayerInput;
