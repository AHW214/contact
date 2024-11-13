import type { Ref } from "react";

import Input, { type InputProps } from "contact/app/components/input";
// TODO - lift into own module?
import type { ContactState } from "contact/app/components/player";
import type { PlayerAction } from "contact/app/data/player";

export interface PlayerInputProps extends InputProps {
  contactState: ContactState | undefined;
  currentAction: PlayerAction;
  isAnyoneContacting: boolean;
  ref: Ref<HTMLInputElement>;
}

const PlayerInput = ({
  contactState,
  currentAction,
  isAnyoneContacting,
  ref,
  value,
  ...restProps
}: PlayerInputProps) => {
  return (
    <Input
      {...restProps}
      className={`${
        currentAction.tag === "hinting" && !isAnyoneContacting
          ? "font-bold caret-transparent border-zinc-800"
          : "font-normal caret-inherit border-inherit"
      } ${
        contactState === undefined
          ? "border-zinc-300"
          : contactState.tag === "declared"
          ? "border-blue-800"
          : contactState.success
          ? "border-green-800"
          : "border-red-800"
      } ${isAnyoneContacting ? "cursor-not-allowed" : "cursor-auto"}`}
      ref={ref}
      placeholder={
        isAnyoneContacting
          ? "...suspense..."
          : currentAction.tag === "contact"
          ? "type your guess here..."
          : "type your hint here..."
      }
      value={isAnyoneContacting ? "" : value}
      disabled={isAnyoneContacting}
    />
  );
};

export default PlayerInput;
