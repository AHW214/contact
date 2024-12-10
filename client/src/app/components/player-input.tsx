import type { Ref } from "react";

import Input, { type InputProps } from "contact/app/components/input";
// TODO - lift into own module?
import type { PlayerState } from "contact/app/components/player";
import type { PlayerAction } from "contact/app/data/player";

export interface PlayerInputProps extends InputProps {
  currentAction: PlayerAction;
  hideContactResult: boolean;
  ref: Ref<HTMLInputElement>;
  secretWordPrefix: string;
  state: PlayerState;
  value: string;
  misclick: boolean;
}

const guessMask = (
  secretWordPrefix: string,
  misclick: boolean,
  guess: string
) => {
  const prefixTyped = guess.slice(0, secretWordPrefix.length);
  const prefixRemaining = secretWordPrefix.slice(guess.length);

  return (
    <div className="absolute flex p-1.5 font-normal">
      <div className="invisible">{prefixTyped}</div>
      <div
        className={`text-zinc-400 ${
          misclick ? "first-letter:text-red-400" : ""
        }`}
      >
        {prefixRemaining}
      </div>
    </div>
  );
};

export default function PlayerInput({
  currentAction,
  hideContactResult,
  ref,
  secretWordPrefix,
  state,
  value,
  misclick,
  ...restProps
}: PlayerInputProps) {
  const isSpectating = state.tag === "spectatingContact";

  const isGuessing =
    state.tag === "performingContact" &&
    (state.contact.tag === "declared" || hideContactResult);

  const wasSilentContact =
    state.tag === "performingContact" &&
    state.contact.tag === "revealed" &&
    state.contact.word === undefined;

  const isInputEmboldened =
    (currentAction.tag === "hinting" && !isSpectating) ||
    (currentAction.tag === "contact" && currentAction.confirmed);

  return (
    <div className="relative">
      {isGuessing && guessMask(secretWordPrefix, true, value)}
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
        } ${isSpectating ? "cursor-not-allowed" : "cursor-auto"}
        `}
        ref={ref}
        placeholder={
          isSpectating
            ? "...suspense..."
            : state.tag === "hintingWord"
            ? "type your hint here..."
            : ""
        }
        value={
          isSpectating
            ? ""
            : wasSilentContact && !hideContactResult
            ? "..."
            : value
        }
        disabled={isSpectating}
      />
    </div>
  );
}
