import {
  type ChangeEventHandler,
  type KeyboardEventHandler,
  forwardRef,
} from "react";

import type { ContactState } from "contact/app/data/player";

export type Props = {
  contactState: ContactState | undefined;
  onChange: ChangeEventHandler<HTMLInputElement>;
  onEnter: KeyboardEventHandler<HTMLInputElement>;
  placeholder: string;
  value: string;
};

const Input = forwardRef<HTMLInputElement, Props>(
  ({ contactState, onChange, onEnter, placeholder, value }, ref) => {
    const onKeyDown: KeyboardEventHandler<HTMLInputElement> = (ev) => {
      if (ev.key === "Enter") {
        onEnter(ev);
      }
    };

    const borderColor =
      contactState === "declared"
        ? "border-blue-800"
        : contactState === "failed"
        ? "border-red-800"
        : contactState === "succeeded"
        ? "border-green-800"
        : "border-zinc-300";

    return (
      <input
        className={`p-1 min-w-96 border-2 rounded-lg focus:outline-zinc-800 ${borderColor}`}
        onChange={onChange}
        onKeyDown={onKeyDown}
        placeholder={placeholder}
        ref={ref}
        value={value}
      ></input>
    );
  }
);

export default Input;
