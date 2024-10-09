import {
  type ChangeEventHandler,
  type KeyboardEventHandler,
  type InputHTMLAttributes,
  forwardRef,
} from "react";

import type { ContactState } from "contact/app/data/player";

export interface Props extends InputHTMLAttributes<HTMLInputElement> {
  onChange: ChangeEventHandler<HTMLInputElement>;
  onEnter: KeyboardEventHandler<HTMLInputElement>;
  onEscape: KeyboardEventHandler<HTMLInputElement>;
}

const Input = forwardRef<HTMLInputElement, Props>(
  ({ onChange, onEnter, onEscape, ...props }, ref) => {
    const onKeyDown: KeyboardEventHandler<HTMLInputElement> = (ev) => {
      if (ev.key === "Enter") {
        onEnter(ev);
      } else if (ev.key === "Escape") {
        onEscape(ev);
      }
    };

    const { className, ...restProps } = props;

    return (
      <input
        {...restProps}
        className={`p-1 min-w-96 border-2 rounded-lg focus:outline-zinc-800 ${className}`}
        onChange={onChange}
        onKeyDown={onKeyDown}
        ref={ref}
      ></input>
    );
  }
);

export default Input;
