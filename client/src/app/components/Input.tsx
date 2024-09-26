import {
  type ChangeEventHandler,
  type KeyboardEvent,
  type KeyboardEventHandler,
  forwardRef,
} from "react";

export type Props = {
  onChange: ChangeEventHandler<HTMLInputElement>;
  onEnter: KeyboardEventHandler<HTMLInputElement>;
};

const Input = forwardRef<HTMLInputElement, Props>(
  ({ onChange, onEnter }, ref) => {
    const onKeyDown = (ev: KeyboardEvent<HTMLInputElement>) => {
      if (ev.key === "Enter") {
        onEnter(ev);
      }
    };

    return (
      <input
        className="p-1 min-w-96 border-2 border-zinc-300 rounded-lg focus:outline-zinc-800"
        onChange={onChange}
        onKeyDown={onKeyDown}
        placeholder="guess here..."
        ref={ref}
      ></input>
    );
  }
);

export default Input;
