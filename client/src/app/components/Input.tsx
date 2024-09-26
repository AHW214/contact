import {
  type ChangeEventHandler,
  type KeyboardEventHandler,
  forwardRef,
} from "react";

export type Props = {
  onChange: ChangeEventHandler<HTMLInputElement>;
  onEnter: KeyboardEventHandler<HTMLInputElement>;
  placeholder: string;
  value: string;
};

const Input = forwardRef<HTMLInputElement, Props>(
  ({ onChange, onEnter, placeholder, value }, ref) => {
    const onKeyDown: KeyboardEventHandler<HTMLInputElement> = (ev) => {
      if (ev.key === "Enter") {
        onEnter(ev);
      }
    };

    return (
      <input
        className="p-1 min-w-96 border-2 border-zinc-300 rounded-lg focus:outline-zinc-800"
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
