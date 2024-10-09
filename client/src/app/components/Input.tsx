import {
  type ChangeEventHandler,
  type KeyboardEventHandler,
  type InputHTMLAttributes,
  forwardRef,
} from "react";

export interface Props extends InputHTMLAttributes<HTMLInputElement> {
  onChange: ChangeEventHandler<HTMLInputElement>;
  onEnter: KeyboardEventHandler<HTMLInputElement>;
}

const Input = forwardRef<HTMLInputElement, Props>(
  ({ onChange, onEnter, ...props }, ref) => {
    const onKeyDown: KeyboardEventHandler<HTMLInputElement> = (ev) => {
      if (ev.key === "Enter") {
        onEnter(ev);
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
