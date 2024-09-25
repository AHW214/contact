import { type ChangeEventHandler, forwardRef } from "react";

export type Props = {
  onChange: ChangeEventHandler<HTMLInputElement>;
};

const Input = forwardRef<HTMLInputElement, Props>(({ onChange }, ref) => {
  return (
    <input
      className="p-1 min-w-96 border-2 border-zinc-300 rounded-lg focus:outline-zinc-800"
      onChange={onChange}
      placeholder="guess here..."
      ref={ref}
    ></input>
  );
});

export default Input;
