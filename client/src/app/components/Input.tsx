import { forwardRef } from "react";

const Input = forwardRef<HTMLInputElement>((_props, ref) => {
  return (
    <input
      className="p-1 min-w-96 border-2 border-zinc-300 rounded-lg focus:outline-zinc-800"
      placeholder="guess here..."
      ref={ref}
    ></input>
  );
});

export default Input;
