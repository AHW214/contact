export type Props = {
  id: string;
  name: string;
};

export default function Wordmaster({ name }: Props) {
  return (
    <div className="px-0 pt-0 w-40 h-16 p-2 border-2 border-zinc-800 rounded-lg bg-zinc-800 text-zinc-100">
      <div className="w-fit -mx-[.125rem] -mt-[.125rem] px-2 border-2 border-zinc-600 border-l-zinc-800 border-t-zinc-800 rounded-tl-lg rounded-br-lg ">
        <h3>{name}</h3>
      </div>
      <p className="ml-2">placeholder</p>
    </div>
  );
}
