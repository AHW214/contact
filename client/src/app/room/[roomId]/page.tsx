import Room from "contact/app/components/room";

const REST_URL = "http://localhost:1234";
const WEB_SOCKET_URL = "ws://localhost:1234";

export default async function Page({
  params,
}: {
  params: Promise<{ roomId: string }>;
}) {
  const { roomId } = await params;

  const res = await fetch(`${REST_URL}/room/${roomId}`);
  const { players } = await res.json();

  return (
    <div className="grid grid-rows-[20px_1fr_20px] items-center justify-items-center min-h-screen p-8 pb-20 gap-16 sm:p-20 font-[family-name:var(--font-geist-sans)]">
      <main className="flex flex-col gap-8 row-start-2 items-center sm:items-start">
        {Room({ players, roomId, webSocketUrl: WEB_SOCKET_URL })}
      </main>
    </div>
  );
}
