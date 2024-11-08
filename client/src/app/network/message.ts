import { Codec } from "purify-ts";
import * as C from "purify-ts/Codec";

import { type PlayerId, playerIdCodec } from "contact/app/data/player";

namespace Codec_ {
  export const tagged = <T extends string, U extends { [x: string]: any }>(
    tag: T,
    data: U
  ) =>
    Codec.interface({
      tag: C.exactly(tag),
      data: Codec.interface(data),
    });
}

export type ClientMessage =
  | { tag: "chooseName"; data: { name: string } }
  | { tag: "contact"; data: { player: PlayerId; word: string } }
  | { tag: "hint"; data: { description: string } };

export type ServerMessage =
  | {
      tag: "declaredContact";
      data: { fromPlayer: PlayerId; toPlayer: PlayerId };
    }
  | { tag: "joinedGame"; data: { playerName: PlayerId } }
  | { tag: "leftGame"; data: { playerName: PlayerId } }
  | {
      tag: "revealedContact";
      data: {
        from: { player: PlayerId; word: string };
        to: { player: PlayerId; word: string };
        success: boolean;
      };
    }
  | { tag: "sharedHint"; data: { description: string; player: PlayerId } }
  | {
      tag: "syncGame";
      data: { myPlayerName: PlayerId; players: PlayerId[] };
    };

export const clientMessageCodec: Codec<ClientMessage> = C.oneOf([
  Codec_.tagged("chooseName", { name: C.string }),
  Codec_.tagged("contact", { player: playerIdCodec, word: C.string }),
  Codec_.tagged("hint", { description: C.string }),
]);

export const serverMessageCodec: Codec<ServerMessage> = C.oneOf([
  Codec_.tagged("declaredContact", {
    fromPlayer: playerIdCodec,
    toPlayer: playerIdCodec,
  }),
  Codec_.tagged("joinedGame", { playerName: playerIdCodec }),
  Codec_.tagged("leftGame", { playerName: playerIdCodec }),
  Codec_.tagged("revealedContact", {
    from: Codec.interface({ player: playerIdCodec, word: C.string }),
    to: Codec.interface({ player: playerIdCodec, word: C.string }),
    success: C.boolean,
  }),
  Codec_.tagged("sharedHint", { description: C.string, player: playerIdCodec }),
  Codec_.tagged("syncGame", {
    myPlayerName: playerIdCodec,
    players: C.array(playerIdCodec),
  }),
]);
