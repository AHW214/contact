import { Codec } from "purify-ts";
import * as C from "purify-ts/Codec";

import { type PlayerId, playerIdCodec } from "contact/app/data/player";

namespace Codec_ {
  // TODO - merge this module with Purify.Codec (underscore name kind of silly)

  // TODO - add codec to convert between undefined in TS and null in JSON
  // (Maybe datatype™)

  export const tagged = <T extends string, U extends { [x: string]: any }>(
    tag: T,
    data: U
  ) =>
    Codec.interface({
      tag: C.exactly(tag),
      data: Codec.interface(data),
    });

  export const nullary = <T extends string>(tag: T) =>
    Codec.interface({
      tag: C.exactly(tag),
    });
}

export type ClientMessage =
  | { tag: "chooseName"; data: { name: string } }
  | { tag: "clearHint" }
  | { tag: "confirmContact"; data: { maybeWord: string | null } }
  | { tag: "declareContact"; data: { player: PlayerId } }
  | { tag: "disconnect" }
  | { tag: "hint"; data: { description: string } };

export type SyncGamePlayer = { name: PlayerId; message: string };

export type RevealedContactResult = {
  isGameOver: boolean;
  revealedLetter: string;
};

export type ServerMessage =
  | { tag: "clearedHint"; data: { playerName: PlayerId } }
  | { tag: "confirmedContact" }
  | {
      tag: "declaredContact";
      data: { fromPlayer: PlayerId; toPlayer: PlayerId };
    }
  | { tag: "endContact" }
  | { tag: "joinedGame"; data: { playerName: PlayerId } }
  | { tag: "leftGame"; data: { playerName: PlayerId } }
  | {
      tag: "revealedContact";
      data: {
        guessedWord: string | null;
        guessingPlayer: PlayerId;
        hintedWord: string | null;
        hintingPlayer: PlayerId;
        maybeResult: RevealedContactResult | null;
      };
    }
  | { tag: "sharedHint"; data: { description: string; player: PlayerId } }
  | {
      tag: "syncGame";
      data: {
        myPlayerName: PlayerId;
        players: Record<PlayerId, SyncGamePlayer>;
      };
    };

// export const clientMessageCodec: Codec<ClientMessage> = C.oneOf([
//   Codec_.tagged("chooseName", { name: C.string }),
//   Codec_.nullary("clearHint"),
//   Codec_.tagged("contact", { player: playerIdCodec, word: C.string }),
//   Codec_.nullary("disconnect"),
//   Codec_.tagged("hint", { description: C.string }),
// ]);

const syncGamePlayerCodec: Codec<SyncGamePlayer> = Codec.interface({
  name: playerIdCodec,
  message: C.string,
});

const revealedContactResultCodec: Codec<RevealedContactResult> =
  Codec.interface({
    isGameOver: C.boolean,
    revealedLetter: C.string,
  });

export const serverMessageCodec: Codec<ServerMessage> = C.oneOf([
  Codec_.tagged("clearedHint", { playerName: playerIdCodec }),
  Codec_.nullary("confirmedContact"),
  Codec_.tagged("declaredContact", {
    fromPlayer: playerIdCodec,
    toPlayer: playerIdCodec,
  }),
  Codec_.nullary("endContact"),
  Codec_.tagged("joinedGame", { playerName: playerIdCodec }),
  Codec_.tagged("leftGame", { playerName: playerIdCodec }),
  Codec_.tagged("revealedContact", {
    guessedWord: C.nullable(C.string),
    guessingPlayer: playerIdCodec,
    hintedWord: C.nullable(C.string),
    hintingPlayer: playerIdCodec,
    maybeResult: C.nullable(revealedContactResultCodec),
  }),
  Codec_.tagged("sharedHint", { description: C.string, player: playerIdCodec }),
  Codec_.tagged("syncGame", {
    myPlayerName: playerIdCodec,
    players: C.record(playerIdCodec, syncGamePlayerCodec),
  }),
]);
