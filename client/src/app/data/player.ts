import type { WithBrand } from "@coderspirit/nominal";
import { Codec } from "purify-ts/Codec";
import * as C from "purify-ts/Codec";

export type PlayerId = WithBrand<string, "Player">;

export type HintState = { tag: "sharing"; word: string } | { tag: "thinking" };

export type ContactState =
  | { tag: "guessing"; didMisclick: boolean }
  | { tag: "confirmed" };

export type PlayerAction =
  | { tag: "contact"; state: ContactState }
  | { tag: "hinting" }
  | { tag: "thinking" };

export type Player = {
  hintState: HintState;
  // TODO - implement typing indicator
  isTyping: boolean;
  name: PlayerId;
};

export const playerIdCodec: Codec<PlayerId> = Codec.custom({
  decode: (input) => C.string.decode(input).map((str) => str as PlayerId),
  encode: (input) => input,
});
