import type { WithBrand } from "@coderspirit/nominal";
import { Codec } from "purify-ts/Codec";
import * as C from "purify-ts/Codec";

export type PlayerId = WithBrand<string, "Player">;

export type HintState = { tag: "sharing"; word: string } | { tag: "thinking" };

export type Player = {
  hintState: HintState;
  id: PlayerId;
  isTyping: boolean;
  name: string;
};

export const playerIdCodec: Codec<PlayerId> = Codec.custom({
  decode: (input) => C.string.decode(input).map((str) => str as PlayerId),
  encode: (input) => input,
});
