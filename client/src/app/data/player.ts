import type { WithBrand } from "@coderspirit/nominal";
import { Codec } from "purify-ts/Codec";
import * as C from "purify-ts/Codec";

export type PlayerId = WithBrand<string, "Player">;

export type ContactState = "declared" | "failed" | "succeeded";

export type Player = {
  contactState: ContactState | undefined;
  hint: string | undefined;
  id: PlayerId;
  isTyping: boolean;
  name: string;
};

export const playerIdCodec: Codec<PlayerId> = Codec.custom({
  decode: (input) => C.string.decode(input).map((str) => str as PlayerId),
  encode: (input) => input,
});
