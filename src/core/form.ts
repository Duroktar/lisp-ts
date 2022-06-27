import { type Character } from "./data/char";
import type { Binding, Expansion } from "./data/macro";
import { type Pair } from "./data/pair";
import { type Closure } from "./data/proc";
import { type Vector } from "./data/vec";

export type Atom = symbol | string | number;
export type List = Pair | symbol;
export type Form = List | Pair | Atom | Character | Vector | Closure | Binding | Expansion;
