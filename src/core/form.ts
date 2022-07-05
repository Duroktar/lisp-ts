import { type Character } from "./data/char";
import type { Binding, Expansion } from "./callable/macro";
import { type Pair } from "./data/pair";
import { type Closure } from "./callable/proc";
import { type Vector } from "./data/vec";
import { type MutableString } from "./data/string";

export type Atom = symbol | number | MutableString;
export type List = Pair | symbol;
export type Form = List | Pair | Atom | Character | Vector | Closure | Binding | Expansion;
