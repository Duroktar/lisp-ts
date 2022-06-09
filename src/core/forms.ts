import { type Character } from "./char";
import { type Pair } from "./pair";
import { type Closure } from "./proc";
import { type Vector } from "./vec";

export type Atom = symbol | string | number;
export type List = Pair | symbol;
export type Form = List | Pair | Atom | Character | Vector | Closure;
