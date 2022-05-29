import { type Character } from "./char";
import { type Vector } from "./vec";

export type Atom = symbol | string | number;
export type List = Term[];
export type Term = List | Atom | Vector | Character;
