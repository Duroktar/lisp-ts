import { type Character } from "./char";
import { type Vector } from "./vec";

export type Atom = symbol | string | number;
export type List = Form[];
export type Form = List | Atom | Vector | Character;
