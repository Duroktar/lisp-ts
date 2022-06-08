import { type Character } from "./char";
import { type Num } from "./num";
import { type Pair } from "./pair";
import { type Proc } from "./proc";
import { type Vector } from "./vec";

export type Atom = symbol | string;
export type List = Pair | symbol;
export type Form = List | Pair | Atom | Character | Num | Vector | Proc;
