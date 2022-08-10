import { type Binding } from "./binding";
import { type Expansion } from "./callable/macro/expansion";
import { type Closure } from "./callable/proc";
import { type Syntax } from "./callable/syntax";
import { type Character } from "./data/char";
import { type Number } from "./data/num";
import { type Pair } from "./data/pair";
import { type MutableString } from "./data/string";
import { type Symbol } from "./data/sym";
import { type Vector } from "./data/vec";
import { TSchemeModule } from "./module/base";
import { Port } from "./port";

export type List = Pair | Symbol;

export type Atom =
  | Symbol
  | Number
  | MutableString
  ;

export type Form =
  | List
  | Atom
  | Character
  | Vector
  | Binding
  | Closure
  | Expansion
  | Syntax
  | Port
  | TSchemeModule
  ;
