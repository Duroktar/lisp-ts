import { Sym } from "./sym";
import { Atom, Expr } from "./terms";

export const EMPTY: Expr = [];
export const TRUE:  Atom = Sym('#t');
export const FALSE: Atom = Sym('#f');
