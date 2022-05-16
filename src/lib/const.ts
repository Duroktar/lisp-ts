import { Sym } from "./sym";
import { Expr } from "./terms";

export const EMPTY: Expr   = [];
export const TRUE:  symbol = Sym('#t');
export const FALSE: symbol = Sym('#f');
export const UNDEF: symbol = Sym('#<undef>');
