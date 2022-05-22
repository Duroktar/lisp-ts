import { Sym } from "./sym";
import { Term } from "./terms";

export const EMPTY: Term   = [];
export const TRUE:  symbol = Sym('#t');
export const FALSE: symbol = Sym('#f');
export const UNDEF: symbol = Sym('#<undef>');
