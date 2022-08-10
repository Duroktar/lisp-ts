import { Form } from "../form";
import { type SymbolToken } from "../read";

export class Symbol {
  constructor(
    public name: string,
    public token?: SymbolToken,
  ) {
    this.value = global.Symbol.for(name)
  }

  public value: symbol;

  public equal(other: Form): boolean {
    return other instanceof Symbol && other.eq(this)
  }

  public eq(other: Symbol): boolean {
    return other.value === this.value
  }
}

export const Sym = (name: string, token?: SymbolToken) => {
  return new Symbol(name, token)
};

export const SymTable = {
  APPEND: Sym('append'),
  APPLY: Sym('apply'),
  ATOM: Sym('atom?'),
  BEGIN: Sym('begin'),
  CAR: Sym('car'),
  CDR: Sym('cdr'),
  COND: Sym('cond'),
  CONS: Sym('cons'),
  DEFINE: Sym('define'),
  DEFINESYNTAX: Sym('define-syntax'),
  DO: Sym('do'),
  EQ: Sym('eq'),
  IF: Sym('if'),
  LAMBDA: Sym('lambda'),
  LET: Sym('let'),
  QUASIQUOTE: Sym('quasiquote'),
  QUOTE: Sym('quote'),
  SET: Sym('set!'),
  UNQUOTE: Sym('unquote'),
  UNQUOTESPLICING: Sym('unquote-splicing'),
};

export const Symbols =
  Object.fromEntries(
    Object
      .entries(SymTable)
      .map(([k, v]) => [k, v.value]))
