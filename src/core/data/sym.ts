
export const Sym = Symbol.for;

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
