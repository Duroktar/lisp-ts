
export const Sym = Symbol.for;

export const SymTable = {
  APPEND: Sym('append'),
  ATOM: Sym('atom'),
  BEGIN: Sym('begin'),
  CAR: Sym('car'),
  CDR: Sym('cdr'),
  COND: Sym('cond'),
  CONS: Sym('cons'),
  DEFINE: Sym('define'),
  DEFINEMACRO: Sym('define-macro'),
  DEFUN: Sym('defun'),
  DO: Sym('do'),
  IF: Sym('if'),
  EQ: Sym('eq'),
  LAMBDA: Sym('lambda'),
  LET: Sym('let'),
  SET: Sym('set!'),
  QUASIQUOTE: Sym('quasiquote'),
  QUOTE: Sym('quote'),
  UNQUOTE: Sym('unquote'),
  UNQUOTESPLICING: Sym('unquote-splicing'),
};
