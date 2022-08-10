import { Sym, Symbol } from "./data/sym";

export const NIL:   Symbol = Sym('()');
export const TRUE:  Symbol = Sym('#t');
export const FALSE: Symbol = Sym('#f');
export const UNDEF: Symbol = Sym('#<undef>');
export const EOF: Symbol = Sym('#<eof-object>')

export const ellipsis = Sym('...')

export const specialInitials     = new Set('! $ % & * / : = < > ? ~ _ ^'.split(' '));
export const specialSubsequents  = new Set('+ - . @'.split(' '));
export const peculiarIdentifiers = new Set('+ - ...'.split(' '));
export const delimiters          = new Set('( ) " ;'.split(' '));

export const expressionKeywords  = new Set([
  'quote', 'lambda', 'if', 'set!', 'begin',
  'cond', 'and', 'or', 'case', 'let', 'let*',
  'letrec', 'do', 'delay', 'quasiquote']);

export const syntacticKeywords = new Set([
  'else', '=>', 'define', 'unquote', 'unquote-splicing',
  ...expressionKeywords,
]);
