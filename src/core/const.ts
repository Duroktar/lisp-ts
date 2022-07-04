import { Sym } from "./data/sym";

export const NIL:   symbol = Sym('()');
export const TRUE:  symbol = Sym('#t');
export const FALSE: symbol = Sym('#f');
export const UNDEF: symbol = Sym('#<undef>');
export const EOF: symbol = Sym('#<eof-object>')

export const ellipsis = Symbol.for('...')

export const specialInitials     = new Set('! $ % & * / : = < > ? ~ _ ^'.split(' '));
export const specialSubsequents  = new Set('+ - . @'.split(' '));
export const peculiarIdentifiers = new Set('+ - ...'.split(' '));
export const delimiters          = new Set('( ) " ;'.split(' '));

export const expressionKeywords  = new Set([
  'quote', 'lambda', 'if', 'set!', 'begin',
  'cond', 'and', 'or', 'case', 'let', 'let*',
  'letrec', 'do', 'delay', 'quasiquote']);

export const syntacticKeywords = new Set([...expressionKeywords,
  'else', '=>', 'define', 'unquote', 'unquote-splicing']);
