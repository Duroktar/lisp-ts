import type { specialInitial, specialSubsequent, peculiarIdentifier, syntacticKeyword, expressionKeyword } from "../syntax";
import { Sym } from "./sym";
import type { Term } from "./terms";

export const EMPTY: Term   = [];
export const NIL:   symbol = Sym('nil');
export const TRUE:  symbol = Sym('#t');
export const FALSE: symbol = Sym('#f');
export const UNDEF: symbol = Sym('#<undef>');
export const EOF = '#<eof-object>'

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

export const isSpecialInitial = (c: any): c is specialInitial => specialInitials.has(c);
export const isSpecialSubsequent = (c: any): c is specialSubsequent => specialSubsequents.has(c);
export const isPeculiarIdentifier = (c: any): c is peculiarIdentifier => peculiarIdentifiers.has(c);
export const isSyntacticKeyword = (c: any): c is syntacticKeyword => syntacticKeywords.has(c);
export const isExpressionKeyword = (c: any): c is expressionKeyword => expressionKeywords.has(c);
