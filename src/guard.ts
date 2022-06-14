import { EMPTY, expressionKeywords, FALSE, peculiarIdentifiers, specialInitials, specialSubsequents, syntacticKeywords, TRUE } from "./core/const";
import { Character } from "./core/data/char";
import { Pair } from "./core/data/pair";
import { NativeProc, Procedure } from "./core/data/proc";
import { Sym } from "./core/data/sym";
import { isSyntaxRulesDef, SyntaxRulesDef } from "./core/data/syntax";
import { Vector } from "./core/data/vec";
import { Atom, Form, List } from "./core/form";
import { File, InPort, IOPort, OutPort } from "./core/data/port";
import { character, digit, expressionKeyword, identifier, initial, letter, peculiarIdentifier, specialInitial, specialSubsequent, subsequent, syntacticKeyword, whitespace } from "./interface/syntax";

export const isList = (x: Form): x is List => (isPair(x) && x.isList()) || isEmpty(x);
export const isPair = (x: Form): x is Pair => Pair.is(x);
export const isAtom = (x: Form): x is Atom => isSym(x) || isString(x) || isNum(x);
export const isSym = (x: Form): x is symbol => typeof x === 'symbol';
export const isNum = (x: Form): x is number => typeof x === 'number';
export const isVec = (x: Form): x is Vector => x instanceof Vector;
export const isString = (c: Form): c is string => typeof c === 'string';
export const isChar = (x: Form): x is Character => x instanceof Character;
export const isEmpty = (x: Form): x is symbol => x === EMPTY;
export const isNone = (x: unknown): x is undefined | null => x === undefined || x === null;
export const isExpr = (x: Form): x is Form => isPair(x) || isAtom(x);
export const isConst = (x: Form) => isNum(x) || isString(x) || isChar(x);
export const isIdent = (x: Form): x is symbol => isSym(x) && !isEmpty(x);

export const isCallable = (x: Form): x is Procedure | NativeProc | SyntaxRulesDef => {
  return isProc(x) || isNativeProc(x) || isSyntaxRulesDef(x);
};

export const isNativeProc = (x: unknown): x is NativeProc => x instanceof NativeProc;
export const isProc = (x: unknown): x is Procedure => x instanceof Procedure;

export const isInputPort = (obj: any) => obj instanceof InPort
export const isOutputPort = (obj: any) => obj instanceof OutPort
export const isIOPort = (obj: any) => obj instanceof IOPort

export const isTruthy = (e: Form): boolean => !isF(e) && !isNil(e);
export const isNil = (e: Form): boolean => e === Sym('nil');
export const isF = (e: Form): boolean => e === FALSE;
export const isT = (e: Form): boolean => e === TRUE;

export const isEofString = (obj: any) => obj === File.EOF_STRING
// export const isDelimiter = (c: any): c is delimiter => isWhiteSpace(c) || delimiters.has(c);
// export const isQuoteChar = (c: any): boolean => quotes[c] !== undefined;
// export const isWhiteSpace = (c: any): c is whitespace => c === ' ' || c === '\t' || c === '\n'

export const isNewline = (c: any): c is whitespace => c === '\n'
export const isWhiteSpace = (c: any): c is whitespace => c === ' ' || c === '\t' || isNewline(c)
export const isIdentifier = (c: any): c is identifier => {
  const [x, ...xs] = isString(c) ? c : []
  return (isInitial(x) && xs.every(isSubsequent)) || isPeculiarIdentifier(c)
};
export const isInitial = (c: any): c is initial => isLetter(c) || isSpecialInitial(c)
export const isLetter = (c: any): c is letter => !! (isString(c) && c.match(/^[A-z]*$/));
export const isSubsequent = (c: any): c is subsequent => isInitial(c) || isDigit(c) || isSpecialSubsequent(c);
export const isDigit = (c: any): c is digit => !! (isString(c) && c.match(/^[0-9]*$/));
export const isCharacter = (c: any): c is character => !! (isString(c) && c.match(/^#\\([A-z]|(space|newline)){1}$/));

export const isSpecialInitial = (c: any): c is specialInitial => specialInitials.has(c);
export const isSpecialSubsequent = (c: any): c is specialSubsequent => specialSubsequents.has(c);
export const isPeculiarIdentifier = (c: any): c is peculiarIdentifier => peculiarIdentifiers.has(c);
export const isSyntacticKeyword = (c: any): c is syntacticKeyword => syntacticKeywords.has(c);
export const isExpressionKeyword = (c: any): c is expressionKeyword => expressionKeywords.has(c);
