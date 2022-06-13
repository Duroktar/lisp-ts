import { Character } from "./core/data/char";
import { EMPTY, FALSE, TRUE } from "./core/const";
import { Atom, Form, List } from "./core/forms";
import { Pair } from "./core/data/pair";
import { isNativeProc, isProc, NativeProc, Procedure } from "./core/data/proc";
import { Sym } from "./core/data/sym";
import { isSyntaxRulesDef, SyntaxRulesDef } from "./core/data/syntax";
import { Vector } from "./core/data/vec";

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

export const isTruthy = (e: Form): boolean => !isF(e) && !isNil(e);
export const isNil = (e: Form): boolean => e === Sym('nil');
export const isF = (e: Form): boolean => e === FALSE;
export const isT = (e: Form): boolean => e === TRUE;
export const toL = (e: boolean): Form => e ? TRUE : FALSE;
