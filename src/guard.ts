import { NIL, FALSE, TRUE } from "./core/const";
import { Character } from "./core/data/char";
import { Pair } from "./core/data/pair";
import { File, InPort, IOPort, OutPort } from "./core/port";
import { Procedure } from "./core/callable/proc";
import { NativeFunc } from "./core/callable/func";
import { Symbol } from "./core/data/sym";
import { Vector } from "./core/data/vec";
import { Atom, Form, List } from "./core/form";
import { whitespace } from "./interface/syntax";
import { MutableString } from "./core/data/string";
import { Binding } from "./core/binding";
import { Macro } from "./core/callable/macro";
import { Expansion } from "./core/callable/macro/expansion";
import { Syntax, isSyntax } from "./core/callable/syntax";
import { Number } from "./core/data/num";

export const isList = (x: any): x is List => (isPair(x) && x.isList()) || isEmpty(x);
export const isAtom = (x: any): x is Atom => isSym(x) || isString(x) || isNum(x);
export const isPair = (x: any): x is Pair => x instanceof Pair;
export const isSym = (x: any): x is Symbol => x instanceof Symbol;
export const isNum = (x: any): x is Number => x instanceof Number;
export const isVec = (x: any): x is Vector => x instanceof Vector;
export const isString = (c: any): c is MutableString => c instanceof MutableString;
export const isChar = (x: any): x is Character => x instanceof Character;
export const isEmpty = (x: any): x is Symbol => x === NIL;
export const isExpr = (x: any): x is Form => isPair(x) || isAtom(x);
export const isConst = (x: any) => isNum(x) || isString(x) || isChar(x);
export const isIdent = (x: any): x is Symbol => isSym(x) && !isEmpty(x) && !isBool(x);
export const isMacro = (obj: any): obj is Macro => obj instanceof Macro;
export const isBinding = (obj: any): obj is Binding => obj instanceof Binding;
export const isExpansion = (obj: any): obj is Expansion => obj instanceof Expansion;

export const isNullOrUndefined = (x: unknown): x is undefined | null => x === undefined || x === null;

export const isCallable = (x: Form | Syntax): x is Procedure | NativeFunc => {
  return isProc(x) || isNativeProc(x);
};

export const isNativeProc = (x: unknown): x is NativeFunc => x instanceof NativeFunc;
export const isProc = (x: unknown): x is Procedure => x instanceof Procedure;

export const isInputPort = (obj: any) => obj instanceof InPort
export const isOutputPort = (obj: any) => obj instanceof OutPort
export const isIOPort = (obj: any) => obj instanceof IOPort

export const isNil = (e: Form): e is Symbol => NIL.equal(e);
export const isF = (e: Form): boolean => e === FALSE;
export const isT = (e: Form): boolean => e === TRUE;
export const isBool = (e: Form): boolean => e === TRUE || e === FALSE;

export const isEofString = (obj: any) => obj === File.EOF_STRING

export const isNewline = (c: any): c is whitespace => c === '\n'
export const isWhiteSpace = (c: any): c is whitespace => c === ' ' || c === '\t' || isNewline(c)

