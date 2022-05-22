import * as Lisp from "./core/lisp";
import type { Env } from "./core/env";
import { BaseProcedure, Proc } from "./core/proc";
import type { Atom, Term, List } from "./core/terms";
import { TRUE, FALSE, UNDEF } from "./core/const";
import { Sym, SymTable } from "./core/sym";
import { quotes } from "./core/macro";
import { SyntaxRulesDef } from "./core/syntax";
import { TSchemeModule } from "./core/module";
// import { TSchemeModule } from "./globals";

export const assert = <T extends any>(p: T, msg = ''): T extends false ? never : T => {
  if (p !== true) {
    throw new Error(msg || `assert error: ${p}`);
  } else {
    return p as any;
  }
};


type Exists<P> = Exclude<P, undefined | null>;

export const exists = <P>(p: P, msg = ''): Exists<P> => {
  if (p == null) {
    throw new Error(msg || `exists error: ${p}`);
  } else {
    return p as any;
  }
};

export const expect = <E, P extends boolean | ((e: E) => boolean)>(e: E, p: P, msg = ''): E => {
  if (!((typeof p === 'boolean') ? p : p(e))) {
    throw new Error(msg || `expect error: ${toString(e as any)}`);
  } else {
    return e;
  }
};

export const isPair = (x: unknown) => isList(x) && !isEmpty(x);
export const isList = (x: unknown): x is List => !isSym(x) && Array.isArray(x);
export const isAtom = (x: unknown): x is Atom => isSym(x);
export const isSym = (x: unknown): x is symbol => typeof x === 'symbol';
export const isNum = (x: unknown): x is number => typeof x === 'number';
export const isString = (x: unknown): x is string => typeof x === 'string';
export const isChar = (x: unknown): x is string & {length: 1} => isString(x) && x.length === 1;
export const isEmpty = (x: unknown): x is [] => isList(x) && x.length === 0;
export const isNone = (x: unknown): x is undefined | null => x === undefined || x === null;
export const isCallable = (x: unknown): x is Proc | BaseProcedure => isProc(x) || isNativeFn(x);
export const isProc = (x: unknown): x is Proc => x instanceof Proc;
export const isNativeFn = (x: unknown): x is BaseProcedure => x instanceof BaseProcedure;
export const isExpr = (x: unknown): x is Term => isAtom(x) || isList(x) || isCallable(x) || isString(x) || isNum(x);
export const isConst = (x: unknown) => isNum(x) || isString(x)
export const isIdent = isSym
export const isEqual = (x: unknown, y: unknown) => {
  return JSON.stringify(x) === JSON.stringify(y)
}
export const isEq = (x: unknown, y: unknown) => {
  return (
    isList(x) && x.length === 0 &&
    isList(y) && y.length === 0
  ) || (x === y)
}

export const symName = (s: symbol): string => s.description!;

export const isF = (e: Term): boolean => e === FALSE;
export const isT = (e: Term): boolean => e === TRUE;
export const toL = (e: boolean): Term => e ? TRUE : FALSE;

export const zip = (...rows: Term[][]) => isEmpty(rows) ? [[], []] : rows[0].map((_, c) => rows.map(row => row[c]));

export const map = (func: (m: Term) => Term, m: Term): Term => {
  if (isList(m)) {
    return m.map(child => map(func, child));
  }
  return func(m);
};
export const find = (func: (m: Term, i: number) => boolean, m: Term, __i = 0): Term | undefined => {
  if (isList(m)) {
    for (const child of m) {
      const r = find(func, child, __i++);
      if (r !== undefined)
        return r;
    }
  }
  if (func(m, __i))
    return m;
};

export const getSafe = <T>(a: T[] | any, i: number) => {
  if (isList(a)) return a[i]
  return undefined
}

export const toStringSafe = (expr: Term, inspect = false, lambdaSymbol = 'lambda'): string => {
  try {
    return toString(expr, inspect, lambdaSymbol)
  } catch (e) {
    return UNDEF.description!
  }
}

export const toString = (expr: Term, inspect = false, lambdaSymbol = 'lambda'): string => {
  if (expr === undefined) return UNDEF.description!
  if (isSym(expr)) {
    // if (inspect) return String(expr)
    return expr?.description!;
  }
  if (expr instanceof BaseProcedure) {
    return `(nativefunc ${expr.name})`;
  }
  if (expr instanceof TSchemeModule) {
    return `(module "${expr.basename}")`;
  }
  if (expr instanceof Proc || expr instanceof SyntaxRulesDef) {
    if (inspect) {
      const parms = toString(expr.params, inspect, lambdaSymbol);
      const body = toString(expr.expr, inspect, lambdaSymbol);
      return `(${lambdaSymbol} ${expr.name} ${parms} ${body})`;
    }
    return `(${lambdaSymbol} ${expr.name})`;
  }
  if (isString(expr))
    return `"${expr}"`;
  if (isNone(expr))
    return expr;
  if (isNum(expr))
    return String(expr);
  if (isEmpty(expr))
    return '()';
  if (Lisp.car(expr) === SymTable.LAMBDA) {
    const repr = (<any>Lisp.cdr(expr)).map((x: any) => toString(x, inspect, lambdaSymbol)).join(' ');
    return `(${lambdaSymbol} ${repr})`;
  }
  if (symName(<symbol>Lisp.car(expr)) in quotes) {
    const val = toString(expr[1], inspect, lambdaSymbol);
    return `${quotes[symName(<symbol>Lisp.car(expr))]}${val}`;
  }
  return `(${expr.map(c => toString(c, inspect, lambdaSymbol)).join(' ')})`;
};
export const print = (e: Term, inspect = false, lambdaSymbol = 'lambda'/* λ */): void => {
  console.log(toString(e, inspect, lambdaSymbol));
};

export function mkNativeFunc(env: Env, name: string, params: string[], cb: (args: Term, env: Env) => any): Term | BaseProcedure {
  const func = new class extends BaseProcedure {
    public name = name;
    public env = env;
    public params = params.map(Sym);
    public _call = cb;
  };

  env.set(name, func);
  return func;
}

export const mkLambda = (params: string[] | string, body: Term): Term => {
  return [SymTable.LAMBDA, isList(params) ? params.map(Sym) : Sym(params), body];
};

export const eqC = (a: any) => (b: any) => a === b;

export const searchIdx = (...keys: string[]) => keys.reduce((acc, key) => ({...acc, [key]: 1}), {})
