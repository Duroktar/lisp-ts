import { Lisp } from "./lib/lisp";
import type { Env } from "./lib/env";
import { NativeFunc, Proc } from "./lib/proc";
import type { Atom, Expr } from "./lib/terms";

type Exists<P> = Exclude<P, undefined | null>;

export const assert = <P>(p: P, msg = ''): Exists<P> => {
  if (p == null) {
    throw new Error(msg || `assert error: ${p}`);
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

export const isPair = (x: unknown) => !isSym(x) && !isEmpty(x);
export const isArray = (x: unknown): x is Expr[] => !isSym(x) && Array.isArray(x);
export const isAtom = (x: unknown): x is Atom => isSym(x);
export const isSym = (x: unknown): x is symbol => typeof x === 'symbol';
export const isNum = (x: unknown): x is number => typeof x === 'number';
export const isString = (x: unknown): x is string => typeof x === 'string';
export const isEmpty = (x: unknown): x is [] => isArray(x) && x.length === 0;
export const isNone = (x: unknown): x is undefined | null => x === undefined || x === null;
export const isCallable = (x: unknown): x is Proc | NativeFunc => isProc(x) || isNativeFn(x);
export const isProc = (x: unknown): x is Proc => x instanceof Proc;
export const isNativeFn = (x: unknown): x is NativeFunc => x instanceof NativeFunc;
export const isExpr = (x: unknown): x is Expr => isAtom(x) || isArray(x) || isCallable(x) || isString(x) || isNum(x);

export const symName = (s: symbol): string => s.description!;

export const isT = (e: Expr): boolean => e === Lisp.TRUE;
export const toL = (e: boolean): Expr => e ? Lisp.TRUE : Lisp.EMPTY;

export const zip = (...rows: Expr[][]) => rows[0].map((_, c) => rows.map(row => row[c]));

export const map = (func: (m: Expr) => Expr, m: Expr): Expr => {
  if (isArray(m)) {
    return m.map(child => map(func, child));
  }
  return func(m);
};
export const find = (func: (m: Expr, i: number) => boolean, m: Expr, __i = 0): Expr | undefined => {
  if (isArray(m)) {
    for (const child of m) {
      const r = find(func, child, __i++);
      if (r !== undefined)
        return r;
    }
  }
  if (func(m, __i))
    return m;
};

export const toString = (expr: Expr, inspect = false): string => {
  if (isSym(expr)) {
    // if (inspect) return String(expr)
    return expr.description!;
  }
  if (expr instanceof NativeFunc) {
    return `(nativefunc ${expr.name})`;
  }
  if (expr instanceof Proc) {
    if (inspect) {
      const parms = toString(expr.params, inspect);
      const body = toString(expr.expr, inspect);
      return `(λ ${expr.name} ${parms} ${body})`;
    }
    return `(λ ${expr.name})`;
  }
  if (isString(expr) || isNone(expr))
    return expr;
  if (isNum(expr))
    return String(expr);
  if (isEmpty(expr))
    return '()';
  if (Lisp.car(expr) === Lisp.SymTable.LAMBDA) {
    return `(λ ${(<any>Lisp.cdr(expr)).map((x: any) => toString(x, inspect)).join(' ')}`;
  }
  if (symName(<symbol>Lisp.car(expr)) in Lisp.quotes) {
    const val = toString(Lisp.cadr(expr), inspect);
    return `${Lisp.quotes[symName(<symbol>Lisp.car(expr))]}${val}`;
  }
  return `(${expr.map(c => toString(c, inspect)).join(' ')})`;
};
export const print = (e: Expr, inspect = false): void => {
  console.log(toString(e, inspect));
};

export function mkNativeFunc(env: Env, name: string, params: string[], cb: (args: Expr, env: Env) => any): Expr | NativeFunc {
  const func = new class extends NativeFunc {
    public name = name;
    public env = env;
    public params = params.map(Lisp.Sym);
    public _call = cb;
  };

  env.set(name, func);
  return func;
}

export const mkLambda = (params: string[] | string, body: Expr): Expr => {
  return [Lisp.SymTable.LAMBDA, isArray(params) ? params.map(Lisp.Sym) : Lisp.Sym(params), body];
};
