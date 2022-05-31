import { Character } from "./core/char";
import { FALSE, isPeculiarIdentifier, isSpecialInitial, isSpecialSubsequent, TRUE } from "./core/const";
import { Sym, SymTable } from "./core/sym";
import type { Atom, List, Term } from "./core/terms";
import { toString } from "./core/toString";
import { Vector } from "./core/vec";
import { whitespace, identifier, initial, letter, subsequent, digit, character } from "./syntax";

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
export const isVec = (x: unknown): x is Vector => x instanceof Vector;
export const isString = (c: any): c is string => typeof c === 'string';
export const isChar = (x: unknown): x is Character => x instanceof Character;
export const isEmpty = (x: unknown): boolean => isList(x) && x.length === 0;
export const isNone = (x: unknown): x is undefined | null => x === undefined || x === null;
export const isExpr = (x: unknown): x is Term => isAtom(x) || isList(x) || isString(x) || isNum(x);
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

export const isTruthy = (e: Term): boolean => !isF(e) && !isNil(e);
export const isNil = (e: Term): boolean => e === Sym('nil');
export const isF = (e: Term): boolean => e === FALSE;
export const isT = (e: Term): boolean => e === TRUE;
export const toL = (e: boolean): Term => e ? TRUE : FALSE;

export const zip = (...rows: Term[][]) => isEmpty(rows) ? [[], []] : rows[0].map((_, c) => rows.map(row => row[c]));
export const zipUp = (...rows: (Term[] | Term)[]) => {
  if (isEmpty(rows)) return []
  const h: ProxyHandler<any> = {
    get(target, prop) {
      switch (prop) {
        case "length": return 1
        default: {
          if (typeof prop === "string" && prop.match(/[0-9]*/)) {
            return target[0]
          }
          return target[prop]
        }
      }
    }
  };
  const stuffz = rows.map(i => isList(i) ? i : new Proxy([i], h));
  const runs = Math.max(...stuffz.map(i => i.length));
  const entries = []
  for (let c = 0; c < runs; c++) {
    entries.push(stuffz.map((row: any) => row[c]))
  }
  return entries;
}

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

export const mkLambda = (params: string[] | string, body: Term): Term => {
  return [SymTable.LAMBDA, isList(params) ? params.map(Sym) : Sym(params), body];
};

export const eqC = (a: any) => (b: any) => a === b;

export const searchIdx = (...keys: string[]) => keys.reduce((acc, key) => ({...acc, [key]: 1}), {})

export const first = (a: any) => isList(a) ? a[0] : undefined;

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
export const isNumber = (c: any): c is number => typeof c === 'number';
export const isCharacter = (c: any): c is character => !! (isString(c) && c.match(/^#\\([A-z]|(space|newline)){1}$/));
