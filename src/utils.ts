import { Character } from "./core/char";
import { EMPTY, FALSE, isPeculiarIdentifier, isSpecialInitial, isSpecialSubsequent, TRUE } from "./core/const";
import { Sym, SymTable } from "./core/sym";
import type { Atom, Form } from "./core/forms";
import { toString } from "./core/toString";
import { Vector } from "./core/vec";
import { whitespace, identifier, initial, letter, subsequent, digit, character } from "./syntax";
import { Pair, list } from "./core/pair";
import { NativeProc, isNativeProc, isProc, Procedure } from "./core/proc";
import { SyntaxRulesDef, isSyntaxRulesDef } from "./core/syntax";

export type Predicate = (...args: any[]) => boolean

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

export const isList = (x: unknown): x is Pair | symbol => (isPair(x) && x.isList()) || isEmpty(x);
export const isPair = (x: unknown): x is Pair => Pair.is(x);
export const isAtom = (x: unknown): x is Atom => isSym(x);
export const isSym = (x: unknown): x is symbol => typeof x === 'symbol';
export const isNum = (x: unknown): x is number => typeof x === 'number';
export const isVec = (x: unknown): x is Vector => x instanceof Vector;
export const isString = (c: any): c is string => typeof c === 'string';
export const isChar = (x: unknown): x is Character => x instanceof Character;
export const isEmpty = (x: unknown): x is symbol => x === EMPTY;
export const isNone = (x: unknown): x is undefined | null => x === undefined || x === null;
export const isExpr = (x: unknown): x is Form => isPair(x) || isAtom(x) || isString(x) || isNum(x);
export const isConst = (x: unknown) => isNum(x) || isString(x)
export const isIdent = (x: unknown): x is symbol => isSym(x) && !isEmpty(x)

export const isCallable = (x: unknown): x is Procedure | NativeProc | SyntaxRulesDef => {
  return isProc(x) || isNativeProc(x) || isSyntaxRulesDef(x);
}

export const isEqv = (x: unknown, y: unknown): boolean => {
  return (x === y);
}
export const isEq = (x: unknown, y: unknown): boolean => {
  return isEqv(x, y)
}
export const isEqual = (a1: Form, b1: Form): boolean => {
  function walk(a: Form, b: Form): boolean {
    if (isEqv(a, b))
      return true
    if (isVec(a))
      return isVec(b) && zip(a.data, b.data).every(([a, b]) => walk(a, b))
    else if (isVec(b))
      return false
    return isPair(a) && isPair(b) && a.equal(b)
  }
  return walk(a1, b1)
}

export const symName = (s: symbol): string => s.description!;

export const isTruthy = (e: Form): boolean => !isF(e) && !isNil(e);
export const isNil = (e: Form): boolean => e === Sym('nil');
export const isF = (e: Form): boolean => e === FALSE;
export const isT = (e: Form): boolean => e === TRUE;
export const toL = (e: boolean): Form => e ? TRUE : FALSE;

export const zip = (...rows: Form[][]) => {
  if (isEmpty(rows) || !rows[0]) return [[], []]
  return rows[0].map((_, c) => rows.map(row => row[c]));
}
export const zipUp = (...rows: (Form[] | Form)[]) => {
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
  const stuffz = rows.map(i => Array.isArray(i) ? i : new Proxy([i], h));
  const runs = Math.max(...stuffz.map(i => i.length));
  const entries = []
  for (let c = 0; c < runs; c++) {
    entries.push(stuffz.map((row: any) => row[c]))
  }
  return entries;
}

export const map = (func: (m: Form) => Form, m: Form): Form => {
  if (isPair(m)) {
    return m.map(child => map(func, child));
  }
  return func(m);
};
export const find = (func: (m: Form, i: number) => boolean, m: Form, __i = 0): Form | undefined => {
  if (isPair(m)) {
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
  if (isPair(a)) return a.car
  return undefined
}

export const mkLambda = (params: string[] | string, body: Form): Form => {
  return list(SymTable.LAMBDA, Array.isArray(params) ? list(...params.map(Sym)) : Sym(params), body);
};

export const eqC = (a: any) => (b: any) => a === b;

export const searchIdx = (...keys: string[]) => keys.reduce((acc, key) => ({...acc, [key]: 1}), {})

export const first = (a: any) => isPair(a) ? a.car : undefined;

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
// export const isNumber = (c: any): c is number => typeof c === 'number';
export const isCharacter = (c: any): c is character => !! (isString(c) && c.match(/^#\\([A-z]|(space|newline)){1}$/));

export function gcd(x: number, y: number) {
  x = Math.abs(x);
  y = Math.abs(y);
  while (y) {
    var t = y;
    y = x % y;
    x = t;
  }
  return x;
}

export function lcm(n1: number, n2: number) {
  //Find the gcd first
  let gcdr = gcd(n1, n2);

  //then calculate the lcm
  return Math.abs((n1 * n2) / gcdr);
}

export function tryProve<T>(fn: (t: T) => boolean, o: {proofs: T[], counters: any[]}): void {
  o.proofs.forEach(p => assert(!!fn(p), `found proof that doesn't hold: ${p}`))
  o.counters.forEach(c => assert(!fn(c), `found counter-proof that doesn't hold: ${c}`))
}

export const debounce = <T extends Function>(func: T, timeout = 300) => {
  let timer: NodeJS.Timer | undefined;
  return ((...args: any[]): any => {
    clearTimeout(timer!);
    timer = setTimeout(() => { func(...args); }, timeout);
  }) as any as T;
}
