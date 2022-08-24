import { Range } from 'ariadne-ts'
import { FALSE, TRUE } from "./core/const";
import { AssertionError, SwitchFallthroughError } from "./core/error";
import { cons, list } from "./core/data/pair";
import { Sym, SymTable } from "./core/data/sym";
import type { Form, List } from "./core/form";
import { car, cdr } from "./core/lisp";
import { toString } from "./core/print";
import { isChar, isEmpty, isIdent, isNum, isPair, isString, isVec } from "./guard";

export type Predicate = (...args: any[]) => boolean

export function as<T>(value: unknown, klass: new (...args: any[]) => T, message?: string | Error): T {
  assert(value instanceof klass, message)
  return value
}

export function assert(value: unknown, message?: string | Error, form?: Form): asserts value {
  if (value === false || value === null || value === undefined) {
    if (message instanceof Error)
      throw message
    else
      throw new AssertionError(message ?? '', form)
  }
}

export const exists = <P>(p: P, msg = ''): Exclude<P, undefined | null> => {
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

export const isEqv = (x: Form, y: Form): boolean => {
  if (isChar(x))
    return x.equal(y)
  if (isNum(x))
    return x.equal(y)
  if (isString(x))
    return x.equal(y)
  if (isIdent(x))
    return x.equal(y)
  return (x === y);
}
export const isEq = (x: Form, y: Form): boolean => {
  return isEqv(x, y)
}
export const isEqual = (a: Form, b: Form): boolean => {
  if (isVec(a) && isVec(b))
    return zip(a.data, b.data).every(([a, b]) => isEqual(a, b))
  if (isPair(a))
    return a.equal(b)
  return isEqv(a, b)
}

export const symName = (s: symbol): string => s.description!;

export const zip = (...rows: Form[][]) => {
  if (rows.length === 0 || !rows[0]) return [[], []]
  return rows[0].map((_, c) => rows.map(row => row[c]));
}

export const zipUp = (...rows: (Form[] | Form)[]) => {
  if (rows.length === 0) return []
  const h: ProxyHandler<any> = {
    get(target, prop) {
      switch (prop) {
        case "length": return 1
        default: {
          if (typeof prop === "string" && prop.match(/[0-9]*/)) {
            isPair(target)
              ? target.at(Number(prop))
              : target[prop]
          }
          return target[prop]
        }
      }
    }
  };
  const stuffz: any[] = rows.map(i => Array.isArray(i) ? i : new Proxy([i], h));
  const runs = Math.max(...stuffz.map(i => i.length));
  const entries: any[] = []
  for (let c = 0; c < runs; c++) {
    entries.push(stuffz.map((row: any) => row[c]))
  }
  return list(...entries);
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
  return list(SymTable.LAMBDA, Array.isArray(params) ? list(...params.map(s => Sym(s))) : Sym(params), body);
};

export const eqC = (a: any) => (b: any) => a === b;

export const searchIdx = (...keys: string[]) => keys.reduce((acc, key) => ({...acc, [key]: 1}), {})

// export const first = (a: any) => isPair(a) ? a.car : undefined;

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

export function append(first: List, ...rest: Form[]): List {
  if (rest.length === 0) {
    return first
  }
  else if (isEmpty(first)) {
    return append(<List>rest[0], ...rest.slice(1))
  }
  else {
    return cons(
      car(first),
      append(<List>cdr(first), append(<List>rest[0], ...rest.slice(1)))
    )
  }
}

export const push = (lst: List, item: any): List => {
  if (isEmpty(lst))
    return list(item)
  else
    return lst.push(item)
}

export const toL = (e: boolean): Form => e ? TRUE : FALSE;

export function range(start: number, end: number) {
  const r = []
  for (let i = start; i < end; i++) {
    r.push(i)
  }
  return r
}

export function sequence<T, R>(...fns: [...((t: T) => R)[], T]) {
  const data = fns.pop() as T
  return (<((t: T) => R)[]>fns).map(f => f(data))
}

export function error(msg?: string): never { throw new Error(msg) }

export function underline(range: SourceRange, leftMargin = 0) {
  const numArrows = Math.abs(range.end.col - range.start.col)
  const space = ' '.repeat(clamp(0, Infinity, range.start.col - 1 + leftMargin))
  const arrows = '^'.repeat(clamp(0, Infinity, numArrows))
  return space + arrows
}

export class StringBuilder {
  addLine(line: string) {
      this.lines.push(line)
      return this
  }
  addText(text: string) {
      let line = this.lines.pop() ?? ''
      this.lines.push(line + text)
      return this
  }
  build() {
      return this.lines.join('\n')
  }
  private lines: string[] = []
}

export const clamp: (min: number, max: number, num: number) => number
    = (min, max, num) => Math.max(min, Math.min(max, num))

  export type Position = {
    line: number;
    col: number;
    cursor: number;
}

export type SourceRange = {
    start: Position;
    end: Position;
};

export type Expand<T> = T extends (...args: infer A) => infer R
  ? (...args: Expand<A>) => Expand<R>
  : T extends infer O
  ? { [K in keyof O]: O[K] }
  : never;

export type ExpandRecursively<T> = T extends (...args: infer A) => infer R
  ? (...args: ExpandRecursively<A>) => ExpandRecursively<R>
  : T extends object
  ? T extends infer O
    ? { [K in keyof O]: ExpandRecursively<O[K]> }
    : never
  : T;

export function rangeBuilder() {
  type InferType<T> = T extends ({ withStart(value: number): any } | { withEnd(value: number): any })
    ? T
    : { build(): Range }
  return new class RangeBuilder {
    private start?: number;
    private end?: number;
    withStart<T>(this: T, value: number): InferType<Omit<T, 'withStart'>> {
      (this as any).start = value;
      return this as any
    }
    withEnd<T>(this: T, value: number): InferType<Omit<T, 'withEnd'>> {
      (this as any).end = value;
      return this as any
    }
    private build(): Range {
      assert(typeof this.start === 'number' && typeof this.end === 'number')
      return Range.new(this.start, this.end);
    }
  }
}

export function assertNever(token: never, msg?: string): never {
  throw new SwitchFallthroughError(`Unexpected fallthrough: ${token}` + msg ? msg : '');
}

export function updateTokenFrom(source: Form, to: Form) {
  if (source.token)
    to.token = source.token;
}
