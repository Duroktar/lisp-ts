import { isPair, isSym } from "../../guard";
import type { iEnv } from "../../interface/iEnv";
import { EMPTY } from "../const";
import * as Errors from "./error";
import type { Atom, Form } from "../form";
import { car, cdr } from "../lisp";
import { cons, list, Pair } from "./pair";
import type { Closure } from "./proc";
import { toString } from "../print";

export class Env implements iEnv {
  constructor(params: Form = EMPTY, args: Form = EMPTY, public outer?: iEnv) {
    if (isPair(params) && isPair(args)) {

      function getParams(params: Pair, args: Pair): [string, any][] {
        // if (params.length === 0) return []
        // if (car(params) === Sym('...')) {
        //   assert(cdr(params) === EMPTY, 'no args allowed after `...`')
        //   return [[toString(car(params)), args]]
        // }
        const x0 = car(params)
        const x1 = car(args)
        const xs0 = cdr(params)
        const xs1 = cdr(args)
        if (!params.isList() && isSym(xs0)) {
          return [[toString(x0), x1], [toString(xs0), xs1]]
        }
        if (isPair(xs0) && isPair(xs1)) {
          return [[toString(x0), x1], ...getParams(xs0, xs1)]
        }
        return [[toString(x0), x1]]
      }

      const formals = getParams(params, args);
      this.inner = Object.fromEntries(formals);
      return
    }
    else if (params === EMPTY || args === EMPTY) {
      this.inner = {}
      return
    }
    else if (isSym(params)) {
      this.inner = { [params.description!]: args };
      return
    }
    else {
      throw new Errors.InvalidEnvArgumentsError(params, args);
    }

  }
  get<T extends Form | Closure>(name: string): T {
    const result = this.inner[name] ?? this.outer?.get(name);
    if (result === undefined) {
      throw new Errors.UndefinedVariableError(String(name));
    }
    return result as T;
  }
  getFrom<T extends Form | Closure>(expr: Form): T {
    return this.get(toString(expr)) as T
  }
  getOrDefault<T extends Form | Closure, R = T>(name: string, d?: R): T | R {
    const result = this.inner[name] ?? this.outer?.getOrDefault(name);
    return (result ?? d) as any;
  }
  set(name: string, value: Form | Closure): void {
    this.inner[name] = value;
  }
  setFrom(expr: Form, value: Form | Closure): void {
    this.inner[toString(expr)] = value;
  }
  mergeFrom(expr: Form, value: Form | Closure): void {
    if (!this.hasFrom(expr)) {
      this.setFrom(expr, list(value));
      return
    }
    const merger: Pair = this.getFrom(expr)
    if (isPair(merger)) {
      merger.push(value as Form)
    } else {
      this.setFrom(expr, cons(merger, value))
    }
  }
  update(name: string, value: Form | Closure): void {
    let env = this.find(name)
    if (env) { env.set(name, value) }
  }
  updateFrom(expr: Form, value: Form | Closure): void {
    return this.update(toString(expr), value)
  }
  find(name: string): iEnv | undefined {
    let env = this as Env
    while (env.inner[name] === undefined && env.outer) {
      env = env.outer as Env
    }
    if (env.inner[name] !== undefined) {
      return env
    }
  }
  has(name: string): boolean {
    try {
      return (this.inner[name] ?? this.outer?.get(name)) !== undefined;
    } catch {
      return false
    }
  }
  hasFrom(expr: Form): boolean {
    const name = toString(expr)
    try {
      return (this.inner[name] ?? this.outer?.get(name)) !== undefined;
    } catch {
      return false
    }
  }
  size(): number {
    return this.keys().length
  }
  map<T>(fn: (args: [k: string, v: Form]) => T): T[] {
    let accum: T[] = []
    for (let env: Env = this; env !== undefined; env = env.outer! as Env) {
      Object.entries(env.inner).forEach(([k ,v]) => accum.push(fn([k, <any>v])))
    }
    return accum
  }
  merge(env: iEnv): iEnv {
    for (let [key, value] of env.entries()) {
      this.set(key, value)
    }
    return this
  }
  keys(): string[] {
    return this.map(([key, _]) => key)
  }
  values(): Form[] {
    return this.map(([_, value]) => value)
  }
  entries(): [string, Form][] {
    return this.map(([key, value]) => [key, value])
  }
  private inner: Record<Atom, Form | Closure>;
}

/*
(mcons
  'a
  (mcons
    (mcons
      'quasiquote
      (mcons
        (mcons
          'b
          (mcons
            (mcons (unquote (mcons (mcons '+ (mcons 1 (mcons 2 '()))) '())))
            (mcons
              (mcons (unquote (mcons (mcons 'foo (mcons (mcons (unquote (mcons (mcons '+ (mcons 1 (mcons 3 '()))) '()))) (mcons 'd '()))) '())))
              (mcons 'e '()))))
        '()))
    (mcons 'f '())))

*/
