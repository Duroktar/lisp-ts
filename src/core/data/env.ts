import { isEmpty, isList, isPair, isString, isSym } from "../../guard";
import type { iEnv } from "../../interface/iEnv";
import { NIL } from "../const";
import { Syntax } from "../callable/macro/syntax";
import type { Atom, Form } from "../form";
import { car, cdr } from "../lisp";
import { toString } from "../print";
import * as Errors from "./error";
import { cons, list, Pair } from "./pair";
import { Callable, Closure } from "../callable/proc";
import { NativeFunc } from "../callable/func";
import { Sym } from "./sym";

export class Env implements iEnv {

  // TODO
  public runtime: any;

  constructor(params: Form = NIL, args: Form = NIL, public outer?: iEnv) {
    if (isPair(params) && isPair(args)) {

      function getParams(params: Pair, args: Pair): [string, any][] {
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
    else if (params === NIL || args === NIL) {
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
  define(name: string, params: string | string[], cb: (args: Form[] | Form, env: iEnv) => any, toArray = true): Callable {
    const paramList = isString(params) ? Sym(params) : list(...params.map(Sym));
    const handler = (args: Form, env: iEnv) => cb(parseArgs(toArray, args), env);
    const callable = new NativeFunc(this, paramList, handler, name);
    this.set(name, callable);
    return callable
  }
  syntax(name: string, cb: (args: Form, env: iEnv) => any): void {
    this.set(name, new Syntax(name, this, cb));
  }

  private inner: Record<Atom, Form | Closure>;
}

function parseArgs(toArray: boolean, args: Form): Form | Form[] {
  return toArray ? (isList(args) ? (isEmpty(args) ? [] : args.toArray()) : args) : args;
}
