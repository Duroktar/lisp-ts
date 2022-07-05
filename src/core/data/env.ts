import { isIdent, isNil, isPair, isSym } from "../../guard";
import type { iEnv } from "../../interface/iEnv";
import { NativeFunc } from "../callable/func";
import { Syntax } from "../callable/macro/syntax";
import { Callable, Closure } from "../callable/proc";
import { NIL } from "../const";
import type { Form } from "../form";
import { toString, toStringSafe } from "../print";
import * as Errors from "./error";
import { cons, list, Pair } from "./pair";
import { Sym } from "./sym";

export class Env implements iEnv {

  // TODO
  public runtime: any;

  constructor(params: Form = NIL, args: Form = NIL, public outer?: iEnv) {
    if (isPair(params) && isPair(args)) {
      const formals = this.getParams(params, args);
      this.inner = Object.fromEntries(formals);
      return
    }
    // If the list of formals is not a list but a single identifier,
    // that identifier will be assigned a list of all the parameters
    else if (!isNil(params) && isSym(params)) {
      this.inner = { [params.description!]: args };
      return
    }
    else if (isNil(params) || isNil(args)) {
      this.inner = {}
    }
    else {
      console.log(toStringSafe(params))
      console.log(args)
      throw new Errors.InvalidEnvArgumentsError(params, args);
    }

  }

  private getParams(params: Pair, args: Pair): [string, Form][] {
    // If the list of formals is an improper list, the tail name
    // will be assigned a list of any parameters remaining after
    // all the preceeding formals have been assigned values.

    // (form . options)
    // ('(1 3) 'a 'b 'c)
    // => {form: '(1 3), options: ('a 'b 'c)}
    const result: [string, Form][] = []
    const tail = params.tail.cdr
    let idx = 0
    params.forEach((p: symbol) => {
      result.push([p.description!, args.at(idx)])
      idx++
    })
    if (isIdent(tail)) {
      result.push([tail.description!, args.slice(idx-1)])
    }
    return result
    // const x0 = car(params)
    // const x1 = car(args)
    // const xs0 = cdr(params)
    // const xs1 = cdr(args)
    // if (!params.isList() && isSym(xs0)) {
    //   return [[toString(x0), x1], [toString(xs0), xs1]]
    // }
    // if (isPair(xs0) && isPair(xs1)) {
    //   return [[toString(x0), x1], ...this.getParams(xs0, xs1)]
    // }
    // return [[toString(x0), x1]]
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
    const paramList = typeof params === 'string' ? Sym(params) : list(...params.map(Sym));
    const handler = (args: Form, env: iEnv) => cb(parseArgs(args, toArray), env);
    const callable = new NativeFunc(this, paramList, handler, name);
    this.set(name, callable);
    return callable
  }
  syntax(name: string, cb: (args: Form, env: iEnv) => any): void {
    this.set(name, new Syntax(name, this, cb));
  }

  private inner: Record<string, Form | Closure>;
}

function parseArgs(args: Form, toArray: boolean): Form | Form[] {
  if (toArray === false)
    return args
  if (isNil(args))
    return []
  if (isPair(args))
    return args.toArray()
  throw new Error('Inavlid args in parseArgs')
}
'cond'
