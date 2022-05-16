import * as Errors from "./error";
import type { BaseProcedure, Proc } from "./proc";
import type { Atom, Expr, List } from "./terms";
import * as Utils from "../utils";
import { Sym } from "./sym";

export class Env {
  constructor(params: Expr = [], args: Expr = [], private outer?: Env) {
    if (Utils.isList(params) && Utils.isList(args) && params.every(Utils.isSym)) {
      const getParams = (params: List, args: List): [string, any][] => {
        if (params.length === 0) return []
        if (params[0] === Sym('...')) {
          if (params.slice(1).length !== 0) {
            debugger
          }
          Utils.expect(params, params.slice(1).length === 0, 'no args allowed after `...`')
          return [[Utils.toString(params[0]), args]]
        }
        if (params[0] === Sym('.')) {
          Utils.expect(params, params.slice(2).length === 0, 'only one arg allowed after `.`')
          return [[Utils.toString(params[1]), args]]
        }
        const [x0, ...xs0] = params
        const [x1, ...xs1] = args
        return [[Utils.toString(x0), x1], ...getParams(xs0, xs1)]
      }
      const formals = getParams(params, args);
      this.inner = Object.fromEntries(formals);
    } else if (Utils.isSym(params)) {
      this.inner = { [params.description!]: args };
    }
    else {
      throw new Errors.InvalidEnvArgumentsError(params, args);
    }
  }
  get(name: Atom): Expr | Proc | BaseProcedure {
    const result = this.inner[name] ?? this.outer?.get(name);
    if (result === undefined) {
      throw new Errors.UndefinedVariableError(String(name));
    }
    return result as Expr;
  }
  getOrDefault<T extends Expr, R = undefined>(name: Atom, d?: R): T | R {
    const result = this.inner[name] ?? this.outer?.getOrDefault(name);
    return (result ?? d) as any;
  }
  set(name: Atom, value: Expr | Proc | BaseProcedure): void {
    this.inner[name] = value;
  }
  setFrom(expr: Expr, value: Expr | Proc | BaseProcedure): void {
    this.inner[Utils.toString(expr)] = value;
  }
  update(name: Atom, value: Expr | Proc | BaseProcedure): void {
    let env = this.find(name)
    if (env) { env.set(name, value) }
  }
  find(name: Atom): Env | undefined {
    let env = this as Env
    while (env.inner[name] === undefined && env.outer) {
      env = env.outer
    }
    if (env.inner[name] !== undefined) {
      return env
    }
  }
  has(name: Atom): boolean {
    try {
      return (this.inner[name] ?? this.outer?.get(name)) !== undefined;
    } catch {
      return false
    }
  }
  hasFrom(expr: Expr): boolean {
    const name = Utils.toString(expr)
    try {
      return (this.inner[name] ?? this.outer?.get(name)) !== undefined;
    } catch {
      return false
    }
  }
  size(): number {
    return this.keys().length
  }
  map<T>(fn: (args: [k: string, v: Expr]) => T): T[] {
    let accum: T[] = []
    for (let env: Env = this; env !== undefined; env = env.outer!) {
      Object.entries(env.inner).forEach(([k ,v]) => accum.push(fn([k, <any>v])))
    }
    return accum
  }
  keys(): string[] {
    return this.map(([key, _]) => key)
  }
  values(): Expr[] {
    return this.map(([_, value]) => value)
  }
  entries(): [string, Expr][] {
    return this.map(([key, value]) => [key, value])
  }
  private inner: Record<Atom, Expr | Proc | BaseProcedure>;
}
