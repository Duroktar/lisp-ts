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
        if (params[0] === Sym('.')) {
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
  set(name: Atom, value: Expr | Proc | BaseProcedure): void {
    this.inner[name] = value;
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
    return (this.inner[name] ?? this.outer?.get(name)) !== undefined;
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
