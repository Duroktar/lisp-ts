import * as Errors from "./error";
import type { NativeFunc, Proc } from "./proc";
import type { Atom, Expr } from "./terms";
import * as Utils from "../utils";

export class Env {
  constructor(params: Expr = [], args: Expr = [], private outer?: Env) {
    if (Utils.isList(params) && Utils.isList(args) && params.every(Utils.isSym))
      this.inner = Object.fromEntries(params.map((p, i) => [p.description!, args[i]]));
    else if (Utils.isSym(params)) {
      this.inner = { [params.description!]: args };
    }
    else {
      throw new Errors.InvalidEnvArgumentsError(params, args);
    }
  }
  get(name: Atom): Expr | Proc | NativeFunc {
    const result = this.inner[name] ?? this.outer?.get(name);
    if (result === undefined) {
      throw new Errors.UndefinedVariableError(String(name));
    }
    return result as Expr;
  }
  set(name: Atom, value: Expr | Proc | NativeFunc): void {
    this.inner[name] = value;
  }
  update(name: Atom, value: Expr | Proc | NativeFunc): void {
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
  private inner: Record<Atom, Expr | Proc | NativeFunc>;
}
