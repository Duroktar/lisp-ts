import * as Errors from "./error";
import type { BaseProcedure, Proc } from "./proc";
import type { Atom, Term, List } from "./terms";
import * as Utils from "../utils";
import { Sym } from "./sym";

export class Env {
  constructor(params: Term = [], args: Term = [], public outer?: Env) {
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
  get<T extends Term | Proc | BaseProcedure>(name: string): T {
    const result = this.inner[name] ?? this.outer?.get(name);
    if (result === undefined) {
      throw new Errors.UndefinedVariableError(String(name));
    }
    return result as T;
  }
  getFrom<T extends Term | Proc | BaseProcedure>(expr: Term): T {
    return this.get(Utils.toString(expr)) as T
  }
  getOrDefault<T extends Term | Proc | BaseProcedure, R = T>(name: string, d?: R): T | R {
    const result = this.inner[name] ?? this.outer?.getOrDefault(name);
    return (result ?? d) as any;
  }
  set(name: string, value: Term | Proc | BaseProcedure): void {
    this.inner[name] = value;
  }
  setFrom(expr: Term, value: Term | Proc | BaseProcedure): void {
    this.inner[Utils.toString(expr)] = value;
  }
  mergeFrom(expr: Term, value: Term | Proc | BaseProcedure): void {
    if (!this.hasFrom(expr)) {
      this.setFrom(expr, [<any>value]);
      return
    }
    const merger: List = this.getFrom(expr)
    merger.push(value as Term)
  }
  update(name: string, value: Term | Proc | BaseProcedure): void {
    let env = this.find(name)
    if (env) { env.set(name, value) }
  }
  updateFrom(expr: Term, value: Term | Proc | BaseProcedure): void {
    return this.update(Utils.toString(expr), value)
  }
  find(name: string): Env | undefined {
    let env = this as Env
    while (env.inner[name] === undefined && env.outer) {
      env = env.outer
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
  hasFrom(expr: Term): boolean {
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
  map<T>(fn: (args: [k: string, v: Term]) => T): T[] {
    let accum: T[] = []
    for (let env: Env = this; env !== undefined; env = env.outer!) {
      Object.entries(env.inner).forEach(([k ,v]) => accum.push(fn([k, <any>v])))
    }
    return accum
  }
  merge(env: Env): Env {
    const n = new Env([], [], this)
    for (let [key, value] of env.entries()) {
      n.set(key, value)
    }
    return n
  }
  keys(): string[] {
    return this.map(([key, _]) => key)
  }
  values(): Term[] {
    return this.map(([_, value]) => value)
  }
  entries(): [string, Term][] {
    return this.map(([key, value]) => [key, value])
  }
  private inner: Record<Atom, Term | Proc | BaseProcedure>;
}
