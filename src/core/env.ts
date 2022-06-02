import assert from "assert";
import { isList, isSym } from "../utils";
import { toString } from "./toString";
import * as Errors from "./error";
import type { Proc } from "./proc";
import { Sym } from "./sym";
import type { Atom, List, Form } from "./forms";

export class Env {
  constructor(params: Form = [], args: Form = [], public outer?: Env) {
    if (isList(params) && isList(args) && params.every(isSym)) {
      const getParams = (params: List, args: List): [string, any][] => {
        if (params.length === 0) return []
        if (params[0] === Sym('...')) {
          assert(params.slice(1).length === 0, 'no args allowed after `...`')
          return [[toString(params[0]), args]]
        }
        if (params[0] === Sym('.')) {
          assert(params.slice(2).length === 0, 'only one arg allowed after `.`')
          return [[toString(params[1]), args]]
        }
        const [x0, ...xs0] = params
        const [x1, ...xs1] = args
        return [[toString(x0), x1], ...getParams(xs0, xs1)]
      }
      const formals = getParams(params, args);
      this.inner = Object.fromEntries(formals);
    } else if (isSym(params)) {
      this.inner = { [params.description!]: args };
    }
    else {
      throw new Errors.InvalidEnvArgumentsError(params, args);
    }
  }
  get<T extends Form | Proc>(name: string): T {
    const result = this.inner[name] ?? this.outer?.get(name);
    if (result === undefined) {
      throw new Errors.UndefinedVariableError(String(name));
    }
    return result as T;
  }
  getFrom<T extends Form | Proc>(expr: Form): T {
    return this.get(toString(expr)) as T
  }
  getOrDefault<T extends Form | Proc, R = T>(name: string, d?: R): T | R {
    const result = this.inner[name] ?? this.outer?.getOrDefault(name);
    return (result ?? d) as any;
  }
  set(name: string, value: Form | Proc): void {
    this.inner[name] = value;
  }
  setFrom(expr: Form, value: Form | Proc): void {
    this.inner[toString(expr)] = value;
  }
  mergeFrom(expr: Form, value: Form | Proc): void {
    if (!this.hasFrom(expr)) {
      this.setFrom(expr, [<any>value]);
      return
    }
    const merger: List = this.getFrom(expr)
    if (isList(merger)) {
      merger.push(value as Form)
    } else {
      this.setFrom(expr, [merger, <any>value])
    }
  }
  update(name: string, value: Form | Proc): void {
    let env = this.find(name)
    if (env) { env.set(name, value) }
  }
  updateFrom(expr: Form, value: Form | Proc): void {
    return this.update(toString(expr), value)
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
    for (let env: Env = this; env !== undefined; env = env.outer!) {
      Object.entries(env.inner).forEach(([k ,v]) => accum.push(fn([k, <any>v])))
    }
    return accum
  }
  merge(env: Env): Env {
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
  private inner: Record<Atom, Form | Proc>;
}
