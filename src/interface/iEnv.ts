import { Form } from "../core/form"
import { Callable, Closure } from "../core/callable/proc"
import { NativeFunc } from "../core/callable/func"

export interface iEnv {
  define(name: string, params: string | string[], cb: (args: Form[] | Form, env: iEnv) => any, toArray?: boolean): Callable
  syntax(name: string, cb: (args: Form, env: iEnv) => any): void
  get<T extends Form | Closure>(name: string): T
  getFrom<T extends Form | Closure>(expr: Form): T
  getOrDefault<T extends Form | Closure>(name: string, d?: any): T | any
  set(name: string, value: Form | Closure): void
  setFrom(expr: Form, value: Form | Closure): void
  mergeFrom(expr: Form, value: Form | Closure): void
  update(name: string, value: Form | Closure): void
  updateFrom(expr: Form, value: Form | Closure): void
  find(name: string): iEnv | undefined
  has(name: string): boolean
  hasFrom(expr: Form): boolean
  size(): number
  map<T>(fn: (args: [k: string, v: Form]) => T): T[]
  merge(env: iEnv): iEnv
  keys(): string[]
  values(): Form[]
  entries(): [string, Form][]
}
