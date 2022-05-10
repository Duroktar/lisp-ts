import * as Errors from "./errors";
import type { NativeFunc, Proc } from "./proc";
import type { Atom, Expr } from "./terms";
import * as Utils from "../utils";

export class Env {
  constructor(params: Expr = [], args: Expr = [], private outer?: Env) {
    if (Utils.isArray(params) && Utils.isArray(args) && params.every(Utils.isSym))
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
  private inner: Record<Atom, Expr | Proc | NativeFunc>;
}
