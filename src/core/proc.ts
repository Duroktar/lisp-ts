import { Env } from "./env";
import { evaluate } from "./eval";
import type { Expr } from "./terms";

export abstract class BaseProcedure {
  abstract params: Expr;
  abstract env: Env;
  public expr: Expr = [];
  public name = 'λ';
  public call = (args: Expr, env: Env) => {
    return this._call(args, this.getClosure(args, env));
  };

  abstract _call(args: Expr, env: Env): Expr;

  public getClosure(args: Expr, env: Env): Env {
    return new Env(this.params, args, env);
  }
}

export class Proc extends BaseProcedure {
  constructor(public params: Expr, public expr: Expr, public env: Env) {
    super()
  }
  public _call = (args: Expr, env: Env) => {
    return evaluate(this.expr, env);
  };
}
