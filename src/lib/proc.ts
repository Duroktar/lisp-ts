import { Env } from "./env";
import { evaluate } from "./eval";
import type { Expr } from "./terms";

export class Proc {
  constructor(public params: Expr, public expr: Expr, public env: Env) { }
  public name: string = 'λ';
  public call = (args: Expr) => {
    const env = new Env(this.params, args, this.env);
    return evaluate(this.expr, env);
  };
}

export abstract class NativeFunc {
  abstract params: symbol[];
  abstract env: Env;
  public expr = [];
  public name: string = 'λ';
  public call = (args: Expr) => {
    const env = new Env(this.params, args, this.env);
    return this._call(args, env);
  };
  abstract _call(args: Expr, env: Env): Expr;
}
