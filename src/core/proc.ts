import { Env } from "./env";
import { evaluate } from "./eval";
import type { Term } from "./terms";

export abstract class BaseProcedure {
  abstract params: Term;
  abstract env: Env;
  public expr: Term = [];
  public name = 'λ';
  public call = (args: Term, env: Env) => {
    return this._call(args, this.getClosure(args, env));
  };

  abstract _call(args: Term, env: Env): Term;

  public getClosure(args: Term, env: Env): Env {
    return new Env(this.params, args, env);
  }
}

export class Proc extends BaseProcedure {
  constructor(public params: Term, public expr: Term, public env: Env) {
    super()
  }
  public _call = (args: Term, env: Env) => {
    return evaluate(this.expr, env);
  };
}
