import type { iEnv } from "../../interface/iEnv";
import type { Form } from "../form";
import { Env } from "../data/env";

export class NativeFunc {
  constructor(
    public env: iEnv,
    public formals: Form,
    public expr: any,
    public name = 'λ'
  ) { }
  public call(args: Form): Form {
    const closure = new Env(this.formals, args, this.env);
    return this.expr(args, closure);
  }
}
