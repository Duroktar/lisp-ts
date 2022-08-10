import type { iEnv } from "../../interface/iEnv";
import type { Form } from "../form";
import { Env } from "../env";
import { Token } from "../read";

export class NativeFunc {
  constructor(
    public env: iEnv,
    public formals: Form,
    public expr: any,
    public name = 'λ'
  ) { }
  public call(args: Form, scope?: iEnv): Form {
    const closure = new Env(this.formals, args, this.env);
    return this.expr(args, closure);
  }
  public token?: Token
}
