import type { iEnv } from "../../interface/iEnv";
import type { Form } from "../form";
import { Env } from "../data/env";

export class NativeFunc {
  constructor(
    public env: iEnv,
    public params: Form,
    public expr: any,
    public name = 'λ'
  ) { }
  public call(args: Form, env?: iEnv): Form {
    const closure = new Env(this.params, args, env ?? this.env);
    return this.expr(args, closure);
  }
}
