import type { iEnv } from "../../interface/iEnv";
import { evaluate } from "../eval";
import type { Form } from "../form";
import { Env } from "./env";
import type { Syntax } from "../data/macro/syntax";

export type CallableFunc = (args: Form, env: iEnv) => Promise<Form> | Form;

export class NativeFunc {
  constructor(
    public env: iEnv,
    public params: Form,
    public expr: any,
    public name = 'λ',
  ) {}
  public async call(args: Form, env?: iEnv): Promise<Form> {
    const closure = new Env(this.params, args, env ?? this.env)
    return await this.expr(args, closure);
  }
}

export class Procedure {
  constructor(
    public env: iEnv,
    public params: Form,
    public expr: Form,
    public name = 'λ',
  ) {}
  public async call(args: Form, env?: iEnv): Promise<Form> {
    const closure = new Env(this.params, args, env ?? this.env)
    return await evaluate(this.expr, closure);
  }
}

export type Callable = Procedure | NativeFunc | Syntax
export type Closure = Callable | Function
