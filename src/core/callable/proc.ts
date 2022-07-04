import type { iEnv } from "../../interface/iEnv";
import { evaluate } from "../eval";
import type { Form } from "../form";
import { Env } from "../data/env";
import type { Syntax } from "../callable/macro/syntax";
import { NativeFunc } from "./func";

export type CallableFunc = (args: Form, env: iEnv) => Form;

export class Procedure {
  constructor(
    public env: iEnv,
    public params: Form,
    public expr: Form,
    public name = 'λ',
  ) {}
  public call(args: Form, env?: iEnv): Form {
    const closure = new Env(this.params, args, env ?? this.env)
    return evaluate(this.expr, closure);
  }
}

export type Callable = Procedure | NativeFunc | Syntax
export type Closure = Callable | Function
