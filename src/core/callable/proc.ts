import type { iEnv } from "../../interface/iEnv";
import { evaluate } from "../eval";
import type { Form } from "../form";
import { Env } from "../env";
import type { Syntax } from "./syntax";
import { NativeFunc } from "./func";
import { Token } from "../read";

export type CallableFunc = (args: Form, env: iEnv) => Form;

export class Procedure {
  constructor(
    public env: iEnv,
    public params: Form,
    public expr: Form,
    public name = 'λ',
  ) {}
  public call(args: Form, scope?: iEnv): Form {
    const closure = new Env(this.params, args, this.env)
    return evaluate(this.expr, closure);
  }
  public token?: Token
}

export type Callable = Procedure | NativeFunc | Syntax
export type Closure = Procedure | NativeFunc
