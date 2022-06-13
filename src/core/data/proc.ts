import type { iEnv } from "../../interface/iEnv";
import { evaluate } from "../eval";
import type { Form } from "../forms";
import { Env } from "./env";
import type { SyntaxRulesDef } from "./syntax";

export abstract class NativeProc {
  abstract name: string;
  abstract params: Form;
  abstract env: iEnv;
  public async call(args: Form, env?: iEnv): Promise<Form> {
    const closure = new Env(this.params, args, env ?? this.env)
    return await this._call(args, closure);
  }
  abstract _call(args: Form, env?: iEnv): Promise<Form>;
}

export class Procedure {
  constructor(
    public params: Form,
    public expr: Form,
    public env: iEnv,
    public name = 'λ',
  ) {}
  public async call(args: Form, env?: iEnv): Promise<Form> {
    const closure = new Env(this.params, args, env ?? this.env)
    return await evaluate(this.expr, closure);
  }
}

export const isNativeProc = (x: unknown): x is NativeProc => x instanceof NativeProc;
export const isProc = (x: unknown): x is Procedure => x instanceof Procedure;

export type Callable = Procedure | NativeProc | SyntaxRulesDef
export type Closure = Callable | Function
