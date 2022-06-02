import type { Env } from "./env";
import { evaluate } from "./eval";
import { SyntaxRulesDef } from "./syntax";
import type { Form } from "./forms";

export abstract class NativeProc {
  abstract name: string;
  abstract params: Form;
  abstract env: Env;
  abstract call(args: Form, env: Env): Form;
}

export class Procedure {
  constructor(
    public params: Form,
    public expr: Form,
    public env: Env,
    public name = 'λ',
  ) {}
  public async call(args: Form, env: Env): Promise<Form> {
    return await evaluate(args, env)
  }
}

export const isNativeProc = (x: unknown): x is NativeProc => x instanceof NativeProc;
export const isProc = (x: unknown): x is Procedure => x instanceof Procedure;
export const isSyntaxRulesDef = (x: unknown): x is SyntaxRulesDef => x instanceof SyntaxRulesDef;

export const isCallable = (x: unknown): x is Procedure | NativeProc | SyntaxRulesDef => {
  return isProc(x) || isNativeProc(x) || isSyntaxRulesDef(x);
}

export type Callable = Procedure | NativeProc | SyntaxRulesDef
export type Proc = Callable | Function
