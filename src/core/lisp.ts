import * as Utils from "../utils";
import { evaluate } from "./eval";
import { expand } from "./expand";
import { InPort } from "./port";
import { read } from "./read";
import type { Form } from "./forms";
import { Environment } from "../env";
import { Resume } from "./cont";
import assert from "assert";
import { toStringSafe } from "./toString";

// primitives (7)
export const quote = (expr: Form): Form => {
  assert(Utils.isPair(expr), 'quote operates on a pair');
  return cdr(expr);
}
export const atom = (expr: Form): Form => {
  return Utils.toL(Utils.isAtom(expr));
}

export const eq = (x: Form, y: Form): Form => {
  return Utils.toL(Utils.isPair(x) ? x.equal(y) : x === y);
}
export const car = (expr: Form): Form => {
  assert(Utils.isPair(expr), `Argument to car must be a pair. got: ${toStringSafe(expr)}`)
  return expr.car;
}
export const cdr = (expr: Form): Form => {
  assert(Utils.isPair(expr), `Argument to cdr must be a pair. got: ${toStringSafe(expr)}`)
  return expr.cdr!;
}
// END primitives

export const cadr = (e: any) => car(cdr(e))
export const caar = (e: any) => car(car(e))
export const cddr = (e: any) => cdr(cdr(e))
export const caddr = (e: any) => car(cdr(cdr(e)))
export const cdddr = (e: any) => cdr(cdr(cdr(e)))
export const cadddr = (e: any) => car(cdr(cdr(cdr(e))))
export const caaddr = (e: any) => car(car(cdr(cdr(e))))

export const tokenize = async (code: string, env: Environment): Promise<Form> => {
  return await read(InPort.fromString(code), env.readerEnv);
};

export const parse = async (code: string, {readerEnv}: Environment): Promise<Form> => {
  return await expand(await read(InPort.fromString(code), readerEnv), true, readerEnv);
};

export const execute = async (code: string, env: Environment): Promise<Form> => {
  const parsed = await parse(code, env);
  return await evaluate(parsed, env.env);
};

export const debugExecute = async (code: string, env: Environment): Promise<Form> => {
  async function innerDebugExecute(level = 1): Promise<any> {
    try {
      const result = await execute(code, env);
      return result;
    } catch (outerError) {
      if (outerError instanceof Error) {
        // console.error('outerError')
        console.error(outerError.stack)
        try {
          return await execute(`
            (begin (
              (write ${JSON.stringify(outerError.message)})
              (newline)
              (write "Entering REPL...")
              (newline)
              (repl)))`, env)
        } catch (innerError) {
          // console.error('innerError')
          // console.error(innerError)
          if (innerError instanceof Resume) {
            return await innerDebugExecute(level++)
          }
          // throw innerError
        }
      }
      throw outerError
    }
  }

  await execute(`(load "stdlib/io.scm")`, env)
  return await innerDebugExecute();
};
