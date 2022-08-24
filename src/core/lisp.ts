import { isPair } from "../guard";
import { assert } from "../utils";
import { Resume } from "./callable/cont";
import { evaluate } from "./eval";
import { expand } from "./expand";
import type { Form } from "./form";
import { InPort, Port } from "./port";
import { read } from "./read";
import { toStringSafe } from "./print";
import { decorateErrorWithSourceInfo } from "./error";
import { iEnv } from "../interface/iEnv";

// primitives (7)
export const quote = (expr: Form): Form => {
  assert(isPair(expr), 'quote operates on a pair');
  return cdr(expr);
}

export const car = (expr: Form): Form => {
  assert(isPair(expr), `Argument to car must be a pair. got: ${toStringSafe(expr)}`)
  return expr.car;
}
export const cdr = (expr: Form): Form => {
  assert(isPair(expr), `Argument to cdr must be a pair. got: ${toStringSafe(expr)}`)
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

export function tokenizeWithPort(code: string, env: iEnv): [Form, Port] {
  const port = InPort.fromString(code);
  return [read(port, env), port];
};

export function tokenize(code: string, env: iEnv): Form {
  return tokenizeWithPort(code, env)[0]
};

export function parseWithPort(code: string, env: iEnv): [Form, Port] {
  const port = InPort.fromString(code);
  const tokens = read(port, env);
  return [expand(tokens, env, true), port];
};

export function parse(code: string, env: iEnv): Form {
  return parseWithPort(code, env)[0]
};

export function execute(code: string, env: iEnv): Form {
  const [parsed, port] = parseWithPort(code, env);
  const result = evaluate(parsed, env);
  return result;
};

export function debugExecute(code: string, env: iEnv): Form {
  function innerDebugExecute(level = 1): any {
    try {
      const result = execute(code, env);
      return result;
    } catch (outerError) {
      if (outerError instanceof Error) {
        // console.error('outerError')
        console.error(outerError.stack)
        try {
          return execute(`
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
            return innerDebugExecute(level++)
          }
          // throw innerError
        }
      }
      throw outerError
    }
  }

  execute(`(load "stdlib/io.scm")`, env)
  return innerDebugExecute();
};
