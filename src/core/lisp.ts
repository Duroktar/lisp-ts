import { isPair } from "../guard";
import { iWorld } from "../interface/iWorld";
import { assert, toL } from "../utils";
import { Resume } from "./data/cont";
import { evaluate } from "./eval";
import { expand } from "./expand";
import type { Form } from "./form";
import { InPort } from "./port";
import { read } from "./read";
import { toString, toStringSafe } from "./print";

// primitives (7)
export const quote = (expr: Form): Form => {
  assert(isPair(expr), 'quote operates on a pair');
  return cdr(expr);
}

export const eq = (x: Form, y: Form): Form => {
  return toL(isPair(x) ? x.equal(y) : x === y);
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

export function tokenize(code: string, world: iWorld): Form {
  return read(InPort.fromString(code), world);
};

export function parse(code: string, world: iWorld): Form {
  const tokens = read(InPort.fromString(code), world);
  const result = expand(tokens, true, world);
  const repr = toString(tokens)
  return result;
};

export function execute(code: string, world: iWorld): Form {
  const parsed = parse(code, world);
  // const r = toString(parsed, true);
  // console.log(r)
  return evaluate(parsed, world.env);
};

export function debugExecute(code: string, world: iWorld): Form {
  function innerDebugExecute(level = 1): any {
    try {
      const result = execute(code, world);
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
              (repl)))`, world)
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

  execute(`(load "stdlib/io.scm")`, world)
  return innerDebugExecute();
};
