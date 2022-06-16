import { isPair } from "../guard";
import { iWorld } from "../interface/iWorld";
import { assert, toL } from "../utils";
import { Resume } from "./data/cont";
import { evaluate } from "./eval";
import { expand } from "./expand";
import type { Form } from "./form";
import { InPort } from "./data/port";
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

export const tokenize = async (code: string, world: iWorld): Promise<Form> => {
  return await read(InPort.fromString(code), world);
};

export const parse = async (code: string, world: iWorld): Promise<Form> => {
  return await expand(await read(InPort.fromString(code), world), true, world);
};

export const execute = async (code: string, world: iWorld): Promise<Form> => {
  const parsed = await parse(code, world);
  // const r = toString(parsed, true);
  // console.log(r)
  return await evaluate(parsed, world.env);
};

export const debugExecute = async (code: string, world: iWorld): Promise<Form> => {
  async function innerDebugExecute(level = 1): Promise<any> {
    try {
      const result = await execute(code, world);
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
              (repl)))`, world)
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

  await execute(`(load "stdlib/io.scm")`, world)
  return await innerDebugExecute();
};
