import { isEmpty, isList, isNativeProc, isPair, isProc, isSym, isTruthy } from "../guard";
import { iEnv } from "../interface/iEnv";
import { assert } from "../utils";
import { NIL, UNDEF } from "./const";
import { Env } from "./data/env";
import { list, Pair } from "./data/pair";
import { Procedure } from "./callable/proc";
import { SymTable } from "./data/sym";
import type { Form } from "./form";
import { cadddr, caddr, cadr, car, cdr } from "./lisp";
import { toString } from "./print";

export const evaluate = async (e: Form, a: iEnv): Promise<Form> => {
  while (true) {
    // console.log('[evaluate]: evaluating term:'.dim, toString(e));
    if (isSym(e)) return a.getFrom<Form>(e);
    else if (!isPair(e)) return e ?? NIL;
    else switch (e.car) {
      case SymTable.QUOTE: return cadr(e);
      case SymTable.CAR: return car(await evaluate(cadr(e), a));
      case SymTable.CDR: return cdr(await evaluate(cadr(e), a));
      case SymTable.LAMBDA: {
        return new Procedure(a, cadr(e), caddr(e));
      }
      case SymTable.DEFINE: {
        const value = await evaluate(caddr(e), a);
        const name = toString(cadr(e))
        if (isProc(value)) { value.name = name; }
        a.set(name, value);
        return UNDEF
      }
      case SymTable.BEGIN: {
        const exprs = (<Pair>e.cdr).slice(0, -1);
        for (let expr of (isEmpty(exprs) ? [] : exprs)) {
          await evaluate(expr, a)
        }
        e = (<Pair>e.cdr).at(-1)
        break
      }
      case SymTable.IF: {
        if (isTruthy(await evaluate(cadr(e), a)))
          e = caddr(e)
        else
          e = cadddr(e)
        break
      }
      case SymTable.SET: {
        assert(a.hasFrom(cadr(e)), 'Variable must be bound');
        const name = toString(cadr(e));
        a.find(name)!.set(name, await evaluate(caddr(e), a));
        return UNDEF
      }
      default: {
        const proc = await evaluate(car(e), a);
        const args = await evaluateList(cdr(e), a);
        if (isNativeProc(proc)) {
          let rv = await proc.call(args, a);
          return rv
        }
        else if (isProc(proc)) {
          e = proc.expr
          a = new Env(proc.params, args, proc.env)
          break
        }
        else {
          throw new Error('Unexpected: ' + e.constructor.name)
        }
      }
    }
  }
};

const evaluateList = async (e: Form, a: iEnv): Promise<Form> => {
  assert(isList(e), 'evaluateList passed a non list value')
  if (isEmpty(e)) return e
  let rv = []
  for (let expr of e) {
    rv.push(await evaluate(expr, a))
  }
  return list(...rv)
}
