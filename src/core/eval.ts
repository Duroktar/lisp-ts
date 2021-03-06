import { isEmpty, isList, isNativeProc, isPair, isProc, isSym } from "../guard";
import { iEnv } from "../interface/iEnv";
import { as, assert } from "../utils";
import { FALSE, NIL, UNDEF } from "./const";
import { Env } from "./data/env";
import { list, Pair } from "./data/pair";
import { Procedure } from "./callable/proc";
import { SymTable } from "./data/sym";
import type { Form } from "./form";
import { cadddr, caddr, cadr, car, cdr } from "./lisp";
import { toString, toStringSafe } from "./print";

const debug = false;

function debugLog(...args: any[]): void {
  if (debug) { console.log('[Eval]:'.cyan, ...args); }
}

export function evaluate(e: Form, a: iEnv): Form {
  while (true) {
    debugLog('[evaluate]: evaluating term:'.dim, toString(e));
    if (isSym(e)) return a.getFrom<Form>(e);
    else if (!isPair(e)) return e ?? NIL;
    else switch (e.car) {
      case SymTable.QUOTE: return cadr(e);
      case SymTable.CAR: return car(evaluate(cadr(e), a));
      case SymTable.CDR: return cdr(evaluate(cadr(e), a));
      case SymTable.LAMBDA: {
        return new Procedure(a, cadr(e), caddr(e));
      }
      case SymTable.DEFINE: {
        const value = evaluate(caddr(e), a);
        const name = toString(cadr(e))
        if (isProc(value)) { value.name = name; }
        a.set(name, value);
        return UNDEF
      }
      case SymTable.BEGIN: {
        const exprs = (<Pair>e.cdr).slice(0, -1);
        for (let expr of (isEmpty(exprs) ? [] : exprs)) {
          evaluate(expr, a)
        }
        e = (<Pair>e.cdr).at(-1)
        break
      }
      case SymTable.IF: {
        if (evaluate(cadr(e), a) !== FALSE)
          e = caddr(e)
        else
          e = cadddr(e)
        break
      }
      case SymTable.SET: {
        assert(a.hasFrom(cadr(e)), 'Variable must be bound');
        const name = toString(cadr(e));
        a.find(name)!.set(name, evaluate(caddr(e), a));
        return UNDEF
      }
      default: {
        const proc = evaluate(car(e), a);
        const args = evaluateList(cdr(e), a);
        if (isNativeProc(proc)) {
          let rv = proc.call(args, a);
          return rv
        }
        else if (isProc(proc)) {
          e = proc.expr
          a = new Env(proc.params, args, proc.env)
          break
        }
        else {
          throw new Error(`Unexpected: ${e.constructor.name}\n${toStringSafe(e)}`)
        }
      }
    }
  }
};

function evaluateList(e: Form, a: iEnv): Form {
  assert(isList(e), 'evaluateList passed a non list value')
  if (isEmpty(e)) return e
  let rv = []
  for (let expr of e) {
    rv.push(evaluate(expr, a))
  }
  return list(...rv)
}
