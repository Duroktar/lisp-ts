import { isBinding, isEmpty, isList, isNativeProc, isPair, isProc, isSym } from "../guard";
import { iEnv } from "../interface/iEnv";
import { LogConfig } from "../logging";
import { assert } from "../utils";
import { Procedure } from "./callable/proc";
import { FALSE, UNDEF } from "./const";
import { cons, Pair } from "./data/pair";
import { SymTable } from "./data/sym";
import { Env } from "./env";
import { AssertionError } from "./error";
import type { Form } from "./form";
import { cadddr, caddr, cadr, car, cdr } from "./lisp";
import { toString, toStringSafe } from "./print";

const DEBUG = LogConfig.eval;

function debugLog(...args: any[]): void {
  if (DEBUG) { console.log('[Evaluate]:'.cyan, ...args); }
}

export function evaluate(e: Form, a: iEnv, stack: [proc: string, args: string][] = []): Form {
  while (true) {
    console.log('evaluating:', toString(e))
    if (isSym(e)) {
      const rv = a.getFrom<Form>(e);
      return rv;
    }
    else if (!isPair(e)) {
      if (isBinding(e)) {
        const rv = e.force();
        return rv
      }
      return e;
    }
    else {
      switch (true) {
        case SymTable.QUOTE.equal(e.car): return cadr(e);
        case SymTable.CAR.equal(e.car): return car(evaluate(cadr(e), a));
        case SymTable.CDR.equal(e.car): return cdr(evaluate(cadr(e), a));
        case SymTable.LAMBDA.equal(e.car): {
          return new Procedure(a, cadr(e), caddr(e));
        }
        case SymTable.DEFINE.equal(e.car): {
          const value = evaluate(caddr(e), a);
          const name = toString(cadr(e))
          if (isProc(value)) { value.name = name; }
          a.set(name, value);
          return UNDEF
        }
        case SymTable.BEGIN.equal(e.car): {
          const exprs = (<Pair>e.cdr).slice(0, -1);
          for (let expr of (isEmpty(exprs) ? [] : exprs)) {
            evaluate(expr, a)
          }
          e = (<Pair>e.cdr).at(-1)
          break
        }
        case SymTable.IF.equal(e.car): {
          const result = evaluate(cadr(e), a);
          if (!FALSE.equal(result))
            e = caddr(e)
          else {
            if (e.length < 4)
              return result
            e = cadddr(e)
          }
          break
        }
        case SymTable.SET.equal(e.car): {
          assert(a.hasFrom(cadr(e)), 'Variable must be bound', e.car);
          const name = toString(cadr(e));
          a.find(name)!.set(name, evaluate(caddr(e), a));
          return UNDEF
        }
        default: {

          const proc = evaluate(car(e), a);
          const args = evaluateList(cdr(e), a);

          const procStr = toStringSafe(proc);
          const argsStr = toStringSafe(args);

          // debugLog('proc:', procStr);
          // debugLog('args:', argsStr);

          if (isNativeProc(proc)) {
            stack.push([procStr, argsStr])

            debugLog('evaluating native proc', (<any>proc)?.name);
            // debugLog(' - form', toStringSafe(e));
            printStack(debugLog, stack);

            const rv = proc.call(args);
            stack.pop();
            return rv
          }
          else if (isProc(proc)) {
            e = proc.expr
            a = new Env(proc.params, args, proc.env)

            debugLog('evaluating proc', (<any>proc)?.name);
            // debugLog(' - form', toStringSafe(e));

            printStack(debugLog, stack);

            continue
          }

          throw new AssertionError(
            `Cannot evaluate form: ${
              toStringSafe(e)
            }\n${
              printStack(debugLog, stack)
            }`)
        }
      }
    }
  }
};


function evaluateList(e: Form, a: iEnv): Form {
  assert(isList(e), 'evaluateList passed a non list value')
  // debugLog('evaluating list', toStringSafe(e));
  if (isEmpty(e)) return e
  if (isPair(e)) {
    return cons(evaluate(car(e), a), evaluateList(cdr(e), a))
  }
  return evaluate(e, a)
}

function printStack(cb: (arg: string) => any, stack: [proc: string, args: string][]) {
  cb(' -- stack -- ')
  stack.forEach((s, i) => {
    const msg = `(${s[0]} ${s[1]})`;
    const pad = msg.length + (i * 2);
    cb(msg.padStart(pad, ' '));
  })
}
