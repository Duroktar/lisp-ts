import assert from "assert";
import { isSym, isTruthy, isPair, isList, isEmpty } from "../utils";
import { EMPTY, UNDEF } from "./const";
import { Env } from "./env";
import { atom, cadddr, caddr, cadr, car, cdr } from "./lisp";
import { isNativeProc, isProc, Procedure } from "./proc";
import { SymTable } from "./sym";
import type { Form } from "./forms";
import { toString, toStringSafe } from "./toString";
import { list, Pair } from "./pair";

export const evaluate = async (e: Form, a: Env): Promise<Form> => {
  while (true) {
    // console.log('evaluating term:', toStringSafe(e));
    if (isSym(e)) return a.getFrom<Form>(e);
    else if (!isPair(e)) return e ?? EMPTY;
    else switch (e.car) {
      case SymTable.QUOTE: return cadr(e);
      case SymTable.ATOM: return atom(await evaluate(cadr(e), a));
      case SymTable.CAR: return car(await evaluate(cadr(e), a));
      case SymTable.CDR: return cdr(await evaluate(cadr(e), a));
      case SymTable.LAMBDA: {
        return new Procedure(cadr(e), caddr(e), a) as any;
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
          return await proc.call(args)
        }
        else if (isProc(proc)) {
          e = proc.expr
          a = new Env(proc.params, args, proc.env)
          break
        }
        else
          return UNDEF
      }
    }
  }
};

const evaluateList = async (e: Form, a: Env): Promise<Form> => {
  assert(isList(e), 'evaluateList passed a non list value')
  if (isEmpty(e)) return e
  let rv = []
  for (let expr of e) {
    rv.push(await evaluate(expr, a))
  }
  return list(...rv)
}
