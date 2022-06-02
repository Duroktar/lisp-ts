import assert from "assert";
import { isList, isSym, isTruthy } from "../utils";
import { EMPTY } from "./const";
import { Env } from "./env";
import { atom, car, cdr, _do } from "./lisp";
import { isNativeProc, isProc, Procedure } from "./proc";
import { SymTable } from "./sym";
import type { Atom, Form } from "./forms";
import { toString } from "./toString";

export const evaluate = async (e: Form, a: Env): Promise<Form> => {
  while (true) {
    // console.log('evaluating term:', toStringSafe(e));
    if (isSym(e)) return a.getFrom<Form>(e);
    else if (!isList(e)) return e ?? EMPTY;
    else switch (e[0]) {
      case SymTable.QUOTE: return e[1];
      case SymTable.ATOM: return atom(await evaluate(e[1], a));
      case SymTable.CAR: return car(await evaluate(e[1], a));
      case SymTable.CDR: return cdr(await evaluate(e[1], a));
      case SymTable.LAMBDA: {
        return new Procedure(e[1], e[2], a) as any;
      }
      case SymTable.DEFUN:
      case SymTable.DEFINE: {
        const [_def, variable, expr] = e as [Atom, symbol, Form];
        const name = toString(variable)
        const value = await evaluate(expr, a);
        if (isProc(value)) { value.name = name; }
        a.set(name, value);
        return undefined as any
      }
      case SymTable.DO: {
        const [_def, ...exprs] = e as [Atom, ...Form[]];
        return _do(exprs, a);
      }
      case SymTable.BEGIN: {
        const [_def, ...exprs] = e as [Atom, ...Form[]];
        exprs.slice(0, -1)
          .forEach(async expr => await evaluate(expr, a));
        e = exprs.pop()!
        break
      }
      case SymTable.IF: {
        const [_if, cond, then_, else_] = e as [Atom, Form, Form, Form];
        if (isTruthy(await evaluate(cond, a)))
          e = then_;
        else
          e = else_;
        // e = (isTruthy(evaluate(cond, a))) ? then_ : else_;
        break
      }
      case SymTable.SET: {
        const [_set, variable, value] = e as [Atom, Atom, Form];
        assert(a.hasFrom(variable), 'Variable must be bound');
        const name = toString(variable);
        a.find(name)!.set(name, await evaluate(value, a));
        return undefined as any
      }
      default: {
        const [proc, ...args] = await Promise.all(e.map(expr => evaluate(expr, a)));
        if (isNativeProc(proc)) {
          const env = new Env(proc.params, args, proc.env)
          return await proc.call(args, env)
        }
        else if (isProc(proc)) {
          e = proc.expr
          a = new Env(proc.params, args, proc.env)
          break
        }
        else
          return undefined as any
      }
    }
  }
};
