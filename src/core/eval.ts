import assert from "assert";
import { isEmpty, isF, isList, isNativeFn, isNum, isProc, isString, isSym, toString, toStringSafe } from "../utils";
import { EMPTY, FALSE } from "./const";
import { Env } from "./env";
import { atom, car, cdr, eq, _do } from "./lisp";
import { Proc } from "./proc";
import { SymTable } from "./sym";
import { Atom, Term } from "./terms";


export const evaluate = (e: Term, a: Env): Term => {
  while (true) {
    if (isSym(e)) return a.getFrom(e);
    else if (!isList(e)) return e ?? EMPTY;
    else  switch (e[0]) {
      case SymTable.QUOTE: return e[1];
      case SymTable.ATOM: return atom(evaluate(e[1], a));
      case SymTable.EQ: return eq(evaluate(e[1], a),
                                  evaluate(e[2], a));
      case SymTable.CAR: return car(evaluate(e[1], a));
      case SymTable.CDR: return cdr(evaluate(e[1], a));
      case SymTable.COND: return evalCond(e[1], a, e);
      case SymTable.LAMBDA: {
        return new Proc(e[1], e[2], a) as any;
      }
      case SymTable.DEFUN:
      case SymTable.DEFINE: {
        const [_def, variable, expr] = e as [Atom, symbol, Term];
        const name = toString(variable)
        const value = evaluate(expr, a);
        if (isProc(value)) { value.name = name; }
        a.set(name, value);
        return value;
      }
      case SymTable.DO: {
        const [_def, ...exprs] = e as [Atom, ...Term[]];
        return _do(exprs, a);
      }
      case SymTable.BEGIN: {
        const [_def, ...exprs] = e as [Atom, ...Term[]];
        exprs.slice(0, -1).forEach(expr => evaluate(expr, a));
        e = exprs.pop()!
        break
      }
      case SymTable.IF: {
        const [_if, cond, then_, else_] = e as [Atom, Term, Term, Term];
        e = (isF(evaluate(cond, a))) ? else_ : then_;
        break
      }
      case SymTable.SET: {
        const [_set, variable, value] = e as [Atom, Atom, Term];
        assert(a.hasFrom(variable), 'Variable must be bound');
        const name = toString(variable);
        a.find(name)!.set(name, evaluate(value, a));
        return [];
      }
      default: {
        const [proc, ...args] = e.map(expr => evaluate(expr, a));
        if (isProc(proc)) {
          e = proc.expr
          a = new Env(proc.params, args, proc.env)
          break
        }
        if (!isNativeFn(proc)) console.log('here', e)
        return proc ? (<any>proc).call(args, a) : [];
        // return (isNativeFn(proc)) ? proc.call(args, a) : args;
      }
    }
  }
};

export const evalCond = (c: Term, a: Env, mm: any): Term => {
  assert(isEmpty(c) === false, 'Fallthrough condition');
  const [[cond, then], ...rest] = c as any[];
  if (evaluate(cond, a) !== FALSE) {
    return evaluate([SymTable.BEGIN, ...then], a);
  }
  return evalCond(rest, a, mm);
};
