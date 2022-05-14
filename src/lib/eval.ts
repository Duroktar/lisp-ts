import * as Utils from "../utils";
import { EMPTY } from "./const";
import { Env } from "./env";
import { atom, caddr, cadr, car, cdr, cons, eq, quote, _do } from "./lisp";
import { Proc } from "./proc";
import { SymTable } from "./sym";
import { Expr } from "./terms";


export const evaluate = (e: Expr, a: Env): Expr => {
  // console.log('evaluating:', Utils.toString(e))
  if (Utils.isSym(e)) return a.get(Utils.toString(e)) as Expr;
  else if (!Utils.isList(e)) {
    if (Utils.isString(e)) return JSON.parse(e);
    if (Utils.isNum(e)) return e;
    throw new Error(`unknown thingy: ${Utils.toString(e, true)}`);
  } else {
    switch (car(e)) {
      case SymTable.QUOTE: return quote(e);
      case SymTable.ATOM: return atom(evaluate(cadr(e), a));
      case SymTable.EQ: return eq(evaluate(cadr(e), a),
                                  evaluate(caddr(e), a));
      case SymTable.CAR: return car(evaluate(cadr(e), a));
      case SymTable.CDR: return cdr(evaluate(cadr(e), a));
      case SymTable.CONS: return cons(evaluate(cadr(e), a),
                                  evaluate(caddr(e), a));
      case SymTable.COND: return evalCond(cadr(e), a, e);
      case SymTable.LAMBDA: {
        return new Proc(cadr(e), caddr(e), a) as any;
      }
      case SymTable.DEFINE:
      case SymTable.DEFUN: {
        const [_def, variable, expr] = e;
        const name = Utils.toString(variable);
        const value = evaluate(expr, a);
        if (Utils.isProc(value)) {
          value.name = name;
        }
        a.set(name, value);
        return value;
      }
      case SymTable.BEGIN: {
        const [_begin, ...exprs] = e;
        const rv = exprs.reduce((_, expr) => evaluate(expr, a), []);
        return rv;
      }
      case SymTable.DO: {
        return _do(<any>cdr(e), a);
      }
      case SymTable.IF: {
        const [_if, cond, then_, else_] = e;
        const c = evaluate(cond, a);
        if (Utils.isT(c)) {
          return evaluate(then_, a);
        } else {
          return evaluate(else_ ?? EMPTY, a);
        }
      }
      case SymTable.SET: {
        const [_set, variable, value] = e;
        Utils.expect(e, Utils.isSym(variable), 'First arg to set! must be a symbol');
        const r = evaluate(value, a);
        const name = Utils.toString(variable);
        Utils.expect(e, a.has(name), 'Variable must be bound');
        a.find(name)!.set(name, r);
        return [];
      }
      default: {
        const [proc, ...args] = e.map(expr => evaluate(expr, a));
        if (Utils.isCallable(proc)) {
          // const c = Utils.toString(car(e));
          // console.log(`calling: ${proc.name} ${proc.name === c ? '' : c}`)
          return proc.call(args);
        }
        // console.log('evaluating list')
        return args;
      }
    }
  }
};
export const evalCond = (c: Expr, a: Env, mm: any): Expr => {
  Utils.expect(c, Utils.isEmpty(c) === false, 'Fallthrough condition');
  const [[cond, then], ...rest] = c as any[];
  if (Utils.isT(evaluate(cond, a))) {
    return evaluate([SymTable.BEGIN, ...then], a);
  }
  return evalCond(rest, a, mm);
};
