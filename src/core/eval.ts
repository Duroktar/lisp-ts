import * as Utils from "../utils";
import { FALSE, UNDEF } from "./const";
import { Env } from "./env";
import { atom, car, cdr, eq, _do } from "./lisp";
import { Proc } from "./proc";
import { SymTable } from "./sym";
import { Atom, Expr } from "./terms";


export const evaluate = (e: Expr, a: Env): Expr => {
  // console.log('evaluating:', Utils.toString(e))
  if (Utils.isSym(e)) return a.get(Utils.toString(e)) as Expr;
  else if (!Utils.isList(e)) {
    if (Utils.isString(e) || Utils.isNum(e)) return e;
    throw new Error(`unknown thingy: ${Utils.toString(e, true)}`);
  } else {
    switch (car(e)) {
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
      case SymTable.DEFUN: // console.log(`defining function: ${Utils.toString(e[1])}`);
      case SymTable.DEFINE: {
        const [_def, variable, expr] = e as [Atom, symbol, Expr];
        const name = Utils.toString(variable)
        const value = evaluate(expr, a);
        if (Utils.isProc(value)) {
          value.name = name;
        }
        a.set(name, value);
        return value;
      }
      case SymTable.BEGIN: {
        const [_def, ...exprs] = e as [Atom, ...Expr[]];
        return exprs.map(expr => evaluate(expr, a)).pop()!
      }
      case SymTable.DO: {
        const [_def, ...exprs] = e as [Atom, ...Expr[]];
        return _do(exprs, a);
      }
      case SymTable.IF: {
        const [_if, cond, then_, else_] = e as [Atom, Expr, Expr, Expr?];
        const c = evaluate(cond, a);
        if (!Utils.isF(c)) return evaluate(then_, a);
        return else_ ? evaluate(else_, a) : UNDEF;
      }
      case SymTable.SET: {
        const [_set, variable, value] = e;
        Utils.expect(e, Utils.isSym(variable), 'First arg to set! must be a symbol');
        const r = evaluate(value, a);
        const name = Utils.toString(variable);
        Utils.expect(e, a.has(name), 'Variable must be bound');
        a.sourceEnv(name)!.set(name, r);
        return [];
      }
      default: {
        const [proc, ...args] = e.map(expr => evaluate(expr, a));
        if (Utils.isCallable(proc)) {
          const r = proc.call(args, a);
          // const c = Utils.toString(car(e));
          // console.log(`Evaluating procedure: ${proc.name} ${proc.name === c ? '' : c}`)
          return r;
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
  if (evaluate(cond, a) !== FALSE) {
    return evaluate([SymTable.BEGIN, ...then], a);
  }
  return evalCond(rest, a, mm);
};
