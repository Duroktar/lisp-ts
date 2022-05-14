import * as Utils from "../utils";
import { Env } from "./env";
import { evaluate } from "./eval";
import { macroTable } from "./macro";
import { Proc } from "./proc";
import { SymTable } from "./sym";
import { Expr, List } from "./terms";

export const expand = (expr: Expr, topLevel = false, env: Env = new Env()): Expr => {
  const e = expr as Expr[];
  if (!Utils.isList(e)) { return e; }
  if (Utils.isEmpty(e)) { return e; }
  if (SymTable.QUOTE === e[0]) {
    Utils.expect(e, e.length === 2);
    return e;
  }
  else if (SymTable.COND === e[0]) {
    const [_def, ...exprs] = e;
    const preds = exprs.map(pair => {
      const [head, ...tail] = pair as any[];
      const res = tail.map(x => expand(x, false, env));
      // console.log([head, res])
      return [head, res];
    });
    Utils.expect(preds, Utils.isList(preds) && preds.every(x => x.length === 2 && x.every(e => Utils.isNone(e) === false)), `found invalid cond entry where (length != 2): (${(Utils.isList(preds) ? preds.find(x => x.length !== 2) : preds)})`);
    return [_def, preds];
  }
  else if (SymTable.BEGIN === e[0]) {
    const [_begin, ...exprs] = e;
    if (Utils.isEmpty(exprs))
      return [];
    return [_begin, ...exprs.map(x => expand(x, topLevel, env))];
  }
  else if (SymTable.DEFINE === e[0]) {
    const [_def, name, args, ...body] = e;
    if (Utils.isEmpty(body)) {
      return [_def, name, args];
    }
    return expand([SymTable.DEFUN, name, args, ...body], false, env);
  }
  else if ([SymTable.DEFUN, SymTable.DEFINEMACRO].includes(<any>e[0])) {
    Utils.expect(e, e.length >= 3);
    const [_def, name, args, body] = e;
    Utils.expect(e, Utils.isSym(name));
    Utils.expect(e, Utils.isList(args) || Utils.isSym(args));
    const expr: List = expand([SymTable.LAMBDA, args, body], false, env) as any;
    Utils.expect(expr, expr.length >= 1, `body list size should be at least 1, got: ${expr.length}`);
    if (_def === SymTable.DEFINEMACRO) {
      Utils.expect(e, topLevel, 'define-macro only allowed at top level');
      const callee: Proc = evaluate(expr, env) as any;
      callee.name = Utils.toString(name);
      Utils.expect(e, Utils.isProc(callee), 'macro must be a procedure');
      macroTable[callee.name] = callee as any;
      return [];
    }
    return [_def, name, expr];
  }
  else if (SymTable.LAMBDA === e[0]) {
    const [_lambda, params, ...expression] = e;
    const allAtoms = Utils.isList(params) && params.every(Utils.isSym);
    Utils.expect(e, (allAtoms || Utils.isSym(params)), 'Invalid args');
    Utils.expect(e, expression.length >= 1, `lambda expression empty`);
    const body: any = expression.length === 1 ? expression[0] : [SymTable.BEGIN, ...expression];
    return [_lambda, params, expand(body, false, env)];
  }
  else if (SymTable.QUASIQUOTE === e[0]) {
    Utils.expect(e, e.length === 2);
    return expandQuasiquote(e[1]);
  }
  else if (Utils.toString(e[0]) in macroTable) {
    const name = Utils.toString(e[0]);
    const proc: any = macroTable[name];
    const args = e.slice(1);
    if (Utils.isProc(proc)) {
      const args1 = args.map(expr => expand(expr, topLevel, env));
      const env1 = new Env(proc.params, args1, proc.env);
      const result = evaluate(proc.expr, env1);
      return expand(result, topLevel, env1);
    }
    return expand(proc(...args), topLevel, env);
  }
  return e.map(x => expand(x, false, env));
};

export const expandQuasiquote = (x: Expr): Expr => {
  if (!Utils.isPair(x)) return [SymTable.QUOTE, x];
  Utils.expect(x, x !== SymTable.UNQUOTESPLICING, "can't slice here");
  if (Array.isArray(x)) {
    if (Utils.isList(x) && x[0] === SymTable.UNQUOTE) {
      Utils.expect(x, Utils.isList(x) && x.length === 2);
      return x[1];
    }
    if (Utils.isList(x[0]) && x[0][0] === SymTable.UNQUOTESPLICING) {
      Utils.expect(x[0], Utils.isList(x[0]) && (<List>x[0]).length === 2);
      return [SymTable.APPEND, x[0].slice(1), expandQuasiquote(x.slice(1))];
    }
    else {
      return [SymTable.CONS, expandQuasiquote(x[0]), expandQuasiquote(x.slice(1))];
    }
  }
  throw new Error('unexpected state (expandQuasiquote)')
};
