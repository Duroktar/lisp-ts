import assert from "assert";
import * as Utils from "../utils";
import { UNDEF } from "./const";
import type { Env } from "./env";
import { isCallable, Proc } from "./proc";
import { Sym, SymTable } from "./sym";
import { SyntaxRulesDef } from "./syntax";
import type { List, Term } from "./terms";
import { toString, toStringSafe } from "./toString";

export const expand = (e: Term, topLevel = false, env: Env): Term => {
  assert(Utils.isEmpty(e) === false, `() => Error`)
  if (!Utils.isList(e)) { return e }
  else if (SymTable.QUOTE === e[0]) {
    Utils.expect(e, e.length === 2);
    return e;
  }
  else if (SymTable.IF === e[0]) {
    if (e.length === 3) e.push(UNDEF)
    assert(e.length === 4, `Invalid if form: ${toStringSafe(e)}`)
    return e.map(e => expand(e, false, env));
  }
  else if (SymTable.SET === e[0]) {
    const [_set, variable, value] = e;
    assert(Utils.isSym(variable), 'First arg to set! must be a symbol');
    return [_set, variable, value];
  }
  else if (SymTable.BEGIN === e[0]) {
    const [_begin, ...exprs] = e;
    Utils.expect(e, exprs.length >= 1, 'expand begin');
    const rrv = exprs.map(x => expand(x, topLevel, env));
    return [_begin, ...rrv];
  }
  else if (SymTable.DEFINESYNTAX === e[0]) {
    Utils.expect(e, e.length >= 3, 'expand define-syntax');
    return expandDefineSyntax(e, topLevel, env);
  }
  else if (SymTable.DEFINE === e[0]) {
    const [_def, v, ...rest] = e;
    if (Utils.isList(v) && !Utils.isEmpty(v)) {
      const [name, ...args] = v
      return expand([SymTable.DEFUN, name, args, ...rest], false, env);
    }
    const [_, name, args, ...body] = e;
    if (Utils.isEmpty(body)) {
      return [_def, name, expand(args, false, env)];
    }
    return expand([SymTable.DEFUN, name, args, ...body], false, env);
  }
  else if (SymTable.DEFUN === e[0]) {
    Utils.expect(e, e.length >= 3, 'expand defun');
    return expandDefun(e, env);
  }
  else if (SymTable.LAMBDA === e[0]) {
    Utils.expect(e, e.length >= 3, 'expand lambda');
    return expandLambda(e, env);
  }
  else if (SymTable.QUASIQUOTE === e[0]) {
    Utils.expect(e, e.length === 2, 'expand quasi-quote');
    return expandQuasiquote(e[1]);
  }
  else if (env.hasFrom(e[0])) {
    const proc = env.getFrom<Proc>(e[0]);
    const form = e.slice(1);
    if (isCallable(proc)) {
      return proc.call(form, env);
    }
    // allow functions as well
    return expand(proc(...form), topLevel, env);
  }
  return e.map(x => expand(x, false, env));
};

export const expandQuasiquote = (x: Term): Term => {
  if (!Utils.isPair(x)) return [SymTable.QUOTE, x];
  Utils.expect(x, x !== SymTable.UNQUOTESPLICING, "can't slice here");
  if (Array.isArray(x)) {
    if (x[0] === SymTable.UNQUOTE) {
      Utils.expect(x, Utils.isList(x) && x.length === 2);
      return x[1];
    }
    if (Utils.isList(x[0]) && x[0][0] === SymTable.UNQUOTESPLICING) {
      Utils.expect(x, Utils.isList(x[0]) && x[0].length === 2);
      return [SymTable.APPEND, x[0][1], expandQuasiquote(x.slice(1))];
    }
    else {
      return [SymTable.CONS, expandQuasiquote(x[0]), expandQuasiquote(x.slice(1))];
    }
  }
  throw new Error('unexpected state (expandQuasiquote)')
};

function expandDefineSyntax(e: List, topLevel: boolean, env: Env) {
  let [_def, name, ...rest] = e;
  Utils.expect(e, topLevel, 'define-syntax only allowed at top level');
  Utils.expect(rest, Utils.isList(rest[0]) && rest[0][0] === Sym('syntax-rules'));
  const [_syn, literals, ...syntaxRules] = rest[0] as any[];
  Utils.expect(e, Utils.isList(literals) && literals.every(Utils.isIdent));
  Utils.expect(e, syntaxRules.every(rule => Utils.isList(rule) && rule.length === 2));
  syntaxRules.forEach(([pattern, template]) => {
    const [id, ...listPattern] = pattern as any[];
    Utils.expect(e, id === name, 'syntax-rule patterns must begin with the keyword for the macro');
    Utils.expect(e, Utils.isList(listPattern) && listPattern.every(p => Utils.isList(p) || Utils.isIdent(p) || Utils.isConst(p)), 'malformed list pattern');
    Utils.expect(e, Utils.isIdent(template) || Utils.isConst(template) || Utils.isList(template), 'malformed template');
  });

  env.setFrom(name, new SyntaxRulesDef(name, env, syntaxRules, literals) as any);
  return [];
}

function expandLambda(e: List, env: Env) {
  const [_lambda, params, ...expression] = e;
  const allAtoms = Utils.isList(params) && params.every(Utils.isSym);
  assert(allAtoms || Utils.isSym(params), `Invalid lambda args. Expected a list of atoms or a single atom but instead got: ${toString(params)}, ${toString(e)}`);
  assert(expression.length >= 1, `lambda expression empty`);
  const body = (expression.length > 1) && [SymTable.BEGIN, ...expression]
  return [_lambda, params, expand(body || expression[0], false, env)];
}

function expandDefun(e: List, env: Env): Term {
  let [_def, name, args, body] = e;
    Utils.expect(e, Utils.isSym(name), `Can only define a symbol`);
    Utils.expect(e, Utils.isList(args) || Utils.isSym(args), `Invalid args`);
    const expr = expand([SymTable.LAMBDA, args, body], false, env);
    return [_def, name, expr]
}

