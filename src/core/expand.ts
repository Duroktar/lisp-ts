import assert from "assert";
import * as Utils from "../utils";
import { UNDEF } from "./const";
import { Env } from "./env";
import { evaluate } from "./eval";
import type { Form } from "./forms";
import { caaddr, caar, caddr, cadr, car, cddr, cdr } from "./lisp";
import { cons, list, Pair } from "./pair";
import { isProc, Closure } from "./proc";
import { Sym, SymTable } from "./sym";
import { isSyntaxRulesDef, SyntaxRulesDef } from "./syntax";
import { toString, toStringSafe } from "./toString";

export const expand = async (e: Form, topLevel = false, lexicalEnv: Env): Promise<Form> => {
  // assert(Utils.isEmpty(e) === false, `() => Error`)
  if (!Utils.isPair(e)) { return e }
  if (Utils.isEmpty(e)) { return e }
  else if (SymTable.QUOTE === car(e)) {
    assert(e.cdr, 'Invalid <datum> in `quote`');
    return e;
  }
  else if (SymTable.IF === car(e)) {
    if (e.length === 3) e.push(UNDEF)
    assert(e.length === 4, `Invalid if form: ${toStringSafe(e)} (length: ${e.length})`)
    return list(car(e)).append(await expand(cdr(e), false, lexicalEnv));
  }
  else if (SymTable.SET === car(e)) {
    assert(Utils.isSym(cadr(e)), 'First arg to set! must be a symbol');
    return e
  }
  else if (SymTable.BEGIN === car(e)) {
    const { car: _begin, cdr: exprs } = e;
    assert(Pair.is(exprs) && exprs.length >= 1, 'expand begin');
    return cons(_begin, await expand(exprs, topLevel, lexicalEnv));
  }
  else if (SymTable.DEFINESYNTAX === car(e)) {
    Utils.expect(e, e.length >= 3, 'expand define-syntax');
    return await expandDefineSyntax(e, topLevel, lexicalEnv);
  }
  else if (SymTable.DEFINE === car(e)) {
    const _def = car(e)
    const v = cadr(e)
    const body = cddr(e)
    if (Utils.isPair(v)) {
      const name = car(v)
      const args = cdr(v)
      return await expand(list(_def, name, list(SymTable.LAMBDA, args).append(body)), false, lexicalEnv);
    } else {
      assert(e.length === 3, '(define non-var/list exp) => Error')
      assert(Utils.isSym(v), 'can define only a symbol')
      const exp = await expand(caddr(e), false, lexicalEnv)
      return list(_def, v, exp);
    }
  }
  else if (SymTable.LAMBDA === car(e)) {
    Utils.expect(e, e.length >= 3, 'expand lambda');
    return await expandLambda(e, lexicalEnv);
  }
  else if (SymTable.QUASIQUOTE === car(e)) {
    Utils.expect(e, e.length === 2, 'expand quasi-quote');
    return await expandQuasiquote(cadr(e));
  }
  else if (lexicalEnv.hasFrom(car(e))) {
    const proc = lexicalEnv.getFrom<Closure>(car(e));
    if (Utils.isCallable(proc)) {
      const result = await proc.call(cdr(e), lexicalEnv);
      return await expand(result, false, lexicalEnv);
    }
    // allow functions as well
    return await expand(await proc(cdr(e)), topLevel, lexicalEnv);
  }

  const e1 = await expand(car(e), false, lexicalEnv);
  return cons(e1, await expand(cdr(e), false, lexicalEnv));
};

export const expandQuasiquote = async (x: Form): Promise<Form> => {
  if (!Utils.isPair(x)) return list(SymTable.QUOTE, x);
  assert(car(x) !== SymTable.UNQUOTESPLICING, "can't slice here");
  if (car(x) === SymTable.UNQUOTE) {
    assert(x.length === 2);
    return cadr(x);
  }
  if (Utils.isPair(car(x)) && caar(x) === SymTable.UNQUOTESPLICING) {
    assert((car(x) as Pair).length === 2);
    // return (cdr(car(x)) as Pair).append(await expandQuasiquote(cdr(x)));
    return list(SymTable.APPEND, cdr(car(x)), await expandQuasiquote(cdr(x)));
  }
  else {
    // return cons(await expandQuasiquote(car(x)), await expandQuasiquote(cdr(x)));
    return list(SymTable.CONS, await expandQuasiquote(car(x)), await expandQuasiquote(cdr(x)));
  }
};

async function expandDefineSyntax(e: Pair, topLevel: boolean, env: Env): Promise<any> {
  assert(topLevel, 'define-syntax only allowed at top level');
  assert(Utils.isPair(caddr(e)) && caaddr(e) === Sym('syntax-rules'));
  const syntaxRulesDefList = cdr(caddr(e))
  const literals: any = car(syntaxRulesDefList);
  const syntaxRules: any = cdr(syntaxRulesDefList);
  assert(Utils.isEmpty(literals) || (Utils.isPair(literals) && literals.every(Utils.isIdent)));
  assert(Utils.isPair(syntaxRules) && syntaxRules.every(rule => Utils.isPair(rule) && rule.length === 2));
  syntaxRules.forEach(form => {
    const pattern = car(form)
    const template = cadr(form)
    const id = car(pattern)
    const listPattern = cdr(pattern)
    assert(id === cadr(e), 'syntax-rule patterns must begin with the keyword for the macro');
    assert(Utils.isEmpty(listPattern) || Utils.isPair(listPattern) && listPattern.every(p => Utils.isList(p) || Utils.isIdent(p) || Utils.isConst(p)), `malformed list pattern: ${toString(listPattern)}`);
    assert(Utils.isIdent(template) || Utils.isConst(template) || Utils.isPair(template), 'malformed template');
  });
  env.setFrom(cadr(e), new SyntaxRulesDef(cadr(e), env, syntaxRules, literals) as any);
  return [];
}

async function expandLambda(e: Pair, env: Env): Promise<Pair> {
  const _lambda = car(e)
  const params = cadr(e)
  const expression = cddr(e)
  const allAtoms = Utils.isPair(params) && params.every(Utils.isSym);
  const improper = (Utils.isPair(params) && !params.isList()) && params.dottedEvery(Utils.isSym)
  if (!(allAtoms || improper || Utils.isSym(params))) {
    let repr = toString(e)
    debugger
  }
  assert(allAtoms || improper || Utils.isSym(params), `Invalid lambda args. Expected a list of atoms, an improper list of atoms, or a single atom but instead got: ${toString(params)}, ${toString(e)}`);
  assert(Pair.is(expression) && expression.length >= 1, `lambda expression empty`);
  const body = (expression.length > 1) && cons(SymTable.BEGIN, expression)
  return list(_lambda, params, await expand(body || car(expression), false, env));
}
