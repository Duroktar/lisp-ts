import { isCallable, isConst, isEmpty, isIdent, isList, isPair, isString, isSym } from "../guard";
import type { iEnv } from "../interface/iEnv";
import { assert } from "../utils";
import { UNDEF } from "./const";
import { cons, list, type Pair } from "./data/pair";
import type { Closure } from "./data/proc";
import { Sym, SymTable } from "./data/sym";
import { SyntaxRulesDef } from "./data/syntax";
import type { Form } from "./form";
import { caaddr, caar, caddr, cadr, car, cddr, cdr } from "./lisp";
import { toString, toStringSafe } from "./print";

const DEBUG = true

export const expand = async (e: Form, topLevel = false, lexicalEnv: iEnv): Promise<Form> => {
  // assert(isEmpty(e) === false, `() => Error`)
  if (!isPair(e)) { return e }
  if (isEmpty(e)) { return e }
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
    assert(isSym(cadr(e)), 'First arg to set! must be a symbol');
    return e
  }
  else if (SymTable.BEGIN === car(e)) {
    const { car: _begin, cdr: exprs } = e;
    assert(isPair(exprs) && exprs.length >= 1, 'expand begin');
    return cons(_begin, await expand(exprs, topLevel, lexicalEnv));
  }
  else if (SymTable.DEFINESYNTAX === car(e)) {
    assert(e.length >= 3, 'expand define-syntax');
    return await expandSyntax(e, topLevel, lexicalEnv);
  }
  else if (SymTable.DEFINE === car(e)) {
    const _def = car(e)
    const v = cadr(e)
    const body = cddr(e)
    if (isPair(v)) {
      const name = car(v)
      const args = cdr(v)
      if (isString(car(body))) {
        // strip comments
        return await expand(list(_def, name, list(SymTable.LAMBDA, args).append(cdr(body))), false, lexicalEnv);
      }
      return await expand(list(_def, name, list(SymTable.LAMBDA, args).append(body)), false, lexicalEnv);
    } else {
      assert(e.length === 3, '(define non-var/list exp) => Error')
      assert(isSym(v), 'can define only a symbol')
      const exp = await expand(caddr(e), false, lexicalEnv)
      return list(_def, v, exp);
    }
  }
  else if (SymTable.LAMBDA === car(e)) {
    assert(e.length >= 3, 'expand lambda');
    return await expandLambda(e, lexicalEnv);
  }
  else if (SymTable.QUASIQUOTE === car(e)) {
    assert(e.length === 2, 'expand quasi-quote');
    const rv = await expandQuasiquote(cadr(e));
    // debugPrint('expandQuasiquote .>'.dim, 'Returning..'.green, toString(rv));
    return rv;
  }
  else if (lexicalEnv.hasFrom(car(e))) {
    const proc = lexicalEnv.getFrom<Closure>(car(e));
    if (isCallable(proc)) {
      const result = await proc.call(cdr(e), lexicalEnv);
      return await expand(result, false, lexicalEnv);
    }
    // allow functions as well
    return await expand(await proc(cdr(e)), topLevel, lexicalEnv);
  }

  const e1 = await expand(car(e), false, lexicalEnv);
  return cons(e1, await expand(cdr(e), false, lexicalEnv));
};

export const expandQuasiquote = async (x: Form, level = 0): Promise<Form> => {
  const outerLevel = level
  let innerLevel = level
  const debugPrint = (...args: any[]) => {
    if (DEBUG) console.log(`(${innerLevel})`, ...args)
  }
  const expandQuasiquoteInner = async (x: Form): Promise<Form> => {
    if (!isPair(x)) {
      if (typeof x === 'number') {
        debugPrint('expandQuasiquote .>'.dim, toString(x), 'Not a pair..'.dim, 'LITERAL'.green, `(type: ${typeof x})`.dim);
        return x
      }
      debugPrint('expandQuasiquote .>'.dim, toString(x), 'Not a pair..'.dim, 'Quoting'.cyan, `(type: ${typeof x})`.dim);
      return list(SymTable.QUOTE, x);
    }

    assert(car(x) !== SymTable.UNQUOTESPLICING, "can't slice here");

    if (car(x) === SymTable.UNQUOTE) {
      // console.log('expandQuasiquote .>'.dim, toString(x), 'pair with `unquote` at the head..'.dim, 'UNQUOTING'.red);
      assert(x.length === 2, 'Bad unquote length');
      if (innerLevel-1 === outerLevel) {
        innerLevel = clamp(innerLevel-1, 0, Infinity)
        debugPrint('expandQuasiquote .>'.dim, toString(x), 'pair with `unquote` at the head..'.dim, 'NOT UNQUOTING'.magenta, 'WRONG LEVEL ..'.red.bgWhite);
        // const rv = list(SymTable.CONS, await expandQuasiquoteInner(cdr(x)));
        // innerLevel = clamp(innerLevel-1, 0, Infinity)
        const rv = list(SymTable.CONS, list(SymTable.QUOTE, SymTable.UNQUOTE), await expandQuasiquote(cdr(x)));
        innerLevel += 1
        return rv
      } else {
        debugPrint('expandQuasiquote .>'.dim, toString(x), 'pair with `unquote` at the head..'.dim, 'UNQUOTING'.red);
        // innerLevel += 1
        // innerLevel = clamp(innerLevel-1, 0, Infinity)
        return cadr(x);
      }
    }
    if (isPair(car(x)) && caar(x) === SymTable.QUASIQUOTE) {
      debugPrint('expandQuasiquote .>'.dim, toString(x), 'pair with `quasiquote` at the head..'.dim, 'UNQUOTING'.red);
      assert(x.length === 2, 'Bad unquote length');
      innerLevel++
      const rv = list(SymTable.CONS, await expandQuasiquoteInner(car(x)), await expandQuasiquoteInner(cdr(x)));
      return rv;
      // const x1 = await expandQuasiquoteInner(caar(x));
      // const xs = await expandQuasiquoteInner(cdr(car(x)));
      // return list(SymTable.CONS, list(SymTable.CONS, x1, xs));
    }
    // (define phrase-of-the-day '(the Lord helps those who take a big helping for themselves))
    // `(Remember that ,@phrase-of-the-day)
    if (isPair(car(x)) && caar(x) === SymTable.UNQUOTESPLICING) {
      debugPrint('expandQuasiquote .>'.dim, toString(x), 'pair with `unquote-splicing` at the head..'.dim, 'UNQUOTE-SPLICING'.blue);
      const first = car(x);
      assert(isPair(first) && first.length === 2, 'Bad unquote-splicing length');
      return list(SymTable.APPEND, cadr(first), await expandQuasiquoteInner(cdr(x)));
    }
    else {
      debugPrint('expandQuasiquote .>'.dim, toString(x), 'default case..'.dim, 'CONSING'.yellow);
      return list(SymTable.CONS, await expandQuasiquoteInner(car(x)), await expandQuasiquoteInner(cdr(x)));
    }
  };

  return expandQuasiquoteInner(x)
}

async function expandSyntax(e: Pair, topLevel: boolean, env: iEnv): Promise<any> {
  assert(topLevel, 'define-syntax only allowed at top level');
  assert(isPair(caddr(e)) && caaddr(e) === Sym('syntax-rules'));
  const syntaxRulesDefList = cdr(caddr(e))
  const literals: any = car(syntaxRulesDefList);
  const syntaxRules: any = cdr(syntaxRulesDefList);
  assert(isEmpty(literals) || (isPair(literals) && literals.every(isIdent)), 'nvvievlrwkdnmv');
  assert(isPair(syntaxRules) && syntaxRules.every(rule => isPair(rule) && rule.length === 2), 'uqwerytyioek');
  syntaxRules.forEach(form => {
    const pattern = car(form)
    const template = cadr(form)
    const id = car(pattern)
    const listPattern = cdr(pattern)
    assert(id === cadr(e), 'syntax-rule patterns must begin with the keyword for the macro');
    assert(isEmpty(listPattern) || isPair(listPattern) && listPattern.every(p => isList(p) || isIdent(p) || isConst(p)), `malformed list pattern: ${toString(listPattern)}`);
    assert(isIdent(template) || isConst(template) || isPair(template), 'malformed template');
  });
  env.setFrom(cadr(e), new SyntaxRulesDef(cadr(e), env, syntaxRules, literals) as any);
  return [];
}

async function expandLambda(e: Pair, env: iEnv): Promise<Pair> {
  const _lambda = car(e)
  const params = cadr(e)
  const expression = cddr(e)
  const allAtoms = isPair(params) && params.every(isSym);
  const improper = (isPair(params) && !params.isList()) && params.dottedEvery(isSym)
  if (!(allAtoms || improper || isSym(params))) {
    let repr = toString(e)
    debugger
  }
  assert(allAtoms || improper || isSym(params), `Invalid lambda args. Expected a list of atoms, an improper list of atoms, or a single atom but instead got: ${toString(params)}, ${toString(e)}`);
  assert(isPair(expression) && expression.length >= 1, `lambda expression empty`);
  const body = (expression.length > 1) && cons(SymTable.BEGIN, expression)
  return list(_lambda, params, await expand(body || car(expression), false, env));
}

const clamp = (value: number, min: number, max: number): number => {
  return Math.max(min, Math.min(max, value))
}
