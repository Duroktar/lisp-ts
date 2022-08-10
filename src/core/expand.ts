import { isBinding, isEmpty, isExpansion, isList, isMacro, isPair, isSym } from "../guard";
import { iEnv } from "../interface/iEnv";
import { LogConfig } from "../logging";
import { assert, clamp, map, sequence } from "../utils";
import type { Closure } from "./callable/proc";
import { isSyntax } from "./callable/syntax";
import { NIL } from "./const";
import { cons, list, type Pair } from "./data/pair";
import { SymTable } from "./data/sym";
import { Env } from "./env";
import type { Form, List } from "./form";
import { caar, caddr, cadr, car, cddr, cdr } from "./lisp";
import { toString, toStringSafe } from "./print";

const DEBUG = LogConfig.expand;

function debugLog(...args: string[]): void {
  if (DEBUG) { console.log('[Expand]:'.yellow, ...args) }
}

export function expand(e: Form, env: iEnv, topLevel = false): Form {
  if (!isPair(e)) { return e }
  if (isEmpty(e)) { return e }
  else if (SymTable.QUOTE.equal(car(e))) {
    assert(e.cdr, 'Invalid <datum> in `quote`');
    return e;
  }
  else if (SymTable.IF.equal(car(e))) {
    assert(e.length > 2, `Invalid if form: ${toStringSafe(e)}`);
    return list(car(e)).append(expand(cdr(e), env));
  }
  else if (SymTable.SET.equal(car(e))) {
    assert(isSym(cadr(e)), 'First arg to set! must be a symbol');
    getUpdatedScope(cadr(e), env, topLevel);
    return e
  }
  else if (SymTable.DEFINE.equal(car(e))) {
    const [_def, v, body] = sequence(car, cadr, cddr, e);
    if (isPair(v)) {
      const l = list(SymTable.LAMBDA, cdr(v)).append(body);
      const scope = getUpdatedScope(car(v), env, topLevel);
      return expand(list(_def, car(v), l), scope);
    } else {
      assert(e.length === 3, '(define non-var/list exp) => Error');
      assert(isSym(v) || isBinding(v), `can define only a symbol, got: ${toStringSafe(v)}`);
      const scope = getUpdatedScope(v, env, topLevel);
      return list(_def, v, expand(caddr(e), scope));
    }
  }
  else if (SymTable.LAMBDA.equal(car(e))) {
    assert(e.length >= 3, 'expand lambda');
    return expandLambda(e, env);
  }
  else if (SymTable.QUASIQUOTE.equal(car(e))) {
    assert(e.length === 2, 'expand quasi-quote');
    return expandQuasiquote(cadr(e));
  }
  else if (env.hasFrom(car(e))) {
    const [name, formals] = sequence(car, cdr, e) as [Pair, List];
    const proc = env.getFrom<Closure>(name);
    if (isSyntax(proc) || isMacro(proc)) {
      debugLog('calling macro: ', proc.name);
      const result = proc.call(formals, env);
      debugLog('result of calling macro: ', toStringSafe(result));
      if (isExpansion(result)) {
        const rv = expand(result.expression, env);
        debugLog('macro expansion: ', toStringSafe(rv));
        return rv;
      }
      return expand(result, env);
    }
  }

  const head = expand(car(e), env);
  return cons(head, expand(cdr(e), env));
};

function getUpdatedScope(form: Form, env: iEnv, topLevel: boolean) {
  const scope = topLevel ? env : new Env(list(), list(), env);
  debugLog('defining:', toString(form));
  scope.setFrom(form, NIL);
  return scope;
}

function expandQuasiquote(x: Form, level = 0): Form {
  const outerLevel = level
  let innerLevel = level

  const debugPrint = (...args: any[]) => {
    if (DEBUG) console.log(`(${innerLevel})`, ...args)
  }

  const expandQuasiquoteInner = (x: Form): Form => {
    if (!isPair(x)) {
      if (typeof x === 'number') {
        debugPrint('expandQuasiquote .>'.dim, toString(x), 'Not a pair..'.dim, 'LITERAL'.green, `(type: ${typeof x})`.dim);
        return x
      }
      debugPrint('expandQuasiquote .>'.dim, toString(x), 'Not a pair..'.dim, 'Quoting'.cyan, `(type: ${typeof x})`.dim);
      return list(SymTable.QUOTE, x);
    }

    assert(!SymTable.UNQUOTESPLICING.equal(car(x)), "can't slice here");

    if (SymTable.UNQUOTE.equal(car(x))) {
      // console.log('expandQuasiquote .>'.dim, toString(x), 'pair with `unquote` at the head..'.dim, 'UNQUOTING'.red);
      assert(x.length === 2, 'Bad unquote length');
      if (innerLevel-1 === outerLevel) {
        innerLevel = clamp(0, Infinity, innerLevel-1)
        debugPrint('expandQuasiquote .>'.dim, toString(x), 'pair with `unquote` at the head..'.dim, 'NOT UNQUOTING'.magenta, 'WRONG LEVEL ..'.red.bgWhite);
        // const rv = list(SymTable.CONS, expandQuasiquoteInner(cdr(x)));
        // innerLevel = clamp(0, Infinity, innerLevel-1)
        const rv = list(SymTable.CONS, list(SymTable.QUOTE, SymTable.UNQUOTE), expandQuasiquote(cdr(x)));
        innerLevel += 1
        return rv
      } else {
        debugPrint('expandQuasiquote .>'.dim, toString(x), 'pair with `unquote` at the head..'.dim, 'UNQUOTING'.red);
        return cadr(x);
      }
    }
    if (isPair(car(x)) && SymTable.QUASIQUOTE.equal(caar(x))) {
      debugPrint('expandQuasiquote .>'.dim, toString(x), 'pair with `quasiquote` at the head..'.dim, 'UNQUOTING'.red);
      assert(x.length === 2, 'Bad unquote length');
      innerLevel++
      return list(SymTable.CONS, expandQuasiquoteInner(car(x)), expandQuasiquoteInner(cdr(x)));
    }
    if (isPair(car(x)) && SymTable.UNQUOTESPLICING.equal(caar(x))) {
      debugPrint('expandQuasiquote .>'.dim, toString(x), 'pair with `unquote-splicing` at the head..'.dim, 'UNQUOTE-SPLICING'.blue);
      const first = car(x);
      assert(isPair(first) && first.length === 2, 'Bad unquote-splicing length');
      return list(SymTable.APPEND, cadr(first), expandQuasiquoteInner(cdr(x)));
    }
    else {
      debugPrint('expandQuasiquote .>'.dim, toString(x), 'default case..'.dim, 'CONSING'.yellow);
      return list(SymTable.CONS, expandQuasiquoteInner(car(x)), expandQuasiquoteInner(cdr(x)));
    }
  };

  const rv = expandQuasiquoteInner(x);

  debugPrint('expandQuasiquote .>'.dim, 'Returning..'.green, toString(rv));
  return rv
}

function expandLambda(e: Pair, env: iEnv): Pair {
  const [_lambda, params, expression] = sequence(car, cadr, cddr, e)
  const allAtoms = isPair(params) && params.every(isSym);
  const improper = (isPair(params) && !params.isList()) && params.dottedEvery(isSym)

  assert(allAtoms || improper || isSym(params),
    `Invalid lambda args: ${toString(params)}, ${toString(e)}`);

  assert(isPair(expression) && expression.length >= 1,
    `lambda expression empty`);

  const body: Form = (expression.length > 1)
    ? cons(SymTable.BEGIN, expression)
    : car(expression)

  const scope = new Env(NIL, NIL, env);

  if (isSym(params)) {
    scope.setFrom(params, NIL)
  }
  else if (allAtoms) {
    params.forEach(param => {
      scope.setFrom(param, NIL)
    })
  }

  return list(_lambda, params, expand(body, scope));
}
