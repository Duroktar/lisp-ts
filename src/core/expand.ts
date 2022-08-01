import { isEmpty, isExpansion, isMacro, isPair, isSym } from "../guard";
import type { iWorld } from "../interface/iWorld";
import { assert, clamp, sequence } from "../utils";
import { isSyntax } from "./callable/macro";
import type { Closure } from "./callable/proc";
import { NIL, UNDEF } from "./const";
import { Env } from "./data/env";
import { cons, list, type Pair } from "./data/pair";
import { SymTable } from "./data/sym";
import type { Form, List } from "./form";
import { caar, caddr, cadr, car, cddr, cdr } from "./lisp";
import { toString, toStringSafe } from "./print";

const DEBUG = false;

function debugLog(...args: string[]): void {
  if (DEBUG) { console.log('[Expand]:'.yellow, ...args) }
}

export function expand(e: Form, world: iWorld, topLevel = false): Form {
  if (!isPair(e)) { return e }
  if (isEmpty(e)) { return e }
  else if (SymTable.QUOTE === car(e)) {
    assert(e.cdr, 'Invalid <datum> in `quote`');
    return e;
  }
  else if (SymTable.IF === car(e)) {
    if (e.length === 3) e.push(UNDEF)
    assert(e.length === 4, `Invalid if form: ${toStringSafe(e)}`);
    return list(car(e)).append(expand(cdr(e), world));
  }
  else if (SymTable.SET === car(e)) {
    assert(isSym(cadr(e)), 'First arg to set! must be a symbol');
    return e
  }
  else if (SymTable.DEFINE === car(e)) {
    const [_def, v, body] = sequence(car, cadr, cddr, e);
    if (isPair(v)) {
      const l = list(SymTable.LAMBDA, cdr(v)).append(body);
      return expand(list(_def, car(v), l), world);
    } else {
      assert(e.length === 3, '(define non-var/list exp) => Error');
      assert(isSym(v), 'can define only a symbol');
      return list(_def, v, expand(caddr(e), world));
    }
  }
  else if (SymTable.LAMBDA === car(e)) {
    assert(e.length >= 3, 'expand lambda');
    return expandLambda(e, world);
  }
  else if (SymTable.QUASIQUOTE === car(e)) {
    assert(e.length === 2, 'expand quasi-quote');
    return expandQuasiquote(cadr(e));
  }
  else if (world.lexicalEnv.hasFrom(car(e))) {
    const proc = world.lexicalEnv.getFrom<Closure>(car(e));
    if (isSyntax(proc) || isMacro(proc)) {
      debugLog('calling macro: ', proc.name);
      const result = proc.call(cdr(e) as List);
      debugLog('result of calling macro: ', toString(result));
      if (isExpansion(result)) {
        const rv = expand(result.expression, world);
        debugLog('macro expansion: ', toString(rv));
        return rv;
      }
      return expand(result, world);
    }
  }

  const head = expand(car(e), world);
  return cons(head, expand(cdr(e), world));
};

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

    assert(car(x) !== SymTable.UNQUOTESPLICING, "can't slice here");

    if (car(x) === SymTable.UNQUOTE) {
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
    if (isPair(car(x)) && caar(x) === SymTable.QUASIQUOTE) {
      debugPrint('expandQuasiquote .>'.dim, toString(x), 'pair with `quasiquote` at the head..'.dim, 'UNQUOTING'.red);
      assert(x.length === 2, 'Bad unquote length');
      innerLevel++
      return list(SymTable.CONS, expandQuasiquoteInner(car(x)), expandQuasiquoteInner(cdr(x)));
    }
    if (isPair(car(x)) && caar(x) === SymTable.UNQUOTESPLICING) {
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

function expandLambda(e: Pair, world: iWorld): Pair {
  const [_lambda, params, expression] = sequence(car, cadr, cddr, e)
  const allAtoms = isPair(params) && params.every(isSym);
  const improper = (isPair(params) && !params.isList()) && params.dottedEvery(isSym)

  assert(allAtoms || improper || isSym(params),
    `Invalid lambda args: ${toString(params)}, ${toString(e)}`);

  assert(isPair(expression) && expression.length >= 1,
    `lambda expression empty`);

  const body = (expression.length > 1)
    ? cons(SymTable.BEGIN, expression)
    : car(expression)

  const scope = new Env(NIL, NIL, world.env);

  if (isSym(params)) {
    scope.setFrom(params, NIL)
  }
  else if (allAtoms) {
    params.forEach(param => {
      scope.setFrom(param, NIL)
    })
  }

  const lambdaScope = {
    ...world,
    env: scope
  }

  return list(_lambda, params, expand(body, lambdaScope));
}
