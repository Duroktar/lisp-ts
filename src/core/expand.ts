import { isCallable, isEmpty, isExpansion, isPair, isSym } from "../guard";
import { iWorld } from "../interface/iWorld";
import { assert, sequence } from "../utils";
import { UNDEF } from "./const";
import { cons, list, type Pair } from "./data/pair";
import type { Closure } from "./data/proc";
import { SymTable } from "./data/sym";
import type { Form } from "./form";
import { caar, caddr, cadr, car, cddr, cdr } from "./lisp";
import { toString, toStringSafe } from "./print";

const DEBUG = false

export const expand = async (e: Form, topLevel = false, world: iWorld): Promise<Form> => {
  // console.log('EXPANDING')
  if (!isPair(e)) { return e }
  if (isEmpty(e)) { return e }
  else if (SymTable.QUOTE === car(e)) {
    assert(e.cdr, 'Invalid <datum> in `quote`');
    return e;
  }
  else if (SymTable.IF === car(e)) {
    if (e.length === 3) e.push(UNDEF)
    assert(e.length === 4, `Invalid if form: ${toStringSafe(e)}`);
    return list(car(e)).append(await expand(cdr(e), false, world));
  }
  else if (SymTable.SET === car(e)) {
    assert(isSym(cadr(e)), 'First arg to set! must be a symbol');
    return e
  }
  else if (SymTable.BEGIN === car(e)) {
    const { car: _begin, cdr: exprs } = e;
    assert(isPair(exprs) && exprs.length >= 1, 'expand begin');
    return cons(_begin, await expand(exprs, topLevel, world));
  }
  else if (SymTable.DEFINE === car(e)) {
    const [_def, v, body] = sequence(car, cadr, cddr, e);
    if (isPair(v)) {
      const l = list(SymTable.LAMBDA, cdr(v)).append(body);
      return await expand(list(_def, car(v), l), false, world);
    } else {
      assert(e.length === 3, '(define non-var/list exp) => Error');
      assert(isSym(v), 'can define only a symbol');
      return list(_def, v, await expand(caddr(e), false, world));
    }
  }
  else if (SymTable.LAMBDA === car(e)) {
    assert(e.length >= 3, 'expand lambda');
    return await expandLambda(e, world);
  }
  else if (SymTable.QUASIQUOTE === car(e)) {
    assert(e.length === 2, 'expand quasi-quote');
    return await expandQuasiquote(cadr(e));
  }
  else if (world.lexicalEnv.hasFrom(car(e))) {
    // console.log('[Expand]: found macro: ', toString(car(e)));
    const proc = world.lexicalEnv.getFrom<Closure>(car(e));
    if (isCallable(proc)) {
      // console.log('[Expand]: calling macro: ', proc.name);
      const result = await proc.call(cdr(e), world.env);
      // console.log('[Expand]: result of calling macro: ', toString(result));
      if (isExpansion(result)) {
        const rv = await expand(result.expression, false, world);
        // console.log('[Expand]: macro expansion: ', toString(rv));
        return rv;
      }
      return await expand(result, false, world);
    }
    // allow functions as well
    return await expand(await proc(cdr(e)), topLevel, world);
  }

  const head = await expand(car(e), false, world);
  return cons(head, await expand(cdr(e), false, world));
};

async function expandQuasiquote(x: Form, level = 0): Promise<Form> {
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
        return cadr(x);
      }
    }
    if (isPair(car(x)) && caar(x) === SymTable.QUASIQUOTE) {
      debugPrint('expandQuasiquote .>'.dim, toString(x), 'pair with `quasiquote` at the head..'.dim, 'UNQUOTING'.red);
      assert(x.length === 2, 'Bad unquote length');
      innerLevel++
      return list(SymTable.CONS, await expandQuasiquoteInner(car(x)), await expandQuasiquoteInner(cdr(x)));
    }
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

  const rv = await expandQuasiquoteInner(x);

  debugPrint('expandQuasiquote .>'.dim, 'Returning..'.green, toString(rv));
  return rv
}

async function expandLambda(e: Pair, world: iWorld): Promise<Pair> {
  const [_lambda, params, expression] = sequence(car, cadr, cddr, e)
  const allAtoms = isPair(params) && params.every(isSym);
  const improper = (isPair(params) && !params.isList()) && params.dottedEvery(isSym)
  assert(allAtoms || improper || isSym(params), `Invalid lambda args. Expected a list of atoms, an improper list of atoms, or a single atom but instead got: ${toString(params)}, ${toString(e)}`);
  assert(isPair(expression) && expression.length >= 1, `lambda expression empty`);
  const body = (expression.length > 1) && cons(SymTable.BEGIN, expression)
  return list(_lambda, params, await expand(body || car(expression), false, world));
}

function clamp(value: number, min: number, max: number): number {
  return Math.max(min, Math.min(max, value))
}
