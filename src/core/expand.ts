import { isBinding, isEmpty, isExpansion, isMacro, isPair, isSym } from "../guard";
import { iEnv } from "../interface/iEnv";
import { LogConfig } from "../logging";
import { assert, assertNever, sequence } from "../utils";
import { ForwardDeclaration } from "./binding";
import type { Closure } from "./callable/proc";
import { isSyntax } from "./callable/syntax";
import { NIL, UNDEF } from "./const";
import { cons, list, type Pair } from "./data/pair";
import { SymTable } from "./data/sym";
import { Env } from "./env";
import type { Form, List } from "./form";
import { caddr, cadr, car, cddr, cdr } from "./lisp";
import { toString, toStringSafe } from "./print";

const DEBUG = LogConfig.expand;

function debugLog(...args: string[]): void {
  if (DEBUG) { console.log('[Expand]:'.yellow, ...args) }
}

export type ExpandOptions = {
  hide: string[]
} | {
  only: string[]
} | {
  predicate: (obj: string) => boolean
}

export function expand(e: Form, env: iEnv, topLevel = false, options?: ExpandOptions): Form {
  if (!isPair(e)) { return e }
  if (isEmpty(e)) { return e }
  else if (SymTable.QUOTE.equal(e.car)) {
    assert(e.cdr, 'Invalid <datum> in `quote`');
    return e;
  }
  else if (SymTable.IF.equal(e.car)) {
    assert(e.length > 2, `Invalid if form: ${toStringSafe(e)}`);
    return list(e.car).append(expand(cdr(e), env, topLevel, options));
  }
  else if (SymTable.BEGIN.equal(e.car)) {
    if (e.length === 1) return NIL
    assert(isPair(e.cdr), `Invalid BEGIN form: ${toStringSafe(e)}`);
  }
  else if (SymTable.SET.equal(e.car)) {
    const [_set, first, second] = sequence(car, cadr, cddr, e);
    assert(isSym(first), 'First arg to set! must be a symbol');
    getForwardDeclarationScope(first, env, topLevel);
    return list(_set, first).append(expand(second, env, topLevel, options))
  }
  else if (SymTable.DEFINE.equal(e.car)) {
    const [_def, v, body] = sequence(car, cadr, cddr, e);
    if (isPair(v)) {
      const l = list(SymTable.LAMBDA, cdr(v)).append(body);
      const scope = getForwardDeclarationScope(car(v), env, topLevel);
      return expand(list(_def, car(v), l), scope, topLevel, options);
    } else {
      assert(e.length === 3, '(define non-var/list exp) => Error');
      assert(isSym(v) || isBinding(v), `can define only a symbol, got: ${toStringSafe(v)}`);
      const scope = getForwardDeclarationScope(v, env, topLevel);
      return list(_def, v, expand(caddr(e), scope, topLevel, options));
    }
  }
  else if (SymTable.LAMBDA.equal(e.car)) {
    assert(e.length >= 3, 'invalid lambda args (length is not >= 3)', e.car);
    return expandLambda(e, env, topLevel, options);
  }
  else if (env.hasFrom(e.car) && shouldExpand(e.car, options)) {
    const [name, formals] = sequence(car, cdr, e) as [Pair, List];
    const proc = env.getFrom<Closure>(name);
    if (isSyntax(proc) || isMacro(proc)) {
      debugLog('calling macro: '.dim, proc.name);
      const result = proc.call(formals, env);
      debugLog('result of calling macro:\n\t'.green, toString(result));
      const form = isExpansion(result) ? result.expression : result;
      return expand(form, env, topLevel, options);
    }
  }

  const head = expand(e.car, env, topLevel, options);
  return cons(head, expand(cdr(e), env, topLevel, options));
};

function expandLambda(e: Pair, env: iEnv, topLevel?: boolean, options?: ExpandOptions): Pair {
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
    scope.setFrom(params, UNDEF)
  }
  else if (allAtoms) {
    params.forEach(param => {
      scope.setFrom(param, UNDEF)
    })
  }

  return list(_lambda, params, expand(body, scope, topLevel, options));
}

function shouldExpand(arg: Form, options: ExpandOptions | undefined): boolean {
  if (options === undefined)
    return true

  assert(isSym(arg) || isBinding(arg), `shouldExpand passed a non-(symbol|binding) ${toString(arg)}`)

  if ('hide' in options) {
    return ! options.hide.includes(arg.name)
  }
  if ('only' in options) {
    return options.only.includes(arg.name)
  }
  if ('predicate' in options) {
    return options.predicate(arg.name)
  }

  assertNever(options, 'shouldExpand')
}

function getForwardDeclarationScope(form: Form, env: iEnv, topLevel: boolean) {
  const scope = topLevel ? env : new Env(list(), list(), env);
  debugLog('forward declaring:'.dim.blue, toString(form));
  scope.setFrom(form, new ForwardDeclaration(form));
  return scope;
}
