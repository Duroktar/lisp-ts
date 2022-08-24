import { isBinding, isChar, isExpansion, isNil, isNum, isPair, isString, isSym, isVec } from "../guard";
import { NIL, UNDEF } from "./const";
import { Syntax } from "./callable/syntax";
import { TSchemeModule } from "./module/base";
import { Pair } from "./data/pair";
import { Port } from "./port";
import { Procedure } from "./callable/proc";
import { NativeFunc } from "./callable/func";
import { quoteMap } from "./data/quote";
import { Form, List } from "./form";
import { assert, assertNever, isEq } from "../utils";

export const toDisplayString = (expr: Form): string => {
  return toString(expr, undefined, undefined, false)
}

export const toString = (expr: Form, inspect_ = false, lambdaSymbol = 'lambda', repr = true): string => {
  if (expr === undefined)
    return expr
  if (isSym(expr))
    return expr.name;
  if (isVec(expr))
    return `#(${expr.data.map(x => toString(x, inspect_, lambdaSymbol, repr)).join(' ')})`;
  if (isString(expr))
    return repr ? JSON.stringify(expr.toString()) : expr.toString()
  if (isChar(expr))
    return repr ? `#\\${expr.sym.description!}` : expr.displayText;
  if (isNum(expr))
    return expr.toString();
  if (isNil(expr))
    return '()';
  if (isExpansion(expr))
    return inspect_
      ? `#<expansion ${toString(expr.expression, inspect_, lambdaSymbol, repr)}>`
      : toString(expr.expression, inspect_, lambdaSymbol, repr)
  if (isBinding(expr))
    return inspect_
      ? `#<binding ${toString(expr.expression, inspect_, lambdaSymbol, repr)}>`
      : toString(expr.expression, inspect_, lambdaSymbol, repr)
  if (expr instanceof TSchemeModule) {
    return `(module "${expr.displayName}")`;
  }
  if (expr instanceof NativeFunc) {
    return `(nativefunc ${expr.name})`;
  }
  if (expr instanceof Port) {
    return `#<${expr.name}>`;
  }
  if (expr instanceof Procedure || expr instanceof Syntax) {
    if (inspect_ && expr instanceof Procedure) {
      const parms = toString(expr.params, inspect_, lambdaSymbol, repr);
      const body = toString(expr.expr, inspect_, lambdaSymbol, repr);
      return `(${lambdaSymbol} ${expr.name} ${parms} ${body})`;
    }
    return `(${lambdaSymbol} ${expr.name})`;
  }

  if (isPair(expr)) {
    if (inspect_ && isSym(expr.car) && quoteMap[expr.car.name]) {
      const str = toString(expr.cdr, inspect_, lambdaSymbol, repr);
      if (str.startsWith('(') && str.endsWith(')'))
        return `${quoteMap[expr.car.name]}${str.slice(1, -1)}`
      else
        return `${quoteMap[expr.car.name]}${str}`
    }
    const res: any[] = []
    let next: Form = expr
    while (Pair.is(next)) {
      res.push(toString(next.car, inspect_, lambdaSymbol, repr))
      next = next.cdr
      if (hasCycle(next)) {
        res.push('<CircularRef>')
        next = NIL
        break
      }
    }
    if (next !== NIL) {
      res.push('.')
      res.push(toString(next, inspect_, lambdaSymbol, repr))
    }
    return `(${res.join(' ')})`
  }

  assertNever(expr)
};

export const toStringSafe = (expr: Form, inspect = false, lambdaSymbol = 'lambda', repr = true): string => {
  try {
    return toString(expr, inspect, lambdaSymbol, repr);
  } catch (e) {
    if (typeof expr === 'string') {
      return expr
    }
    return UNDEF.name;
  }
};

export const print = (e: Form, inspect = false, lambdaSymbol = 'lambda' /* λ */, repr = true): void => {
  console.log(toString(e, inspect, lambdaSymbol, repr));
};

function _hasCycleH(slowLs: List, fastLs: List): boolean {
  if (isNil(fastLs)) return false
  if (isNil(fastLs.cdr)) return false
  if (isEq(fastLs, slowLs)) return true
  if (!(isPair(slowLs) && isPair(fastLs)))
    return false
  if (!(isPair(slowLs.cdr) && isPair(fastLs.cdr) && isPair(fastLs.cdr.cdr)))
    return false
  return _hasCycleH(slowLs.cdr, fastLs.cdr.cdr)
}

function hasCycle(ls: Form) {
  if (isNil(ls)) return false
  if (!isPair(ls)) return false
  if (!isPair(ls.cdr)) return false

  return _hasCycleH(ls, ls.cdr)
}
