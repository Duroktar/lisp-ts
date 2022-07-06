import { isBinding, isChar, isExpansion, isNil, isNum, isString, isSym, isVec } from "../guard";
import { NIL, UNDEF } from "./const";
import { Syntax } from "./callable/macro";
import { TSchemeModule } from "./module/base";
import { Pair } from "./data/pair";
import { Port } from "./port";
import { Procedure } from "./callable/proc";
import { NativeFunc } from "./callable/func";
import { quoteMap } from "./data/quote";
import { Form } from "./form";

export const toDisplayString = (expr: Form): string => {
  return toString(expr, undefined, undefined, false)
}

export const toString = (expr: Form, inspect_ = false, lambdaSymbol = 'lambda', repr = true): string => {
  if (expr === undefined)
    return expr
  if (isSym(expr))
    return expr?.description!;
  if (isVec(expr))
    return `#(${expr.data.map(x => toString(x, inspect_, lambdaSymbol, repr)).join(' ')})`;
  if (isString(expr))
    return repr ? JSON.stringify(expr.toString()) : expr.toString()
  if (isChar(expr))
    return repr ? expr.sym.description! : expr.displayText;
  if (isNum(expr))
    return String(expr);
  if (isNil(expr))
    return '()';
  if (isExpansion(expr))
    return toString(expr.expression, inspect_, lambdaSymbol, repr)
    // return `<Expansion:${toString(expr.expression, inspect_, lambdaSymbol, repr)}>`
  if (isBinding(expr))
    return toString(expr.expression, inspect_, lambdaSymbol, repr)
    // return `<Binding:${toString(expr.expression, inspect_, lambdaSymbol, repr)}>`
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

  if (Pair.is(expr)) {
    if (!inspect_ && typeof expr.car === 'symbol' && quoteMap[expr.car]) {
      const str = toString(expr.cdr, inspect_, lambdaSymbol, repr);
      if (str.startsWith('(') && str.endsWith(')'))
        return `${quoteMap[expr.car]}${str.slice(1, -1)}`
      else
        return `${quoteMap[expr.car]}${str}`
    }
    const res: any[] = []
    let next: Form = expr
    while (Pair.is(next)) {
      if (Array.isArray(next.car)) {
        throw new Error("FUUUUK")
      }
      res.push(toString(next.car, inspect_, lambdaSymbol, repr))
      next = next.cdr
    }
    if (next !== NIL) {
      res.push('.')
      res.push(toString(next, inspect_, lambdaSymbol, repr))
    }
    return `(${res.join(' ')})`
  }

  const err = new Error('cannot convert to string');
  console.log(`d'oh!`, expr)
  console.log(`ctor-name:`, expr.constructor.name)
  console.log(err.stack)
  throw err
};

export const toStringSafe = (expr: Form, inspect = false, lambdaSymbol = 'lambda', repr = true): string => {
  try {
    return toString(expr, inspect, lambdaSymbol, repr);
  } catch (e) {
    return UNDEF.description!;
  }
};

export const print = (e: Form, inspect = false, lambdaSymbol = 'lambda' /* λ */, repr = true): void => {
  console.log(toString(e, inspect, lambdaSymbol, repr));
};
