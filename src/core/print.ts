import { isBinding, isChar, isExpansion, isNil, isNullOrUndefined, isNum, isString, isSym, isVec } from "../guard";
import { NIL, UNDEF } from "./const";
import { Syntax } from "./callable/macro";
import { TSchemeModule } from "./module/base";
import { Pair } from "./data/pair";
import { Port } from "./port";
import { Procedure } from "./callable/proc";
import { NativeFunc } from "./callable/func";
import { quoteMap } from "./data/quote";
import { Form } from "./form";

export const toString = (expr: Form, inspect_ = false, lambdaSymbol = 'lambda'): string => {
  if (expr === undefined)
    return expr
  if (isSym(expr))
    return expr?.description!;
  if (isVec(expr))
    return `#(${expr.data.map(x => toString(x, inspect_, lambdaSymbol)).join(' ')})`;
  if (isString(expr))
    return `"${expr}"`;
  if (isChar(expr))
    return expr.displayText;
  if (isNum(expr))
    return String(expr);
  if (isNil(expr))
    return '()';
  if (isExpansion(expr))
    return `<Expansion:${toString(expr.expression)}>`
  if (isBinding(expr))
    return `<Binding:${toString(expr.expression)}>`
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
      const parms = toString(expr.params, inspect_, lambdaSymbol);
      const body = toString(expr.expr, inspect_, lambdaSymbol);
      return `(${lambdaSymbol} ${expr.name} ${parms} ${body})`;
    }
    return `(${lambdaSymbol} ${expr.name})`;
  }

  if (Pair.is(expr)) {
    if (!inspect_ && typeof expr.car === 'symbol' && quoteMap[expr.car]) {
      const str = toString(expr.cdr, inspect_);
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
      res.push(toString(next.car, inspect_, lambdaSymbol))
      next = next.cdr
    }
    if (next !== NIL) {
      res.push('.')
      res.push(toString(next, inspect_, lambdaSymbol))
    }
    return `(${res.join(' ')})`
  }

  // iveNever(expr)

  if (Array.isArray(expr)) {
    const rv = expr.map(e => toString(e, inspect_, lambdaSymbol)).join(' ');
    // console.log('SHOULD NOT BE AN ARRAY HERE: %s', rv)
    return rv
  }

  // if (isNullOrUndefined(expr))
  //   return expr;

  const err = new Error('cannot convert to string');
  console.log(`d'oh!`, expr)
  console.log(err.stack)
  throw err
};

export const toStringSafe = (expr: Form, inspect = false, lambdaSymbol = 'lambda'): string => {
  try {
    return toString(expr, inspect, lambdaSymbol);
  } catch (e) {
    return UNDEF.description!;
  }
};

export const print = (e: Form, inspect = false, lambdaSymbol = 'lambda' /* λ */): void => {
  console.log(toString(e, inspect, lambdaSymbol));
};
