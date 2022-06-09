import { EMPTY, UNDEF } from "./const";
import { TSchemeModule } from "./module";
import { NativeProc, Procedure } from "./proc";
import { SyntaxRulesDef } from "./syntax";
import { Form } from "./forms";
import { isSym, isString, isNone, isNum, isEmpty, symName, isChar, isVec } from "../utils";
import { Port } from "./port";
import { Pair } from "./pair";
import { inspect } from "util";

export const toString = (expr: Form, inspect_ = false, lambdaSymbol = 'lambda'): string => {
  if (expr === undefined)
    return expr
  if (isSym(expr))
    return expr?.description!;
  if (isVec(expr))
    return `#(${expr.data.map(x => toString(x)).join(' ')})`;
  if (isString(expr))
    return `"${expr}"`;
  if (isChar(expr))
    return expr.displayText;
  if (isNone(expr))
    return expr;
  if (isNum(expr))
    return String(expr);
  if (isEmpty(expr))
    return '()';
  if (expr instanceof TSchemeModule) {
    return `(module "${expr.basename}")`;
  }
  if (expr instanceof NativeProc) {
    return `(nativefunc ${expr.name})`;
  }
  if (expr instanceof Port) {
    return `#<${expr.name}>`;
  }
  if (expr instanceof Procedure || expr instanceof SyntaxRulesDef) {
    if (inspect_ && expr instanceof Procedure) {
      const parms = toString(expr.params, inspect_, lambdaSymbol);
      const body = toString(expr.expr, inspect_, lambdaSymbol);
      return `(${lambdaSymbol} ${expr.name} ${parms} ${body})`;
    }
    return `(${lambdaSymbol} ${expr.name})`;
  }
  if (Pair.is(expr)) {
    const res: any[] = []
    let next: Form = expr
    while (Pair.is(next)) {
      if (Array.isArray(next.car)) {
        debugger
      }
      res.push(toString(next.car, inspect_, lambdaSymbol))
      next = next.cdr
    }
    if (next !== EMPTY) {
      res.push('.')
      res.push(toString(next, inspect_, lambdaSymbol))
    }
    return `(${res.join(' ')})`
  }

  if (Array.isArray(expr)) {
    const rv = expr.map(e => toString(e, inspect_, lambdaSymbol)).join(' ');
    console.log('SHOULD NOT BE AN ARRAY HERE: %s', rv)
    return rv
  }

  const err = new Error('fallthrough condition');
  console.log(`d'oh!`, expr)
  console.log(inspect(expr))
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
