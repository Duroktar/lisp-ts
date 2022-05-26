import { UNDEF } from "./const";
import { quoteAtomToString } from "./macro";
import { TSchemeModule } from "./module";
import { NativeProc, Procedure } from "./proc";
import { SymTable } from "./sym";
import { SyntaxRulesDef } from "./syntax";
import { Term } from "./terms";
import { isSym, isString, isNone, isNum, isEmpty, symName } from "../utils";

export const toString = (expr: Term, inspect = false, lambdaSymbol = 'lambda'): string => {
  if (expr === undefined)
    return UNDEF.description!;
  if (isSym(expr)) {
    return expr?.description!;
  }
  if (expr instanceof NativeProc) {
    return `(nativefunc ${expr.name})`;
  }
  if (expr instanceof TSchemeModule) {
    return `(module "${expr.basename}")`;
  }
  if (expr instanceof Procedure || expr instanceof SyntaxRulesDef) {
    if (inspect && expr instanceof Procedure) {
      const parms = toString(expr.params, inspect, lambdaSymbol);
      const body = toString(expr.expr, inspect, lambdaSymbol);
      return `(${lambdaSymbol} ${expr.name} ${parms} ${body})`;
    }
    return `(${lambdaSymbol} ${expr.name})`;
  }
  if (isString(expr))
    return `"${expr}"`;
  if (isNone(expr))
    return expr;
  if (isNum(expr))
    return String(expr);
  if (isEmpty(expr))
    return '()';
  if (expr[0] === SymTable.LAMBDA) {
    const repr = (<any>expr.slice(1)).map((x: any) => toString(x, inspect, lambdaSymbol)).join(' ');
    return `(${lambdaSymbol} ${repr})`;
  }
  if (symName(<symbol>expr[0]) in quoteAtomToString) {
    const val = toString(expr[1], inspect, lambdaSymbol);
    return `${quoteAtomToString[symName(<symbol>expr[0])]}${val}`;
  }
  return `(${expr.map(c => toString(c, inspect, lambdaSymbol)).join(' ')})`;
};

export const toStringSafe = (expr: Term, inspect = false, lambdaSymbol = 'lambda'): string => {
  try {
    return toString(expr, inspect, lambdaSymbol);
  } catch (e) {
    return UNDEF.description!;
  }
};

export const print = (e: Term, inspect = false, lambdaSymbol = 'lambda' /* λ */): void => {
  console.log(toString(e, inspect, lambdaSymbol));
};
