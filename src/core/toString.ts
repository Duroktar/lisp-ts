import { UNDEF } from "./const";
import { quoteAtomToString } from "./macro";
import { TSchemeModule } from "./module";
import { NativeProc, Procedure } from "./proc";
import { SymTable } from "./sym";
import { SyntaxRulesDef } from "./syntax";
import { Form } from "./forms";
import { isSym, isString, isNone, isNum, isEmpty, symName, isChar, isVec } from "../utils";
import { Port } from "./port";

export const toString = (expr: Form, inspect = false, lambdaSymbol = 'lambda'): string => {
  if (expr === undefined)
    return expr
  if (isSym(expr))
    return expr?.description!;
  if (isVec(expr))
    return `#${toString(expr.data)}`;
  if (isString(expr))
    return `"${expr}"`;
  if (isChar(expr))
    return `#\\${expr}`;
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
    if (inspect && expr instanceof Procedure) {
      const parms = toString(expr.params, inspect, lambdaSymbol);
      const body = toString(expr.expr, inspect, lambdaSymbol);
      return `(${lambdaSymbol} ${expr.name} ${parms} ${body})`;
    }
    return `(${lambdaSymbol} ${expr.name})`;
  }
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
