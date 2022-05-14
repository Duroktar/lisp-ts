import * as Utils from "../utils";
import { expand } from "./expand";
import { Proc } from "./proc";
import { Sym, SymTable } from "./sym";
import { Expr } from "./terms";

export function _async(...args: Expr[]) {
  const x = [SymTable.DO, args];
  Utils.expect(x, args.length > 0, '`do` blocks must container an expression');
  const result = args.reduce((acc: any[], expr) => {
    let cont: any[] = [Sym('call/cc'), Utils.mkLambda(['throw'], [Utils.mkLambda(['arg'], expr)])];
    if (acc.length === 0)
      return cont;

    cont[1][2].push(acc);
    return cont;
  }, []);
  // console.log(Utils.toString(result))
  return result;
}

export function _let(...args: Expr[]): any {
  Utils.expect(args, args.length !== 0, 'Must provide arguments to "let" macro');
  if (Utils.isAtom(args[0])) {
    const [name, bindings, body] = args
    const [parms, vals]: any = Utils.zip(...bindings as any) || [[], []];
    const outer = [SymTable.LAMBDA, [],
      [SymTable.DEFINE, name, [SymTable.LAMBDA, parms, expand(body)]],
      [name, ...vals],
    ];
    return [outer]
  }
  const [bindings, ...body] = args;
  Utils.expect(args, Utils.isList(bindings) && bindings.every(b => Utils.isList(b) && b.length === 2 && Utils.isSym(b[0])));
  const [vars, vals] = Utils.zip(...bindings as any);
  Utils.expect(args, (vars.length === new Set(vars).size), 'let bindings must be unique');
  return [[SymTable.LAMBDA, vars, ...expand(body) as any]].concat(<any>vals);
}

export const readMacroTable: Record<string, (...args: any[]) => Expr> = {};

export const macroTable: Record<string, Proc | Function> = {
  async: _async,
  let: _let,
  // and: _and,
  // or: _or,
};

export const quotes = {
  [SymTable.QUASIQUOTE.description!]: "`",
  [SymTable.QUOTE.description!]: "'",
  [SymTable.UNQUOTE.description!]: ",",
  [SymTable.UNQUOTESPLICING.description!]: ",@",
};
