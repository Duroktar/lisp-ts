import * as Utils from "../utils";
import { Proc } from "./proc";
import { Sym, SymTable } from "./sym";
import { Term } from "./terms";

export function _async(...args: Term[]) {
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

export const readMacroTable: Record<string, (...args: any[]) => Term> = {};

export const macroTable: Record<string, Proc | Function> = {
  async: _async,
};

export const quotes = {
  [SymTable.QUASIQUOTE.description!]: "`",
  [SymTable.QUOTE.description!]: "'",
  [SymTable.UNQUOTE.description!]: ",",
  [SymTable.UNQUOTESPLICING.description!]: ",@",
};
