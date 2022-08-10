import { SymTable, Symbol } from "./sym";

export const quotes: Record<string, Symbol> = {
  "`": SymTable.QUASIQUOTE,
  "'": SymTable.QUOTE,
  ",": SymTable.UNQUOTE,
  ",@": SymTable.UNQUOTESPLICING,
};

export const quoteMap: Record<string, string>
  = Object.fromEntries(Object.entries(quotes).map(([k, v]) => [v.name, k]))
