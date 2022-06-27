import { SymTable } from "./sym";

export const quotes: Record<string, symbol> = {
  "`": SymTable.QUASIQUOTE,
  "'": SymTable.QUOTE,
  ",": SymTable.UNQUOTE,
  ",@": SymTable.UNQUOTESPLICING,
};

export const quoteMap: Record<symbol, string>
  = Object.fromEntries(Object.entries(quotes).map(([k, v]) => [v, k]))
