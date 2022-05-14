import { Position, Predicate } from "../types";
import { EMPTY, TRUE } from "./const";
import { FormatErrorOptions, MalformedStringError, MissingParenthesisError, UnexpectedParenthesisError } from "./error";
import { readMacroTable } from "./macro";
import { SymTable } from "./sym";
import { Expr, List } from "./terms";

  // system
  export const read = (text: string): Expr => {
    let cursor = 0, end = text.length - 1, line = 1, col = 1;

    const advance = () => {
      const char = text[cursor++];
      if (char === '\n') {
        line++;
        col = 0;
      };
      col++;
      return char;
    };

    const current = () => text[cursor];
    const isDblQt = () => text[cursor] === '"';
    const isOpenS = () => text[cursor] === '(';
    const isCloseS = () => text[cursor] === ')';
    const isOpenM = () => text[cursor] === '[';
    const isCloseM = () => text[cursor] === ']';
    const isOpenP = () => text[cursor] === '{';
    const isCloseP = () => text[cursor] === '}';
    const isSpace = () => text[cursor] === ' ';
    const isNewLine = () => text[cursor] === '\n';
    const isHash = () => text[cursor] === '#';
    const isSemi = () => text[cursor] === ';';
    const isEscape = () => text[cursor] === '\\';
    const isAlpha = (c: string) => (((c >= 'a') && (c <= 'z')) || ((c >= 'A') && (c <= 'Z')));
    const isDigit = (c: string) => ((c >= '0') && (c <= '9'));
    const isSpecial = (c: string) => ((c === '.') || (c === '_') || (c === '^') || (c === '=') || (c === '?') || (c === '!'));
    const isMathOp = (c: string) => ((c === '+') || (c === '-') || (c === '*') || (c === '/') || (c === '<') || (c === '>'));
    const isAlnum = (c: string) => isAlpha(c) || isDigit(c);
    const isValid = (c: string) => isAlnum(c) || isSpecial(c) || isMathOp(c) || isHash() || isEscape();
    const isEOF = () => cursor > end;

    const consumeIgnored = () => {
      while ((isSemi() || isSpace() || isNewLine()) && !isEOF()) {
        if (isSemi() === false) advance();
        else advanceAndConsumeToEndOfLine()
      }
    };

    const advanceAndConsumeToEndOfLine = () => {
      advance();
      while (!isNewLine() && !isEOF())
        advance();
    }

    const toLisp = (funcs: Record<string, any>) => Object.entries(funcs).reduce((acc: any, [key, val]: any) => { acc[key] = (...args: any[]) => val(...args) ? TRUE : EMPTY; return acc; }, {} as Record<string, any>);

    const parseDelimitedList = (open: Predicate, close: Predicate): Expr | undefined => {
      if (open()) {
        const open = { col, line, cursor };
        advance();

        const exprs: Expr[] = [];
        while (!close() && !isEOF()) {
          exprs.push(parse());
        }

        if (close()) { advance(); }
        else
          throw new MissingParenthesisError(text, error(open));

        return exprs;
      }

      else if (current() === ')')
        throw new UnexpectedParenthesisError(text, error({ col, line, cursor }));
    }
    const listDelimiterPredicates = [
      [isOpenS, isCloseS],
      [isOpenM, isCloseM],
      [isOpenP, isCloseP]
    ];

    const readMacroLocals = { parse, advance, current, eatSpace: consumeIgnored, ...toLisp({ isEOF, isSpace, isNewLine }) };

    const error = (start: Position, message?: string): FormatErrorOptions => {
      return { end: { line, col, cursor }, message, start };
    };

    function parseAtom(): Expr {
      let atom: string = '';
      do {
        if (isEscape())
          advance();
        atom += advance();
      } while (isValid(current()) && !isEOF());
      const num = parseInt(atom);
      if (Number.isNaN(num) === false)
        return num;
      if (atom === 'undefined') {
        debugger
        return atom
      }
      return Symbol.for(atom);
    }

    function parseQuote(): Expr {
      if (current() === "'") {
        advance();
        return [SymTable.QUOTE, parse()];
      }
      if (current() === "`") {
        advance();
        return [SymTable.QUASIQUOTE, parse()];
      }
      if (current() === ",") {
        advance();
        if (current() === "@") {
          advance();
          return [SymTable.UNQUOTESPLICING, parse()];
        }
        return [SymTable.UNQUOTE, parse()];
      }
      return parseAtom();
    }

    function parseReadMacro(): Expr {
      if (current() in readMacroTable) {
        const macro = readMacroTable[current()];
        advance();
        return macro(readMacroLocals);
      }
      return parseQuote();
    }

    function parseString(): Expr {
      if (isDblQt()) {
        const start = { col, line, cursor };
        advance();

        let exprs: string = '';
        while (!isDblQt() && !isNewLine() && !isEOF()) {
          exprs += advance();
        }

        if (isDblQt())
          advance();
        else
          throw new MalformedStringError(text, error(start));

        return JSON.stringify(exprs);
      }

      return parseReadMacro();
    }

    function parseList(): Expr {
      for (let [open, close] of listDelimiterPredicates) {
        const result = parseDelimitedList(open, close)
        if (result) return result;
      }
      return parseString();
    }

    function parse(): Expr {
      consumeIgnored();
      const expr = parseList();
      consumeIgnored();
      return expr;
    }

    function parseProgram(): Expr {
      const res: List = []

      while (isEOF() === false)
      { res.push(parse()) }

      if (res.length === 1)
        return res[0]

      return [SymTable.BEGIN, ...res];
    }

    return parseProgram();
  };
