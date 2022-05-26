import { Predicate } from "../types";
import { EMPTY, TRUE } from "./const";
import { Env } from "./env";
import { MalformedStringError, MissingParenthesisError, UnexpectedParenthesisError } from "./error";
import { InPort, isEofObject } from "./port";
import { SymTable } from "./sym";
import { List, Term } from "./terms";


  // system
  export const read = (port: InPort, readerEnv: Env): Term => {
    let cursor = port.readChar()

    const advance = () => {
      const c = cursor;
      cursor = port.readChar();
      return c;
    }

    const current = () => cursor;
    const isDblQt = () => cursor === '"';
    const isOpenS = () => cursor === '(';
    const isCloseS = () => cursor === ')';
    const isOpenM = () => cursor === '[';
    const isCloseM = () => cursor === ']';
    const isOpenP = () => cursor === '{';
    const isCloseP = () => cursor === '}';
    const isSpace = () => cursor === ' ';
    const isNewLine = () => cursor === '\n';
    const isHash = () => cursor === '#';
    const isSemi = () => cursor === ';';
    const isEscape = () => cursor === '\\';
    const isAlpha = (c: string) => (((c >= 'a') && (c <= 'z')) || ((c >= 'A') && (c <= 'Z')));
    const isDigit = (c: string) => ((c >= '0') && (c <= '9'));
    const isSpecial = (c: string) => ((c === '.') || (c === '_') || (c === '^') || (c === '=') || (c === '?') || (c === '!'));
    const isMathOp = (c: string) => ((c === '+') || (c === '-') || (c === '*') || (c === '/') || (c === '<') || (c === '>'));
    const isAlnum = (c: string) => isAlpha(c) || isDigit(c);
    const isValid = (c: string) => isAlnum(c) || isSpecial(c) || isMathOp(c) || isHash() || isEscape();

    const consumeIgnored = () => {
      while ((isSemi() || isSpace() || isNewLine()) && !isEofObject(current)) {
        if (isSemi() === false) advance();
        else advanceAndConsumeToEndOfLine()
      }
    };

    const advanceAndConsumeToEndOfLine = () => {
      advance();
      while (!isNewLine() && !isEofObject(cursor))
        advance();
    }

    const toLisp = (funcs: Record<string, any>) => Object.entries(funcs).reduce((acc: any, [key, val]: any) => { acc[key] = (...args: any[]) => val(...args) ? TRUE : EMPTY; return acc; }, {} as Record<string, any>);

    const parseDelimitedList = (open: Predicate, close: Predicate): Term | undefined => {
      if (open()) {
        advance();

        const exprs: Term[] = [];
        while (!close() && !isEofObject(cursor)) {
          exprs.push(parse());
        }

        if (close()) { advance(); }
        else
          throw new MissingParenthesisError();

        return exprs;
      }

      else if (current() === ')')
        throw new UnexpectedParenthesisError();
    }

    const listDelimiterPredicates = [
      [isOpenS, isCloseS],
      [isOpenM, isCloseM],
      [isOpenP, isCloseP]
    ];

    const readMacroLocals = { parse, advance, current, eatSpace: consumeIgnored, ...toLisp({ isEOF: isEofObject, isSpace, isNewLine }) };

    function parseAtom(): Term {
      let atom: string = '';
      do {
        if (isEscape())
          advance();
        atom += advance();
      } while (isValid(current()) && !isEofObject(cursor));
      const num = parseInt(atom);
      if (Number.isNaN(num) === false)
        return num;
      if (atom === 'undefined') {
        debugger
        return atom
      }
      return Symbol.for(atom);
    }

    function parseQuote(): Term {
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

    function parseReadMacro(): Term {
      if (readerEnv.has(current())) {
        const macro = readerEnv.get<Function>(advance());
        return macro(readMacroLocals);
      }
      return parseQuote();
    }

    function parseString(): Term {
      if (isDblQt()) {
        advance();

        let exprs: string = '';
        while (!isDblQt() && !isNewLine() && !isEofObject(cursor)) {
          exprs += advance();
        }

        if (isDblQt())
          advance();
        else
          throw new MalformedStringError();

        return exprs;
      }

      return parseReadMacro();
    }

    function parseList(): Term {
      for (let [open, close] of listDelimiterPredicates) {
        const result = parseDelimitedList(open, close)
        if (result) return result;
      }
      return parseString();
    }

    function parse(): Term {
      consumeIgnored();
      const expr = parseList();
      consumeIgnored();
      return expr;
    }

    function parseProgram(): Term {
      const list: List = []

      while (!isEofObject(cursor))
      { list.push(parse()) }

      if (list.length === 1)
        return list[0]

      return [SymTable.BEGIN, ...list];
    }

    return parseProgram();
  };
