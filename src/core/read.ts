import assert from "assert";
import { Predicate } from "../types";
import { isList } from "../utils";
import { Character } from "./char";
import { EMPTY, TRUE } from "./const";
import { Env } from "./env";
import { MalformedStringError, MissingParenthesisError, UnexpectedParenthesisError } from "./error";
import { InPort, isEofString } from "./port";
import { Sym, SymTable } from "./sym";
import { List, Term } from "./terms";
import { toString } from "./toString";
import { Vector } from "./vec";

// system
export const read = async (port: InPort, readerEnv: Env): Promise<Term> => {
  let cursor = await port.readChar()

  const advance = async () => {
    const c = cursor;
    cursor = await port.readChar();
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
  const isValidAndSameLine = () => isValid(current()) && !isEofString(cursor)

  const consumeIgnored = async () => {
    while ((isSemi() || isSpace() || isNewLine()) && !isEofString(current)) {
      if (isSemi() === false) await advance();
      else await advanceAndConsumeToEndOfLine()
    }
  };

  const advanceAndConsumeToEndOfLine = async () => {
    await advance();
    while (!isNewLine() && !isEofString(cursor))
      await advance();
  }

  const toLisp = (funcs: Record<string, any>) => Object.entries(funcs).reduce((acc: any, [key, val]: any) => { acc[key] = (...args: any[]) => val(...args) ? TRUE : EMPTY; return acc; }, {} as Record<string, any>);

  const parseDelimitedList = async (open: Predicate, close: Predicate): Promise<Term | undefined> => {
    if (open()) {
      await advance();

      const exprs: Term[] = [];
      while (!close() && !isEofString(cursor)) {
        exprs.push(await parse());
      }

      if (close()) { await advance(); }
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

  const readMacroLocals = { parse, advance, current, eatSpace: consumeIgnored, ...toLisp({ isEOF: isEofString, isSpace, isNewLine }) };

  async function parseAtom(): Promise<Term> {
    let atom: string = '';
    do {
      if (isEscape()) { await advance(); }
      atom += await advance();
    } while (isValidAndSameLine());
    const num = parseInt(atom);
    if (Number.isNaN(num) === false)
      return num;
    if (atom === 'undefined') {
      return atom
    }
    return Symbol.for(atom);
  }

  async function parseQuote(): Promise<Term> {
    if (current() === "'") {
      await advance();
      return [SymTable.QUOTE, await parse()];
    }
    if (current() === "`") {
      await advance();
      return [SymTable.QUASIQUOTE, await parse()];
    }
    if (current() === ",") {
      await advance();
      if (current() === "@") {
        await advance();
        return [SymTable.UNQUOTESPLICING, await parse()];
      }
      return [SymTable.UNQUOTE, await parse()];
    }
    return await parseAtom();
  }

  async function parseHashPrefix(): Promise<Term> {
    if (current() === "#") {
      await advance();

      if (current() === "(") {
        const items = await parseList()
        assert(isList(items), "what is going on?")
        return new Vector(<List>items)
      }

      if (current() === "\\") {
        await advance();

        const val = await parseAtom();

        if (typeof val === 'symbol') {
          const char = val.description
          assert(
            char &&
            (char.length === 1 || char.match(/^(newline|space)$/i)),

            `error parsing character: #\\${char}`
          )
          return new Character(val) as any
        }

        throw new Error('What am i doing here?')
      }

      const val = await parseAtom();
      assert(val && typeof val === 'symbol')

      return Sym(`#${toString(val)}`)
    }

    return await parseQuote()
  }

  async function parseReadMacro(): Promise<Term> {
    if (readerEnv.has(current())) {
      const value = await advance();
      const macro = readerEnv.get<Function>(value);
      return await macro(readMacroLocals);
    }
    return await parseHashPrefix();
  }

  async function parseString(): Promise<Term> {
    if (isDblQt()) {
      await advance();

      let exprs: string = '';
      while (!isDblQt() && !isNewLine() && !isEofString(cursor)) {
        exprs += await advance();
      }

      if (isDblQt())
        await advance();
      else
        throw new MalformedStringError();

      return exprs;
    }

    return await parseReadMacro();
  }

  async function parseList(): Promise<Term> {
    for (let [open, close] of listDelimiterPredicates) {
      const result = await parseDelimitedList(open, close)
      if (result) return result;
    }
    return await parseString();
  }

  async function parse(): Promise<Term> {
    await consumeIgnored();
    const expr = await parseList();
    return expr;
  }

  return await parse();
};
