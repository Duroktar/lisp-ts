import { isEofString, isPair } from "../guard";
import { iWorld } from "../interface/iWorld";
import { append, assert, Predicate, push } from "../utils";
import { EMPTY, TRUE } from "./const";
import { Character } from "./data/char";
import { MalformedStringError, MissingParenthesisError, UnexpectedParenthesisError } from "./data/error";
import { cons, list } from "./data/pair";
import { InPort } from "./port";
import { Sym, SymTable } from "./data/sym";
import { Vector } from "./data/vec";
import { Form, List } from "./form";


const numberRegex = /^\#?(?:(?<radix>(?:(?:[e|i]?[b|o|d|x]{1})|(?:[b|o|d|x]{1}[e|i]?))?)(?:(?<integer>\d*)|(?<number>(?:\d+(?:\.(?:\d)+))))(?<precision>(?:[s|f|d|l]{1}\d+))?)$/gim

export const read = async (port: InPort, world: iWorld): Promise<Form> => {
  let cursor = await port.readChar()

  const advance = async () => {
    const c = cursor;
    cursor = await port.readChar();
    return c;
  }

  const peek = async () => await port.peekChar();
  const current = (c = cursor) => c;
  const isDblQt = (c = cursor) => c === '"';
  const isOpenS = (c = cursor) => c === '(';
  const isCloseS = (c = cursor) => c === ')';
  const isOpenM = (c = cursor) => c === '[';
  const isCloseM = (c = cursor) => c === ']';
  const isOpenP = (c = cursor) => c === '{';
  const isCloseP = (c = cursor) => c === '}';
  const isSpace = (c = cursor) => c === ' ';
  const isNewLine = (c = cursor) => c === '\n';
  const isHash = (c = cursor) => c === '#';
  const isSemi = (c = cursor) => c === ';';
  const isEscape = (c = cursor) => c === '\\';
  const isEOF = (c = cursor) => isEofString(c)
  const isAlpha = (c: string) => (((c >= 'a') && (c <= 'z')) || ((c >= 'A') && (c <= 'Z')));
  const isDigit = (c: string) => ((c >= '0') && (c <= '9'));
  const isDelimiter = (c: string) => '([{}])'.includes(c);
  const isSpecial = (c: string) => ((c === '.') || (c === '_') || (c === '^') || (c === '=') || (c === '?') || (c === '!'));
  const isMathOp = (c: string) => ((c === '+') || (c === '-') || (c === '*') || (c === '/') || (c === '<') || (c === '>'));
  const isAlnum = (c: string) => isAlpha(c) || isDigit(c);
  const isValid = (c: string) => isAlnum(c) || isSpecial(c) || isMathOp(c) || isHash() || isEscape();
  const isValidAndSameLine = () => isValid(current()) && !isEOF()

  const consumeIgnored = async () => {
    while ((isSemi() || isSpace() || isNewLine()) && !isEOF()) {
      if (isSemi() === false) await advance();
      else await advanceAndConsumeToEndOfLine()
    }
  };

  const advanceAndConsumeToEndOfLine = async () => {
    await advance();
    while (!isNewLine() && !isEofString(cursor))
      await advance();
  }

  const listDelimiterPredicates = [
    [isOpenS, isCloseS],
    [isOpenM, isCloseM],
    [isOpenP, isCloseP]
  ];

  const parseDelimitedList = async (open: Predicate, close: Predicate): Promise<Form | undefined> => {
    if (open()) {
      await advance();

      const exprs: Form[] = [];
      while (!close() && !isEOF()) {
        exprs.push(await parse());
      }

      if (close()) { await advance(); }
      else
        throw new MissingParenthesisError();

      let result: List = list()

      for (let i = 0; i < exprs.length; i++) {
        const expr = exprs[i];
        const mDot = exprs[i+1];
        if (mDot === Sym('.')) {
          const cell = cons(expr, exprs[i+2])
          result = append(result, cell)
          i += 2
        } else {
          result = push(result, expr)
        }
      }

      return result;
    }

    else if (current() === ')')
      throw new UnexpectedParenthesisError();
  }

  const toLisp = (funcs: Record<string, any>) => Object.entries(funcs).reduce((acc: any, [key, val]: any) => { acc[key] = (...args: any[]) => val(...args) ? TRUE : EMPTY; return acc; }, {} as Record<string, any>);

  const readMacroLocals = {
    parse, advance, current, eatSpace: consumeIgnored,
    ...toLisp({ isEOF: isEofString, isSpace, isNewLine })
  };

  async function parseWhileValid(): Promise<string> {
    let atom: string = '';
    do {
      if (isEscape()) { await advance(); }
      atom += await advance();
    } while (isValidAndSameLine());
    assert(atom !== 'undefined', "parseWhileValid parsed 'undefined'")
    return atom;
  }

  async function parseAtom(): Promise<Form> {
    let atom = await parseWhileValid()
    if (Number.isNaN(parseInt(atom)) === false)
      return parseInt(atom)
    return Symbol.for(atom);
  }

  async function parseQuote(): Promise<Form> {
    if (current() === "'") {
      await advance();
      return list(SymTable.QUOTE, await parse());
    }
    if (current() === "`") {
      await advance();
      return list(SymTable.QUASIQUOTE, await parse());
    }
    if (current() === ",") {
      await advance();
      if (current() === "@") {
        await advance();
        return list(SymTable.UNQUOTESPLICING, await parse());
      }
      return list(SymTable.UNQUOTE, await parse());
    }
    return await parseAtom();
  }

  async function parseHashPrefix(): Promise<Form> {
    if (current() === "#") {
      await advance();
      const lookahead = current();

      if (lookahead === "(") {
        const items = await parseList()
        assert(isPair(items), "what is going on?")
        return new Vector(items.toArray())
      }

      if (lookahead === "\\") {
        await advance();

        const val = await parseAtom();

        if (typeof val === 'symbol') {
          const char = val.description
          assert(
            char && (char.length === 1 || char.match(/^(newline|space)$/i)),

            `error parsing character: #\\${char}`
          )

          return new Character(val) as any
        }

        throw new Error('What am i doing here?')
      }

      if (lookahead === 't' || lookahead === 'f') {
        const next = await peek();
        const valid = isDelimiter(next) || isSpace(next) || isNewLine(next) || isEOF(next);
        assert(valid, `bad-syntax \`#${lookahead + next}\``)

        await advance()
        return Sym(`#${lookahead}`)
      }

      if ('eibodx'.includes(lookahead)) {
        const value = await parseWhileValid()
        const matches = value.match(numberRegex);
        if (matches && matches.groups) {
          const number = matches.groups['number']
          const integer = matches.groups['integer']
          const radix = matches.groups['radix']
          // const precision = matches.groups['precision']
          // const type = number ? NumType.number : NumType.integer;
          const repr = number ?? integer;
          // return new Num(repr, type, radix, precision)
          return parseInt(repr, parseInt(radix))
        }
      }

      throw new SyntaxError(`bad-syntax \`#${lookahead}\``)
    }

    return await parseQuote()
  }

  async function parseReadMacro(): Promise<Form> {
    if (world.readerEnv.has(current())) {
      const value = await advance();
      const macro = world.readerEnv.get<Function>(value);
      return await macro(readMacroLocals);
    }
    return await parseHashPrefix();
  }

  async function parseString(): Promise<Form> {
    if (isDblQt()) {
      await advance();

      let exprs: string = '';
      while (!isDblQt() && !isNewLine() && !isEOF()) {
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

  async function parseList(): Promise<Form> {
    for (let [open, close] of listDelimiterPredicates) {
      const result = await parseDelimitedList(open, close)
      if (result) return result;
    }
    return await parseString();
  }

  async function parse(): Promise<Form> {
    await consumeIgnored();
    const expr = await parseList();
    return expr;
  }

  return await parse();
};
