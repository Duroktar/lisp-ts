import { isEofString, isPair } from "../guard";
import { iWorld } from "../interface/iWorld";
import { append, assert, Predicate, push } from "../utils";
import { NIL, TRUE } from "./const";
import { Character } from "./data/char";
import { MalformedStringError, MissingParenthesisError, UnexpectedParenthesisError, InvalidCharacterError } from "./data/error";
import { cons, list } from "./data/pair";
import { InPort } from "./port";
import { Sym, SymTable } from "./data/sym";
import { Vector } from "./data/vec";
import { Form, List } from "./form";
import { toStringSafe } from "./print";
import { Str } from "./data/string";

const DEBUG = false

function debugLog(...args: string[]): void {
  if (DEBUG) { console.log('[Read]:', ...args) }
}

const numberRegex = /^\#?(?:(?<radix>(?:(?:[e|i]?[b|o|d|x]{1})|(?:[b|o|d|x]{1}[e|i]?))?)(?:(?<integer>\d*)|(?<number>(?:\d+(?:\.(?:\d)+))))(?<precision>(?:[s|f|d|l]{1}\d+))?)$/gim

export function read(port: InPort, world: iWorld): Form {
  let cursor = port.readChar()

  const advance = () => {
    const c = cursor;
    cursor = port.readChar();
    return c;
  }

  const peek = () => port.peekChar();
  const current = () => cursor;
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
  const isSpecial = (c: string) => '!$%&*/:<~._^=?'.includes(c);
  const isMathOp = (c: string) => '+-*/<>'.includes(c);
  const isAlnum = (c: string) => isAlpha(c) || isDigit(c);
  const isValid = (c: string) => isAlnum(c) || isSpecial(c) || isMathOp(c) || isHash() || isEscape();
  const isValidAndSameLine = () => isValid(current()) && !isEOF()

  const consumeIgnored = () => {
    while ((isSemi() || isSpace() || isNewLine()) && !isEOF()) {
      if (isSemi() === false) advance();
      else advanceAndConsumeToEndOfLine()
    }
  };

  const advanceAndConsumeToEndOfLine = () => {
    advance();
    while (!isNewLine() && !isEofString(cursor))
      advance();
  }

  const listDelimiterPredicates = [
    [isOpenS, isCloseS],
    [isOpenM, isCloseM],
    [isOpenP, isCloseP]
  ];

  const parseDelimitedList = (open: Predicate, close: Predicate): Form | undefined => {
    if (open()) {
      advance();

      const exprs: Form[] = [];
      while (!close() && !isEOF()) {
        exprs.push(parse());
      }

      if (close()) { advance(); }
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

  const toLisp = (funcs: Record<string, any>) => Object.entries(funcs).reduce((acc: any, [key, val]: any) => { acc[key] = (...args: any[]) => val(...args) ? TRUE : NIL; return acc; }, {} as Record<string, any>);

  const readMacroLocals = {
    parse, advance, current, eatSpace: consumeIgnored,
    ...toLisp({ isEOF: isEofString, isSpace, isNewLine })
  };

  function parseWhileValid(): string {
    let atom: string = '';
    do {
      if (isEscape()) { advance(); }
      atom += advance();
    } while (isValidAndSameLine());
    assert(atom !== 'undefined', "parseWhileValid parsed 'undefined'")
    return atom;
  }

  function parseAtom(): Form {
    debugLog('parseAtom')
    let atom = parseWhileValid()
    if (Number.isNaN(parseInt(atom)) === false)
      return parseInt(atom)
    return Symbol.for(atom);
  }

  function parseQuote(): Form {
    debugLog('parseQuote')
    if (current() === "'") {
      advance();
      return list(SymTable.QUOTE, parse());
    }
    if (current() === "`") {
      advance();
      return list(SymTable.QUASIQUOTE, parse());
    }
    if (current() === ",") {
      advance();
      if (current() === "@") {
        advance();
        return list(SymTable.UNQUOTESPLICING, parse());
      }
      return list(SymTable.UNQUOTE, parse());
    }
    return parseAtom();
  }

  function parseHashPrefix(): Form {
    debugLog('parseHashPrefix')
    if (current() === "#") {
      advance();
      const lookahead = current();

      if (lookahead === "(") {
        const items = parseList()
        assert(isPair(items), "what is going on?")
        return new Vector(items.toArray())
      }

      if (lookahead === "\\") {
        advance();

        const char = (current() === '\\') // edge case
          ? advance()
          : parseWhileValid()

        if (char && (char.length === 1 || char.match(/^(newline|space)$/i))) {
          return new Character(char)
        }

        throw new InvalidCharacterError(port.file.cursor, port.file.data, lookahead)
      }

      if (lookahead === 't' || lookahead === 'f') {
        const next = peek();
        const valid = isDelimiter(next) || isSpace(next) || isNewLine(next) || isEOF(next);
        assert(valid, `bad-syntax \`#${lookahead + next}\``)

        advance()
        return Sym(`#${lookahead}`)
      }

      if ('eibodx'.includes(lookahead)) {
        const value = parseWhileValid()
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

    return parseQuote()
  }

  function parseReadMacro(): Form {
    debugLog('parseReadMacro')
    if (world.readerEnv.has(current())) {
      const value = advance();
      const macro = world.readerEnv.get<Function>(value);
      return macro(readMacroLocals);
    }
    return parseHashPrefix();
  }

  function parseString(): Form {
    debugLog('parseString')
    if (isDblQt()) {
      advance();

      let stringValue: string = '';
      while (!isDblQt() && !isNewLine() && !isEOF()) {
        if (current() === '\\')
          advance();
        stringValue += advance();
      }

      if (isDblQt())
        advance();
      else
        throw new MalformedStringError(port.file.cursor, port.file.data);

      return Str(stringValue);
    }

    return parseReadMacro();
  }

  function parseList(): Form {
    debugLog('parseList')
    for (let [open, close] of listDelimiterPredicates) {
      const result = parseDelimitedList(open, close)
      debugLog('parsed list:', toStringSafe(result!));
      if (result) return result;
    }
    return parseString();
  }

  function parse(): Form {
    debugLog('parse')
    consumeIgnored();
    const expr = parseList();
    return expr;
  }

  const expr = parse();
  debugLog('parsed expr:', toStringSafe(expr));
  return expr;
};
