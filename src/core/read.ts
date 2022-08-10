import 'colors';
import { Range } from "ariadne-ts";
import { isPair } from "../guard";
import { assert, assertNever, push as pushList, append as appendList} from '../utils';
import { Char } from './data/char';
import { list } from './data/pair';
import { Str } from './data/string';
import { Sym, Symbol } from './data/sym';
import { Vector } from './data/vec';
import { InvalidCharacterError, MalformedStringError } from "./error";
import { Form, List } from './form';
import { File, InPort, Port } from "./port";
import { Int, Num, Number } from './data/num';
import { quotes } from './data/quote';
import { EOF } from './const';
import { iEnv } from '../interface/iEnv';
import { LogConfig } from '../logging';

const DEBUG = LogConfig.read

function debugLog(...args: string[]): void {
  if (DEBUG) { console.log('[Read.NEW]:', ...args) }
}

const numberRegex = /^\#?(?:(?<radix>(?:(?:[e|i]?[b|o|d|x]{1})|(?:[b|o|d|x]{1}[e|i]?))?)(?:(?<integer>\d*)|(?<number>(?:\d+(?:\.(?:\d)+))))(?<precision>(?:[s|f|d|l]{1}\d+))?)$/gim

type TokenOf<T extends string, V> = {
  type: T
  value: V
  range: Range;
  port: Port;
}

export type StringToken = TokenOf<'string',  string>
export type NumberToken = TokenOf<'number',  Number>
export type SymbolToken = TokenOf<'symbol',  string>
export type QuoteToken  = TokenOf<'quote',   string>
export type HashToken   = TokenOf<'hash',    string>
export type CharToken   = TokenOf<'char',    string>
export type ParenToken  = TokenOf<ParenType, string>
export type EOFToken = TokenOf<'eof', string>

export type ParenType = 'open' | 'close'

export type Token =
  | StringToken
  | NumberToken
  | SymbolToken
  | ParenToken
  | QuoteToken
  | HashToken
  | CharToken
  | EOFToken

function shortQuoteToLongQuote(val: string): Symbol {
  if (val && val in quotes)
    return quotes[val]

  throw new Error(`Invalid quote character: '${val}'`)
}

export function read(port: InPort, env: iEnv): Form {
  let cursor = port.readChar()
  let idx = port.file.cursor - 1

  const advance = () => {
    const c = cursor;
    cursor = port.readChar();
    idx = port.file.cursor - 1
    return c;
  }

  const mkToken = <T extends Token['type']>(type: T, value: Extract<Token, {type: T}>['value'], range: Range) => {
    return { type, value, range, port };
  }

  const current = () => cursor;
  const peek = () => port.peekChar();

  const isDblQt   = (c = cursor) => c === '"';
  const isSpace   = (c = cursor) => c === ' ';
  const isNewLine = (c = cursor) => c === '\n';
  const isEscape  = (c = cursor) => c === '\\';
  const isDigit   = (c = cursor) => !!c.match(/[0-9]/)
  const isHash    = (c = cursor) => c === '#';
  const isAlpha   = (c = cursor) => (((c >= 'a') && (c <= 'z')) || ((c >= 'A') && (c <= 'Z')));
  const isSpecial = (c = cursor) => '!$%&*/:<~._^=?'.includes(c);
  const isMathOp  = (c = cursor) => '+-*/<>'.includes(c);
  const isDelimit = (c: string) => '([])'.includes(c);
  const isAlnum   = (c = cursor) => isAlpha(c) || isDigit(c);
  const isValid   = (c = cursor) => isAlnum(c) || isSpecial(c) || isMathOp(c) || isHash() || isEscape();
  const isEOF     = (c = cursor) => c === File.EOF_STRING;

  const parseAtom = (): [string, Range] => {
    let startIdx = idx,
        lastIdx = idx,
        atom: string = '';
    do {
      if (isEscape()) { advance(); }
      atom += advance();
      lastIdx++
    } while (isValid() && !isEOF());

    return [atom, Range.new(startIdx, lastIdx)];
  }

  const string = (): Token => {
    let stringValue = '',
        openIdx = idx;

    advance();

    while (!isDblQt() && !isNewLine() && !isEOF()) {
      stringValue += advance()
    }

    if (isDblQt())
      advance();
    else
      throw new MalformedStringError(openIdx, idx);

    return mkToken('string', stringValue, Range.new(openIdx, idx));
  }

  const number = (value: Number, range: Range): Token => {
    const token = mkToken('number', value, range);
    value.token = token
    return token
  }

  const symbol = (value: string, range: Range): Token => {
    return mkToken('symbol', value, range);
  }

  const comment = (): void => {
    while (!isNewLine() && !isEOF()) {
      advance()
    }
  }

  const paren = (type: ParenType): Token => {
    const t = mkToken(type, current(), Range.new(idx, idx + 1));
    advance();
    return t
  }

  const quote = (val: string): Token => {
    const t = mkToken('quote', val, Range.new(idx, idx + 1));
    advance();
    return t
  }

  const hash = (): Token => {
    let startIdx = idx,
        lookahead = peek();

    advance()
    if (lookahead === "\\") {
      advance();

      function parseWhileValid(): string {
        let atom: string = '';
        do {
          if (isEscape()) { advance(); }
          atom += advance();
        } while (isValid() && !isEOF());
        assert(atom !== 'undefined', "parseWhileValid parsed 'undefined'")
        return atom;
      }

      const char = (current() === '\\') // edge case
        ? advance()
        : parseWhileValid()

      if (char && (char.length === 1 || char.match(/^(newline|space)$/i))) {
        return mkToken('char', char, Range.new(startIdx, idx))
      } else {
        throw new InvalidCharacterError(port.file.cursor, port.file.data, lookahead)
      }
    }

    else if (lookahead === 't' || lookahead === 'f') {
      const next = peek();
      const valid = isDelimit(next) || isSpace(next) || isNewLine(next) || isEOF(next);
      assert(valid, `bad-syntax \`#${lookahead + next}\``)

      advance()
      return mkToken('symbol', `#${lookahead}`, Range.new(startIdx, idx - 1));
    }

    else if ('eibodx'.includes(lookahead)) {
      const [atom, range] = parseAtom()
      const parsed = readNumber(atom, {prefix: lookahead, range});
      if (parsed !== undefined) {
        return number(parsed, range)
      }
    }

    return mkToken('hash', '#', Range.new(idx, idx + 1));
  }

  const next_token = (): Token => {
    while (!isEOF()) {
      switch (current()) {
      case ' ':
      case '\t':
      case '\n':
        advance();
        continue;
      case ';':
        comment();
        continue;
      case '#':
        return hash();
      case ',': {
        if (peek() === '@') {
          advance();
          return quote(',@');
        }
      }
      case "'":
      case '`':
        return quote(cursor);
      case '(':
      case '[':
        return paren('open');
      case ')':
      case ']':
        return paren('close');
      case '"':
        return string();
      default:
        const [atom, range] = parseAtom();
        const maybeNum = readNumber(atom, {range});

        return maybeNum
          ? number(maybeNum, range)
          : symbol(atom, range);
      }
    }

    return mkToken('eof', EOF.value as any, Range.new(idx, idx))
  }

  function read_ahead(token: Token): Form {
    switch(token.type) {
      case 'open': {
        let L: List = list(),
            openIdx = token.range.start;

        const push = (val: Form) => { L = pushList(L, val) }
        const append = (val: Form) => { L = appendList(L, val) }

        while (true) {
          token = next_token()
          if (token.type === 'close') {
            if (isPair(L)) {
              L.range = Range.new(openIdx, token.range.start)
              L.token = token
            }
            return L
          }
          else if (token.type === 'symbol' && token.value === '.') {
            token = next_token()
            append(read_ahead(token))
          }
          else {
            push(read_ahead(token))
          }
        }
      }
      case 'close': {
        throw SyntaxError('unexpected )')
      }
      case 'quote': {
        const quote = shortQuoteToLongQuote(token.value)
        return list(quote, read_ahead(next_token()))
      }
      case 'hash': {
        token = next_token()
        if (token.type === 'open') {
          const items = read_ahead(token);
          assert(isPair(items), `Expected a pair`, items)
          return new Vector(items.toArray())
        }
        throw new SyntaxError('expected an opening paren')
      }
      case 'eof': {
        throw new SyntaxError('unexpected EOF')
      }
      case 'char':
        return Char(token.value, token)
      case 'string':
        return Str(token.value, token)
      case 'symbol':
        return Sym(token.value, token)
      case 'number':
        return token.value
      default: {
        return assertNever(token)
      }
    }
  }

  let token1 = next_token()

  if (token1.type === 'eof')
    return EOF

  return read_ahead(token1)
}

type ReadNumberOptions = {
  prefix?: string;
  radix?: number;
  range?: Range;
};

export function readNumber(value: string, options: ReadNumberOptions = {}): Number | undefined {
  const matches = numberRegex.exec(value);
  const exact = isExactNumber(value, options.prefix);

  if (matches?.groups && (matches.groups['number'] || matches.groups['integer'])) {
    const {radix: rdx, number, integer, precision} = matches.groups

    const radix = rdx ? parseInt(rdx) : options.radix ?? 10

    const Type =
      number  ? Num :
      integer ? Int :
      /* else */Num ;

    const num = Type(value, {
      exact,
      precision: parseInt(precision),
      prefix: options.prefix,
      radix,
    })

    return num
  }

  const radix = options.radix ?? 10;

  const num = Num(value, {
    exact,
    prefix: options.prefix,
    radix,
  });

  if (!global.Number.isNaN(num.value))
    return num
}

function isExactNumber(value: string, prefix?: string) {
  // A numerical constant may be specified to be either exact or inexact
  // by a prefix. The prefixes are `#e' for exact, and `#i' for inexact.
  // An exactness prefix may appear before or after any radix prefix that
  // is used. If the written representation of a number has no exactness
  // prefix, the constant may be either inexact or exact. It is inexact
  // if it contains a decimal point, an exponent, or a "#" character in
  // the place of a digit, otherwise it is exact.
  if (prefix)
    return prefix === 'e'

  if (value.match(/[.|e|#]/))
    return false

  return true
}

function parsePrefix(lookahead: string): string {
  // A number may be written in binary, octal, decimal, or hexadecimal
  // by the use of a radix prefix.

  // The radix prefixes are `#b' (binary), `#o' (octal), `#d' (decimal),
  // and `#x' (hexadecimal).

  // !!! With no radix prefix, a number is assumed to be expressed in decimal.

  // A numerical constant may be specified to be either exact or inexact by a prefix.
  // The prefixes are `#e' for exact, and `#i' for inexact.

  // !!! If the written representation of a number has no exactness prefix,
  // !!! the constant may be either inexact or exact. It is inexact if it
  // !!! contains a decimal point, an exponent, or a "#" character in the
  // !!! place of a digit, otherwise it is exact.

  return (<any>{
    e: 'exact', i: 'inexact', b: 'binary',
    o: 'octal', d: 'decimal', x: 'hexadecimal',
  })[lookahead]
}

// function parsePrecision(lookahead: string): string {
//   return (<any>{
//     s: 'short', d: 'single', f: 'double', l: 'long',
//   })[lookahead]
// }
