import { expect, test, describe } from 'vitest'
import { Char, Character } from "../core/data/char";
import { tokenize } from "../core/lisp";
import { list } from "../core/data/pair";
import { Sym } from "../core/data/sym";
import { Vector } from "../core/data/vec";
import { createServerEnvironment } from "../env/server";
import { toStringSafe } from '../core/print';
import { Num } from '../core/data/num';

describe("(read) tests", () => {
  test("(read) character", () => {
    const env = createServerEnvironment()
    const space = tokenize('#\\space', env);
    expect(space).toBeInstanceOf(Character);

    expect(tokenize('#ve', env))
      .rejects
      .toThrow('bad-syntax `#v`')

    expect(tokenize('#tyd', env))
      .rejects
      .toThrow('bad-syntax `#ty`')
  });

  test("(read) string", () => {
    const env = createServerEnvironment()
    expect(tokenize('hello', env))
      .toStrictEqual(Sym('hello'));
  });

  test("(read) number", () => {
    const env = createServerEnvironment()
    expect(tokenize("3", env))
      .toStrictEqual(3);
  });

  test("(read) symbol", () => {
    const env = createServerEnvironment()
    expect(tokenize("x", env))
      .toStrictEqual(Sym('x'));
  });

  test("(read) quote", () => {
    const env = createServerEnvironment()
    const actual = tokenize("'x", env);
    const expected = list(Sym('quote'), Sym('x'));
    expect(expected.equal(actual)).toBe(true);
  });

  test("(read) list", () => {
    const env = createServerEnvironment()
    const actual = tokenize("(1 2 3)", env);
    const expected = list(Num(1), Num(2), Num(3));
    expect(expected.equal(actual)).toBe(true);
  });

  test("(read) vector", () => {
    const env = createServerEnvironment()
    const result: Vector = (tokenize("#(1 2 3)", env)) as any;
    expect(result).toBeInstanceOf(Vector);
    expect(result.data).toStrictEqual([1, 2, 3]);
  });

  test("(read) (eq 'x 'y) => (eq (quote x) (quote y))", () => {
    const env = createServerEnvironment()
    const actual = tokenize("(eq 'x 'y)", env);
    const expected = list(Sym('eq'), list(Sym('quote'), Sym('x')), list(Sym('quote'), Sym('y')));
    expect(expected.equal(actual)).toBe(true);
  });
});

test("(read) test `((unquote))", () => {
  const env = createServerEnvironment()
  const actual = tokenize("`((unquote))", env);
  const expected = list(Sym('quasiquote'), list(list(Sym('unquote'))))
  expect(actual).toEqual(expected);
});

test("(read) test string to list wild cases", () => {
  const env = createServerEnvironment()

  const expected = list(...`#'(),;\`\"`.split('').map(c => Char(c)))

  const actual = tokenize(`
    (string->list "#'(),;\`\\"")
  `, env);

  expect(toStringSafe(actual)).toEqual(toStringSafe(expected));
});

test("(read) test string to list wild cases", () => {
  const env = createServerEnvironment()

  const expected = list(...`#'(),;\`\"`.split('').map(c => Char(c)))

  const actual = tokenize(`
    (string->list "+-.*/<=>!?@:$%_&~^")
  `, env);

  expect(actual).toEqual(expected);
});
