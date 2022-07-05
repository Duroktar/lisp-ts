import { expect, test, describe } from 'vitest'
import { Character } from "../core/data/char";
import { tokenize } from "../core/lisp";
import { list } from "../core/data/pair";
import { Sym } from "../core/data/sym";
import { Vector } from "../core/data/vec";
import { createServerWorld } from "../world/server";
import { toStringSafe } from '../core/print';

describe("(read) tests", () => {
  test("(read) character", () => {
    const env = createServerWorld()
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
    const env = createServerWorld()
    expect(tokenize('hello', env))
      .toStrictEqual(Sym('hello'));
  });

  test("(read) number", () => {
    const env = createServerWorld()
    expect(tokenize("3", env))
      .toStrictEqual(3);
  });

  test("(read) symbol", () => {
    const env = createServerWorld()
    expect(tokenize("x", env))
      .toStrictEqual(Sym('x'));
  });

  test("(read) quote", () => {
    const env = createServerWorld()
    const actual = tokenize("'x", env);
    const expected = list(Sym('quote'), Sym('x'));
    expect(expected.equal(actual)).toBe(true);
  });

  test("(read) list", () => {
    const env = createServerWorld()
    const actual = tokenize("(1 2 3)", env);
    const expected = list(1, 2, 3);
    expect(expected.equal(actual)).toBe(true);
  });

  test("(read) vector", () => {
    const env = createServerWorld()
    const result: Vector = (tokenize("#(1 2 3)", env)) as any;
    expect(result).toBeInstanceOf(Vector);
    expect(result.data).toStrictEqual([1, 2, 3]);
  });

  test("(read) (eq 'x 'y) => (eq (quote x) (quote y))", () => {
    const env = createServerWorld()
    const actual = tokenize("(eq 'x 'y)", env);
    const expected = list(Sym('eq'), list(Sym('quote'), Sym('x')), list(Sym('quote'), Sym('y')));
    expect(expected.equal(actual)).toBe(true);
  });
});

test("(read) test `((unquote))", () => {
  const env = createServerWorld()
  const actual = tokenize("`((unquote))", env);
  const expected = list(Sym('quasiquote'), list(list(Sym('unquote'))))
  expect(actual).toEqual(expected);
});


test.only("(read) test string to list wild cases", () => {
  const env = createServerWorld()

  const char = (c: string) => new Character(Sym(c))
  const expected = list(...`#'(),;\`\"`.split('').map(char))

  const actual = tokenize(`
    (string->list "#'(),;\`\\"")
  `, env);

  expect(toStringSafe(actual)).toEqual(toStringSafe(expected));
});

test("(read) test string to list wild cases", () => {
  const env = createServerWorld()

  const char = (c: string) => new Character(Sym(c))
  const expected = list(...`#'(),;\`\"`.split('').map(char))

  const actual = tokenize(`
    (string->list "+-.*/<=>!?@:$%_&~^")
  `, env);

  expect(actual).toEqual(expected);
});
