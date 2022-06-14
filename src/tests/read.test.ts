import { expect, test, describe } from 'vitest'
import { Character } from "../core/data/char";
import { tokenize } from "../core/lisp";
import { list } from "../core/data/pair";
import { Sym } from "../core/data/sym";
import { Vector } from "../core/data/vec";
import { createServerWorld } from "../world/server";

describe("(read) tests", () => {
  test("(read) character", async () => {
    const env = await createServerWorld()
    const space = await tokenize('#\\space', env);
    expect(space).toBeInstanceOf(Character);

    await expect(tokenize('#ve', env))
      .rejects
      .toThrow('bad-syntax `#v`')

    await expect(tokenize('#tyd', env))
      .rejects
      .toThrow('bad-syntax `#ty`')
  });

  test("(read) string", async () => {
    const env = await createServerWorld()
    expect(await tokenize('hello', env))
      .toStrictEqual(Sym('hello'));
  });

  test("(read) number", async () => {
    const env = await createServerWorld()
    expect(await tokenize("3", env))
      .toStrictEqual(3);
  });

  test("(read) symbol", async () => {
    const env = await createServerWorld()
    expect(await tokenize("x", env))
      .toStrictEqual(Sym('x'));
  });

  test("(read) quote", async () => {
    const env = await createServerWorld()
    const actual = await tokenize("'x", env);
    const expected = list(Sym('quote'), Sym('x'));
    expect(expected.equal(actual)).toBe(true);
  });

  test("(read) list", async () => {
    const env = await createServerWorld()
    const actual = await tokenize("(1 2 3)", env);
    const expected = list(1, 2, 3);
    expect(expected.equal(actual)).toBe(true);
  });

  test("(read) vector", async () => {
    const env = await createServerWorld()
    const result: Vector = (await tokenize("#(1 2 3)", env)) as any;
    expect(result).toBeInstanceOf(Vector);
    expect(result.data).toStrictEqual([1, 2, 3]);
  });

  test("(read) (eq 'x 'y) => (eq (quote x) (quote y))", async () => {
    const env = await createServerWorld()
    const actual = await tokenize("(eq 'x 'y)", env);
    const expected = list(Sym('eq'), list(Sym('quote'), Sym('x')), list(Sym('quote'), Sym('y')));
    expect(expected.equal(actual)).toBe(true);
  });
});

test("(read) test `((unquote))", async () => {
  const env = await createServerWorld()
  const actual = await tokenize("`((unquote))", env);
  const expected = list(Sym('quasiquote'), list(list(Sym('unquote'))))
  expect(actual).toEqual(expected);
});
