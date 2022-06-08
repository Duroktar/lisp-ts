import { Character } from "../core/char";
import { EMPTY } from "../core/const";
import { tokenize } from "../core/lisp";
import { Num } from "../core/num";
import { cons, list } from "../core/pair";
import { Sym } from "../core/sym";
import { Vector } from "../core/vec";
import { createEnvironment } from "../env";

let ONE   = Num.ofInt(1),
    TWO   = Num.ofInt(2),
    THREE = Num.ofInt(3);

describe("(read) tests", () => {
  test("(read) character", async () => {
    const env = await createEnvironment()
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
    const env = await createEnvironment()
    expect(await tokenize('hello', env))
      .toStrictEqual(Sym('hello'));
  });

  test("(read) number", async () => {
    const env = await createEnvironment()
    expect(await tokenize("3", env))
      .toStrictEqual(THREE);
  });

  test("(read) symbol", async () => {
    const env = await createEnvironment()
    expect(await tokenize("x", env))
      .toStrictEqual(Sym('x'));
  });

  test("(read) quote", async () => {
    const env = await createEnvironment()
    const actual = await tokenize("'x", env);
    const expected = list(Sym('quote'), Sym('x'));
    expect(expected.equal(actual)).toBe(true);
  });

  test("(read) list", async () => {
    const env = await createEnvironment()
    const actual = await tokenize("(1 2 3)", env);
    const expected = list(ONE, TWO, THREE);
    expect(expected.equal(actual)).toBe(true);
  });

  test("(read) vector", async () => {
    const env = await createEnvironment()
    const result: Vector = (await tokenize("#(1 2 3)", env)) as any;
    expect(result).toBeInstanceOf(Vector);
    expect(result.data).toStrictEqual([ONE, TWO, THREE]);
  });

  test("(read) (eq 'x 'y) => (eq (quote x) (quote y))", async () => {
    const env = await createEnvironment()
    const actual = await tokenize("(eq 'x 'y)", env);
    const expected = list(Sym('eq'), list(Sym('quote'), Sym('x')), list(Sym('quote'), Sym('y')));
    expect(expected.equal(actual)).toBe(true);
  });
});
