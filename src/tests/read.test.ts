import { Character } from "../core/char";
import { tokenize } from "../core/lisp";
import { Sym } from "../core/sym";
import { Vector } from "../core/vec";
import { createEnvironment } from "../env";

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
      .toStrictEqual(3);
  });

  test("(read) symbol", async () => {
    const env = await createEnvironment()
    expect(await tokenize("x", env))
      .toStrictEqual(Sym('x'));
  });

  test("(read) quote", async () => {
    const env = await createEnvironment()
    expect(await tokenize("'x", env))
      .toStrictEqual([Sym('quote'), Sym('x')]);
  });

  test("(read) list", async () => {
    const env = await createEnvironment()
    expect(await tokenize("(1 2 3)", env))
      .toStrictEqual([1, 2, 3]);
  });

  test("(read) vector", async () => {
    const env = await createEnvironment()
    const result: Vector = (await tokenize("#(1 2 3)", env)) as any;
    expect(result).toBeInstanceOf(Vector);
    expect(result.data).toStrictEqual([1, 2, 3]);
  });

  test("(read) (eq 'x 'y) => (eq (quote x) (quote y))", async () => {
    const env = await createEnvironment()
    expect(await tokenize("(eq 'x 'y)", env))
      .toStrictEqual([Sym('eq'), [Sym('quote'), Sym('x')], [Sym('quote'), Sym('y')]]);
  });
});
