import { parse } from "../core/lisp";
import { Sym } from "../core/sym";
import { createEnvironment } from "../env";

describe('expand works', () => {
  test('(expand) testing (+ .. n)', async () => {
    const env = await createEnvironment()
    expect(await parse("(+ 1)", env))
      .toStrictEqual([Sym('+'), 1])
  })
})
