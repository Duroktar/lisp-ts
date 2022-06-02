import { parse } from "../core/lisp";
import { Sym } from "../core/sym";
import { toString } from "../core/toString";
import { createEnvironment } from "../env";

const env = createEnvironment()

describe('expand works', () => {
  test('(expand) testing (+ .. n)', async () => {
    expect(await parse("(+ 1)", env))
      .toStrictEqual([Sym('+'), 1])
  })

  test('(expand) . TODO', async () => {
  });
})
