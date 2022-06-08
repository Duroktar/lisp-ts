import { parse } from "../core/lisp";
import { Num } from "../core/num";
import { list } from "../core/pair";
import { Sym } from "../core/sym";
import { createEnvironment } from "../env";

describe('expand works', () => {
  test('(expand) testing (+ .. n)', async () => {
    const env = await createEnvironment()
    const actual = await parse("(+ 1)", env);
    const expected = list(Sym('+'), Num.ofInt(1));
    expect(expected.equal(actual)).toBe(true)
  })
})
