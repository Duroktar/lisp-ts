import { expect, test, describe } from 'vitest'
import { parse } from "../core/lisp";
import { list } from "../core/data/pair";
import { Sym } from "../core/data/sym";
import { createServerWorld } from "../world/server";

describe('expand works', () => {
  test('(expand) testing (+ .. n)', async () => {
    const env = await createServerWorld()
    const actual = await parse("(+ 1)", env);
    const expected = list(Sym('+'), 1);
    expect(expected.equal(actual)).toBe(true)
  })
})
