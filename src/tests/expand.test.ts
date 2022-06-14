import { expect, test } from 'vitest';
import { list } from "../core/data/pair";
import { Sym } from "../core/data/sym";
import { parse } from "../core/lisp";
import { createServerWorld } from "../world/server";

test("(expand) testing (+ .. n)", async () => {
  const env = await createServerWorld()
  const actual = await parse("(+ 1)", env);
  const expected = list(Sym('+'), 1);
  expect(expected.equal(actual)).toBe(true)
})

test("(expand) testing `((unquote))", async () => {
  const env = await createServerWorld()
  const actual = await parse("`((unquote))", env).catch(err => err.stack)
  const expected = list(Sym('quasiquote'), list(list(Sym('unquote'))))
  expect(actual).toEqual(expected);
})
