import { expect, test } from 'vitest';
import { list } from "../core/data/pair";
import { Sym } from "../core/data/sym";
import { parse } from "../core/lisp";
import { createServerWorld } from "../world/server";

test("(expand) testing (+ .. n)", () => {
  const env = createServerWorld()
  const actual = parse("(+ 1)", env);
  const expected = list(Sym('+'), 1);
  expect(expected.equal(actual)).toBe(true)
})

test("(expand) testing `((unquote))", () => {
  const env = createServerWorld()
  const actual = parse("`((unquote))", env)
  const expected = list(Sym('quasiquote'), list(list(Sym('unquote'))))
  expect(actual).toEqual(expected);
})
