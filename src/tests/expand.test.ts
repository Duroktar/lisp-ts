import { expect, test } from 'vitest';
import { Num } from '../core/data/num';
import { list } from "../core/data/pair";
import { Sym } from "../core/data/sym";
import { parse } from "../core/lisp";
import { createServerEnvironment } from "../env/server";

test("(expand) testing (+ .. n)", () => {
  const env = createServerEnvironment()
  const actual = parse("(+ 1)", env);
  const expected = list(Sym('+'), Num(1));
  expect(expected.equal(actual)).toBe(true)
})

test("(expand) testing `((unquote))", () => {
  const env = createServerEnvironment()
  const actual = parse("`((unquote))", env)
  const expected = list(Sym('quasiquote'), list(list(Sym('unquote'))))
  expect(actual).toEqual(expected);
})
