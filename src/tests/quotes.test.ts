import { expect, test } from 'vitest'
import { parse } from "../core/lisp";
import { createServerEnvironment } from "../env/server";
import { toString } from '../core/print';

test("(expand) testing quasiquotes", () => {
  const env = createServerEnvironment()
  const actual = parse("`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)", env);
  const expected = (
    "(cons 'a (cons (+ 1 2) " +
                          "(append (map abs '(4 -5 6)) " +
                                  "(cons 'b '()))))")

  expect(toString(actual)).toEqual(expected)
})
