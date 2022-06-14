import { expect, test } from 'vitest'
import { parse } from "../core/lisp";
import { list } from "../core/data/pair";
import { Sym } from "../core/data/sym";
import { createServerWorld } from "../world/server";
import { toString } from '../core/print';

test("(expand) testing quasiquotes", async () => {
  const env = await createServerWorld()
  const actual = await parse("`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)", env);
  const expected = (
    "(cons 'a (cons (+ 1 2) " +
                          "(append (map abs '(4 -5 6)) " +
                                  "(cons 'b '()))))")

  expect(toString(actual)).toEqual(expected)
})
