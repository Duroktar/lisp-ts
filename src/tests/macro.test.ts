import { expect, test } from 'vitest';
import { list } from "../core/data/pair";
import { Sym } from "../core/data/sym";
import { execute, parse } from "../core/lisp";
import { createServerWorld } from "../world/server";

test("(macro) testing (define-syntax .. )", async () => {
  const env = await createServerWorld()
  const actual = await parse(`
    (define-syntax or
      (syntax-rules ()
        ((or) #f)
        ((or test) test)
        ((or test1 test2 ...)
         (let ((x test1))
          (if x x (or test2 ...))))))
  `, env);
  const expected = list(Sym('+'), 1);
  expect(expected.equal(actual)).toBe(true)
})

test("(macro) testing 2", async () => {
  const env = await createServerWorld()
  const actual = await execute(`
    (load "samples/hygiene.scm")
  `, env);
  const expected = list(Sym('+'), 1);
  expect(expected.equal(actual)).toBe(true)
})

test.only("(macro) testing 3", async () => {
  const env = await createServerWorld()
  const actual = await execute(`
    (run-tests)
  `, env);
  const expected = list(Sym('+'), 1);
  expect(expected.equal(actual)).toBe(true)
})
