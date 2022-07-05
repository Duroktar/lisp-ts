import { expect, test } from 'vitest'
import { execute } from "../core/lisp";
import { createServerWorld } from "../world/server";
import { toString } from '../core/print';

test("(lib) testing apply (0)", () => {
  const env = createServerWorld()
  const actual = execute("(apply cons 1 2 '())", env);
  const expected = '(1 . 2)'

  expect(toString(actual)).toEqual(expected)
})

test("(lib) testing apply (1)", () => {
  const env = createServerWorld()
  const actual = execute("(apply + (list 3 4))", env);
  const expected = 7

  expect(actual).toEqual(expected)
})

test("(lib) testing apply (2)", () => {
  const env = createServerWorld()

  execute(`
    (define compose
      (lambda (f g)
        (lambda args
          (f (apply g args)))))
  `, env);

  const actual = execute(`
    ((compose sqrt *) 12 75)
  `, env);

  const expected = 30

  expect(actual).toEqual(expected)
})

test("(lib) testing apply (3)", () => {
  const env = createServerWorld()
  const actual = execute("(apply (lambda (z) z) 1 '())", env);
  const expected = 1

  expect(actual).toEqual(expected)
})

test.only("(lib) testing apply (4)", () => {
  const env = createServerWorld()
  const actual = execute('(begin (load-from-library "pretty-print.scm") (pp-string "(let ((a 1) (b 2)) (cons a b))"))', env);
  const expected = 1

  expect(actual).toEqual(expected)
})
