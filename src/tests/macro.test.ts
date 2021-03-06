import { expect, test } from 'vitest';
import { list } from "../core/data/pair";
import { Sym } from "../core/data/sym";
import { execute, parse } from "../core/lisp";
import { createServerWorld } from "../world/server";

test("(macro) testing (define-syntax .. )", () => {
  const env = createServerWorld()
  const actual = parse(`
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

test("(macro) testing 2", () => {
  const env = createServerWorld()
  const actual = execute(`
    (load "samples/hygiene.scm")
  `, env);
  const expected = list(Sym('+'), 1);
  expect(expected.equal(actual)).toBe(true)
})

test.only("(macro) testing 2.2", () => {
  const env = createServerWorld()
  const actual = execute(`
    (let ((=> #f)) (cond (#t => 'ok)))
  `, env);
  const expected = list(Sym('+'), 1);
  expect(expected.equal(actual)).toBe(true)
})

test("(macro) testing 3", () => {
  const env = createServerWorld()
  const actual = execute(`
    (do ((vec (make-vector 5))
         (i 0 (+ i 1)))
        ((= i 5) vec)
        (vector-set! vec i i))
  `, env);
  const expected = list(Sym('+'), 1);
  expect(expected.equal(actual)).toBe(true)
})

test("(macro) testing do (works)", () => {
  const env = createServerWorld()
  const actual = execute(`
    (do ((x 0 (+ x 1))) ((= x 3) (writeln 'done)) (writeln 'looping))
  `, env);
  const expected = list(Sym('+'), 1);
  expect(expected.equal(actual)).toBe(true)
})

test("(macro) testing do (broken 0)", () => {
  const env = createServerWorld()
  const actual = execute(`
    (do ((x 0 (+ x 1)) (y 666)) ((= x 3) (writeln 'done)) (writeln 'looping))
  `, env);
  const expected = list(Sym('+'), 1);
  expect(expected.equal(actual)).toBe(true)
})

test("(macro) testing do (broken 1)", () => {
  const env = createServerWorld()
  const actual = execute(`
    (do ((vec (make-vector 5)) (i 0 (+ i 1))) ((= i 5) vec) (vector-set! vec i i))
  `, env);
  const expected = list(Sym('+'), 1);
  expect(expected.equal(actual)).toBe(true)
})

test("(macro) testing do (broken 2)", () => {
  const env = createServerWorld()
  const actual = execute(`
  (do ([x 6 (- x 1)]
       [acc 1])
    ((zero? x) acc)
    (display x) (newline)
    (set! acc (* acc x)))
  `, env);
  const expected = list(Sym('+'), 1);
  expect(expected.equal(actual)).toBe(true)
})
