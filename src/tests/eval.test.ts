import { expect, test, describe } from 'vitest'
import * as Lisp from "../core/lisp";
import { list } from "../core/data/pair";
import { Procedure } from "../core/callable/proc";
import { createServerWorld } from "../world/server";

describe('evaluate works', () => {

  test('(eval) testing (+ .. n)', async () => {
    const env = await createServerWorld()
    expect(await Lisp.execute("(+ 1)", env)).toBe(1)
    expect(await Lisp.execute("(+ 1 1)", env)).toBe(2)
    expect(await Lisp.execute("(+ 1 1 1)", env)).toBe(3)
    expect(await Lisp.execute("(+ 1 1 1 1)", env)).toBe(4)
  })

  test('(eval) testing (* .. n)', async () => {
    const env = await createServerWorld()
    expect(await Lisp.execute("(* 2)", env)).toBe(2)
    expect(await Lisp.execute("(* 2 2)", env)).toBe(4)
    expect(await Lisp.execute("(* 2 2 2)", env)).toBe(8)
    expect(await Lisp.execute("(* 2 2 2 2)", env)).toBe(16)
  });

  test('(eval) (quote t) -> t', async () => {
    const env = await createServerWorld()
    const actual = await Lisp.execute(
      `(quote 5)`,
      env);
    expect(actual).toBe(5)
  });

  test("(eval) (car '(1 3)) -> 1", async () => {
    const env = await createServerWorld()
    const actual = await Lisp.execute(
      `(car '(1 3))`
    , env)
    expect(actual).toBe(1)
  });

  test("(eval) (cdr '(1 3)) -> 3", async () => {
    const env = await createServerWorld()
    const actual = await Lisp.execute(
      `(cdr '(1 3))`,
      env);
    const expected = list(3);
    expect(expected.equal(actual)).toBe(true)
  });

  test('(eval) lambda -> Procedure', async () => {
    const env = await createServerWorld()
    expect(await Lisp.execute(`
      (lambda (x) (+ x 3))
    `, env))
    .toBeInstanceOf(Procedure)
  });

  test('(eval) define x -> x ∈ env', async () => {
    const env = await createServerWorld()
    await Lisp.execute(`(define x 11)`, env);
    const actual = env.env.get('x');
    expect(actual).toBe(11)
  });

  test('(eval) define (x y) -> (lambda y) ∈ env', async () => {
    const env = await createServerWorld()
    await Lisp.execute(`(define (y x) x)`, env);

    expect(env.env.get('y'))
      .toBeInstanceOf(Procedure)
  });

  test('(eval) begin x1 x2 ... xɴ -> xɴ', async () => {
    const env = await createServerWorld()
    const actual = await Lisp.execute(
      `(begin (+ 1 3) (+ 2 3) (+ 3 3))
    `, env)
    expect(actual).toBe(6)
  });

  test('(eval) if x y z -> y if x else z', async () => {
    const env = await createServerWorld()

    // happy path
    const actual = await Lisp.execute(
      `(if (+ 1 3) 1 2)
    `, env);

    expect(actual).toBe(1)

    // else path
    const nextResult = await Lisp.execute(
      `(if (< 3 1) 1 2)
    `, env);
    expect(nextResult).toBe(2)
  });

  test('(eval) (x ∈ env) set! x y -> (x ∈ env) == y', async () => {
    const env = await createServerWorld()

    await Lisp.execute(`(define x 11)`, env);
    await Lisp.execute(`(set! x 99)`, env)

    expect(env.env.get('x')).toBe(99)
  });

  test('(eval) ((lambda) x) -> x', async () => {
    const env = await createServerWorld()
    const actual = await Lisp.execute(
      `((lambda (x) x) 55)`, env);
    const expected = 55
    expect(actual).toBe(expected)
  });
})
