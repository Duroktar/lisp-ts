import * as Lisp from "../core/lisp";
import { NativeProc, Procedure } from "../core/proc";
import { createEnvironment } from "../env";

const env = createEnvironment()

describe('evaluate works', () => {
  test('(eval) testing (+ .. n)', async () => {
    expect(await Lisp.execute("(+ 1)", env)).toBe(1)
    expect(await Lisp.execute("(+ 1 1)", env)).toBe(2)
    expect(await Lisp.execute("(+ 1 1 1)", env)).toBe(3)
    expect(await Lisp.execute("(+ 1 1 1 1)", env)).toBe(4)
  })

  test('(eval) testing (* .. n)', async () => {
    expect(await Lisp.execute("(* 2)", env)).toBe(2)
    expect(await Lisp.execute("(* 2 2)", env)).toBe(4)
    expect(await Lisp.execute("(* 2 2 2)", env)).toBe(8)
    expect(await Lisp.execute("(* 2 2 2 2)", env)).toBe(16)
  });

  test('(eval) (quote t) -> t', async () => {
    expect(await Lisp.execute(
      `(quote 5)`
    , env)).toBe(5)
  });

  test("(eval) (car '(1 3)) -> 1", async () => {
    expect(await Lisp.execute(
      `(car '(1 3))`
    , env)).toBe(1)
  });

  test("(eval) (cdr '(1 3)) -> 3", async () => {
    expect(await Lisp.execute(
      `(cdr '(1 3))`
    , env)).toStrictEqual([3])
  });

  test('(eval) lambda -> Procedure', async () => {
    expect(await Lisp.execute(`
      (lambda (x) (+ x 3))
    `, env))
    .toBeInstanceOf(Procedure)
  });

  test('(eval) define x -> x ∈ env', async () => {
    await Lisp.execute(`(define x 11)`, env);

    expect(env.env.get('x'))
      .toBe(11)
  });

  test('(eval) define (x y) -> (lambda y) ∈ env', async () => {
    await Lisp.execute(`(define (y x) x)`, env);

    expect(env.env.get('y'))
      .toBeInstanceOf(Procedure)
  });

  test('(eval) begin x1 x2 ... xɴ -> xɴ', async () => {
    expect(await Lisp.execute(
      `(begin (+ 1 3) (+ 2 3) (+ 3 3))
    `, env))
    .toBe(6)
  });

  test('(eval) if x y z -> y if x else z', async () => {

    // happy path
    expect(await Lisp.execute(
      `(if (+ 1 3) 1 2)
    `, env))
    .toBe(1)

    // else path
    expect(await Lisp.execute(
      `(if (< 3 1) 1 2)
    `, env))
    .toBe(2)
  });

  test('(eval) (x ∈ env) set! x y -> (x ∈ env) == y', async () => {
    await Lisp.execute(`(set! x 99)`, env)

    expect(env.env.get('x'))
    .toBe(99)
  });

  test('(eval) ((lambda) x) -> x', async () => {
    expect(await Lisp.execute(
      `((lambda (x) x) 55)`, env))
    .toBe(55)
  });
})
