import * as Lisp from "../core/lisp";
import { Num } from "../core/num";
import { list } from "../core/pair";
import { Procedure } from "../core/proc";
import { createEnvironment } from "../env";

describe('evaluate works', () => {

  test('(eval) testing (+ .. n)', async () => {
    const env = await createEnvironment()
    expect(Num.ofInt(1).equal(await Lisp.execute("(+ 1)", env))).toBe(true)
    expect(Num.ofInt(2).equal(await Lisp.execute("(+ 1 1)", env))).toBe(true)
    expect(Num.ofInt(3).equal(await Lisp.execute("(+ 1 1 1)", env))).toBe(true)
    expect(Num.ofInt(4).equal(await Lisp.execute("(+ 1 1 1 1)", env))).toBe(true)
  })

  test('(eval) testing (* .. n)', async () => {
    const env = await createEnvironment()
    expect(Num.ofInt(2).equal(await Lisp.execute("(* 2)", env))).toBe(true)
    expect(Num.ofInt(4).equal(await Lisp.execute("(* 2 2)", env))).toBe(true)
    expect(Num.ofInt(8).equal(await Lisp.execute("(* 2 2 2)", env))).toBe(true)
    expect(Num.ofInt(16).equal(await Lisp.execute("(* 2 2 2 2)", env))).toBe(true)
  });

  test('(eval) (quote t) -> t', async () => {
    const env = await createEnvironment()
    const actual = await Lisp.execute(
      `(quote 5)`,
      env);
    const expected = Num.ofInt(5);
    expect(expected.equal(actual)).toBe(true)
  });

  test("(eval) (car '(1 3)) -> 1", async () => {
    const env = await createEnvironment()
    const actual = await Lisp.execute(
      `(car '(1 3))`
    , env)
    expect(Num.ofInt(1).equal(actual)).toBe(true)
  });

  test("(eval) (cdr '(1 3)) -> 3", async () => {
    const env = await createEnvironment()
    const actual = await Lisp.execute(
      `(cdr '(1 3))`,
      env);
    const expected = list(Num.ofInt(3));
    expect(expected.equal(actual)).toBe(true)
  });

  test('(eval) lambda -> Procedure', async () => {
    const env = await createEnvironment()
    expect(await Lisp.execute(`
      (lambda (x) (+ x 3))
    `, env))
    .toBeInstanceOf(Procedure)
  });

  test('(eval) define x -> x ∈ env', async () => {
    const env = await createEnvironment()
    await Lisp.execute(`(define x 11)`, env);
    const actual = env.env.get('x');
    const expected = Num.ofInt(11)
    expect(expected.equal(actual)).toBe(true)
  });

  test('(eval) define (x y) -> (lambda y) ∈ env', async () => {
    const env = await createEnvironment()
    await Lisp.execute(`(define (y x) x)`, env);

    expect(env.env.get('y'))
      .toBeInstanceOf(Procedure)
  });

  test('(eval) begin x1 x2 ... xɴ -> xɴ', async () => {
    const env = await createEnvironment()
    const actual = await Lisp.execute(
      `(begin (+ 1 3) (+ 2 3) (+ 3 3))
    `, env)
    const expected = Num.ofInt(6);
    expect(expected.equal(actual)).toBe(true)
  });

  test('(eval) if x y z -> y if x else z', async () => {
    const env = await createEnvironment()

    // happy path
    const actual = await Lisp.execute(
      `(if (+ 1 3) 1 2)
    `, env);
    const expected = Num.ofInt(1);
    expect(expected.equal(actual)).toBe(true)

    // else path
    const nextResult = await Lisp.execute(
      `(if (< 3 1) 1 2)
    `, env);
    const thenExpect = Num.ofInt(2);

    expect(thenExpect.equal(nextResult)).toBe(true)
  });

  test('(eval) (x ∈ env) set! x y -> (x ∈ env) == y', async () => {
    const env = await createEnvironment()

    await Lisp.execute(`(define x 11)`, env);
    await Lisp.execute(`(set! x 99)`, env)

    const actual = env.env.get('x');
    expect(Num.ofInt(99).equal(actual)).toBe(true)
  });

  test('(eval) ((lambda) x) -> x', async () => {
    const env = await createEnvironment()
    const expected = Num.ofInt(55);
    const actual = await Lisp.execute(
      `((lambda (x) x) 55)`, env);
    expect(expected.equal(actual)).toBe(true)
  });
})
