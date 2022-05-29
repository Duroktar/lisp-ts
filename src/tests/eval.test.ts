import * as Lisp from "../core/lisp";
import { createEnvironment } from "../env";

const env = createEnvironment()

describe('evaluate works', () => {
  test('(eval) testing (+ .. n)', () => {
    expect(Lisp.execute("(+ 1)", env)).toBe(1)
    expect(Lisp.execute("(+ 1 1)", env)).toBe(2)
    expect(Lisp.execute("(+ 1 1 1)", env)).toBe(3)
    expect(Lisp.execute("(+ 1 1 1 1)", env)).toBe(4)
  })

  test('(eval) testing (* .. n)', () => {
    expect(Lisp.execute("(* 2)", env)).toBe(2)
    expect(Lisp.execute("(* 2 2)", env)).toBe(4)
    expect(Lisp.execute("(* 2 2 2)", env)).toBe(8)
    expect(Lisp.execute("(* 2 2 2 2)", env)).toBe(16)
  });

  test('(eval) testing (* .. n)', () => {
    expect(Lisp.execute(`
      (let
        ((x 'a) (y 'a))
        (if (eq x y) 55 88)
      )
    `, env)).toBe(55)

    expect(Lisp.execute(`
      (let
        ((x 'a) (y 'a))
        (if (eq x 5) 55 88)
      )
    `, env)).toBe(88)
  });
})
