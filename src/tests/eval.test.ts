import { Lisp } from "../lib/lisp";
import * as Runtime from "../globals";

describe('evaluate works', () => {
  test('(eval) testing (+ .. n)', () => {
    expect(Lisp.exec("(+ 1)", Runtime.env)).toBe(1)
    expect(Lisp.exec("(+ 1 1)", Runtime.env)).toBe(2)
    expect(Lisp.exec("(+ 1 1 1)", Runtime.env)).toBe(3)
    expect(Lisp.exec("(+ 1 1 1 1)", Runtime.env)).toBe(4)
  })

  test('(eval) testing (* .. n)', () => {
    expect(Lisp.exec("(* 2)", Runtime.env)).toBe(2)
    expect(Lisp.exec("(* 2 2)", Runtime.env)).toBe(4)
    expect(Lisp.exec("(* 2 2 2)", Runtime.env)).toBe(8)
    expect(Lisp.exec("(* 2 2 2 2)", Runtime.env)).toBe(16)
  });

  test('(eval) testing (* .. n)', () => {
    expect(Lisp.exec(`
      (let
        ((x 'a) (y 'a))
        (if (eq x y) 55 88)
      )
    `, Runtime.env)).toBe(55)

    expect(Lisp.exec(`
      (let
        ((x 'a) (y 'a))
        (if (eq x 5) 55 88)
      )
    `, Runtime.env)).toBe(88)
  });
})
