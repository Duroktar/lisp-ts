import { expect, test, describe } from 'vitest'
import { execute } from "../core/lisp";
import { createServerEnvironment } from "../env/server";
import { toString } from '../core/print';

// 4.2.6 Quasiquotation
describe("(execute) testing quasiquotes", () => {

  // section 1

  test("section 1 :: test 1", () => {
    const world = createServerEnvironment()
    const actual = execute("`(list ,(+ 1 2) 4)", world);
    const expected = "(list 3 4)";

    expect(toString(actual)).toEqual(expected)
  })

  test("section 1 :: test 2", () => {
    const world = createServerEnvironment()
    const actual = execute("(let ((name 'a)) `(list ,name ',name))", world);
    const expected = "(list a 'a)";

    expect(toString(actual)).toEqual(expected)
  })

  test("section 1 :: test 3", () => {
    const world = createServerEnvironment()
    const actual = execute("`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)", world);
    const expected = "(a 3 4 5 6 b)"

    expect(toString(actual)).toEqual(expected)
  })

  test("section 1 :: test 4", () => {
    const world = createServerEnvironment()
    const actual = execute("`((`foo' ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))", world)
    const expected = "((`foo 7) . cons)";

    expect(toString(actual)).toEqual(expected)
  })

  test("section 1 :: test 5", () => {
    const world = createServerEnvironment()
    const actual = execute("`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)", world)
    const expected = "#(10 5 2 4 3 8)";

    expect(toString(actual)).toEqual(expected)
  })


  // section 2
  test("section 2 :: test 1", () => {
    const env = createServerEnvironment()
    const actual = execute("`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)", env)
    const expected = "(a `(b ,(+ 1 2) ,(foo 4 d) e) f)"

    expect(toString(actual)).toEqual(expected)
  })

  test("section 2 :: test 2", () => {
    const env = createServerEnvironment()
    const actual = execute("(let ((name1 'x) (name2 'y)) `(a `(b ,,name1 ,',name2 d) e))", env)
    const expected = "(a `(b ,x ,'y d) e)"

    expect(toString(actual)).toEqual(expected)
  })

  // section 3
  test("section 3 :: test 1", () => {
    const env = createServerEnvironment()
    const actual = execute("(quasiquote (list (unquote (+ 1 2)) 4))", env);
    const expected = "(list 3 4)"

    expect(toString(actual)).toEqual(expected)
  })

  test("section 3 :: test 2", () => {
    const env = createServerEnvironment()
    const actual = execute("'(quasiquote (list (unquote (+ 1 2)) 4))", env);
    const expected = "`(list ,(+ 1 2) 4)"

    expect(toString(actual)).toEqual(expected)
  })

  test("t;lskfdjskldf", () => {
    const env = createServerEnvironment()
    const actual = execute(`(macroexpand '(let ((=> #f)) (cond (#t => 'ok))))`, env);
    const expected = "(let ((=> #f)) (let ((temp #t)) (if temp ('ok temp))))"

    expect(toString(actual)).toEqual(expected)
  })

  test.only("dfdfsfddfdsfdf", () => {
    const env = createServerEnvironment()
    const actual = execute(`(case (car '(c d)) ((a e i o u) 'vowel) ((w y) 'semivowel) (else 'consonant))`, env);
    const expected = ""

    expect(toString(actual)).toEqual(expected)
  })

})
