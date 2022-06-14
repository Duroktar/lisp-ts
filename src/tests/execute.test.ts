import { expect, test, describe } from 'vitest'
import { execute } from "../core/lisp";
import { createServerWorld } from "../world/server";
import { toString } from '../core/print';

// 4.2.6 Quasiquotation
describe("(execute) testing quasiquotes", () => {

  // section 1

  test("section 1 :: test 1", async () => {
    const world = await createServerWorld()
    const actual = await execute("`(list ,(+ 1 2) 4)", world);
    const expected = "(list 3 4)";

    expect(toString(actual)).toEqual(expected)
  })

  test("section 1 :: test 2", async () => {
    const world = await createServerWorld()
    const actual = await execute("(let ((name 'a)) `(list ,name ',name))", world);
    const expected = "(list a 'a)";

    expect(toString(actual)).toEqual(expected)
  })

  test("section 1 :: test 3", async () => {
    const world = await createServerWorld()
    const actual = await execute("`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)", world);
    const expected = "(a 3 4 5 6 b)"

    expect(toString(actual)).toEqual(expected)
  })

  test("section 1 :: test 4", async () => {
    const world = await createServerWorld()
    const actual = await execute("`((`foo' ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))", world).catch(err => err.message);
    const expected = "((`foo 7) . cons)";

    expect(toString(actual)).toEqual(expected)
  })

  test("section 1 :: test 5", async () => {
    const world = await createServerWorld()
    const actual = await execute("`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)", world).catch(err => err.message);
    const expected = "#(10 5 2 4 3 8)";

    expect(toString(actual)).toEqual(expected)
  })


  // section 2
  test("section 2 :: test 1", async () => {
    const env = await createServerWorld()
    const actual = await execute("`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)", env).catch(err => err.message);
    const expected = "(a `(b ,(+ 1 2) ,(foo 4 d) e) f)"

    expect(toString(actual)).toEqual(expected)
  })

  test("section 2 :: test 2", async () => {
    const env = await createServerWorld()
    const actual = await execute("(let ((name1 'x) (name2 'y)) `(a `(b ,,name1 ,',name2 d) e))", env).catch(err => err.message);
    const expected = "(a `(b ,x ,'y d) e)"

    expect(toString(actual)).toEqual(expected)
  })

  // section 3
  test("section 3 :: test 1", async () => {
    const env = await createServerWorld()
    const actual = await execute("(quasiquote (list (unquote (+ 1 2)) 4))", env);
    const expected = "(list 3 4)"

    expect(toString(actual)).toEqual(expected)
  })

  test("section 3 :: test 2", async () => {
    const env = await createServerWorld()
    const actual = await execute("'(quasiquote (list (unquote (+ 1 2)) 4))", env);
    const expected = "`(list ,(+ 1 2) 4)"

    expect(toString(actual)).toEqual(expected)
  })

})
