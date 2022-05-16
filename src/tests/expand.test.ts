import * as Lisp from "../lib/lisp";
import * as Runtime from "../globals";
import { toString } from "../utils";
import { read } from "../lib/read";
import { expand } from "../lib/expand";

test('(expand) (let ...) = (λ ...)', () => {
  expect(
    toString(expand(read(
      "(let ((x 'a) (y 'a)) (eq x y))"
    )))).toBe(
      "((λ (x y) (eq x y)) (quote a) (quote a))"
    );

  Lisp.execute(`
    (let
      ((x 'a) (y 'a))
      (if (eq x y) 55 88)
    )
  `, Runtime.env)

  Lisp.execute(`
    (let
      ((x 'a) (y 'a))
      (if (eq x 5) 55 88)
    )
  `, Runtime.env)
});
