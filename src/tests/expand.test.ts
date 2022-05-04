import { Lisp, Runtime, Utils } from "../bootstrap";

test('(expand) (let ...) = (lambda ...)', () => {
  expect(
    Utils.toString(Lisp.expand(Lisp.read(
      "(let ((x 'a) (y 'a)) (eq x y))"
    )))).toBe(
      "((lambda (x y) (eq x y)) (quote a) (quote a))"
    );

  Lisp.exec(`
    (let
      ((x 'a) (y 'a))
      (if (eq x y) 55 88)
    )
  `, Runtime.env)

  Lisp.exec(`
    (let
      ((x 'a) (y 'a))
      (if (eq x 5) 55 88)
    )
  `, Runtime.env)
});
