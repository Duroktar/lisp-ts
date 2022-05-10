import { Lisp } from "../lib/lisp";
import { toString } from "../utils";

test("(read) (eq 'x 'y) => (eq (quote x) (quote y))", () => {
  expect(
    toString(Lisp.read(
      "(eq 'x 'y)"
    ))).toBe(
      "(eq (quote x) (quote y))"
    );
});
