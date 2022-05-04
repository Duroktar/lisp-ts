import { Lisp, Utils } from "../bootstrap";

test("(read) (eq 'x 'y) => (eq (quote x) (quote y))", () => {
  expect(
    Utils.toString(Lisp.read(
      "(eq 'x 'y)"
    ))).toBe(
      "(eq (quote x) (quote y))"
    );
});
