import { read } from "../lib/read";
import { toString } from "../utils";

test("(read) (eq 'x 'y) => (eq (quote x) (quote y))", () => {
  expect(
    toString(read(
      "(eq 'x 'y)"
    ))).toBe(
      "(eq (quote x) (quote y))"
    );
});
