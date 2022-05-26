import { InPort } from "../core/port";
import { read } from "../core/read";
import { toString } from "../core/toString";
import { createEnvironment } from "../env";

const {readerEnv} = createEnvironment()

test("(read) (eq 'x 'y) => (eq (quote x) (quote y))", () => {
  expect(
    toString(read(
      InPort.fromText("(eq 'x 'y)"),
      readerEnv,
    ))).toBe(
      "(eq (quote x) (quote y))"
    );
});
