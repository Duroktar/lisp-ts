import { InPort } from "../core/port";
import { read } from "../core/read";
import { toString } from "../core/toString";
import { createEnvironment } from "../env";

const {readerEnv} = createEnvironment()

test("(read) (eq 'x 'y) => (eq (quote x) (quote y))", async () => {
  expect(
    toString(await read(
      InPort.fromString("(eq 'x 'y)"),
      readerEnv,
    ))).toBe(
      "(eq (quote x) (quote y))"
    );
});
