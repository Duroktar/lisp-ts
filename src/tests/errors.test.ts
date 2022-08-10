import { expect, test } from 'vitest'
import { execute } from "../core/lisp";
import { createServerEnvironment } from "../env/server";

test("(errors) testing errors (0)", () => {
  const env = createServerEnvironment()

  const actual = execute(`(load-from-library "pretty-print.scm")`, env);

  const expected = '(module "pretty-print.scm")'

  expect(actual).toEqual(expected)
})
