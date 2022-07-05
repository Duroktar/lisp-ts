import { expect, test } from 'vitest'
import { Tree } from '../core/callable/macro/tree';

test("(expand) testing quasiquotes", () => {
  const tree = new Tree('asdf');

  expect(tree).toEqual(tree)
})
