import { expect, test } from 'vitest'
import { Tree } from '../core/callable/macro/tree';

test("(expand) testing quasiquotes", async () => {
  const tree = new Tree('asdf');

  expect(tree).toEqual(tree)
})
