import { expect, test, describe } from 'vitest'
import { NIL } from "../core/const";
import { cons, list, Pair } from "../core/data/pair";

describe("(pair) tests", () => {
  describe("(cons) helper tests", () => {
    test("1", async () => {
      const actual = cons(1, cons(2, cons(3, NIL)))
      const expected = new Pair(1, new Pair(2, new Pair(3, NIL)));
      expect(actual.equal(expected)).toBe(true);
    })
    test("2", async () => {
      const actual = cons(1, NIL)
      const expected = new Pair(1, NIL);
      expect(actual.equal(expected)).toBe(true);
    })
    test("3", async () => {
      const actual = cons(1, 2)
      const expected = new Pair(1, 2);
      expect(actual.equal(expected)).toBe(true);
    })
  })
  test("(list) helper", async () => {
    const actual = list(1, 2, 3)
    const expected = cons(1, cons(2, cons(3, NIL)))
    expect(actual.equal(expected)).toBe(true);
  })
})
