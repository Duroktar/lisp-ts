import { expect, test, describe } from 'vitest'
import { NIL } from "../core/const";
import { Num } from '../core/data/num';
import { cons, list, Pair } from "../core/data/pair";

describe("(pair) tests", () => {
  describe("(cons) helper tests", () => {
    test("1", async () => {
      const actual = cons(Num(1), cons(Num(2), cons(Num(3), NIL)))
      const expected = new Pair(Num(1), new Pair(Num(2), new Pair(Num(3), NIL)));
      expect(actual.equal(expected)).toBe(true);
    })
    test("2", async () => {
      const actual = cons(Num(1), NIL)
      const expected = new Pair(Num(1), NIL);
      expect(actual.equal(expected)).toBe(true);
    })
    test("3", async () => {
      const actual = cons(Num(1), Num(2))
      const expected = new Pair(Num(1), Num(2));
      expect(actual.equal(expected)).toBe(true);
    })
  })
  test("(list) helper", async () => {
    const actual = list(Num(1), Num(2), Num(3))
    const expected = cons(Num(1), cons(Num(2), cons(Num(3), NIL)))
    expect(actual.equal(expected)).toBe(true);
  })
})
