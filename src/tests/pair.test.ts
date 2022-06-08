import { EMPTY } from "../core/const";
import { Num } from "../core/num";
import { cons, list, Pair } from "../core/pair";

let ONE = Num.ofInt(1),
    TWO = Num.ofInt(2),
    THREE = Num.ofInt(3);

describe("(pair) tests", () => {
  describe("(cons) helper tests", () => {
    test("1", async () => {
      const actual = cons(ONE, cons(TWO, cons(THREE, EMPTY)))
      const expected = new Pair(ONE, new Pair(TWO, new Pair(THREE, EMPTY)));
      expect(actual.equal(expected)).toBe(true);
    })
    test("2", async () => {
      const actual = cons(ONE, EMPTY)
      const expected = new Pair(ONE, EMPTY);
      expect(actual.equal(expected)).toBe(true);
    })
    test("3", async () => {
      const actual = cons(ONE, TWO)
      const expected = new Pair(ONE, TWO);
      expect(actual.equal(expected)).toBe(true);
    })
  })
  test("(list) helper", async () => {
    const actual = list(ONE, TWO, THREE)
    const expected = cons(ONE, cons(TWO, cons(THREE, EMPTY)))
    expect(actual.equal(expected)).toBe(true);
  })
})
