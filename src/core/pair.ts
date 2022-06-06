import { EMPTY } from "./const";
import { Form } from "./forms";

import assert from "assert";

// type Form = any
// export const EMPTY = Symbol.for('()');

export class Pair {
  constructor(
    public car: Form,
    public cdr: Form | Pair,
  ) {}

  map = (fn: (value: Form) => any): Pair => {
    const first = fn(this.car);
    return new Pair(first,
      Pair.is(this.cdr)  ? this.cdr.map(fn) :
      this.cdr !== EMPTY ? fn(this.cdr)     :
      /* otherwise */      EMPTY
    )
  }

  reduce = (fn: (acc: any, value: any) => any, initialValue?: any): any => {
    this.forEach(val => { initialValue = fn(initialValue, val) })
    return initialValue
  }

  every = (fn: (value: any) => any): boolean => {
    if (!fn(this.car))
      return false
    if (this.cdr === EMPTY)
      return true
    if (this.isList())
      return this.cdr.every(fn)
    return fn(this.cdr)
  }

  some = (fn: (value: any, idx: number) => any, idx = 0): boolean => {
    if (fn(this.car, idx))
      return true
    if (this.cdr === EMPTY)
      return false
    if (this.isList())
      return this.cdr.some(fn, idx + 1)
    return fn(this.cdr, idx + 1)
  }

  equal = (other: any): boolean => {
    if (!Pair.is(other))
      return false;
    if (Pair.is(this.car))
      return this.car.equal(other.car);
    else if (this.car !== other.car)
      return false;
    if (Pair.is(this.cdr))
      return this.cdr.equal(other.cdr)
    else
      return this.cdr === other.cdr
  }

  forEach = (fn: (value: any) => any): void => {
    this.map(fn)
  }

  isList(): this is Pair & {cdr: Pair} {
    if (this.cdr === EMPTY) return true
    return Pair.is(this.cdr) && this.cdr.isList()
  }

  find(fn: (b: any, idx: number) => boolean, idx = 0): any {
    if (fn(this.car, idx))
      return this.car
    if (this.cdr === EMPTY)
      return null
    if (this.isList())
      return this.cdr.find(fn, idx + 1)
    return fn(this.cdr, idx + 1)
  }

  includes(pattern: any): boolean {
    return !!this.find(p => p === pattern)
  }

  slice(start?: number, end?: number) {
    start = start ?? 0
    end = end ?? this.length
    end = end < 0 ? this.length + end : end
    let currentIdx = 0
    let next: Pair = this
    let rv = []
    while (Pair.is(next.cdr) && currentIdx < end) {
      if (start <= currentIdx && currentIdx <= end)
        rv.push(next.car)
      next = next.cdr
      currentIdx++
    }
    if (start <= currentIdx && currentIdx < end)
      rv.push(next.car)
    return list(...rv)
  }
  at(index: number) {
    if (index >= this.length) return EMPTY
    index = index < 0 ? this.length + index : index
    let currentIdx = 0
    let next: Pair = this
    while (Pair.is(next.cdr) && currentIdx < index) {
      next = next.cdr
      currentIdx++
    }
    assert(currentIdx === index)
    return next.car
  }
  push(val: any) {
    let next: Pair = this
    while (Pair.is(next.cdr)) {
      next = next.cdr
    }
    assert(next.cdr === EMPTY, 'can only push to a list')
    next.cdr = cons(val, EMPTY)
    return this
  }

  append(val: any) {
    let next: Pair = this
    while (Pair.is(next.cdr)) {
      next = next.cdr
    }
    assert(next.cdr === EMPTY, 'can only append to a list')
    if (Pair.is(val))
      next.cdr = val
    else
      next.cdr = cons(val, EMPTY)
    return this
  }

  toArray() {
    const result: Form[] = []
    this.forEach(val => result.push(val))
    return result
  }

  *[Symbol.iterator]() {
    let next: any = this
    while (Pair.is(next.cdr)) {
      yield next.car
      next = next.cdr
    }
    yield next.car
    if (next.cdr !== EMPTY)
      yield this.cdr
  }

  get length() {
    let inner = (obj: Pair, i = 0): number => {
      if (Pair.is(obj) && obj.isList())
        return inner(obj.cdr, i + 1);
      return i
    }
    return inner(this)
  }

  static is(obj: any): obj is Pair {
    return obj instanceof Pair
  }
}

export function cons(a: Form, b: Form) {
  return new Pair(a, b)
}

export function list(...args: any[]): Pair {
  if (args.length === 0)
    return EMPTY as any
  const [head, ...tail] = args
  return cons(head, list(...tail));
}

// const t = list(1, 2, 3, 4, 5)
// const l = [1, 2, 3, 4, 5];
// console.log(t.slice(0, 3).toArray())
// console.log(l.slice(0, 3))

// const n = cons([1, 2, 3, 4, 5], EMPTY);
// console.log(n.toArray())
