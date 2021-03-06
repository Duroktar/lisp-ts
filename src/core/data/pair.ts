import { isAtom, isBinding, isList, isPair } from "../../guard";
import { assert, isEqual, isEqv } from "../../utils";
import { NIL } from "../const";
import { Form, List } from "../form";
import { car } from "../lisp";

export class Pair  {
  constructor(
    public car: Form,
    public cdr: Form | Pair = NIL,
  ) {}

  public parent?: Pair;

  replace(expression: Form) {
    if (this.parent) {
      if (isPair(this.parent)) {
        this.parent.car = expression
        this.parent.hosts(expression)
      }
    }
  }

  hosts(value: Form) {
    if (isPair(value) || isBinding(value))
      value.parent = this
  }

  *each(): any {
    let pair: Form = this,
        tail: List = NIL;
    while (isPair(pair)) {
      yield(pair.car)
      tail = pair
      pair = pair.cdr
    }
    return tail
  }

  map = (fn: (value: Form) => any): Pair => {
    const first = fn(this.car);
    return new Pair(first,
      Pair.is(this.cdr)  ? this.cdr.map(fn) :
      this.cdr !== NIL ? fn(this.cdr)     :
      /* otherwise */      NIL
    )
  }

  reduce = (fn: (acc: any, value: any) => any, initialValue?: any): any => {
    this.forEach(val => { initialValue = fn(initialValue, val) })
    return initialValue
  }

  every = (fn: (value: any) => any): boolean => {
    if (!fn(this.car))
      return false
    if (this.cdr === NIL)
      return true
    if (this.isList())
      return this.cdr.every(fn)
    return fn(this.cdr)
  }

  dottedEvery = (fn: (value: any) => any): boolean => {
    if (!fn(this.car))
      return false
    if (this.cdr === NIL)
      return false
    if (Pair.is(this.cdr))
      return this.cdr.dottedEvery(fn)
    else
      return fn(this.cdr)
  }

  some = (fn: (value: any, idx: number) => any, idx = 0): boolean => {
    if (fn(this.car, idx))
      return true
    if (this.cdr === NIL)
      return false
    if (this.isList())
      return this.cdr.some(fn, idx + 1)
    return fn(this.cdr, idx + 1)
  }

  equal = (other: any): boolean => {
    if (Pair.is(this.car)) {
      if (!this.car.equal(other.car))
        return false
    }
    else if (!isEqual(this.car, other.car))
      return false

    if (Pair.is(this.cdr))
      return this.cdr.equal(other.cdr)

    return isEqual(this.cdr, other.cdr)
  }

  forEach = (fn: (value: any) => any): void => {
    this.map(fn)
  }

  isList(): this is Pair & {cdr: Pair} {
    if (this.cdr === NIL) return true
    return Pair.is(this.cdr) && this.cdr.isList()
  }

  find(fn: (b: any, idx: number) => boolean, idx = 0): any {
    if (fn(this.car, idx))
      return this.car
    if (this.cdr === NIL)
      return null
    if (this.isList())
      return this.cdr.find(fn, idx + 1)
    return fn(this.cdr, idx + 1)
  }

  includes(pattern: Form): boolean {
    return !!this.find(p => isEqv(p, pattern))
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
  pairAt(index: number) {
    if (index >= this.length) return NIL
    index = index < 0 ? this.length + index : index
    let currentIdx = 0
    let next: Pair = this
    while (Pair.is(next.cdr) && currentIdx < index) {
      next = next.cdr
      currentIdx++
    }
    assert(currentIdx === index)
    return next
  }
  at(index: number) {
    const list = this.pairAt(index);
    if (Pair.is(list))
      return car(list)
    return NIL
  }
  push(val: any) {
    let next: Pair = this
    while (Pair.is(next.cdr)) {
      next = next.cdr
    }
    assert(next.cdr === NIL, 'can only push to a list')
    next.cdr = cons(val, NIL)
    return this
  }

  append(val: Form) {
    assert(isList(val) || isAtom(val), `Passed invalid type to append. Got (type: \`${typeof val}\`)`)
    let next: Pair = this
    while (Pair.is(next.cdr)) {
      next = next.cdr
    }
    assert(next.cdr === NIL, 'can only append to a list')
    if (Pair.is(val))
      next.cdr = val
    else {
      next.cdr = cons(val, NIL)
    }
    return this
  }

  get tail(): Pair {
    let pair: Form = this,
        tail: List = NIL;
    while (isPair(pair)) {
      tail = pair
      pair = pair.cdr
    }
    assert(isPair(tail), 'Invalid tail')
    return tail
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
    if (next.cdr !== NIL)
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

export function list(...args: Form[]): typeof args extends {length: 0} ? List : Pair {
  if (args.length === 0)
    return NIL as any
  const [head, ...tail] = args
  return cons(head, list(...tail));
}
