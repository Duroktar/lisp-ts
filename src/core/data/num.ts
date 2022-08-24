import { Form } from "../form";
import { NumberToken } from "../read";

export class Number {
  constructor(
    public raw: string,
    public exact: boolean = true,
    public radix: number = 10,
    public precision: number = 0,
    public prefix?: string,
    public token?: NumberToken,
  ) {
    this.value = toInt(raw, radix)
    this.precision =
      this.exact ? precision ?? getPrecision(this.value) :
      /* else */  (precision ?? getPrecision(this.value)) || 1
  }

  public value: number

  sub(other: Number): Number {
    return Num(this.value - other.value, {exact: this.exact && other.exact});
  }
  add(other: Number): Number {
    return Num(this.value + other.value, {exact: this.exact && other.exact});
  }
  mul(other: Number): Number {
    return Num(this.value * other.value, {exact: this.exact && other.exact});
  }
  div(other: Number): Number {
    return Num(this.value / other.value, {exact: this.exact && other.exact});
  }
  mod(other: Number): Number {
    return Num(this.value % other.value, {exact: this.exact && other.exact});
  }
  gt(other: Number): boolean {
    return this.value > other.value;
  }
  lt(other: Number): boolean {
    return this.value < other.value;
  }
  eq(other: Number, exact = true): boolean {
    return this.value === other.value && (!exact || this.eqExact(other));
  }
  eqExact(other: Number) {
    return other.exact === this.exact
  }
  equal(other: Form) {
    return other instanceof Number && this.eq(other)
  }
  notEq(other: Number): boolean {
    return this.value !== other.value;
  }
  gte(other: Number): boolean {
    return this.value >= other.value;
  }
  lte(other: Number): boolean {
    return this.value <= other.value;
  }
  isZero(): boolean {
    return this.value === 0;
  }

  toJS(): number { return this.value }

  toString(): string {
    const repr = this.value.toString();
    if (this.exact) {
      return repr
    }
    const [head, tail = ''] = repr.split('.')
    return `${head}.${tail.padEnd(this.precision, '0')}`;
  }
}

export class Complex extends Number {}
export class Real extends Complex {}
export class Rational extends Real {}
export class Integer extends Rational {}

type NumberArgs = {
  token?: NumberToken
  radix?: number
  precision?: number
  prefix?: string
  exact?: boolean
}

export const Num = (raw: string | number, args: NumberArgs = {}) => {
  // TODO: calculate type, exactness, etc...
  const { radix, precision, prefix, token, exact } = args
  return new Number(raw.toString(), exact, radix, precision, prefix, token)
}

export const Int = (raw: string | number, args: NumberArgs = {}) => {
  // TODO: calculate type, exactness, etc...
  const { radix, precision, prefix, token, exact } = args
  return new Integer(raw.toString(), exact, radix, precision, prefix, token)
}

export const Cmplx = (raw: string | number, args: NumberArgs = {}) => {
  // TODO: calculate type, exactness, etc...
  const { radix, precision, prefix, token, exact } = args
  return new Complex(raw.toString(), exact, radix, precision, prefix, token)
}

export const Rel = (raw: string | number, args: NumberArgs = {}) => {
  // TODO: calculate type, exactness, etc...
  const { radix, precision, prefix, token, exact } = args
  return new Real(raw.toString(), exact, radix, precision, prefix, token)
}

export const Rtnl = (raw: string | number, args: NumberArgs = {}) => {
  // TODO: calculate type, exactness, etc...
  const { radix, precision, prefix, token, exact } = args
  return new Rational(raw.toString(), exact, radix, precision, prefix, token)
}


function getPrecision(a: number) {
  if (!isFinite(a)) return 0;
  var e = 1, p = 0;
  while (Math.round(a * e) / e !== a) { e *= 10; p++; }
  return p;
}

export function toInt(n: string | number, radix?: number): number {
  if (typeof n === 'number') return n
  return parseInt(global.Number(n).toString(), radix);
}

// const complex = new Complex(5)
// const real = new Real(5)
// const rational = new Rational(5)
// const int = new Integer(5)

// console.log(complex instanceof Number)
// console.log(real instanceof Number)
// console.log(rational instanceof Number)
// console.log(int instanceof Number)

// console.log(complex instanceof Integer)
// console.log(real instanceof Integer)
// console.log(rational instanceof Integer)
// console.log(int instanceof Integer)
