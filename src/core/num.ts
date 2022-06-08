import assert from "assert"

export enum NumType {
  number,
  complex,
  real,
  rational,
  integer,
}

export class Num {
  constructor(
    value: string | number,
    public type: NumType = NumType.integer,
    public radix?: string,
    public precision?: string,
  ) {
    this.value = typeof value === 'string' ? parseInt(value) : value
    this.repr = typeof value === 'string' ? value : String(value)
  }
  public equal(other: any): boolean {
    if (other instanceof Num && other.type <= this.type) {
      return other.value === this.value
    }
    return false
  }

  public add(other: any): Num {
    assert(other instanceof Num && other.type <= this.type)
    return new Num(other.value + this.value, Math.min(other.type, this.type))
  }

  public sub(other: any): Num {
    assert(other instanceof Num && other.type <= this.type)
    return new Num(other.value - this.value, Math.min(other.type, this.type))
  }

  public mul(other: any): Num {
    assert(other instanceof Num && other.type <= this.type)
    return new Num(other.value * this.value, Math.min(other.type, this.type))
  }

  public div(other: any): Num {
    assert(other instanceof Num && other.type <= this.type)
    return new Num(other.value / this.value, Math.min(other.type, this.type))
  }

  public value: number
  public repr: string

  static ofInt(value: number) {
    return new Num(String(value), NumType.integer)
  }

  static ofNum(value: number) {
    return new Num(String(value), NumType.number)
  }
}
