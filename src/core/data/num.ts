
export class ExactNum {
  constructor(private value: number, private repr: string) {}

  toString() {
    return this.repr
  }

  valueOf() {
    return this.value
  }
}
