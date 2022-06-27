import { range } from "../../../utils";
import { Form } from "../../form";

export class Tree {
  constructor(
    public name: string,
    public data: number[] = [],
    public depth: number = 0,
  ) {}
  descend(depth: number) {
    this.tail(depth - 1).push([])
    if (depth > this.depth)
      this.depth = depth
  }
  push(value: Form) {
    this.tail(this.depth).push(value)
  }
  get read() {
    const a = this.current(this.depth);
    const depth = this.indexes[this.depth];
    return a[depth]
  }
  shift(depth: number) {
    if (depth > this.depth) {
      return
    }

    this._indexes[depth] += 1

    const nextIdx = this._indexes[depth]
    const current = this.current(depth);

    if (nextIdx >= current.length) {
      this._indexes[depth] = 0
    }
  }
  size(depth: number): number {
    return (depth > this.depth)
      ? 0
      : this.current(depth)?.length ?? 0
  }
  tail(depth: number) {
    return range(0, depth)
      .reduce(list => list[list.length-1], <any>this.data)
  }
  current(depth: number) {
    return range(0, depth)
      .map(i => this.indexes[i])
      .reduce((list, i) => list[i], <any>this.data)
  }

  get indexes(): number[] {
    if (!this._indexes) {
      const xs = Array(this.depth).fill(0)
      this._indexes = [0, ...xs]
    }
    return this._indexes
  }

  private  _indexes!: number[]
}
