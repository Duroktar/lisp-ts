import { LogConfig } from "../../../logging";
import { range } from "../../../utils";
import { Form } from "../../form";
import { toString } from "../../print";

const DEBUG = LogConfig.tree;

function debugLog(...args: any[]): void {
  if (DEBUG) { console.log('[Tree]:'.yellow, ...args); }
}

export class Tree {
  constructor(
    public name: string,
    public data: number[] = [],
    public depth: number = 0,
  ) {}
  descend(depth: number) {
    debugLog('descend depth:'.dim, depth);
    this.tail(depth - 1).push([])
    if (depth > this.depth)
      this.depth = depth
  }
  push(value: Form) {
    debugLog('push value:'.blue, toString(value));
    // debugLog('push value:', value)
    this.tail(this.depth).push(value)
  }
  read() {
    const tree = this.current(this.depth);
    const depth = this.indexes[this.depth];
    return tree[depth]
  }
  shift(depth: number) {
    debugLog('shift depth:'.green, depth)
    if (depth > this.depth) {
      return
    }

    this._indexes[depth] += 1

    if (this._indexes[depth] >= this.current(depth).length) {
      this._indexes[depth] = 0
    }
  }
  size(depth: number): number {
    const rv = (depth > this.depth)
      ? 0
      : this.current(depth)?.length ?? 0

    debugLog('size depth:'.red, depth, 'rv:', rv)

    return rv
  }
  tail(depth: number) {
    debugLog('tail depth:'.dim, depth)
    return range(0, depth)
      .reduce(list => list[list.length-1], <any>this.data)
  }
  current(depth: number) {
    debugLog('current depth:'.cyan, depth)
    return range(0, depth)
      .map(i => this.indexes[i])
      .reduce((list, i) => list[i], <any>this.data)
  }

  get indexes(): number[] {
    if (!this._indexes) {
      this._indexes = range(0, this.depth + 1).map(() => 0)
    }
    return this._indexes
  }

  private  _indexes!: number[]
}
