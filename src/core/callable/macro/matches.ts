import { LogConfig } from "../../../logging";
import { range } from "../../../utils"
import { Symbol } from "../../data/sym";
import type { Form, List } from "../../form"
import { toString } from "../../print";
import { Syntax } from "../syntax"
import { Tree } from "./tree"

const DEBUG = LogConfig.matches;

function debugLog(...args: any[]): void {
  if (DEBUG) { console.log('[Matches]:'.blue, ...args); }
}

export class Matches {
  public data: Record<string, Tree> = {}
  constructor(
    public pattern: Form,
    public formals: List,
  ) {
    Syntax.patternVars(pattern, formals)
      .forEach(name => { this.data[name] = new Tree(name) })
  }
  descend(names: string[], depth: number) {
    debugLog('descend ' + names.join(', '))
    Object.entries(this.data)
      .forEach(([name, set]) => {
        if (names.includes(name))
          set.descend(depth)
      })
  }
  put(sym: Symbol, value: Form) {
    debugLog('put '.red + sym.name)
    const name = sym.name
    if (this.has(sym))
      this.data[name].push(value)
  }
  has(sym: Symbol) {
    const name = sym.name
    const rv = name in this.data && this.data[name] !== undefined;
    debugLog('has '.yellow + sym.name.blue, String(rv).dim.green)
    return rv
  }
  get(sym: Symbol) {
    const name = sym.name
    const rv = this.data[name].read()
    debugLog('get'.gray, sym.name.blue, 'got'.dim, toString(rv).green)
    return rv
  }
  expand(template: any, depth: number, block: Function): void {
    debugLog('expand'.dim)
    const names = Syntax.patternVars(template);
    range(0, this.size(names, depth)).forEach(() => {
      block();
      this.iterate(names, depth);
    })
  }
  size(names: any[], depth: number): number {
    let sizes: any[] = []
    Object.entries(this.data).forEach(([name, tree]) => {
      if (names.includes(name)) {
        const size = tree.size(depth);
        sizes.push(size)
      }
    })

    // sizes = sizes.compact.uniq
    let _sizes = [...new Set(sizes.filter(o => o !== undefined))]
    if (_sizes.length == 1) {
      const rv = _sizes[0];
      debugLog('size'.rainbow, rv)
      return rv
    }

    throw new Error(
      "Macro could not be expanded: mismatched repetition patterns")
  }
  iterate(names: any[], depth: number) {
    debugLog('iterate')
    Object.entries(this.data).forEach(([name, tree]) => {
      if (names.includes(name))
        tree.shift(depth)
    })
  }
}
