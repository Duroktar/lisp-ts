import { LogConfig } from "../../../logging";
import { range } from "../../../utils"
import { Symbol } from "../../data/sym";
import type { Form, List } from "../../form"
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
    debugLog('descend: ' + names.join(', '))
    Object.entries(this.data)
      .forEach(([name, set]) => {
        if (names.includes(name))
          set.descend(depth)
      })
  }
  put(sym: Symbol, value: Form) {
    debugLog('put: '.red + sym.name)
    const name = sym.name
    if (this.has(sym))
      this.data[name].push(value)
  }
  has(sym: Symbol) {
    debugLog('has: '.yellow + sym.name)
    const name = sym.name
    return name in this.data && this.data[name] !== undefined
  }
  get(sym: Symbol) {
    debugLog('get: '.gray + sym.name)
    const name = sym.name
    return this.data[name].read()
  }
  expand(template: any, depth: number, block: Function): void {
    debugLog('expand')
    const names = Syntax.patternVars(template);
    range(0, this.size(names, depth)).forEach(() => {
      block();
      this.iterate(names, depth);
    })
  }
  size(names: any[], depth: number): number {
    debugLog('size')
    let sizes: any[] = []
    Object.entries(this.data).forEach(([name, tree]) => {
      if (names.includes(name)) {
        const size = tree.size(depth);
        sizes.push(size)
      }
    })

    // sizes = sizes.compact.uniq
    let _sizes = [...new Set(sizes.filter(o => o !== undefined))]
    debugLog('sizes'.rainbow, _sizes)
    if (_sizes.length == 1)
      return _sizes[0]

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
