import { range } from "../../../utils"
import type { Form, List } from "../../form"
import { Syntax } from "./syntax"
import { Tree } from "./tree"

export class Matches {
  public data: Record<string, Tree> = {}
  constructor(
    public pattern: Form,
    public formals: List,
  ) {
    Syntax.pattern_vars(pattern, formals)
      .forEach(name => { this.data[name] = new Tree(name) })
  }
  descend(names: string[], depth: number) {
    Object.entries(this.data)
      .forEach(([name, set]) => {
        if (names.includes(name))
          set.descend(depth)
      })
  }
  put(sym: symbol, value: Form) {
    const name = sym.description!
    if (this.has(sym))
      this.data[name].push(value)
  }
  has(sym: symbol) {
    const name = sym.description!
    return name in this.data
  }
  get(sym: symbol) {
    const name = sym.description!
    return this.data[name].read
  }
  expand(template: any, depth: number, block: Function): void {
    const names = Syntax.pattern_vars(template)
    range(0, this.size(names, depth))
      .forEach(() => (block(), this.iterate(names, depth)))
  }
  size(names: any[], depth: number): number {
    let sizes: any[] = []
    Object.entries(this.data).forEach(([name, tree]) => {
      if (names.includes(name)) { sizes.push(tree.size(depth)) }
    })

    // sizes = sizes.compact.uniq
    let _sizes = [...new Set(sizes)]
    if (_sizes.length == 1)
      return _sizes[0]

    throw new Error(
      "Macro could not be expanded: mismatched repetition patterns")
  }
  iterate(names: any[], depth: number) {
    Object.entries(this.data).forEach(([name, tree]) => {
      if (names.includes(name))
        tree.shift(depth)
    })
  }
}
