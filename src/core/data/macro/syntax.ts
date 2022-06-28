import { isIdentifier, isNil, isPair } from "../../../guard"
import type { iEnv } from "../../../interface/iEnv"
import { NIL } from "../../const"
import type { Form, List } from "../../form"
import type { CallableFunc } from "../proc"

/**
 * Returned by `define-syntax`
 *
 * @export
 * @class Syntax
 */
export class Syntax {
  constructor(
    public name: string,
    public scope: iEnv,
    public expr: CallableFunc,
    public params: List = NIL,
  ) {}

  async call(exprs: Form) {
    return this.expr(exprs, this.scope)
  }

  toString() {
    return `#<syntax:#${this.name}>`
  }

  static RESERVED = ['_', '...']
  static debug: boolean = false

  static patternVars(
    pattern: Form,
    excluded: List = NIL,
    results: string[] = [],
  ): string[] {

    if (isNil(pattern)) {
      return results
    }

    if (isIdentifier(pattern)) {
      const name = pattern.description!

      if ((isPair(excluded) && excluded.includes(name)) || Syntax.RESERVED.includes(name)) {
        // return
      } else if (!results.includes(name))
        results.push(name)
    }

    if (isPair(pattern)) {
      for (let cell of pattern.each()) {
        Syntax.patternVars(cell, excluded, results)
      }
      Syntax.patternVars(pattern.tail.cdr, excluded, results)
    }

    return results
  }
}

export const isSyntax = (obj: any): obj is Syntax => obj instanceof Syntax
