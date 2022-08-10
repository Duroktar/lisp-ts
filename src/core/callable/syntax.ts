import { isIdent, isNil, isPair } from "../../guard"
import { iEnv } from "../../interface/iEnv"
import { NIL } from "../const"
import { Form, List } from "../form"
import { Token } from "../read"
import { CallableFunc } from "./proc"

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

  call(exprs: Form, lexicalScope: iEnv) {
    return this.expr(exprs, lexicalScope)
  }

  toString() {
    return `#<syntax:#${this.name}>`
  }

  static RESERVED = ['_', '...']
  static DEBUG: boolean = false
  public token?: Token

  static patternVars(
    pattern: Form,
    excluded: List = NIL,
    results: string[] = [],
  ): string[] {

    if (isNil(pattern)) { return results }

    if (isIdent(pattern)) {
      const { name } = pattern

      if ((isPair(excluded) && excluded.includes(pattern)) || Syntax.RESERVED.includes(name)) {
        // return
      }
      else if (!results.includes(name))
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
