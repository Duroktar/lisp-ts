import { isNullOrUndefined, isPair, isSym } from "../guard"
import type { iEnv } from "../interface/iEnv"
import { assert } from "../utils"
import { list, Pair } from "./data/pair"
import { MutableString } from "./data/string"
import { Env } from "./env"
import { UndefinedVariableError } from "./error"
import { evaluate } from "./eval"
import type { Form } from "./form"
import { toString } from "./print"
import { Token } from "./read"

export class Binding {
  constructor(
    public expression: Form,
    public scope: iEnv,
    public memoized = true,
  ) {}

  private _value!: any

  public parent?: Pair;
  public token?: Token;

  get name(): string {
    assert(isSym(this.expression), 'Binding expression must be a Symbol', this)
    return this.expression.name
  }

  replace(expression: Form) {
    if (this.parent) {
      if (isPair(this.parent)) {
        this.parent.car = expression
        this.parent.hosts(expression)
      }
    }
  }

  force() {
    if (!isNullOrUndefined(this._value) && this.memoized)
      return this._value
    this._value = evaluate(this.expression, this.scope)
    return this._value
  }

  // This method is provided as a convenience so that a +Binding+ may be
  // treated like any other expression during evaluation. All it does is
  // return the result of calling <tt>force!</tt>.
  eval(scope: iEnv): Form {
    return this.force()
  }

  // We provide an equality method so that a bound +Identifier+ produced by
  // expanding a macro can be matched against literal identifiers in another
  // macro pattern.
  equal(identifier: string | symbol | MutableString) {
    // WARNING: this might f*ck up..
    return this.expression.toString() === (typeof identifier === 'symbol' ? identifier.description! : identifier.toString())
  }

  innermost_binding(identifier: string) {
    return this.scope
  }
}

export class ForwardDeclaration extends Binding {
  constructor(
    expression: Form,
  ) {
    super(expression, new Env(list(), list()), false)
  }
  eval(scope: iEnv): Form {
      throw new UndefinedVariableError(toString(this.expression))
  }
}
