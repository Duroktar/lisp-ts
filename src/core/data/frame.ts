import { isIdent, isList, isNil, isPair, isVec } from "../../guard";
import { iEnv } from "../../interface/iEnv";
import { isEqual } from "../../utils";
import { Expansion, Syntax } from "../callable/macro";
import { NIL } from "../const";
import { evaluate } from "../eval";
import { toString } from "../print";

class Value {
  public value: any
}

class Expression {
  public replace: any
}

export class Frame {

  constructor(
    public expression: Expression,
    public scope: iEnv
  ) {
    this.reset(expression);
  }

  public complete = false;

  public process(): any {
    if (isList(this.expression)) {
      let func = this.values.car;

      if (func instanceof Syntax || isNil(this.current)) {
        this.complete = true;

        if (!(func instanceof Function)) {
          throw new SyntaxError(`Invalid expression: ${toString(this.expression)}`);
        }

        let result = func.call(this.scope, this.values.cdr);

        // If the result of the call is a macro expansion, inline it and
        // set its expression as the new expression for the frame
        if (result instanceof Expansion) {
          return this.reset(result.expression, true);
        }

        return result;
      }

      // Otherwise, we still have work to do on the subexpressions, so we
      // evaluate the next one in the list.
      let stack = this.scope.runtime.stack;
      stack.push(new Frame(this.current.car, this.scope));
    }
    else if (isVec(this.expression)) {
      this.complete = true;
      return this.expression.dup();
    }
    else if (isIdent(this.expression)) {
      this.complete = true;
      return this.scope.getFrom(this.expression);
    }
    else {
      this.complete = true;
      return evaluate(<any>this.expression, this.scope);
    }
  }
  public clone() {
    let copy = new Frame(this.expression, this.scope);
    let values = this.values.clone();
    copy.values = values;
    return copy;
  }
  public fill(subexpr: any, value: any) {
    if (value instanceof Value && this.values && !(this.values.car instanceof Syntax))
      value = value.value;

    let [epair, vpair] = [this.expression, this.values];

    while (isPair(epair)) {

      if (isEqual(epair.car, subexpr)) {
        vpair.car = value;
        this.current = epair.cdr;
      }

      epair = epair.cdr as any;
      vpair = vpair.cdr;
    }
  }
  public replaces(expression: any) {
    this.target = expression;
  }
  set target(value: any) {
    this._target = value;
  }
  get target() {
    return this._target || this.expression;
  }
  private _target: any;
  private current: any;
  private values: any;
  private reset(expression: any, replace = false) {
    if (replace)
      this.expression.replace(expression);
    this.expression = expression;
    this.current = expression;
    this.values = isPair(expression) ? clone(expression) : NIL;
    this.complete = false;
  }
}

function clone(expression: any) {
  return expression;
}
