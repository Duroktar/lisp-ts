import { isIdent, isList, isNil, isPair, isString } from "../../../guard";
import type { iEnv } from "../../../interface/iEnv";
import { error } from "../../../utils";
import { ellipsis } from "../../const";
import type { Form, List } from "../../form";
import { toString } from "../../print";
import { Pair } from "../../data/pair";
import { NativeFunc } from "../func";
import { Expansion } from "./expansion";
import { Matches } from "./matches";
import { Syntax } from "./syntax";

const debug = true;

function debugLog(...args: any[]): void {
  if (debug) { console.log('[Macro]:'.cyan, ...args); }
}

/**
 * Returned by `syntax-rules`
 *
 * @export
 * @class Macro
 * @extends {NativeFunc}
 */
export class Macro extends NativeFunc {
  constructor(
    public env: iEnv,
    public formals: List,
    public expr: Pair,
    public name = 'macro',
  ) {
    super(env, formals, expr, name)
  }

  call(cells: List, scope: iEnv) {
    const [rule, matches] = this.ruleFor(cells, scope);
    debugLog('creating Expansion..')
    return new Expansion(this.env, scope, (<any>rule.cdr).car, matches);
  }

  private ruleFor(cells: List, scope: iEnv): [Pair, Matches] {
    for (let rule of this.expr.each()) {
      const matches = this.ruleMatches(scope, rule.car.cdr, cells)
      if (matches instanceof Matches) {
        debugLog('found match..', '\n\trule:', toString(rule.car.cdr), '\n\tform:', toString(cells))
        debugLog('params:', toString(this.formals))
        return [rule, matches]
      }
    }
    debugLog('no match.. form:', toString(cells))

    return error('No match.. form: ' + toString(cells))
  }

  private ruleMatches(
    scope: iEnv,
    pattern: Form,
    input: Form,
    matches?: Matches,
    depth = 0,
  ): boolean | Matches {

    matches = matches ?? new Matches(pattern, this.formals)

    if (isList(pattern)) {

      // If pattern is NULL, the input must also be NULL
      if (isNil(pattern))
        return (isNil(input)) ? matches : false

      // Fail if the pattern is a list and the input is not
      if (!isPair(input)) return false

      // Iterate over the pattern, consuming input as we go
      let pattern_pair = pattern as any
      let input_pair = input as any

      let skip = () => { pattern_pair = pattern_pair.cdr }

      while (isPair(pattern_pair)) {
        let token = pattern_pair.car

        // Skip the current pattern token if it's an ellipsis
        if (token === ellipsis) {
          skip()
          continue
        }

        // Increment the repetition depth if the next pattern token is an
        // ellipsis, and inform the +Matches+ object that the pattern vars
        // in the current pattern have hit a repetition boundary. Note we
        // do not increment +depth+ itself since this would persist for the
        // remaining tokens in the pattern after we get past the ellipsis.
        let followed_by_ellipsis = ((<any>pattern_pair)?.cdr?.car == ellipsis ?? false)
        let dx = followed_by_ellipsis ? 1 : 0

        if (followed_by_ellipsis)
          matches.descend(Syntax.patternVars(token, this.formals), depth + dx)

        // Set up a closure to consume input using the current pattern
        // expression. Calls +rule_matches+ with the current scope,
        // pattern, input, and +Matches+ object.
        let consume = () => {
          return isPair(input_pair) &&
            this.ruleMatches(scope, token, input_pair.car, matches, depth + dx)
        }

        // If the next pattern token is not an ellipsis, fail unless the
        // pattern token matches the input token.
        //
        // If the next token is an ellipsis, consume input using the
        // current pattern until the pattern no longer matches the current
        // input.
        //
        let consumed = consume()

        if (!(consumed || followed_by_ellipsis))
          return false

        if (consumed)
          input_pair = input_pair.cdr

        while (followed_by_ellipsis && consume()) {
          input_pair = input_pair.cdr
        }

        skip()
      }

      // We're done iterating over the pattern, so the current pattern
      // token will be NULL or some non-Cons object (if the pattern is an
      // improper list). Fail unless the remaining input matches this
      // object.
      if (!this.ruleMatches(scope, pattern_pair, input_pair, matches, depth))
        return false
    }

    // If the pattern is a formal keyword for the macro (a 'literal
    // identifier' in the terms of the spec), return a boolean indicating
    // whether the input is an identifier with the same binding, that is
    // to say the two identifiers refer to the same location in memory (or
    // both refer to no location). If it's a normal pattern variable, store
    // the current input, whatever it is, in the +matches+.
    else if (isIdent(pattern)) {
      if (isPair(this.formals) && this.formals.includes(pattern)) {
        debugLog('pattern === input', {pattern, input})
        if (pattern === input) {
          // const p = this.env.getOrDefault(toString(pattern));
          // const i = scope.getOrDefault(toString(input));
          // return p === i
          // TODO
          return (this.env.innermostBinding(pattern) === scope.innermostBinding(input))
        }
        return false
      } else {
        matches.put(pattern, input)
      }
    }

    // If all above type checks on the pattern fail, assume the pattern is
    // literal data and make sure the input matches.
    else {
      return this.eqLiteral(pattern, input) ? matches : false
    }

    return matches
  }

  eqLiteral(input: Form, pattern: Form) {
    // console.log('patternEqual:', {pattern, input})
    if (isString(input))
      return input.equal(pattern)
    return pattern === input
  }

  toString() {
    return `(syntax-rules ${toString(this.formals)} ${toString(this.expr.car)})`
  }
}
