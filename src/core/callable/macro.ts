import { isChar, isCons, isIdent, isList, isNil, isNum, isPair, isString, isSym, isVec } from "../../guard";
import type { iEnv } from "../../interface/iEnv";
import { LogConfig } from "../../logging";
import { assert } from "../../utils";
import { ellipsis } from "../const";
import { Pair } from "../data/pair";
import type { Form, List } from "../form";
import { toString } from "../print";
import { NativeFunc } from "./func";
import { Expansion } from "./macro/expansion";
import { Matches } from "./macro/matches";
import { Syntax } from "./syntax";

const DEBUG = LogConfig.macro;

function debugLog(...args: any[]): void {
  if (DEBUG) { console.log('[Macro]:'.cyan, ...args); }
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

  call(cells: List, callingScope: iEnv) {
    const [rule, matches] = this.ruleFor(cells, callingScope);
    debugLog('creating Expansion..')
    return new Expansion(this.env, callingScope, (<any>rule.cdr).car, matches);
  }

  private ruleFor(cells: List, scope: iEnv): [Pair, Matches] {
    for (let rule of this.expr.each()) {
      const matches = this.ruleMatches(scope, rule.car.cdr, cells)
      if (matches instanceof Matches) {
        debugLog('found match..'.green, '\n\trule:'.dim, toString(rule.car.cdr), '\n\tform:'.dim, toString(cells))
        debugLog('params:', toString(this.formals))
        return [rule, matches]
      }
      debugLog('no match.. trying next rule'.red)
    }
    debugLog('no match.. form:', toString(cells))

    assert(false, `No match found for: ${toString(cells)}`, cells)
  }

  private ruleMatches(
    scope: iEnv,
    pattern: Form,
    input: Form,
    matches?: Matches,
    depth = 0,
  ): boolean | Matches {

    matches = matches ?? new Matches(pattern, this.formals)

    debugLog(`comparing`, `\n\tpattern:`.dim, toString(pattern), `\n\tinput:\t`.dim, toString(input))

    if (isCons(pattern)) {

      // If pattern is NULL, the input must also be NULL
      if (isNil(pattern))
        return (isNil(input)) ? matches : false

      // Fail if the pattern is a list and the input is not
      if (!isList(input)) { return false }

      // Iterate over the pattern, consuming input as we go
      let pattern_pair = pattern as any
      let input_pair = input as any

      let skip = () => { pattern_pair = pattern_pair.cdr }

      while (isPair(pattern_pair)) {
        let token = pattern_pair.car

        // Skip the current pattern token if it's an ellipsis
        if (ellipsis.equal(token)) {
          skip()
          continue
        }

        // Increment the repetition depth if the next pattern token is an
        // ellipsis, and inform the +Matches+ object that the pattern vars
        // in the current pattern have hit a repetition boundary. Note we
        // do not increment +depth+ itself since this would persist for the
        // remaining tokens in the pattern after we get past the ellipsis.
        let followed_by_ellipsis = (ellipsis.equal((<any>pattern_pair)?.cdr?.car) ?? false)
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

    else if (isVec(pattern)) {

      // Fail if the pattern is a vector and the input is not
      if (!isVec(input))
        return false

      // Iterate over the pattern and input, consuming input cells as we
      // go. This is very similar to how we handle lists, we should
      // probably refactor this.
      let input_index = 0
      pattern.data.forEach((token, pattern_index) => {
        if (ellipsis.equal(token))
          return

        let followed_by_ellipsis = ellipsis.equal(pattern.data[pattern_index + 1])
        let dx = followed_by_ellipsis ? 1 : 0

        if (followed_by_ellipsis) {
          matches!.descend(Syntax.patternVars(token, this.formals), depth + dx)
        }

        const consume = () => {
          const data = input.data[input_index];
          return (data !== undefined) &&
            this.ruleMatches(scope, token, data, matches, depth + dx)
        }

        let consumed = consume()
        if (!(consumed || followed_by_ellipsis))
          return false
        if (consumed)
          input_index += 1
        while (followed_by_ellipsis && consume())
          input_index += 1
      })

      if (input_index !== input.size)
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
        if (pattern.equal(input)) {
          debugLog('pattern === input', {pattern: toString(pattern), input: toString(input)})
          const ibP = this.env.innermostBinding(pattern);
          const ibS = scope.innermostBinding(input);
          const rv = ibP === ibS;
          debugLog('ibP === ibS ?', rv)
          return rv
        }
        return false
      } else {
        matches.put(pattern, input)
      }
    }

    // If all above type checks on the pattern fail, assume the pattern is
    // literal data and make sure the input matches.
    else {
      const rv = this.eqLiteral(pattern, input) ? matches : false;
      debugLog('Comparing literal data.. Match?', !!rv)
      return rv
    }

    return matches
  }

  eqLiteral(input: Form, pattern: Form) {
    if (isString(input))
      return input.equal(pattern)
    if (isSym(input))
      return input.equal(pattern)
    if (isNum(input))
      return input.equal(pattern)
    if (isChar(input))
      return input.equal(pattern)
    debugLog(`[eqLiteral] input: '${toString(input)}' does not match pattern '${toString(pattern)}'`)
    return false
  }

  toString() {
    return `(syntax-rules ${toString(this.formals)} ${toString(this.expr.car)})`
  }
}
