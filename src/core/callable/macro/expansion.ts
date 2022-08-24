import { isCons, isIdent, isList, isNil, isPair } from "../../../guard";
import type { iEnv } from "../../../interface/iEnv";
import { LogConfig } from "../../../logging";
import { Binding } from "../../binding";
import { ellipsis } from "../../const";
import { list, Pair } from "../../data/pair";
import { Sym, Symbol } from "../../data/sym";
import type { Form, List } from "../../form";
import { cadr } from "../../lisp";
import { toString, toStringSafe } from "../../print";
import { Token } from "../../read";
import type { Matches } from "./matches";

const DEBUG = LogConfig.expansion;

function debugLog(...args: any[]): void {
  if (DEBUG) { console.log('[Expansion]:'.green, ...args); }
}

export class Expansion {

  public expression: Form;

  public token?: Token

  constructor(
    private lexicalScope: iEnv,
    private callingScope: iEnv,
    template: List,
    matches: Matches
  ) {
    this.hygienic = Expansion.runtime.hygienic
    this.expression = this.expand(template, matches)
  }

  private expand(
    template: Form,
    matches: Matches,
    depth = 0,
    ignoringEllipses = false,
  ): Form {

    debugLog('expanding template:'.dim.blue, toString(template))

    if (isCons(template)) {
      debugLog('template is a list ..'.dim)

      if (isNil(template)) {
        debugLog('template is nil ..'.dim)
        return template
      }

      if (ellipsis.equal(template.car)) {
        debugLog('template car is an ellipsis.. expanding cadr(template)'.dim)
        return this.expand(cadr(template), matches, depth, true)
      }

      let result        : any = null,
          last          : any = list(),
          repeater      : any = null,
          template_pair : any = template;

      const push = (value: Form) => {
        debugLog('push'.dim, toStringSafe(value))
        let pair = new Pair(value)
        pair.hosts(value)
        result = result ?? pair
        if (isPair(last))
          last.cdr = pair
        last = pair
      }

      while (isPair(template_pair)) {

        const cell = template_pair.car

        // Increment the repetition depth if the current subtemplate is
        // followed by an ellipsis and we are not treating ellipses as
        // literals
        let followed_by_ellipsis = (isPair(template_pair.cdr) &&
                                    ellipsis.equal(cadr(template_pair))) &&
                                    !ignoringEllipses

        const dx = followed_by_ellipsis ? 1 : 0

        if (followed_by_ellipsis) { repeater = cell }

        // Once we reach an ellipsis, expand the preceeding form the
        // correct number of times depending on the +matches+
        if (ellipsis.equal(cell) && !ignoringEllipses) {
          debugLog('Expand the preceeding form the correct number of times'.dim)
          let n = 1
          matches.expand(repeater, depth + 1, () => {
            debugLog(`Expanding (${n})`)
            push(this.expand(repeater, matches, depth + 1))
          })
        }

        // If the current subtemplate is not an ellipsis and is not
        // followed by an ellipsis, expand it and push the result onto
        // the output
        else if (!followed_by_ellipsis) {
          debugLog('Not an ellipsis and is not followed by an ellipsis'.dim)
          push(this.expand(cell, matches, depth + dx, ignoringEllipses))
        }

        template_pair = template_pair.cdr
      }

      // Handle the tail of improper list templates
      if (!isNil(last)) {
        debugLog(`Handle the tail of improper list templates (last: ${toStringSafe(last)})`.dim.red)
        last.cdr = this.expand(template_pair, matches, depth, ignoringEllipses)
      }

      debugLog('returning result:'.blue, toStringSafe(result))
      return result ?? list()
    }
    // TODO: Vectors
    else if (isIdent(template)) {
      // If the template is a pattern variable, return the current match
      // for that variable. See +Matches+ to see how repeated patterns
      // are handled.
      if (matches.has(template)) {
        const rv = matches.get(template);
        debugLog('pattern variable:', toString(template), 'returned:', toString(rv))
        return rv
      }

      // Otherwise, if using unhygienic macros, return the template
      // verbatim.
      if (!this.hygienic) {
        debugLog('returned template:', template, 'unhygienic'.dim)
        return template
      }

      // If using hygienic macros: bind the identifier to the macro's
      // lexical scope if it is defined there, otherwise rename it as
      // appropriate to avoid clashes with variables in the calling scope.
      if (this.lexicalScope.hasFrom(template)) {
        const rv = new Binding(template, this.lexicalScope, false)
        debugLog('returning binding', toString(rv))
        return rv
      }

      else {
        const rv = this.rename(template)
        debugLog('returning renamed', toString(rv))
        return rv
      }
    }
    else {
      debugLog('returning datum:', toString(template))
      return template
    }
  }

  private rename(sym: Symbol) {
    const id = sym.name
    if (this.callingScope.has(id)) {
      let i = 1
      while (this.callingScope.has(`#${id}#${i}`)) {
        i += 1
      }
      const raw = `#${id}#${i}`;
      debugLog('returning renamed', raw)
      return Sym(raw)
    }
    debugLog('returning unchanged', id)
    return sym
  }

  private hygienic: boolean;

  static runtime = { hygienic: true }
}

// .> (print (macroexpand '(let ((=> #f)) (cond (#t => 'ok)))))

// actual:
// ((lambda (=>) ((lambda (temp) (if temp ('ok temp) #f)) #t)) #f)

// actual (hygienic):
// ((lambda (=>) (<Binding:let> ((temp #t)) (if temp ('ok temp) (<Binding:cond>)))) #f)

// target:
// ((lambda (=>) ((lambda (temp) (if temp 'ok #f)) #t)) #f)
