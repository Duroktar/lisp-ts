import { isIdentifier, isList, isNil, isPair } from "../../../guard";
import type { iEnv } from "../../../interface/iEnv";
import { ellipsis, NIL } from "../../const";
import { Pair } from "../pair";
import type { Form, List } from "../../form";
import { cadr } from "../../lisp";
import { Binding } from "./binding";
import type { Matches } from "./matches";
import { toString } from "../../print";

export class Expansion {

  public expression: Form;

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
    if (isList(template)) {

      if (isNil(template)) {
        return NIL
      }

      if (template.car === ellipsis) {
        return this.expand(cadr(template), matches, depth, true)
      }

      let result        : any = null,
          last          : any = null,
          repeater      : any = null,
          template_pair : any = template;

      const push = (value: Form) => {
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
                                    cadr(template_pair) === ellipsis) &&
                                    !ignoringEllipses

        const dx = followed_by_ellipsis ? 1 : 0

        if (followed_by_ellipsis) { repeater = cell }

        // Once we reach an ellipsis, expand the preceeding form the
        // correct number of times depending on the +matches+
        if (cell === ellipsis && !ignoringEllipses) {
          matches.expand(repeater, depth + 1, () => {
            push(this.expand(repeater, matches, depth + dx))
          })
        }

        // If the current subtemplate is not an ellipsis and is not
        // followed by an ellipsis, expand it and push the result onto
        // the output
        else if (!followed_by_ellipsis) {
          push(this.expand(cell, matches, depth + dx, ignoringEllipses))
        }

        template_pair = template_pair.cdr
      }

      // Handle the tail of improper list templates
      if (!isNil(last)) {
        last.cdr = this.expand(template_pair, matches, depth, ignoringEllipses)
      }

      return result
    }
    else if (isIdentifier(template)) {
      // If the template is a pattern variable, return the current match
      // for that variable. See +Matches+ to see how repeated patterns
      // are handled.
      if (matches.has(template)) {
        const rv = matches.get(template);
        // console.log('[Expansion]: pattern variable:', template, 'returned:', toString(rv))
        return rv
      }


      // Otherwise, if using unhygienic macros, return the template
      // verbatim.
      if (!this.hygienic)
        return template

      // If using hygienic macros: bind the identifier to the macro's
      // lexical scope if it is defined there, otherwise rename it as
      // appropriate to avoid clashes with variables in the calling scope.
      return this.lexicalScope.has(template.description!)
        ? new Binding(template, this.lexicalScope, false)
        : this.rename(template)

    }
    else {
      // console.log('[Expansion]: else case returned:', template)
      return template
    }
  }
  private rename(sym: symbol) {
    const id = sym.description!
    if (this.callingScope.has(id)) {
      let i = 1
      while (this.callingScope.has(`#${id}#${i}`)) {
        i += 1
      }
      return Symbol.for(`#${id}#${i}`)
    }
    return sym
  }

  private hygienic: boolean;

  static runtime = { hygienic: false }
}
