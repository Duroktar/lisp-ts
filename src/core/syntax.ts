/* References: An Optimized R5RS Macro Expander - Jay McCarthy */
import assert from "assert";
import { eqC, expect, first, isAtom, isChar, isEmpty, isIdent, isList, isNum, isString, isSym, isVec, zip, zipUp } from "../utils";
import { toString, toStringSafe } from "./toString";
import { InputError, MatchError } from "./error";
import { Sym } from "./sym";
import type { Atom, List, Form } from "./forms";
import { Env } from "./env";

export class SyntaxRulesDef {
  public name: string
  public params: List = []

  /*
    - A syntax-rules definition specifies a program or function that accepts a
      list of syntax and an environment as input arguments and outputs either a
      match error or else a piece of syntax and its associated environment. In
      turn, each pattern and template defines a sub-program or sub-function. A
      pattern function also accepts syntax and an environment as input. It
      outputs either a match error or a pattern environment. A template in turn
      accepts an environment as input and outputs a new syntax/environment pair.

      A <transformer spec> has the following form:

      :  (syntax-rules <literals> <syntax rule> ...)

      Syntax: <Literals> is a list of identifiers and each <syntax rule> should be of the form

      :  (<pattern> <template>)

      Identifiers that appear in <literals> are interpreted as literal identifiers
      to be matched against corresponding subforms of the input. A subform in the
      input matches a literal identifier if and only if it is an identifier and either
      both its occurrence in the macro expression and its occurrence in the macro
      definition have the same lexical binding, or the two identifiers are equal and
      both have no lexical binding.

  */
  constructor(
    public symbol: Form,
    public env: Env,
    public syntaxRules: any[],
    public literals: Atom[],
  ) {
    this.name = toString(symbol)
  }

  static gen = 1

  static debug = false

  public call(form: Form, env: Env): Form {
    const gen = SyntaxRulesDef.gen++

    this.print(`Expanding Syntax (${gen}):`.blue.bold, `${toStringSafe([this.symbol].concat(form))}`);

    // 1. Match the input syntax against each pattern sequentially until one of
    //    the patterns matches the input syntax. If none match, give up and
    //    output an error.
    const [[_pattern, template], patternEnv] = this.getMatch(form, gen);

    const identifiers = this.parseIdentifiers(template, patternEnv, env);

    this.print(`Matched Pattern (${gen}):`.green, toString(_pattern))
    this.print(`Matched Template (${gen}):`.green, toString(template))

    // 2. Create fresh bindings for each template identifier in the corresponding
    //    template that does not refer to a pattern identifier.
    const outputTemplate = this.bindTemplateIds(template, identifiers, patternEnv, gen)

    this.print(`Rewritten Template (${gen}):`.cyan, toString(outputTemplate))

    // this.print(`Template Identifiers:`.cyan, identifiers.templateIds)
    // this.print(`Pattern Identifiers:`.cyan, identifiers.patternIds)
    // this.print(`Literal Identifiers:`.green, identifiers.literalIds)

    // 3. Create a new environment that merges the pattern environment returned
    //   from the pattern match with the regular identifier environment created
    //   in step 2.
    const entries = zip(...identifiers.aliases.entries())
    const symbols = entries.map((e: any) => e.map(Sym))
    const mergedEnv = new Env(...<[List, List]>symbols, patternEnv)

    // 4. Use the template and environment from step 3 to generate the output
    //    syntax.
    const bound = this.generateOutput(outputTemplate, identifiers, mergedEnv)
    this.print(`Generated Output (${gen}):`.yellow.bold, toStringSafe(bound))

    // 5. Create an output environment that extends the input syntax environment
    //    with the new fresh identifier bindings created in step 2.
    // const zipped = zip(...patternEnv.entries())
    // const mergee = zipped.map((e: any, i) => i === 0 ? e.map(Sym) : e)
    // const outputEnv = new Env(...<[List, List]>mergee, env)
    // 6. Return the output syntax from step 4 with the environment from step 5.

    // const expanded = expand(bound, false, outputEnv);
    // const expanded = expand(outputTemplate, false, outputEnv);

    // this.print(`Fully Expanded Syntax (${gen}):`.red, toStringSafe(expanded))

    // return expanded
    // this
    return bound
  }
  // simple wrapper around match
  getMatch(form: Form, _gen: number): [List[], Env] {
    for (let rule of this.syntaxRules) {
      const [[_id, ...pattern], _template] = rule as [matcher: List, template: Form];
      try {
        // console.log(`trying pattern (${gen}): ${toStringSafe(rule[0])}`)
        return [rule, this.match(form, pattern, new Env())];
      } catch (err) {
        if (err instanceof InputError) {
          continue
        }
        if (err instanceof Error) {
          const e = new MatchError(this, form)
          e.cause = err
          throw e
        }
        throw err
      }
    }
    throw new MatchError(this, form)
  }
  match(form: Form, pattern: Form, env: Env): Env {
    /*
      - A subpattern followed by ... can match zero or more elements of the
        input.
      - Within a pattern the identifier ... must follow the last element of a
        nonempty sequence of subpatterns.
      - It is an error for ... to appear in <literals>.

      More formally, an input form F matches a pattern P if and only if:

      #1 - P is a non-literal identifier; or

      #2 - P is a literal identifier and F is an identifier with the same
           binding; or

      #3 - P is a list (P1 ... Pn) and F is a list of n forms that match P1
           through Pn, respectively; or

      #4 - P is an improper list (P1 P2 ... Pn . Pn+1) and F is a list or
           improper list of n or more forms that match P1 through Pn,
           respectively, and whose nth ``cdr'' matches Pn+1; or

      #5 - P is of the form (P1 ... Pn Pn+1 <ellipsis>) where <ellipsis> is the
           identifier ... and F is a proper list of at least n forms, the first
           n of which match P1 through Pn, respectively, and each remaining
           element of F matches Pn+1; or

      #6 - P is a vector of the form #(P1 ... Pn) and F is a vector of n forms
           that match P1 through Pn; or

      #7 - P is of the form #(P1 ... Pn Pn+1 <ellipsis>) where <ellipsis> is the
           identifier ... and F is a vector of n or more forms the first n of
           which match P1 through Pn, respectively, and each remaining element
           of F matches Pn+1; or

      #8 - P is a datum and F is equal to P in the sense of the equal?
           procedure.
    */
    // #1 - P is a non-literal identifier
    if (isIdent(pattern) &&
      !this.literals.includes(pattern)
    ) {
      env.mergeFrom(pattern, form);
      return env;
    }

    // #2 - P is a literal identifier and F is an identifier with the same
    //      binding
    if (isIdent(pattern) &&
        this.literals.includes(pattern) &&
        this.literals.find(eqC(pattern)) === form
    ) {
      return env
    }

    // #3 + #4 + #5
    if (isList(pattern) && isList(form)) {
      // #4 (improper list)
      if (
        pattern[pattern.length - 2] === Sym('.') &&
        pattern[pattern.length - 1] !== undefined &&
        form.length >= pattern.length - 1
      ) {
        const start = pattern.slice(0, -2);
        this.match(form.slice(0, start.length), start, env);
        this.match(form.slice(start.length), pattern[pattern.length - 1], env);
        return env;
      }

      // #5 (ellipsis list)
      if (pattern[pattern.length - 1] === Sym('...')) {
        // an ellipsis list containing n sub-patterns can match an input syntax list if it is a minimum of length n − 1.
        expect(pattern, form.length >= pattern.length-2, `(Length: ${form.length}, pLength: ${pattern.length - 2})`);
        const pHead = pattern.slice(0, -2);
        const fHead = form.slice(0, pHead.length);
        this.match(fHead, pHead, env);
        const pTail = pattern[pattern.length - 2];
        const fTail = form.slice(pHead.length);
        if (fTail.length === 0) {
          if (isAtom(pTail)) {
            this.match([], pTail, env);
          }
          else if (isChar(pTail)) {
            this.match([], pTail, env)
          }
          else if (isVec(pTail)) {
            pTail.data.forEach(p => this.match([], p, env))
          }
          else {
            pTail.forEach(p => this.match([], p, env))
          }
        }
        else fTail.forEach(f => this.match(f, pTail, env));
        return env
      }

      // #3 (proper list)
      if (pattern.length !== form.length)
        throw new InputError(this, form)

      pattern.forEach((p, idx) => this.match(form[idx], p, env));

      return env
    }

    // #8
    if (isString(pattern) || isNum(pattern) || isSym(pattern)) {
      return env;
    }

    throw new InputError(this, form)
  }
  parseIdentifiers(template: Form, patternEnv: Env, env: Env): IdentifierTypes {
    let templateIds = new Set<string>(),
        patternIds  = new Set<string>(),
        literalIds  = new Set<string>(this.literals.map(l => toString(l)));

    patternEnv.keys().forEach(id =>
      isBound(id, env) ? templateIds.add(id) :
      /* otherwise */    patternIds.add(id))

    const visit = (t: Form): Form => {
      if (!isList(t)) {
        if (isSym(t)) {
          const id = toString(t);
          if (id !== '...') {
            templateIds.add(id)
          }
        }
        return t
      }
      else {
        return t.map(visit)
      }
    }

    visit(template)

    return new IdentifierTypes(templateIds, patternIds, literalIds);
  }
  bindTemplateIds(template: Form, ids: IdentifierTypes, env: Env, gen: number): Form {
    const _rewriteTemplate = (template: Form, ids: IdentifierTypes, env: Env, gen: number): Form => {
      if (isList(template)) {
        const items = []

        for (let idx = 0; idx < template.length; idx++) {
          const item = template[idx]
          const isSpread = template[idx+1] === Sym('...')
          const isListSpread = isSpread && isList(item)

          if (isSpread) {
            if (isListSpread) {
              const mapped = (<List>item).map(i => _rewriteTemplate(i, ids, env, gen));
              if (!mapped.some((m, idx) => ids.isPatId(item[idx]) && isEmpty(first(m)))) {
                const zipped = zipUp(...mapped)
                items.push(...zipped)
              }
            } else {
              const mapped = _rewriteTemplate(item, ids, env, gen);
              if (!isEmpty(first(mapped))) { items.push(...<List>mapped) }
            }
            idx++
          }
          else if (isString(item) || isNum(item))
            items.push(item)

          else if (ids.isPatId(item))
            items.push(...env.getFrom<List>(item))

          else if (ids.isLitId(item))
            items.push(env.getFrom(item))

          else {
            const parsed = _rewriteTemplate(item, ids, env, gen)

            if (!isList(parsed)) {
              const name = toString(item)
              const denotation = new Den(name, gen);
              env.mergeFrom(denotation.toAtom(), item)
              ids.setAlias(denotation.toString(), name)
            }

            items.push(parsed)
          }
        }

        return items
      }

      if (isString(template) || isNum(template))
        return env.getFrom(template)

      if (ids.isLitId(template))
        return env.getFrom(template)

      // 2. Create fresh bindings for each template identifier in the
      //    corresponding template that does not refer to a pattern identifier.

      if (ids.isPatId(template))
        return env.getFrom(template)

      assert(ids.isTplId(template), `Template identifier expected. Got: ${toStringSafe(template)}`)
      const name = toString(template);

      const denotation = new Den(name, gen);

      env.mergeFrom(denotation.toAtom(), template);
      ids.setAlias(denotation.toString(), name)

      return denotation.toAtom()
    }

    if (isList(template))
      return _rewriteTemplate(template, ids, env, gen)

    const rewritten = _rewriteTemplate(template, ids, env, gen)
    const result = first([].concat(<any>rewritten))
    assert(result, `Error rewritting template: ${toStringSafe(template)}`)
    return result
  }
  generateOutput(template: Form, ids: IdentifierTypes, env: Env): Form {
    if (isList(template)) {
      return template.map(t => this.generateOutput(t, ids, env))
    }
    else {
      if (Den.isDenotation(template)) {
        const value = env.getFrom(template)
        assert(value, `missing alias: "${toStringSafe(template)}"`)
        if (isSym(value) && isBound(value, env))
          return template
        return value as Form
      }
      return template
    }
  }

  private print(...args: any[]) {
    if (SyntaxRulesDef.debug)
      console.log(...args)
  }
}

class Den /* Denotation */ {
  constructor(
    public val: string,
    public gen: number,
  ) {}
  toString = () => {
    return `${this.val}.${this.gen}`
  }
  toAtom = () => {
    return Sym(this.toString())
  }
  static isDenotation(val: Form): boolean {
    if (!isSym(val))
      return false

    const repr = toString(val);

    if (!repr.includes('.'))
      return false

    const [_name, gen, ...rest] = repr.split('.')

    return isEmpty(rest) && typeof parseInt(gen, 10) === 'number'
  }
}

class IdentifierTypes {
  public aliases = new Map<string, string>()
  constructor(
    public templateIds: Set<string>,
    public patternIds:  Set<string>,
    public literalIds:  Set<string>,
  ) {}
  isLitId = (identifier: Form) => this.literalIds.has(toString(identifier))
  isPatId = (identifier: Form) => this.patternIds.has(toString(identifier))
  isTplId = (identifier: Form) => this.templateIds.has(toString(identifier))

  setAlias = (val: string, alt: string) => {
    this.aliases.set(val, alt)
  }
}

const isBound = (identifier: Form, env: Env) => {
  const rv = env.hasFrom(identifier)
  if (rv)
    console.log('bound:', toStringSafe(identifier))
  return rv
}

// function printDiff(a: string, b: string) {
//   const diff = diffChars(a, b);

//   console.log()

//   diff.forEach((part) => {
//     // green for additions, red for deletions
//     // grey for common parts
//     const color = part.added ? 'green' :
//       part.removed ? 'red' : 'grey';
//     process.stderr.write(part.value[color]);
//   });

//   console.log()
// }
