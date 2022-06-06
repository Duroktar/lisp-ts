
/* References: An Optimized R5RS Macro Expander - Jay McCarthy */

import assert from "assert";
import { eqC, isAtom, isChar, isEmpty, isIdent, isList, isNum, isPair, isString, isSym, isVec, zip, zipUp } from "../utils";
import { Env } from "./env";
import { InputError, MatchError } from "./error";
import type { Form, List } from "./forms";
import { car, cdr } from "./lisp";
import { list, Pair } from "./pair";
import { Sym } from "./sym";
import { toString, toStringSafe } from "./toString";


export class SyntaxRulesDef {
  public name: string
  public params: Form[] = []

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
    public syntaxRules: Pair,
    public literals: List,
  ) {
    this.name = toString(symbol)
  }

  static gen = 1

  static debug = false

  public call(form: Form, env: Env): Form {
    const gen = SyntaxRulesDef.gen++

    this.print(`Expanding Syntax (${gen}):`.blue.bold, `${toStringSafe(list(this.symbol).append(form))}`);
    if (isPair(this.literals)) this.print(`Syntax literals: ${toStringSafe(this.literals)}`)

    // 1. Match the input syntax against each pattern sequentially until one of
    //    the patterns matches the input syntax. If none match, give up and
    //    output an error.
    const [first, patternEnv] = this.getMatch(form, gen);
    const [_pattern, template] = first as any

    const identifiers = this.parseIdentifiers(template, patternEnv, env);

    this.print(`Matched Pattern (${gen}):`.green, toStringSafe(_pattern))
    this.print(`Matched Template (${gen}):`.green, toStringSafe(template))

    // 2. Create fresh bindings for each template identifier in the corresponding
    //    template that does not refer to a pattern identifier.
    const outputTemplate = this.bindTemplateIds(template, identifiers, patternEnv, gen)

    this.print(`Rewritten Template (${gen}):`.cyan, toStringSafe(outputTemplate))

    // this.print(`Template Identifiers:`.cyan, identifiers.templateIds)
    // this.print(`Pattern Identifiers:`.cyan, identifiers.patternIds)
    // this.print(`Literal Identifiers:`.green, identifiers.literalIds)

    // 3. Create a new environment that merges the pattern environment returned
    //   from the pattern match with the regular identifier environment created
    //   in step 2.
    const entries = zip(...identifiers.aliases.entries())
    const symbols = entries.map((e: any) => list(...e.map(Sym)))
    const mergedEnv = new Env(symbols[0], symbols[1], patternEnv)

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
    return bound
  }
  // simple wrapper around match
  getMatch(form: Form, _gen: number): [Form, Env] {
    for (let rule of this.syntaxRules) {
      const patternForm = car(rule)
      const pattern = cdr(patternForm)
      try {
        this.print(`trying pattern (${_gen}): ${toStringSafe(pattern)}`)
        // console.log(`against form (${_gen}): ${toStringSafe(form)}`)
        return [rule, this.match(form, pattern, new Env())];
      } catch (err) {
        if (err instanceof InputError) {
          continue
        }
        if (err instanceof Error) {
          console.error(err)
          const e = new MatchError(this, form)
          e.cause = err
          throw e
        }
        throw err
      }
    }
    throw new MatchError(this, form)
  }
  isLiteral(obj: any): boolean {
    if (isEmpty(this.literals))
      return false
    return this.literals.includes(obj)
  }
  findLiteral(fn: (value: any) => boolean) {
    if (isEmpty(this.literals))
      return null
    return this.literals.find(fn)
  }
  isEqual(value: any, other: any): boolean {
    if (isPair(value))
      return value.equal(other)
    return value === other
  }
  indentSize = 2
  match(form: Form, pattern: Form, env: Env, indent = 0): Env {
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
    let a = toString(pattern)
    let b = toString(form)
    this.print(`${' '.repeat(indent)}trying to match pattern: ${a}`)
    this.print(`${' '.repeat(indent)} - matching against form: ${b}`)

    if (a === 'test')
      debugger

    // #1 - P is a non-literal identifier
    if (isIdent(pattern) && !this.isLiteral(pattern)) {
      env.mergeFrom(pattern, form);
      this.print(`${' '.repeat(indent)} - matches`)
      return env;
    }

    // #2 - P is a literal identifier and F is an identifier with the same
    //      binding
    if (isIdent(pattern) &&
        this.isLiteral(pattern) &&
        this.isEqual(this.findLiteral(eqC(pattern)), form)
    ) {
      this.print(`${' '.repeat(indent)} - matches`)
      return env
    }

    // #3 + #4 + #5
    if (isPair(pattern) && isPair(form)) {
      // #4 (improper list)
      if (!pattern.isList() && (form.length >= pattern.length - 1)) {
        const start = pattern.slice(0, -2);
        this.match(form.slice(0, start.length), start, env, indent + this.indentSize);
        this.match(form.slice(start.length), pattern.at(-1), env, indent + this.indentSize);
        return env;
      }

      // #5 (ellipsis list)
      if (pattern.at(-1) === Sym('...')) {
        // an ellipsis list containing n sub-patterns can match an input syntax list of minimum length n − 1.
        if (form.length < pattern.length-2) {
          this.print(`${' '.repeat(indent)} - NOT A MATCH`)
          throw new InputError(this, form)
        }
        const pHead = pattern.slice(0, -2);
        if (pattern.length > 2) {
          const fHead = form.slice(0, pHead.length);
          this.match(fHead, pHead, env, indent + this.indentSize);
        }
        const pTail = pattern.at(-2);
        const fTail = form.slice(pHead.length);
        if (isList(fTail) && isEmpty(fTail)) {
          if (isAtom(pTail)) {
            // this.match(EMPTY, pTail, env);
            this.print(`${' '.repeat(indent)} - matches`)
            return env
          }
          else if (isChar(pTail)) {
            assert(false, 'should not be a character here')
            // this.match(EMPTY, pTail, env)
          }
          else if (isVec(pTail)) {
            assert(false, 'should not be a vector here')
            // pTail.data.forEach(p => this.match(EMPTY, p, env))
          }
          else if (isPair(pTail)) {
            assert(false, "I'm so confused")
            // pTail.forEach(p => this.match(EMPTY, p, env))
          }
          else {
            assert(false, 'what the everloving fffff')
          }
        }
        else fTail.forEach(f => {
          this.match(f, pTail, env, indent + this.indentSize)
        });
        this.print(`${' '.repeat(indent)} - matches`)
        return env
      }

      // #3 (proper list)
      if (pattern.length !== form.length) {
        this.print(`${' '.repeat(indent)} - NOT A MATCH`)
        throw new InputError(this, form)
      }

      this.match(car(form), car(pattern), env, indent + this.indentSize)
      this.match(cdr(form), cdr(pattern), env, indent + this.indentSize)

      this.print(`${' '.repeat(indent)} - matches`)
      return env
    }

    // #8
    if ((isString(pattern) || isNum(pattern) || isSym(pattern)) && pattern === form) {
      this.print(`${' '.repeat(indent)} - matches`)
      return env;
    }

    this.print(`${' '.repeat(indent)} - NOT A MATCH`)
    throw new InputError(this, form)
  }
  parseIdentifiers(template: Form, patternEnv: Env, env: Env): IdentifierTypes {
    let templateIds = new Set<string>(),
        patternIds  = new Set<string>(),
        literalIds  = new Set<string>(isEmpty(this.literals) ? [] : this.literals.toArray().map(l => toString(l)));

    patternEnv.keys().forEach(id =>
      isBound(id, env) ? templateIds.add(id) :
      /* otherwise */    patternIds.add(id))

    const visit = (t: Form): Form => {
      if (!isPair(t)) {
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
      if (isList(template) && !isEmpty(template)) {
        let items: any[] = []

        for (let idx = 0; idx < template.length; idx++) {
          const item = template.at(idx)
          const isSpread = template.at(idx+1) === Sym('...')
          const isListSpread = isSpread && isPair(item)

          let a = toString(item)
          if (isSpread) {
            if (isListSpread) {
              // list of *lists* of variable lookups spread into the result (0 or more)
              // ex: ((name val) ...) -> res: ((n1 v1) (n2 v2) <etc..>)
              const mapped = (<Pair>item).map(i => _rewriteTemplate(i, ids, env, gen));
              if (!mapped.some((m, idx) => isPair(item) && ids.isPatId(item.at(idx)) && isEmpty(car(m)))) {
                const zipped = zipUp(...mapped.toArray())
                items.push(...zipped)
              }
            } else {
              // list of variable lookups spread into the result (0 or more)
              // ex: (name ...) -> res: (n1 n2 <etc..>)
              const mapped = _rewriteTemplate(item, ids, env, gen);
              if (isPair(mapped)) {
                const arr = mapped.toArray();
                items.push(...arr)
              }
            }
            idx++
          }
          else if (isString(item) || isNum(item))
            items.push(item)

          else if (ids.isPatId(item)) {
            const arr = env.getFrom<any>(item);
            items.push(...arr)
          }

          else if (ids.isLitId(item)) {
            const it = env.getFrom(item);
            items.push(it)
          }

          else {
            const parsed = _rewriteTemplate(item, ids, env, gen)

            if (!isPair(parsed)) {
              const name = toString(item)
              const denotation = new Den(name, gen);
              env.mergeFrom(denotation.toAtom(), item)
              ids.setAlias(denotation.toString(), name)
            }

            items.push(parsed)
          }
        }

        return list(...items)
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
    const result = car(list(rewritten))
    assert(result, `Error rewritting template: ${toStringSafe(template)}`)
    return result
    // throw new Error('How can it be like this')
  }
  generateOutput(template: Form, ids: IdentifierTypes, env: Env): Form {
    if (isPair(template)) {
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

    if ((rest.length !== 0))
      return false

    const n = parseInt(gen, 10);
    return isNum(n)
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

export const isSyntaxRulesDef = (x: unknown): x is SyntaxRulesDef => x instanceof SyntaxRulesDef;

export const isBound = (identifier: Form, env: Env) => {
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
