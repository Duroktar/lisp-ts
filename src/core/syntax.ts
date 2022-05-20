/* References: An Optimized R5RS Macro Expander - Jay McCarthy */
import assert from "assert";
import { isIdent, isList, isAtom, isString, isNum, isSym } from "../utils";
import { expect, eqC, toString, toStringSafe } from "../utils";
import { Env } from "./env";
import { InputError, MatchError } from "./error";
import { BaseProcedure } from "./proc";
import { Sym } from "./sym";
import { Atom, Expr, List } from "./terms";

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
  static from(str: string): Den {
    const [val, gen] = str.split('.')
    return new Den(val, Number(gen))
  }
}

class IdentifierTypes {
  constructor(
    public templateIds: Set<string>,
    public patternIds:  Set<string>,
    public literalIds:  Set<string>,
  ) {}
  isLitId = (identifier: Expr) => this.literalIds.has(toString(identifier))
  isPatId = (identifier: Expr) => this.patternIds.has(toString(identifier))
  isTplId = (identifier: Expr) => this.templateIds.has(toString(identifier))
}

export class SyntaxRulesDef extends BaseProcedure {
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
    public symbol: Expr,
    public env: Env,
    public syntaxRules: any[],
    public literals: Atom[],
  ) {
    super();
    this.name = toString(symbol)
  }

  static gen = 1

  public _call(form: Expr, env: Env): Expr {
    const gen = SyntaxRulesDef.gen++

    console.log(`Expanding Syntax (${gen}):`.blue.bold, `${toStringSafe(form)}`);

    // 1. Match the input syntax against each pattern sequentially until one of
    //    the patterns matches the input syntax. If none match, give up and
    //    output an error.
    const [[_pattern, template], patternEnv] = this.getMatch(form, gen);

    const identifiers = this.parseIdentifiers(patternEnv, env);

    // 2. Create fresh bindings for each template identifier in the corresponding
    //    template that does not refer to a pattern identifier.
    const parsedTemplate = this.parseTemplate(template, identifiers, patternEnv, gen);

    console.log('Matched Pattern:'.green, toString(_pattern));
    console.log('Matched Template:'.green, toString(template));
    console.log('Parsed Template:'.green, toString(parsedTemplate));
    // console.log('Template Identifiers:'.cyan, identifiers.templateIds);
    // console.log('Pattern Identifiers:'.cyan, identifiers.patternIds);
    // console.log('Literal Identifiers:'.green, identifiers.literalIds);

    // 3. Create a new environment that merges the pattern environment returned
    //   from the pattern match with the regular identifier environment created
    //   in step 2.
    const mergedEnv = new Env([], [], patternEnv);

    // 4. Use the template and environment from step 3 to generate the output
    //    syntax.
    const bound = this.genOutput(parsedTemplate, mergedEnv);

    // 5. Create an output environment that extends the input syntax environment
    //    with the new fresh identifier bindings created in step 2.
    // 6. Return the output syntax from step 4 with the environment from step 5.
    this.env = env.merge(mergedEnv);

    console.log(`Expanded Syntax (${gen}):`.blue, toStringSafe(bound))

    return bound
  }
  // simple wrapper around match
  getMatch(form: Expr, gen: number): [List[], Env] {
    for (let rule of this.syntaxRules) {
      const [pattern] = rule;
      try {
        // console.log(`trying pattern (${gen}): ${toStringSafe(rule[0])}`)
        return [rule, this.match(form, pattern)];
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
  match(form: Expr, pattern: Expr, env = new Env()): Env {
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
    // #1
    if (isIdent(pattern)) {
      env.mergeFrom(pattern, form);
      return env;
    }

    // #2
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
  parseIdentifiers(patternEnv: Env, env: Env): IdentifierTypes {
    let templateIds = new Set<string>(),
        patternIds  = new Set<string>(),
        literalIds  = new Set<string>(this.literals.map(l => toString(l)));

    patternEnv.keys().forEach(id =>
      isBound(id, env) ? templateIds.add(id)
       /* otherwise */ : patternIds.add(id))

    return new IdentifierTypes(templateIds, patternIds, literalIds);
  }
  parseTemplate(template: Expr, ids: IdentifierTypes, env: Env, gen: number): List {
    if (isList(template)) {
      const items = []

      for (let idx = 0; idx < template.length; idx++) {
        const item = template[idx]

        const parsed = this.parseTemplate(item, ids, env, gen);
        if (template[idx+1] === Sym('...')) {
          items.push([TemplAstTag.DOTS, parsed, 1])
          idx++
          continue
        }

        items.push(parsed)
      }

      return [TemplAstTag.LIST, items]
    }

    const name = toString(template);

    const { DAT, LIT, PAT, REG } = TemplAstTag

    if (isString(template) ||
        isNum(template))
      return [DAT, template]

    if (ids.isPatId(template))
      return [PAT, template]

    const denotation =
      new Den(name, gen).toAtom();

    env.setFrom(denotation, template);

    if (ids.isLitId(template))
      return [LIT, denotation]

    return [REG, denotation]
  }
  genOutput(template: Expr, env: Env): Expr {
    assert(Array.isArray(template), '`genOutput` expected an arrray')
    switch (template[0]) {
      case TemplAstTag.LIST: {
        const [_def, patterns] = <List[]>template
        let values: Expr[] = []

        for (const ptn of <any[]>patterns) {
          const output = this.genOutput(ptn, env);

          switch (ptn[0]) {
            case TemplAstTag.DOTS:
              if ((output as any)[0].length === 0)
                break
            case TemplAstTag.PAT:
              values = values.concat((<any>output))
              break
            // literal-id
            case TemplAstTag.LIT:
            // regular-id
            case TemplAstTag.REG:
            // datum
            case TemplAstTag.DAT:
            default: {
              values.push(output)
              break
            }
          }
        }
        return values
      }
      case TemplAstTag.DOTS: {
        const [___, pat] = <any>template
        let items = this.genOutput(pat, env);
        if (pat[0] === TemplAstTag.LIST) {
          return items
        }
        return items
      }

      // literal-id
      case TemplAstTag.LIT:
      // datum
      case TemplAstTag.DAT:
        return template[1]
      // regular-id
      case TemplAstTag.REG:
      // pattern-id
      case TemplAstTag.PAT: {
        const id = template[1]
        return env.getFrom<List>(id);
      }

      default: {
        assert(template[0] === undefined, `unexpected key: ${toStringSafe(template)}`)
        throw new Error();
      }
    }
  }
}

const TemplAstTag = {
  REG: Sym('regular-id'),
  PAT: Sym('pattern-id'),
  LIT: Sym('literal-id'),
  DAT: Sym('datum'),
  DOTS: Sym('ellipsis-template'),
  LIST: Sym('tlist'),
}

const isBound = (identifier: Expr, env: Env) => env.hasFrom(identifier)
