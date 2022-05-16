import * as Utils from "../utils";
import { Env } from "./env";
import { evaluate } from "./eval";
import { macroTable } from "./macro";
import { BaseProcedure, Proc } from "./proc";
import { Sym, SymTable } from "./sym";
import { Expr, List } from "./terms";

export const expand = (expr: Expr, topLevel = false, env: Env = new Env()): Expr => {
  const e = expr as Expr[];
  if (!Utils.isList(e)) { return e; }
  if (Utils.isEmpty(e)) { return e; }
  if (SymTable.QUOTE === e[0]) {
    Utils.expect(e, e.length === 2);
    return e;
  }
  else if (SymTable.COND === e[0]) {
    const [_def, ...exprs] = e;
    const preds = exprs.map(pair => {
      const [head, ...tail] = pair as any[];
      const res = tail.map(x => expand(x, false, env));
      // console.log([head, res])
      return [head, res];
    });
    Utils.expect(preds, Utils.isList(preds) && preds.every(x => x.length === 2 && x.every(e => Utils.isNone(e) === false)), `found invalid cond entry where (length != 2): (${(Utils.isList(preds) ? preds.find(x => x.length !== 2) : preds)})`);
    return [_def, preds];
  }
  else if (SymTable.BEGIN === e[0]) {
    const [_begin, ...exprs] = e;
    if (Utils.isEmpty(exprs))
      return [];
    return [_begin, ...exprs.map(x => expand(x, topLevel, env))];
  }
  else if (SymTable.DEFINE === e[0]) {
    const [_def, name, args, ...body] = e;
    if (Utils.isEmpty(body)) {
      return [_def, name, args];
    }
    return expand([SymTable.DEFUN, name, args, ...body], false, env);
  }
  else if (e[0] === SymTable.DEFINESYNTAX) {
    /*
      (define-syntax <keyword> <transformer spec>)

      <Keyword> is an identifier, and the <transformer spec> should be
      an instance of `syntax-rules`. The top-level syntactic environment
      is extended by binding the <keyword> to the specified transformer.

      NOTE: see - https://groups.google.com/g/comp.lang.scheme/c/hsM4g33joCA
      NOTE: see - https://scholarsarchive.byu.edu/cgi/viewcontent.cgi?referer=&httpsredir=1&article=4508&context=etd
    */
    Utils.expect(e, e.length >= 3);
    let [_def, name, ...rest] = e;

    /*
      A <transformer spec> has the following form:

      :  (syntax-rules <literals> <syntax rule> ...)

      Syntax: <Literals> is a list of identifiers and each <syntax rule> should be of the form

      :  (<pattern> <template>)

      The <pattern> in a <syntax rule> is a list <pattern>that begins with the keyword for the macro.

      A <pattern> is either an identifier, a constant, or one of the following

      :  (<pattern> ...)
      :  (<pattern> <pattern> ... . <pattern>)
      :  (<pattern> ... <pattern> <ellipsis>)
      :  #(<pattern> ...)
      :  #(<pattern> ... <pattern> <ellipsis>)

      and a template is either an identifier, a constant, or one of the following

      :  (<element> ...)
      :  (<element> <element> ... . <template>)
      :  #(<element> ...)

      where an <element> is a <template> optionally followed by an <ellipsis> and
      an <ellipsis> is the identifier ``...'' (which cannot be used as an identifier
      in either a template or a pattern).

    */
    Utils.expect(e, topLevel, 'define-syntax only allowed at top level');
    Utils.expect(rest, Utils.isList(rest[0]) && rest[0][0] === Sym('syntax-rules'))
    const [_syn, literals, ...syntaxRules] = rest[0] as any[]
    Utils.expect(e, Utils.isList(literals) && literals.every(Utils.isIdent))
    Utils.expect(e, syntaxRules.every(rule => Utils.isList(rule) && rule.length === 2))
    syntaxRules.forEach(([pattern, template]) => {
      const [id, ...listPattern] = pattern as any[];
      Utils.expect(e, id===name, 'syntax-rule patterns must begin with the keyword for the macro')
      Utils.expect(e, Utils.isList(listPattern) && listPattern.every(p => Utils.isList(p) || Utils.isIdent(p) || Utils.isConst(p)), 'malformed list pattern')
      Utils.expect(e, Utils.isIdent(template) || Utils.isConst(template) || Utils.isList(template), 'malformed template')
    })

    const callee: Proc = new class extends BaseProcedure {
      name = Utils.toString(name);
      params: List = [];
      env: Env = env;
      _call(form: Expr): Expr {
        // console.log('calling macro:', this.name, 'with args:', Utils.toString(form))
        const result = syntaxRules.find(([pattern, template]) => {
          const [_id, ...patternList] = pattern as any[];
          if (this.match(form, patternList)) {
            // console.log(' - id:', id, 'matches.')
            // console.log(' - pattern:', patternList)
            return true
          }
          return false
        })

        Utils.expect(form, Utils.isList(result), `no matches found for pattern`)
        const [[_id, ...bindings], template] = result as any[]

        const closure = new Env([], [], undefined)
        this.bindFormToClosure(bindings, form, closure)
        const bound = this.bindTemplate(template, closure)
        // console.log(Utils.toString(bound))
        return bound
        // const expanded = expand(bound, false, closure)
        // return expanded
      }
      bindFormToClosure(pattern: Expr, form: Expr, closure: Env): void {
        // #1
        if (Utils.isIdent(pattern) && !closure.hasFrom(pattern)) {
          // TODO: toString ???
          closure.setFrom(pattern, form)
          return
        }

        // #2
        if (Utils.isIdent(pattern) && closure.hasFrom(pattern)) {
          return
        }
        // #3 & #4 & #5
        if (Utils.isList(pattern)/*  && Utils.isList(form) */) {
          // #4
          if (
            pattern[pattern.length-2] === Sym('.') &&
            pattern[pattern.length-1] !== undefined &&
            Utils.isList(form) && form.length >= pattern.length-1
          ) {
            const start = pattern.slice(0, -2)
            this.bindFormToClosure(start, form.slice(0, start.length), closure)
            this.bindFormToClosure(pattern[pattern.length - 1], form.slice(start.length), closure)
            return
          }

          // #5
          if (pattern[pattern.length-1] === Sym('...')) {
            if (pattern.length === 2) {
              this.bindFormToClosure(pattern[0], form, closure)
              return
            }
            if (Utils.isList(form)) {
              const pHead = pattern.slice(0, -2)
              const fHead = form.slice(0, pHead.length)
              this.bindFormToClosure(pHead, fHead, closure)
              const pTail = pattern[pattern.length - 2]
              const fTail = form.slice(pHead.length)
              closure.setFrom(pTail, fTail)
              return
            } else {
              const start = pattern.slice(0, -2)
              this.bindFormToClosure(start, [form], closure)
              return
            }
          }

          // #3
          if (Utils.isList(form) && pattern.length === form.length) {
            pattern.forEach((p, idx) => this.bindFormToClosure(p, form[idx], closure));
            return
          }

          throw new Error('fallthrough in `bindForm` #3 & #4 & #5')
        }

        // #8
        const datum = literals.find((o: any) => pattern === o)
        if (datum) {
          this.bindFormToClosure(datum, pattern, closure)
          return
        }

        throw new Error('fallthrough in `bindForm` body')
      }
      bindTemplate(template: Expr, env: Env): Expr {
        if (Utils.isAtom(template)) {
          const name = Utils.toString(template)
          if (env.has(name)) {
            return env.get(name) as any
          }
          return template
        }
        if (Utils.isString(template)) return template
        if (Utils.isNum(template)) return template
        if (Utils.isList(template)) {
          if (template[template.length-2] === Sym('.')) {
            Utils.expect(e, template[template.length-1] !== undefined, 'No variable after dot.')
            const varName = template[template.length-1];
            const rv = [].concat(
              <any>this.bindTemplate(template.slice(0, -2), env),
              env.get(Utils.toString(varName)) as any
            );
            return rv
          }
          if (template[template.length-1] === Sym('...')) {
            Utils.expect(e, template[template.length-2] !== undefined, 'No variable preceding ellipsis.')
            if (template.length === 2) {
              const varName = Utils.toString(template[0]);
              if (Utils.isList(varName)) {
                debugger
                throw 'errr'
              }
              else if (env.has(varName))
                return env.get(varName) as any
              return []
            }
            const varName = template[template.length-2];
            if (Utils.isList(varName)) {
              debugger
              throw 'errr'
            }
            const rv = [].concat(
              <any>this.bindTemplate(template.slice(0, -2), env),
              env.get(Utils.toString(varName)) as any
            )
            return rv
          }
          const rv = template.map(t => this.bindTemplate(t, env));
          return rv
        }

        throw new Error(`Fallthough in "expandTemplate"`)
      }
      match(form: Expr, pattern: Expr): boolean {
        /*
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

          - A subpattern followed by ... can match zero or more elements of the input.
          - It is an error for ... to appear in <literals>.
          - Within a pattern the identifier ... must follow the last element of a nonempty
            sequence of subpatterns.

          More formally, an input form F matches a pattern P if and only if:

          #1 - P is a non-literal identifier; or
          #2 - P is a literal identifier and F is an identifier with the same binding; or
          #3 - P is a list (P1 ... Pn) and F is a list of n forms that match P1 through Pn, respectively; or
          #4 - P is an improper list (P1 P2 ... Pn . Pn+1) and F is a list or improper list of n or more forms that match P1 through Pn, respectively, and whose nth ``cdr'' matches Pn+1; or
          #5 - P is of the form (P1 ... Pn Pn+1 <ellipsis>) where <ellipsis> is the identifier ... and F is a proper list of at least n forms, the first n of which match P1 through Pn, respectively, and each remaining element of F matches Pn+1; or
          #6 - P is a vector of the form #(P1 ... Pn) and F is a vector of n forms that match P1 through Pn; or
          #7 - P is of the form #(P1 ... Pn Pn+1 <ellipsis>) where <ellipsis> is the identifier ... and F is a vector of n or more forms the first n of which match P1 through Pn, respectively, and each remaining element of F matches Pn+1; or
          #8 - P is a datum and F is equal to P in the sense of the equal? procedure.
        */

        // #1
        if (Utils.isIdent(pattern) && !env.hasFrom(pattern))
          return true

        // #2
        if (Utils.isIdent(pattern) && env.hasFrom(pattern)) {
          const domatch = Utils.isIdent(form) && env.has(form);
          return domatch
        }
        // #3 & #4 & #5
        if (Utils.isList(pattern) && Utils.isList(form)) {
          // #4
          if (
            pattern[pattern.length - 2] === Sym('.') &&
            pattern[pattern.length - 1] !== undefined &&
            form.length >= pattern.length-1
          ) {
            return true
          }

          // #5
          if (pattern[pattern.length - 1] === Sym('...')) {
            Utils.expect(pattern, form.length >= pattern.length-2)
            if (pattern.length === 2)
              return true
            const pHead = pattern.slice(0, -2)
            const fHead = form.slice(0, pHead.length)
            const allmatch = pHead.every((p, idx) => this.match(fHead[idx], p));
            return allmatch
          }

          // #3
          if (pattern.length === form.length) {
            const allmatch = pattern.every((p, idx) => this.match(form[idx], p));
            return allmatch
          }

          return false
        }

        // #8
        const datum = literals.find((o: any) => pattern === o)
        if (datum)
          return Utils.toString(datum) === Utils.toString(pattern)
        // console.log('fallthrough pattern:', pattern, 'form:', form)
        return false
      }
    }

    macroTable[callee.name] = callee as any;
    return [];
  }
  else if (e[0] === SymTable.DEFUN || e[0] === SymTable.DEFINEMACRO) {
    Utils.expect(e, e.length >= 3);
    let [_def, name, args, body] = e;
    Utils.expect(e, Utils.isSym(name), `Can only define a symbol`);
    Utils.expect(e, Utils.isList(args) || Utils.isSym(args), `Invalid args`);
    if (Utils.isList(args) && args[0] === SymTable.LAMBDA) {
      const [_def, args_, body_] = args as any
      args = args_
      body = body_
    }
    const expr: List = expand([SymTable.LAMBDA, args, body], false, env) as any;
    if (_def === SymTable.DEFINEMACRO) {
      Utils.expect(e, topLevel, 'define-macro only allowed at top level');
      const callee: Proc = evaluate(expr, env) as any;
      callee.name = Utils.toString(name);
      Utils.expect(e, Utils.isProc(callee), 'macro must be a procedure');
      macroTable[callee.name] = callee as any;
      return [];
    }
    return [_def, name, expr];
  }
  else if (SymTable.LAMBDA === e[0]) {
    const [_lambda, params, ...expression] = e;
    const allAtoms = Utils.isList(params) && params.every(Utils.isSym);
    Utils.expect(e, (allAtoms || Utils.isSym(params)), `Invalid lambda args. Expected a list of atoms or a single atom but instead got: ${Utils.toString(params)}`);
    Utils.expect(e, expression.length >= 1, `lambda expression empty`);
    const body: any = expression.length === 1 ? expression[0] : [SymTable.BEGIN, ...expression];
    return [_lambda, params, expand(body, false, env)];
  }
  else if (SymTable.QUASIQUOTE === e[0]) {
    Utils.expect(e, e.length === 2);
    return expandQuasiquote(e[1]);
  }
  else if (Utils.toString(e[0]) in macroTable) {
    const name = Utils.toString(e[0]);
    const proc: any = macroTable[name];
    const args = e.slice(1);
    if (Utils.isCallable(proc)) {
      const args1 = args.map(expr => expand(expr, topLevel, env));
      const result = proc.call(args1);
      return expand(result, topLevel, env);
    }
    return expand(proc(...args), topLevel, env);
  }
  return e.map(x => expand(x, false, env));
};

export const expandQuasiquote = (x: Expr): Expr => {
  if (!Utils.isPair(x)) return [SymTable.QUOTE, x];
  Utils.expect(x, x !== SymTable.UNQUOTESPLICING, "can't slice here");
  if (Array.isArray(x)) {
    if (x[0] === SymTable.UNQUOTE) {
      Utils.expect(x, Utils.isList(x) && x.length === 2);
      return x[1];
    }
    if (Utils.isList(x[0]) && x[0][0] === SymTable.UNQUOTESPLICING) {
      Utils.expect(x, Utils.isList(x[0]) && x[0].length === 2);
      return [SymTable.APPEND, x[0][1], expandQuasiquote(x.slice(1))];
    }
    else {
      return [SymTable.CONS, expandQuasiquote(x[0]), expandQuasiquote(x.slice(1))];
    }
  }
  throw new Error('unexpected state (expandQuasiquote)')
};
