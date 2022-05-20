import assert from "assert";
import * as Utils from "../utils";
import { Env } from "./env";
import { macroTable } from "./macro";
import { Sym, SymTable } from "./sym";
import { SyntaxRulesDef } from "./syntax";
import { Expr, List } from "./terms";

export const expand = (expr: Expr, topLevel = false, env: Env): Expr => {
  const e = expr as Expr[];
  if (!Utils.isList(e)) { return e; }
  if (Utils.isEmpty(e)) { return e; }
  else if (SymTable.QUOTE === e[0]) {
    Utils.expect(e, e.length === 2);
    return e;
  }
  else if (SymTable.COND === e[0]) {
    const [_def, ...exprs] = e;
    if (exprs.length > 1) {
      const preds = exprs.map(pair => {
        const [head, ...tail] = pair as any[];
        const res = tail.map(x => expand(x, false, env));
        // console.log([head, res])
        return [head, res];
      });
      Utils.expect(preds, Utils.isList(preds) && preds.every(x => x.length === 2 && x.every(e => Utils.isNone(e) === false)), `found invalid cond entry where (length != 2): (${(Utils.isList(preds) ? preds.find(x => x.length !== 2) : preds)})`);
      return [_def, preds];
    }
    return [_def, ...exprs];
  }
  else if (SymTable.BEGIN === e[0]) {
    const [_begin, ...exprs] = e;
    Utils.expect(e, exprs.length >= 1, 'expand begin');
    const rrv = exprs.map(x => expand(x, topLevel, env));
    return [_begin, ...rrv];
  }
  else if (SymTable.DEFINESYNTAX === e[0]) {
    Utils.expect(e, e.length >= 3, 'expand define-syntax');
    return expandDefineSyntax(e, topLevel, env);
  }
  else if (SymTable.DEFINE === e[0]) {
    const [_def, name, args, ...body] = e;
    if (Utils.isEmpty(body)) {
      return [_def, name, expand(args, false, env)];
    }
    return expand([SymTable.DEFUN, name, args, ...body], false, env);
  }
  else if (SymTable.DEFUN === e[0]) {
    Utils.expect(e, e.length >= 3, 'expand defun');
    return expandDefun(e, env);
  }
  else if (SymTable.LAMBDA === e[0]) {
    Utils.expect(e, e.length >= 3, 'expand lambda');
    return expandLambda(e, env);
  }
  else if (SymTable.QUASIQUOTE === e[0]) {
    Utils.expect(e, e.length === 2, 'expand quasi-quote');
    return expandQuasiquote(e[1]);
  }
  else if (Utils.toString(e[0]) in macroTable) {
    const name = Utils.toString(e[0]);
    const proc: any = macroTable[name];
    if (Utils.isCallable(proc)) {
      const result = proc.call(e, env);
      const expanded = expand(result, topLevel, proc.env);
      // console.log(`post-expanded syntax:`.yellow.bold,
      //   Utils.toStringSafe(expanded, undefined, 'lambda'))
      return expanded;
    }
    return expand(proc(...e), topLevel, env);
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

function expandDefineSyntax(e: List, topLevel: boolean, env: Env) {

  /*
    (define-syntax <keyword> <transformer spec>)

    <Keyword> is an identifier, and the <transformer spec> should be
    an instance of `syntax-rules`. The top-level syntactic environment
    is extended by binding the <keyword> to the specified transformer.

    NOTE: see - https://groups.google.com/g/comp.lang.scheme/c/hsM4g33joCA
    NOTE: see - https://scholarsarchive.byu.edu/cgi/viewcontent.cgi?referer=&httpsredir=1&article=4508&context=etd

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
  let [_def, name, ...rest] = e;
  // console.log(`defining syntax: ${Utils.toString(name)}`);
  Utils.expect(e, topLevel, 'define-syntax only allowed at top level');
  Utils.expect(rest, Utils.isList(rest[0]) && rest[0][0] === Sym('syntax-rules'));
  const [_syn, literals, ...syntaxRules] = rest[0] as any[];
  Utils.expect(e, Utils.isList(literals) && literals.every(Utils.isIdent));
  Utils.expect(e, syntaxRules.every(rule => Utils.isList(rule) && rule.length === 2));
  syntaxRules.forEach(([pattern, template]) => {
    const [id, ...listPattern] = pattern as any[];
    Utils.expect(e, id === name, 'syntax-rule patterns must begin with the keyword for the macro');
    Utils.expect(e, Utils.isList(listPattern) && listPattern.every(p => Utils.isList(p) || Utils.isIdent(p) || Utils.isConst(p)), 'malformed list pattern');
    Utils.expect(e, Utils.isIdent(template) || Utils.isConst(template) || Utils.isList(template), 'malformed template');
  });

  const callee = new SyntaxRulesDef(name, env, syntaxRules, literals);
  macroTable[callee.name] = callee as any;
  // console.log(macroTable);
  return [];
}

function expandLambda(e: List, env: Env) {
  const [_lambda, params, ...expression] = e;
  const allAtoms = Utils.isList(params) && params.every(Utils.isSym);
  assert(allAtoms || Utils.isSym(params), `Invalid lambda args. Expected a list of atoms or a single atom but instead got: ${Utils.toString(params)}, ${Utils.toString(e)}`);
  assert(expression.length >= 1, `lambda expression empty`);
  const body = (expression.length > 1) && [SymTable.BEGIN, ...expression]
  return [_lambda, params, expand(body || expression[0], false, env)];
}

function expandDefun(e: List, env: Env): Expr {
  let [_def, name, args, body] = e;
    if (Utils.isList(args) && args[0] === SymTable.LAMBDA) {
      const [_def, args_, body_] = args as any
      args = args_
      body = body_
      // return expand([_def, name, args[1], args[2]])
    }
    Utils.expect(e, Utils.isSym(name), `Can only define a symbol`);
    Utils.expect(e, Utils.isList(args) || Utils.isSym(args), `Invalid args`);
    const expr = expand([SymTable.LAMBDA, args, body], false, env);
    return [_def, name, expr]
}

