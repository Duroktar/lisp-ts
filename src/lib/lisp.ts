import "colors"
import { Env } from "./env";
import * as Errors from "./errors";
import { Proc } from "./proc";
import { Atom, Expr, List } from "./terms";
import * as Utils from "../utils";

export namespace Lisp {

  // symbols
  export const Sym = Symbol.for;
  export const SymTable = {
    APPEND: Sym('append'),
    ATOM: Sym('atom'),
    BEGIN: Sym('begin'),
    CAR: Sym('car'),
    CDR: Sym('cdr'),
    COND: Sym('cond'),
    CONS: Sym('cons'),
    DEFINEMACRO: Sym('define-macro'),
    DEFUN: Sym('defun'),
    DO: Sym('do'),
    EQ: Sym('eq'),
    LAMBDA: Sym('lambda'),
    LET: Sym('let'),
    QUASIQUOTE: Sym('quasiquote'),
    QUOTE: Sym('quote'),
    UNQUOTE: Sym('unquote'),
    UNQUOTESPLICING: Sym('unquotesplicing'),
  };
  // END symbols

  // constants
  export const TRUE: Atom = Sym('#t');
  export const EMPTY: Expr = [];
  // END constants

  // primitives (7)
  export const quote = (expr: Expr): Expr => cadr(expr);
  export const atom = (expr: Expr): Expr => Utils.toL(Utils.isAtom(expr));
  export const eq = (x: Expr, y: Expr): Expr => Utils.toL(Utils.isSym(x) && Utils.isSym(y) && x === y || Utils.isEmpty(x) && Utils.isEmpty(y));
  export const cons = (car: Expr, cdr: Expr): Expr => [car, ...<any>Utils.expect(cdr, Utils.isArray, '2nd Argument to cons must be an array...')];
  export const car = (expr: Expr): Expr => Utils.expect(<any>expr, Utils.isArray, 'Argument to car must be an array..')[0];
  export const cdr = (expr: Expr): Expr => Utils.expect(<any>expr, Utils.isArray, 'Argument to cdr must be an array..').slice(1);
  // END primitives

  // export const compose = (...fns: Function[]) => (arg: any) => fns.reduceRight((acc, fn) => fn(acc), arg)
  // export const cadr    = compose(car, cdr)

  // functions
  export const cadr    = (expr: Expr): Expr => car(cdr(expr));
  export const cdar    = (expr: Expr): Expr => cdr(car(expr));
  export const caar    = (expr: Expr): Expr => car(car(expr));
  export const cadar   = (expr: Expr): Expr => car(cdr(car(expr)));
  export const caddr   = (expr: Expr): Expr => car(cdr(cdr(expr)));
  export const cdadr   = (expr: Expr): Expr => cdr(car(cdr(expr)));
  export const caddar  = (expr: Expr): Expr => car(cdr(cdr(car(expr))));
  export const cadddr  = (expr: Expr): Expr => car(cdr(cdr(cdr(expr))));
  export const cadadr  = (expr: Expr): Expr => car(cdr(car(cdr(expr))));
  export const cadddar = (expr: Expr): Expr => car(cdr(cdr(cdr(car(expr)))));

  export const nil = (x: Expr): Expr => not(atom(x)) && eq(x, EMPTY);
  export const not = (x: Expr): Expr => (x === TRUE) ? EMPTY : TRUE;

  export const list = (...exprs: Expr[]): List => exprs;

  export const _do = (args: Expr[], env: Env) => {
    /*
    (do ((<variable1> <init1> <step1>) ...)
         (<test> <expression> ...)
        <command> ...)
    */
    const [[...preludes], [test, ...expressions], ...commands] = <any>args;

    // Syntax: The <init>s, <step>s, <test>s, and <command>s must be expressions.
    //         The <variable>s must be pairwise distinct variables.
    preludes.forEach(([var1, init1, step1]: any) => {
      Utils.expect(var1, Utils.isAtom(var1));
      Utils.expect(var1, Utils.isExpr(init1));
      Utils.expect(var1, Utils.isExpr(step1));
    });
    Utils.expect(test, Utils.isExpr(test), `Test must be an expression. Got: ${typeof test} .. (value: ${Utils.toString(test)})`);
    expressions.forEach((expression: any) => {
      Utils.expect(expression, Utils.isExpr(expression), `Not an expression. Got: ${typeof test} .. (value: ${Utils.toString(test)})`);
    });
    commands.forEach((command: any) => {
      Utils.expect(command, Utils.isExpr(command), `A Command must be an expression. Got: ${typeof command} .. (value: ${Utils.toString(command)})`);
    });

    const bindings: List = [];
    const steps: List = [];

    // The <init> expressions are evaluated (in some unspecified order),
    // the <variable>s are bound to fresh locations,
    // the results of the <init> expressions are stored in the bindings of the <variable>s,
    // and then the iteration phase begins.
    preludes.forEach(([var1, init1, step1]: any) => {
      bindings.push([Utils.toString(var1), Lisp.evaluate(init1, env)]);
      steps.push([var1, step1]);
    });
    bindings.forEach(([varName, binding]: any) => {
      env.set(varName, binding);
    });

    // Each iteration begins by evaluating <test>;
    // If the result is #f, then the <command>s are evaluated in order for effect,
    // - the <step> expressions are evaluated in some unspecified order,
    // - the <variable>s are bound to fresh locations holding the results,
    // Then the next iteration begins.
    const iterate = (depth = 0): Expr => {
      const testResult = Lisp.evaluate(test, env);
      if (Utils.isT(testResult) === false) {
        commands.forEach((command: any) => {
          Lisp.evaluate(command, env);
        });
        steps.forEach((step: any) => {
          const [varName, result] = step;
          const res = evaluate(result, env);
          env.set(Utils.toString(varName), res);
        });
        if (depth >= 1000) {
          throw new Error('max depth exceeded');
        }
        return iterate(depth + 1);
      } else {

        // If <test> evaluates to a true value;
        // - The <expression>s are evaluated from left to right,
        // and the values of the last <expression> are returned
        // - If no <expression>s are present,
        // then the do expression returns unspecified values
        return expressions.reduce((_: any, expression: any) => {
          return evaluate(expression, env);
        }, []);
      }
    };

    return iterate();
  }
  // END functions

  // macros
  export function _async(...args: Expr[]) {
    const x = cons(SymTable.DO, args);
    Utils.expect(x, args.length > 0, '`do` blocks must container an expression');
    const result = args.reduce((acc: any[], expr) => {
      let cont: any[] = [Sym('call/cc'), Utils.mkLambda(['throw'], [Utils.mkLambda(['arg'], expr)])];
      if (acc.length === 0)
        return cont;

      cont[1][2].push(acc);
      return cont;
    }, []);
    // console.log(Utils.toString(result))
    return result;
  }

  export function _let(...args: Expr[]) {
    const x = cons(SymTable.LET, args);
    Utils.expect(x, args.length > 1, 'Must provide arguments to "let" macro');
    const [bindings, body] = args;
    Utils.expect(x, Utils.isArray(bindings) && bindings.every(b => Utils.isArray(b) && b.length === 2 && Utils.isSym(car(b))));
    const [vars, vals] = Utils.zip(...bindings as any);
    return [[SymTable.LAMBDA, vars, Utils.map(expand, body)], ...Utils.map(expand, vals) as any];
  }

  export const readMacroTable: Record<string, (...args: any[]) => Expr> = {};

  export const macroTable: Record<string, Proc | Function> = {
    async: _async,
    let: _let,
  };

  export const quotes = {
    [SymTable.QUASIQUOTE.description!]: "`",
    [SymTable.QUOTE.description!]: "'",
    [SymTable.UNQUOTE.description!]: ",",
    [SymTable.UNQUOTESPLICING.description!]: ",@",
  };
  // END macros

  // system
  export const read = (text: string): Expr => {
    let cursor = 0, end = text.length - 1, line = 1, col = 1;

    const advance = () => {
      const char = text[cursor++];
      if (char === '\n') {
        line++;
        col = 0;
      };
      col++;
      return char;
    };

    const current = () => text[cursor];
    const isDblQt = () => text[cursor] === '"';
    const isOpenS = () => text[cursor] === '(';
    const isCloseS = () => text[cursor] === ')';
    const isSpace = () => text[cursor] === ' ';
    const isNewLine = () => text[cursor] === '\n';
    const isHash = () => text[cursor] === '#';
    const isEscape = () => text[cursor] === '\\';
    const isAlpha = (c: string) => (((c >= 'a') && (c <= 'z')) || ((c >= 'A') && (c <= 'Z')));
    const isDigit = (c: string) => ((c >= '0') && (c <= '9'));
    const isSpecial = (c: string) => ((c === '.') || (c === '_') || (c === '^') || (c === '=') || (c === '?') || (c === '!'));
    const isMathOp = (c: string) => ((c === '+') || (c === '-') || (c === '*') || (c === '/') || (c === '<') || (c === '>'));
    const isAlnum = (c: string) => isAlpha(c) || isDigit(c);
    const isValid = (c: string) => isAlnum(c) || isSpecial(c) || isMathOp(c) || isHash() || isEscape();
    const isEOF = () => cursor > end;

    const eatSpace = () => {
      while ((isSpace() || isNewLine()) && !isEOF())
        advance();
    };

    const toLisp = (funcs: Record<string, any>) => Object.entries(funcs).reduce((acc: any, [key, val]: any) => { acc[key] = (...args: any[]) => val(...args) ? TRUE : EMPTY; return acc; }, {} as Record<string, any>);

    const readMacroLocals = { parse, advance, current, eatSpace, ...toLisp({ isEOF, isSpace, isNewLine }) };

    const error = (start: Errors.Position, message?: string): Errors.FormatErrorOptions => {
      return { end: { line, col, cursor }, message, start };
    };

    function parseAtom(): Expr {
      let atom: string = '';
      do {
        if (isEscape()) {
          advance();
        }
        atom += advance();
      } while (isValid(current()) && !isEOF());
      const num = parseInt(atom);
      if (Number.isNaN(num) === false)
        return num;
      return Symbol.for(atom);
    }

    function parseComment(): Expr {
      if (current() === ";") {
        advance();
        while (!isNewLine() && !isEOF())
          advance();
        return parse();
      }
      return parseAtom();
    }

    function parseQuote(): Expr {
      if (current() === "'") {
        advance();
        return [SymTable.QUOTE, parse()];
      }
      if (current() === "`") {
        advance();
        return [SymTable.QUASIQUOTE, parse()];
      }
      if (current() === ",") {
        advance();
        if (current() === "@") {
          advance();
          return [SymTable.UNQUOTESPLICING, parse()];
        }
        return [SymTable.UNQUOTE, parse()];
      }
      return parseComment();
    }

    function parseReadMacro(): Expr {
      if (current() in readMacroTable) {
        const macro = readMacroTable[current()];
        advance();
        return macro(readMacroLocals);
      }
      return parseQuote();
    }

    function parseString(): Expr {
      if (isDblQt()) {
        const start = { col, line, cursor };
        advance();

        let exprs: string = '';
        while (!isDblQt() && !isNewLine() && !isEOF()) {
          exprs += advance();
        }

        if (isDblQt())
          advance();
        else
          throw new Errors.MalformedStringError(text, error(start));

        return JSON.stringify(exprs);
      }

      return parseReadMacro();
    }

    function parseList(): Expr {
      if (isOpenS()) {
        const open = { col, line, cursor };
        advance();

        const exprs: Expr[] = [];
        while (!isCloseS() && !isEOF()) {
          exprs.push(parse());
        }

        if (isCloseS()) { advance(); }
        else
          throw new Errors.MissingParenthesisError(text, error(open));

        return exprs;
      }

      else if (current() === ')')
        throw new Errors.UnexpectedParenthesisError(text, error({ col, line, cursor }));

      return parseString();
    }

    function parse(): Expr {
      eatSpace();
      const exprs = parseList();
      eatSpace();
      return exprs;
    }

    return parse();
  };

  export const expand = (expr: Expr, topLevel = false, env: Env = new Env()): Expr => {
    const e = expr as Expr[];
    Utils.expect(e, !Utils.isEmpty(e), "Can't expand empty list");
    if (!Utils.isArray(e)) { return e; }
    if (SymTable.QUOTE === car(e)) {
      Utils.expect(e, e.length === 2);
      return e;
    }
    else if (SymTable.COND === car(e)) {
      const [_def, ...exprs] = e;
      const preds = exprs.map(pair => [(<any>pair)[0], expand((<any>pair)[1], false, env)]);
      Utils.expect(preds, Utils.isArray(preds) && preds.every(x => x.length === 2), `found cond entry where (length != 2): (${(Utils.isArray(preds) ? preds.find(x => x.length !== 2) : preds)})`);
      return [_def, preds];
    }
    else if (SymTable.BEGIN === car(e)) {
      const [_begin, ...exprs] = e;
      if (Utils.isEmpty(exprs))
        return [];
      return [_begin, exprs.map(x => expand(x, topLevel, env))];
    }
    else if (SymTable.DEFUN === car(e) || SymTable.DEFINEMACRO === car(e)) {
      Utils.expect(e, e.length >= 3);
      const [_def, name, args, body] = e;
      Utils.expect(args, Utils.isSym(name) && (Utils.isArray(args) || Utils.isSym(args)));
      const expr: List = expand([SymTable.LAMBDA, args, body], false, env) as any;
      Utils.expect(expr, expr.length >= 1, `body list size should be at least 1, got: ${expr.length}`);
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
    else if (SymTable.LAMBDA === car(e)) {
      Utils.expect(e, e.length === 3);
      const [_lambda, params, expr] = e;
      const allAtoms = Utils.isArray(params) && params.every(Utils.isSym);
      Utils.expect(params, (allAtoms || Utils.isSym(params)), 'Invalid args');
      const body: any = Utils.isArray(expr) ? expr : [SymTable.BEGIN, expr];
      Utils.expect(body, (<any>body).length >= 1, `body list size should be at least 1, got: ${body.length}`);
      return [_lambda, params, expand(body, false, env)];
    }
    else if (SymTable.QUASIQUOTE === car(e)) {
      Utils.expect(e, e.length === 2);
      return expandQuasiquote(cadr(e));
    }
    else if (Utils.toString(car(e)) in macroTable) {
      const name = Utils.toString(car(e));
      const proc: any = macroTable[name];
      if (Utils.isProc(proc)) {
        const args = (<List>cdr(e)).map(expr => expand(expr, topLevel, env));
        const a = new Env(proc.params, args, proc.env);
        const rv = evaluate(proc.expr, a);
        return expand(rv, topLevel, a);
      }
      return expand(proc(...<List>cdr(e)), topLevel, env);
    }
    return e.map(x => expand(x, false, env));
  };
  export const expandQuasiquote = (x: Expr): Expr => {
    if (!Utils.isPair(x))
      return [SymTable.QUOTE, x];
    Utils.expect(x, x !== SymTable.UNQUOTESPLICING, "can't slice here");
    if (car(x) === SymTable.UNQUOTE) {
      Utils.expect(x, Utils.isArray(x) && x.length === 2);
      return cadr(x);
    }
    if (Utils.isPair(car(x)) && caar(x) === SymTable.UNQUOTESPLICING) {
      Utils.expect(car(x), Utils.isArray(car(x)) && (<List>car(x)).length === 2);
      return [SymTable.APPEND, cdar(x), expandQuasiquote(cdr(x))];
    }
    else {
      return [SymTable.CONS, expandQuasiquote(car(x)), expandQuasiquote(cdr(x))];
    }
  };

  export const evaluate = (e: Expr, a: Env): Expr => {
    if (Utils.isSym(e))
      return a.get(Utils.toString(e)) as Expr;
    else if (!Utils.isArray(e)) {
      if (Utils.isString(e))
        return JSON.parse(e);
      if (Utils.isNum(e))
        return e;
      throw new Error(`unknown thingy: ${Utils.toString(e, true)}`);
    }
    else {
      switch (car(e)) {
        case SymTable.QUOTE: return quote(e);
        case SymTable.ATOM: return atom(evaluate(cadr(e), a));
        case SymTable.EQ: return eq(evaluate(cadr(e), a),
          evaluate(caddr(e), a));
        case SymTable.CAR: return car(evaluate(cadr(e), a));
        case SymTable.CDR: return cdr(evaluate(cadr(e), a));
        case SymTable.CONS: return cons(evaluate(cadr(e), a),
          evaluate(caddr(e), a));
        case SymTable.COND: return evalCond(cadr(e), a);
        case SymTable.LAMBDA: {
          return new Proc(cadr(e), caddr(e), a) as any;
        }
        case SymTable.DEFUN: {
          const callee: Proc = evaluate(caddr(e), a) as any;
          callee.name = Utils.toString(cadr(e));
          a.set(callee.name, callee);
          return callee as any;
        }
        case SymTable.BEGIN: {
          return (<List>cdr(e))
            .reduce((_, expr) => evaluate(expr, a), cadr(e));
        }
        case SymTable.DO: {
          return _do(<any>cdr(e), a);
        }
        default: {
          const [proc, ...args] = e.map(expr => evaluate(expr, a));
          if (Utils.isCallable(proc)) {
            return proc.call(args);
          }
          proc;
          return args;
        }
      }
    }
  };
  export const evalCond = (c: Expr, a: Env): Expr => {
    if (Utils.isEmpty(c)) {
      // throw new Error('Fallthrough COND !!!')
      return c;
    }
    if (Utils.isT(evaluate(caar(c), a))) {
      return evaluate(cadar(c), a);
    }
    return evalCond(cdr(c), a);
  };
  // END system

  export const parse = (code: string, a: Env): Expr => {
    return expand(read(code), true, a);
  };
  export const exec = (code: string, a: Env): Expr => {
    return evaluate(parse(code, a), a);
  };
}