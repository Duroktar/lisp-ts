import "colors"
import { inspect } from "util";


export namespace Lisp {

  export const Sym = Symbol.for

  export type Atom = symbol | string | number;
  export type List = Expr[];
  export type Expr = List | Atom;

  export const TRUE:  Atom = Sym('#t');
  export const EMPTY: Expr = [];

  export class Env {
    constructor(params: Expr = [], args: Expr = [], private outer?: Env) {
      if (Utils.isArray(params) && Utils.isArray(args) && params.every(Utils.isSym))
        this.inner = Object.fromEntries(params.map((p, i) => [p.description!, args[i]]))
      else if (Utils.isSym(params)) {
        this.inner = { [params.description!]: args }
      }
      else {
        throw new Errors.InvalidEnvArgumentsError(params, args)
      }
    }
    get(name: Atom): Expr | Proc | NativeFunc {
      const result = this.inner[name] ?? this.outer?.get(name)
      if (result === undefined) {
        throw new Errors.UndefinedVariableError(String(name))
      }
      return result as Expr
    }
    set(name: Atom, value: Expr | Proc | NativeFunc): void {
      this.inner[name] = value
    }
    private inner:  Record<Atom, Expr | Proc | NativeFunc>
  }

  export class Proc {
    constructor(public params: Expr, public expr: Expr, public env: Env) {}
    public name: string = 'λ'
    public call = (args: Expr) => {
      const env = new Env(this.params, args, this.env)
      return evaluate(this.expr, env)
    }
  }

  export abstract class NativeFunc {
    abstract params: symbol[];
    abstract env: Env;
    public expr = [];
    public name: string = 'λ'
    public call = (args: Expr) => {
      const env = new Env(this.params, args, this.env)
      return this._call(args, env)
    }
    abstract _call(args: Expr, env: Env): Expr
  }

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
  }

  // primitives (7)
  export const quote = (expr: Expr): Expr => cadr(expr)
  export const atom  = (expr: Expr): Expr => Utils.toL(Utils.isAtom(expr))
  export const eq    = (x: Expr, y: Expr): Expr => Utils.toL(Utils.isSym(x) && Utils.isSym(y) && x === y || Utils.isEmpty(x) && Utils.isEmpty(y))
  export const cons  = (car: Expr, cdr: Expr): Expr => [car, ...<any>Utils.expect(cdr, Utils.isArray, '2nd Argument to cons must be an array...')]
  export const car   = (expr: Expr): Expr => Utils.expect(<any>expr, Utils.isArray, 'Argument to car must be an array..')[0]
  export const cdr   = (expr: Expr): Expr => Utils.expect(<any>expr, Utils.isArray, 'Argument to cdr must be an array..').slice(1)
  // END Utils.toString(primitives)

  // export const compose = (...fns: Function[]) => (arg: any) => fns.reduceRight((acc, fn) => fn(acc), arg)
  // export const cadr    = compose(car, cdr)

  // functions
  export const cadr    = (expr: Expr): Expr => car(cdr(expr))
  export const cdar    = (expr: Expr): Expr => cdr(car(expr))
  export const caar    = (expr: Expr): Expr => car(car(expr))
  export const cadar   = (expr: Expr): Expr => car(cdr(car(expr)))
  export const caddr   = (expr: Expr): Expr => car(cdr(cdr(expr)))
  export const cdadr   = (expr: Expr): Expr => cdr(car(cdr(expr)))
  export const caddar  = (expr: Expr): Expr => car(cdr(cdr(car(expr))))
  export const cadddr  = (expr: Expr): Expr => car(cdr(cdr(cdr(expr))))
  export const cadadr  = (expr: Expr): Expr => car(cdr(car(cdr(expr))))
  export const cadddar = (expr: Expr): Expr => car(cdr(cdr(cdr(car(expr)))))

  export const nil  = (x: Expr): Expr => not(atom(x)) && eq(x, EMPTY)
  export const not  = (x: Expr): Expr => (x === TRUE) ? EMPTY : TRUE

  export const list = (...exprs: Expr[]): List => exprs
  // END functions

  export const mkLambda = (params: string[] | string, body: Expr): Expr => {
    return [SymTable.LAMBDA, Utils.isArray(params) ? params.map(Sym) : Sym(params), body]
  }

  export function _async(...args: Expr[]) {
    const x = cons(SymTable.DO, args)
    Utils.expect(x, args.length>0, '`do` blocks must container an expression')
    const result = args.reduce((acc: any[], expr) => {
      let cont: any[] = [Sym('call/cc'), mkLambda(['throw'], [mkLambda(['arg'], expr)])]
      if (acc.length === 0)
        return cont

      cont[1][2].push(acc)
      return cont
    }, [])
    // console.log(Utils.toString(result))
    return result
  }

  export function _let(...args: Expr[]) {
    const x = cons(SymTable.LET, args)
    Utils.expect(x, args.length>1, 'Must provide arguments to "let" macro')
    const [bindings, body] = args
    Utils.expect(x, Utils.isArray(bindings) && bindings.every(b => Utils.isArray(b) && b.length===2 && Utils.isSym(car(b))))
    const [vars, vals] = Utils.zip(...bindings as any)
    return [[SymTable.LAMBDA, vars, Utils.map(expand, body)], ...Utils.map(expand, vals) as any];
  }

  export function _sleep(...args: Expr[]) {
    return [Sym('do/sleep'), args[0], Sym('throw')]
  }

  export function _do(args: Expr[], env: Env) {
    /*
    (do ((<variable1> <init1> <step1>) ...)
         (<test> <expression> ...)
        <command> ...)
    */
   const [[...preludes], [test, ...expressions], ...commands] = <any>args

    // Syntax: The <init>s, <step>s, <test>s, and <command>s must be expressions.
    //         The <variable>s must be pairwise distinct variables.
    preludes.forEach(([var1, init1, step1]: any) => {
      Utils.expect(var1, Utils.isAtom(var1))
      Utils.expect(var1, Utils.isExpr(init1))
      Utils.expect(var1, Utils.isExpr(step1))
    });
    Utils.expect(test, Utils.isExpr(test), `Test must be an expression. Got: ${typeof test} .. (value: ${Utils.toString(test)})`)
    expressions.forEach((expression: any) => {
      Utils.expect(expression, Utils.isExpr(expression), `Not an expression. Got: ${typeof test} .. (value: ${Utils.toString(test)})`)
    });
    commands.forEach((command: any) => {
      Utils.expect(command, Utils.isExpr(command), `A Command must be an expression. Got: ${typeof command} .. (value: ${Utils.toString(command)})`)
    });

    const bindings: Lisp.List = [];
    const steps: Lisp.List = [];

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
    const iterate = (depth = 0): Lisp.Expr => {
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
        return iterate(depth+1);
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

  export const readMacroTable: Record<string, (...args: any[]) => Expr> = {}

  export const macroTable: Record<string, Proc | Function> = {
    async: _async,
    let: _let,
    sleep: _sleep,
  }

  export const quotes = {
    [SymTable.QUASIQUOTE.description!]: "`",
    [SymTable.QUOTE.description!]: "'",
    [SymTable.UNQUOTE.description!]: ",",
    [SymTable.UNQUOTESPLICING.description!]: ",@",
  }

  export const read = (text: string): Expr => {
    let cursor = 0, end = text.length - 1, line = 1, col = 1;

    const advance   = () => {
      const char = text[cursor++];
      if (char === '\n') {
        line++
        col = 0
      };
      col++
      return char
    };
    const current   = () => text[cursor];
    const isDblQt   = () => text[cursor] === '"';
    const isOpenS   = () => text[cursor] === '(';
    const isCloseS  = () => text[cursor] === ')';
    const isSpace   = () => text[cursor] === ' ';
    const isNewLine = () => text[cursor] === '\n';
    const isHash    = () => text[cursor] === '#';
    const isEscape  = () => text[cursor] === '\\';
    const isAlpha   = (c: string) => (((c >= 'a') && (c <= 'z')) || ((c >= 'A') && (c <= 'Z')));
    const isDigit   = (c: string) => ((c >= '0') && (c <= '9'));
    const isSpecial = (c: string) => ((c === '.') || (c === '_') || (c === '^') || (c === '<') || (c === '>'));
    const isMathOp  = (c: string) => ((c === '+') || (c === '-') || (c === '*') || (c === '/'));
    const isAlnum   = (c: string) => isAlpha(c) || isDigit(c);
    const isValid   = (c: string) => isAlnum(c) || isSpecial(c) || isMathOp(c) || isHash() || isEscape();
    const isEOF     = () => cursor > end;
    const eatSpace  = () => { while ((isSpace() || isNewLine()) && !isEOF()) advance() }

    const toLisp = (funcs: Record<string, any>) => Object.entries(funcs).reduce((acc: any, [key, val]: any) => { acc[key] = (...args: any[]) => val(...args) ? TRUE : EMPTY; return acc; }, {} as Record<string, any>)

    const readMacroLocals = { parse, advance, current, eatSpace, ...toLisp({ isEOF, isSpace, isNewLine }) }

    const error = (start: Errors.Position, message?: string): Errors.FormatErrorOptions => {
      return { end: { line, col, cursor }, message, start }
    }
    function parseAtom(): Expr {
      let atom: string = ''
      do {
        if (isEscape()) {
          advance()
        }
        atom += advance()
      } while (isValid(current()) && !isEOF())
      const num = parseInt(atom)
      if (!Number.isNaN(num))
        return num
      return Symbol.for(atom)
    }

    function parseComment(): Expr {
      if (current() === ";") {
        advance()
        while (!isNewLine() && !isEOF())
          advance()
        return parse()
      }
      return parseAtom()
    }

    function parseQuote(): Expr {
      if (current() === "'") {
        advance()
        return [SymTable.QUOTE, parse()]
      }
      if (current() === "`") {
        advance()
        return [SymTable.QUASIQUOTE, parse()]
      }
      if (current() === ",") {
        advance()
        if (current() === "@" ) {
          advance()
          return [SymTable.UNQUOTESPLICING, parse()]
        }
        return [SymTable.UNQUOTE, parse()]
      }
      return parseComment()
    }

    function parseReadMacro(): Expr {
      if (current() in readMacroTable) {
        const macro = readMacroTable[current()]
        advance()
        return macro(readMacroLocals)
      }
      return parseQuote()
    }

    function parseString(): Expr {
      if (isDblQt()) {
        const start = {col, line, cursor}
        advance()

        let exprs: string = ''
        while (!isDblQt() && !isNewLine() && !isEOF()) {
          exprs += advance()
        }

        if (isDblQt()) advance()
        else throw new Errors.MalformedStringError(text, error(start))

        return JSON.stringify(exprs)
      }

      return parseReadMacro()
    }

    function parseList(): Expr {
      if (isOpenS()) {
        const open =  {col, line, cursor};
        advance()

        const exprs: Expr[] = []
        while (!isCloseS() && !isEOF()) {
          exprs.push(parse())
        }

        if (isCloseS()) { advance(); }
        else throw new Errors.MissingParenthesisError(text, error(open))

        return exprs
      }

      else if (current() === ')')
        throw new Errors.UnexpectedParenthesisError(text, error({col, line, cursor}));

      return parseString()
    }

    function parse(): Expr {
      eatSpace()
      const exprs = parseList()
      eatSpace()
      return exprs
    }

    return parse()
  }

  export const expand = (expr: Expr, topLevel = false, env: Env = new Env()): Expr => {
    const e = expr as Expr[]
    Utils.expect(e, !Utils.isEmpty(e), "Can't expand empty list")
    if (!Utils.isArray(e)) { return e }
    if (SymTable.QUOTE === car(e)) {
      Utils.expect(e, e.length===2)
      return e
    }
    else if (SymTable.COND === car(e)) {
      const [_def, ...exprs] = e
      const preds = exprs.map(pair => [(<any>pair)[0], expand((<any>pair)[1], false, env)]);
      Utils.expect(preds, Utils.isArray(preds) && preds.every(x => x.length===2), `found cond entry where (length != 2): (${(Utils.isArray(preds) ? preds.find(x => x.length!==2) : preds)})`)
      return [_def, preds]
    }
    else if (SymTable.BEGIN === car(e)) {
      const [_begin, ...exprs] = e
      if (Utils.isEmpty(exprs))
        return []
      return [_begin, exprs.map(x => expand(x, topLevel, env))]
    }
    else if (SymTable.DEFUN === car(e) || SymTable.DEFINEMACRO === car(e)) {
      Utils.expect(e, e.length>=3)
      const [_def, name, args, body] = e;
      Utils.expect(args, Utils.isSym(name) && (Utils.isArray(args) || Utils.isSym(args)))
      const expr: List = expand([SymTable.LAMBDA, args, body], false, env) as any
      Utils.expect(expr, expr.length>=1, `body list size should be at least 1, got: ${expr.length}`)
      if (_def === SymTable.DEFINEMACRO) {
        Utils.expect(e, topLevel, 'define-macro only allowed at top level')
        const callee: Proc = evaluate(expr, env) as any
        callee.name = Utils.toString(name)
        Utils.expect(e, Utils.isProc(callee), 'macro must be a procedure')
        macroTable[callee.name] = callee as any
        return []
      }
      return [_def, name, expr]
    }
    else if (SymTable.LAMBDA === car(e)) {
      Utils.expect(e, e.length===3)
      const [_lambda, params, expr] = e;
      const allAtoms = Utils.isArray(params) && params.every(Utils.isSym);
      Utils.expect(params, (allAtoms || Utils.isSym(params)), 'Invalid args')
      const body: any = Utils.isArray(expr) ? expr : [SymTable.BEGIN, expr];
      Utils.expect(body, (<any>body).length>=1, `body list size should be at least 1, got: ${body.length}`)
      return [_lambda, params, expand(body, false, env)]
    }
    else if (SymTable.QUASIQUOTE === car(e)) {
      Utils.expect(e, e.length===2)
      return expandQuasiquote(cadr(e))
    }
    else if (Utils.toString(car(e)) in macroTable) {
      const name = Utils.toString(car(e));
      const proc: any = macroTable[name];
      if (Utils.isProc(proc)) {
        const args = (<List>cdr(e)).map(expr => expand(expr, topLevel, env))
        const a = new Env(proc.params, args, proc.env)
        const rv = evaluate(proc.expr, a);
        return expand(rv, topLevel, a);
      }
      return expand(proc(...<List>cdr(e)), topLevel, env)
    }
    return e.map(x => expand(x, false, env))
  }
  export const expandQuasiquote = (x: Expr): Expr => {
    if (!Utils.isPair(x)) return [SymTable.QUOTE, x]
    Utils.expect(x, x !== SymTable.UNQUOTESPLICING, "can't slice here")
    if (car(x) === SymTable.UNQUOTE) {
      Utils.expect(x, Utils.isArray(x) && x.length === 2)
      return cadr(x)
    }
    if (Utils.isPair(car(x)) && caar(x) === SymTable.UNQUOTESPLICING) {
      Utils.expect(car(x), Utils.isArray(car(x)) && (<List>car(x)).length === 2)
      return [SymTable.APPEND, cdar(x), expandQuasiquote(cdr(x))]
    }
    else {
      return [SymTable.CONS, expandQuasiquote(car(x)), expandQuasiquote(cdr(x))]
    }
  }

  export const evaluate = (e: Expr, a: Env): Expr => {

    if (Utils.isSym(e)) return a.get(Utils.toString(e)) as Expr
    else if (!Utils.isArray(e)) {
      if (Utils.isString(e)) return JSON.parse(e)
      if (Utils.isNum(e)) return e
      throw new Error(`unknown thingy: ${Utils.toString(e, true)}`)
    }
    else {
      switch (car(e)) {
        case SymTable.QUOTE:   return quote(e)
        case SymTable.ATOM:    return atom(evaluate(cadr(e), a))
        case SymTable.EQ:      return eq(evaluate(cadr(e), a),
                                         evaluate(caddr(e), a))
        case SymTable.CAR:     return car(evaluate(cadr(e), a))
        case SymTable.CDR:     return cdr(evaluate(cadr(e), a))
        case SymTable.CONS:    return cons(evaluate(cadr(e), a),
                                           evaluate(caddr(e), a))
        case SymTable.COND:    return evalCond(cadr(e), a)
        case SymTable.LAMBDA:  {
          return new Proc(cadr(e), caddr(e), a) as any
        }
        case SymTable.DEFUN: {
          const callee: Proc = evaluate(caddr(e), a) as any
          callee.name = Utils.toString(cadr(e))
          a.set(callee.name, callee)
          return callee as any
        }
        case SymTable.BEGIN:  {
          return (<List>cdr(e))
            .reduce((_, expr) => evaluate(expr, a), cadr(e))
        }
        case SymTable.DO:  {
          return _do(<any>cdr(e), a)
        }
        default: {
          const [proc, ...args] = e.map(expr => evaluate(expr, a))
          if (Utils.isCallable(proc)) {
            return proc.call(args)
          }
          proc
          return args
        }
      }
    }
  }
  export const evalCond = (c: Expr, a: Env): Expr => {
    if (Utils.isEmpty(c)) {
      // throw new Error('Fallthrough COND !!!')
      return c
    }
    if (Utils.isT(evaluate(caar(c), a))) {
      return evaluate(cadar(c), a)
    }
    return evalCond(cdr(c), a)
  }

  export const parse = (code: string, a: Env): Expr => {
    return expand(read(code), true, a)
  }

  export const exec = (code: string, a: Env): Expr => {
    const parsed = parse(code, a);
    // console.log(inspect(parsed))
    return evaluate(parsed, a)
  }
}

export namespace Errors {
  export class InvalidEnvArgumentsError extends Error {
    constructor(
      public params: Lisp.Expr,
      public args: Lisp.Expr,
    ) {
      super('Error: Invalid Env params')
    }
  }
  export class UndefinedVariableError extends Error {
    constructor(name: string) {
      super(`Error: undefined variable: ${name}`)
    }
  }
  export class InvalidCallableExpression extends Error {
    constructor(expr: Lisp.Expr) {
      super(`Error: expression is not callable: ${Utils.toString(expr, true)}`)
    }
  }
  export class UnexpectedParenthesisError extends Error {
    constructor(source: string, options: FormatErrorOptions) {
      super(`Error: Unexpected ")" @ Ln ${options.end.line}, Col ${options.end.col})`)
      this.formattedError = formatError(source, { ...options, message: this.message })
    }
    public formattedError: string
  }
  export class MissingParenthesisError extends Error {
    constructor(source: string, options: FormatErrorOptions) {
      super(`Error: Missing ')' @ Ln ${options.end.line}, Col ${options.end.col})`)
      this.formattedError = formatError(source, { ...options, message: this.message })
    }
    public formattedError: string
  }
  export class MalformedStringError extends Error {
    constructor(source: string, options: FormatErrorOptions) {
      super(`Error: Missing '"' @ Ln ${options.end.line}, Col ${options.end.col})`)
      this.formattedError = formatError(source, { ...options, message: this.message })
    }
    public formattedError: string
  }

  export type Position = {
    line: number;
    col: number;
    cursor: number;
  }

  export type FormatErrorOptions = {
    message?: string;
    start: Position;
    end: Position;
  };

  export function formatError(source: string, options: FormatErrorOptions) {
    const lines = source.split('\n')
    const res: string[] = []
    for (let lineNo = lines.length - 1; lineNo >= 0; lineNo--) {
      if (options.end.line === options.start.line) {
        const fst = '^'.padStart(options.start.col, ' ')
        const snd = '^'.padStart(options.end.col - (options.start.col + 1), ' ')
        if (lineNo === options.end.line - 1) {
          res.push(`${fst}${snd}`)
        }
      }
      else if (lineNo === options.start.line - 1) {
        res.push('^'.padStart(options.start.col, ' '))
      }
      else if (lineNo === options.end.line - 1) {
        res.push('^'.padStart(options.end.col, ' '))
      }
      res.push(lines[lineNo])
    }
    return `${options.message}\n\n${res.reverse().join('\n')}`
  }
}

export namespace Utils {

  type Exists<P> = Exclude<P, undefined | null>;

  export const assert = <P>(p: P, msg = ''): Exists<P> => {
    if (p == null) {
      throw new Error(msg || `assert error: ${p}`);
    } else {
      return p as any
    }
  }

  export const expect = <E, P extends boolean | ((e: E) => boolean)>(e: E, p: P, msg = ''): E => {
    if (!((typeof p === 'boolean') ? p : p(e))) {
      throw new Error(msg || `expect error: ${toString(e as any)}`);
    } else {
      return e
    }
  }

  export const isPair = (x: unknown) => !isSym(x) && !isEmpty(x)
  export const isArray = (x: unknown): x is Lisp.Expr[] => !isSym(x) && Array.isArray(x)
  export const isAtom = (x: unknown): x is Lisp.Atom => isSym(x)
  export const isSym = (x: unknown): x is symbol => typeof x === 'symbol'
  export const isNum = (x: unknown): x is number => typeof x === 'number'
  export const isString = (x: unknown): x is string => typeof x === 'string'
  export const isEmpty = (x: unknown): x is [] => isArray(x) && x.length === 0
  export const isNone = (x: unknown): x is undefined | null => x === undefined || x === null
  export const isCallable = (x: unknown): x is Lisp.Proc | Lisp.NativeFunc => isProc(x) || isNativeFn(x)
  export const isProc = (x: unknown): x is Lisp.Proc => x instanceof Lisp.Proc
  export const isNativeFn = (x: unknown): x is Lisp.NativeFunc => x instanceof Lisp.NativeFunc
  export const isExpr = (x: unknown): x is Lisp.Expr => Utils.isAtom(x) || Utils.isArray(x) || Utils.isCallable(x) || Utils.isString(x) || Utils.isNum(x)

  export const symName = (s: symbol): string => s.description!

  export const isT = (e: Lisp.Expr): boolean => e === Lisp.TRUE
  export const toL = (e: boolean): Lisp.Expr => e ? Lisp.TRUE : Lisp.EMPTY

  export const zip = (...rows: Lisp.Expr[][]) => rows[0].map((_, c) => rows.map(row => row[c]))

  export const map = (func: (m: Lisp.Expr) => Lisp.Expr, m: Lisp.Expr): Lisp.Expr => {
    if (isArray(m)) {
      return m.map(child => map(func, child))
    }
    return func(m)
  }
  export const find = (func: (m: Lisp.Expr, i: number) => boolean, m: Lisp.Expr, __i = 0): Lisp.Expr | undefined => {
    if (isArray(m)) {
      for (const child of m) {
        const r = find(func, child, __i++);
        if (r !== undefined) return r;
      }
    }
    if (func(m, __i))
      return m
  }

  export const toString = (expr: Lisp.Expr, inspect = false): string => {
    if (isSym(expr)) {
      // if (inspect) return String(expr)
      return expr.description!
    }
    if (expr instanceof Lisp.NativeFunc) {
      return `(nativefunc ${expr.name})`
    }
    if (expr instanceof Lisp.Proc) {
      if (inspect) {
        const parms = toString(expr.params, inspect)
        const body = toString(expr.expr, inspect)
        return `(λ ${expr.name} ${parms} ${body})`
      }
      return `(λ ${expr.name})`
    }
    if (isString(expr) || isNone(expr)) return expr
    if (isNum(expr)) return String(expr)
    if (isEmpty(expr))  return '()'
    if (Lisp.car(expr) === Lisp.SymTable.LAMBDA) {
      return `(λ ${(<any>Lisp.cdr(expr)).map((x: any) => toString(x, inspect)).join(' ')}`
    }
    if (symName(<symbol>Lisp.car(expr)) in Lisp.quotes) {
      const val = toString(Lisp.cadr(expr), inspect);
      return `${Lisp.quotes[symName(<symbol>Lisp.car(expr))]}${val}`
    }
    return `(${expr.map(c => toString(c, inspect)).join(' ')})`
  }
  export const print = (e: Lisp.Expr, inspect = false): void => {
    console.log(toString(e, inspect))
  }

  export function mkNativeFunc(env: Lisp.Env, name: string, params: string[], cb: (args: Lisp.Expr, env: Lisp.Env) => any): Lisp.Expr | Lisp.NativeFunc {
    const func = new class extends Lisp.NativeFunc {
      public name = name;
      public env = env;
      public params = params.map(Lisp.Sym);
      public _call = cb;
    };

    env.set(name, func)
    return func
  }
}

export namespace Runtime {

  const {Env} = Lisp
  const {toString, mkNativeFunc} = Utils

  export const env = new Env()
  env.set('#t', '#t')
  env.set('#f', '#f')

  mkNativeFunc(env, 'debugnf',  ['name', 'x'], ([name, x]: any) => { console.log('[DEBUG-NF]:', Utils.toString(name)); console.log(x); return []; })
  mkNativeFunc(env, 'debugn',   ['name', 'x'], ([name, x]: any) => { console.log('[DEBUG-N]:', Utils.toString(name)); console.log(x); return x; })
  mkNativeFunc(env, 'debugf',   ['x'], x => { console.log('[DEBUG-F]'); console.log(x); return []; })
  mkNativeFunc(env, 'debug',    ['x'], x => { console.log('[DEBUG]'); console.log(x); return x; })
  mkNativeFunc(env, 'printn',   ['name', 'x'], ([name, x]: any) => { console.log(name, toString(x)); return []; })
  mkNativeFunc(env, 'printl',   ['x'], ([name, x]: any) => { console.log(name, toString(x)); return []; })
  mkNativeFunc(env, 'inspect',  ['x'], ([x]: any) => { return toString(x, true); })
  mkNativeFunc(env, 'print',    ['x'], ([x]: any) => { Utils.print(x); })
  mkNativeFunc(env, 'break',    ['x'], x => { debugger; return x; })

  mkNativeFunc(env, 'gensym',    [], () => Symbol())

  mkNativeFunc(env, 'append', ['args'], (args: any) => args.reduce((acc: any, val: any) => acc.concat(val)))
  mkNativeFunc(env, '+', ['args'], (args: any) => args.reduce((acc: any, val: any) => acc + val))
  mkNativeFunc(env, '-', ['args'], (args: any) => args.reduce((acc: any, val: any) => acc - val))
  mkNativeFunc(env, '*', ['args'], (args: any) => args.reduce((acc: any, val: any) => acc * val))
  mkNativeFunc(env, '/', ['args'], (args: any) => args.reduce((acc: any, val: any) => acc / val))
  mkNativeFunc(env, '=', ['args'], (args: any) => args.reduce((acc: any, val: any) => Utils.toL(acc === val)))

  mkNativeFunc(env, 'set-macro-character', ['char', 'cb'], ([char, cb]: any, env) => {
    Lisp.readMacroTable[toString(char)] = locals => {
      const proc = Lisp.evaluate(cb, env)
      if (Utils.isCallable(proc)) {
        mkNativeFunc(proc.env, 'read',      ['read'],      ([locals]: any) => locals.parse())
        mkNativeFunc(proc.env, 'advance',   ['advance'],   ([locals]: any) => locals.advance())
        mkNativeFunc(proc.env, 'current',   ['current'],   ([locals]: any) => locals.current())
        mkNativeFunc(proc.env, 'isEOF',     ['isEOF'],     ([locals]: any) => locals.isEOF())
        mkNativeFunc(proc.env, 'isSpace',   ['isSpace'],   ([locals]: any) => locals.isSpace())
        mkNativeFunc(proc.env, 'isNewLine', ['isNewLine'], ([locals]: any) => locals.isNewLine())
        return Lisp.evaluate([proc, locals, toString(char)], env)
      }
      throw new Error('Nope @ set-macro-character')
    }
  })

  mkNativeFunc(env, 'do/sleep', ['duration', 'throw'], ([duration, next]: any, env) => {
    setTimeout(() => {
      const rv = next.call([], env);
      return rv
    }, duration)
  })

  mkNativeFunc(env, 'call/cc', ['throw'], ([proc]: any, env) => {
    class RuntimeWarning extends Error { public retval?: any }
    let ball = new RuntimeWarning("Sorry, can't continue this continuation any longer.")
    const throw_ = mkNativeFunc(env, 'throw', ['retval'], retval => {
      ball.retval = retval; throw ball;
    })
    try {
      if (Utils.isCallable(proc)) {
        return proc.call([throw_ as Lisp.Expr]);
      }
      throw new Errors.InvalidCallableExpression(proc)
    } catch(err) {
      if (err instanceof RuntimeWarning) {
        // console.log(`exiting call/cc [${id}] (THROWN)`)
        return ball.retval
      }
      else {
        throw err
      }
    }
  })

  mkNativeFunc(env, 'macroexpand', ['expr'], (args: any, env) => {
    return Lisp.expand(Lisp.car(args), true, env);
  })

  /*
  *
  *  reader macros
  *
  */
  // Lisp.exec(`(begin
  //   (defun hat-quote-reader (stream char)
  //     (list (quote quote) (read stream)))
  //   (set-macro-character '^ 'hat-quote-reader)
  // )`, Runtime.env)

  /*
  *
  *  macros
  *
  */
  Lisp.exec(
    "(define-macro if (c t e) `(cond (,c ,t) ('#t ,e)))"
  , env)

  // Lisp.exec(
  //   `(define-macro lcomp (expression for var in list conditional conditional-test)
  //     ;; create a unique variable name for the result
  //     (let ((result (gensym)))
  //       ;; the arguments are really code so we can substitute them
  //       ;; store nil in the unique variable name generated above
  //       \`(let ((,result nil))
  //         ;; var is a variable name
  //         ;; list is the list literal we are suppose to iterate over
  //         (loop for ,var in ,list
  //               ;; conditional is if or unless
  //               ;; conditional-test is (= (mod x 2) 0) in our examples
  //               ,conditional ,conditional-test
  //               ;; and this is the action from the earlier lisp example
  //               ;; result = result + [x] in python
  //               do (setq ,result (append ,result (list ,expression))))
  //             ;; return the result
  //         ,result)))`
  // , env)

  /*
  *
  *  functions
  *
  */
  Lisp.exec(`(defun caar   (x) (car (car x)))`, env)
  Lisp.exec(`(defun cadr   (x) (car (cdr x)))`, env)
  Lisp.exec(`(defun cadar  (x) (car (cdr (car x))))`, env)
  Lisp.exec(`(defun caddr  (x) (car (cdr (cdr x))))`, env)
  Lisp.exec(`(defun caddar (x) (car (cdr (cdr (car x)))))`, env)
  Lisp.exec(`(defun list x x)`, env)

  // Lisp.exec(`
  //   (defun eval-expr (expr env)
  //     (cond
  //       ((atom expr) (env expr))
  //       ((eq (car expr) 'lambda)
  //         (lambda (arg)
  //           (eval-expr caddr (lambda (y)
  //                             (if (eq (cadr expr) y)
  //                                 arg
  //                                 (env y))
  //             )))))
  //       ('#t (
  //         (eval-expr (car expr) env)
  //         (eval-expr (cadr expr) env)))
  //     ))
  // `, env)

  // Lisp.exec(`(print (eval-expr '((lambda (x y) y) (3 5)) (lambda (y) y)))`, env)

  // Lisp.exec(`
  //   (call/cc (lambda (throw)
  //     ((lambda arg (throw (print 'Finished)))
  //     (call/cc (lambda (throw) (throw (print 1500)))))))`, env)

  // Lisp.exec(`
  // (do
  //   (print 1)
  // )`, env)

  // Lisp.exec(`
  // (print (inspect '(do
  //   (print 'Sleeping)
  //   (sleep 1)
  //   (print 'Done)
  // )))`, env)

  // Lisp.exec(`
  // (do
  //   (print 'Before)
  //   (sleep 1000)
  //   (print 'After)
  // )`, env)

  // Utils.print(Lisp.expand(Lisp.read(`
  // (async
  //   (print 'Before)
  //   (sleep 1000)
  //   (print 'After)
  // )`)))

  Lisp.exec(`
    (do ((i 0 (+ i 1)))
      ((= i 5) (print i) (print "Done!"))
    (print i))
  `, env)

  // Lisp.exec(`
  //   (call/cc (((lambda (throw) ((lambda (arg) (print 'Done'))) 'arg))
  //     (call/cc (lambda (throw) (lambda (arg) (sleep 1500 throw))))))`
  //   , env)

  // Lisp.exec(`
  //   (do
  //     (print 3)
  //     (print 2)
  //     (print 1)
  //   )`
  // , env)

  // Lisp.exec(`(print (= 5 5))`
  //   , env)

  // Utils.print(Lisp.exec(`
  //   (call/cc (lambda (throw)
  //     (throw 'hello)
  //   ))`, env))

  // try {
  //   Lisp.read(`
  //   (begin
  //     (print 'Start)
  //     (= 4 "asdfd)
  //     (print 'Done)
  //   )`)
  // } catch (err: any) {
  //   console.error(err.formattedError)
  //   process.exit(1)
  // }
}
