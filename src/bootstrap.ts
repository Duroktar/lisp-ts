
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
        throw new Error('Invalid Env params')
      }
    }
    get(name: Atom): Expr {
      const result = this.inner[name] ?? this.outer?.get(name)
      if (result === undefined) {
        throw new Error('undefined variable: ' + String(name))
      }
      return result as Expr
    }
    set(name: Atom, value: Expr | Proc | NativeFunc): void {
      this.inner[name] = value
    }
    entries(): any[] {
      return [...Object.entries(this.inner), ...(this.outer?.entries() ?? [])]
    }
    private inner:  Record<Atom, Expr | Proc | NativeFunc>
  }

  export class Proc {
    constructor(public params: Expr, public expr: Expr, public env: Env) {}
    public name!: string
  }

  export abstract class NativeFunc {
    abstract params: symbol[];
    abstract name: string;
    abstract env: Env;
    abstract call(args: Expr, _eval: Function, env: Env): Expr
  }

  // primitives (7)
  export const quote = (expr: Expr): Expr => cadr(expr)
  export const atom = (expr: Expr): Expr => Utils.isAtom(expr) ? TRUE : EMPTY
  export const eq   = (x: Expr, y: Expr): Expr => (Utils.isSym(x) && Utils.isSym(y) && x === y || Utils.isEmpty(x) && Utils.isEmpty(y)) ? TRUE : EMPTY
  export const cons = (car: Expr, cdr: Expr): Expr => [car, ...<any>Utils.expect(cdr, Utils.isArray, '2nd Argument to cons must be an array...')]
  export const car  = (expr: Expr): Expr => Utils.expect(<any>expr, Utils.isArray, 'Argument to car must be an array..')[0]
  export const cdr  = (expr: Expr): Expr => Utils.expect(<any>expr, Utils.isArray, 'Argument to cdr must be an array..').slice(1)
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

  export function _let(...args: Expr[]) {
    let x = cons(Sym('let'), args)
    const [bindings, body] = args
    Utils.expect(x, args.length>1, 'Must provide arguments to "let" function')
    Utils.expect(x, (<any[]>bindings).every(b => Utils.isArray(b) && b.length===2 && Utils.isSym(b[0])))
    const [vars, vals] = Utils.zip(...(<any[]>bindings))
    return [[Sym('lambda'), vars, Utils.map(expand, body)], ...<any[]>Utils.map(expand, vals)];
  }

  export const readMacroTable: Record<string, (...args: any[]) => Expr> = {}

  export const macroTable: Record<string, Proc | Function> = { let: _let }

  export const read = (expr: string): Expr => {
    let cursor = 0, end = expr.length - 1, open: number[] = [];

    const advance   = () => expr[cursor++];
    const current   = () => expr[cursor];
    const isOpenS   = () => expr[cursor] === '(';
    const isCloseS  = () => expr[cursor] === ')';
    const isSpace   = () => expr[cursor] === ' ';
    const isNewLine = () => expr[cursor] === '\n';
    const isHash    = () => expr[cursor] === '#';
    const isEscape  = () => expr[cursor] === '\\';
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
        return [Sym('quote'), parse()]
      }
      if (current() === "`") {
        advance()
        return [Sym('quasiquote'), parse()]
      }
      if (current() === ",") {
        advance()
        if (current() === "@" ) {
          advance()
          return [Sym('unquotesplicing'), parse()]
        }
        return [Sym('unquote'), parse()]
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

    function parseList(): Expr {
      if (isOpenS()) {
        open.push(cursor); advance()

        let exprs: Expr[] = []
        while (!isCloseS() && !isEOF()) {
          exprs.push(parse())
        }

        if (isCloseS()) { advance(); open.pop() }
        else throw new Error("Missing ')'")

        return exprs
      }

      else if (current() === ')')
        throw new Error('Unexpected ")" at cursor: ' + cursor + ', start: ' + open.pop());

      return parseReadMacro()
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
    if (Lisp.Sym('quote') === car(e)) {
      Utils.expect(e, e.length===2)
      return e
    }
    else if (Lisp.Sym('cond') === car(e)) {
      const [_def, ...exprs] = e
      const preds = exprs.map(pair => [(<any>pair)[0], expand((<any>pair)[1], false, env)]);
      Utils.expect(preds, Utils.isArray(preds) && preds.every(x => x.length===2), `found cond entry where (length != 2): (${(Utils.isArray(preds) ? preds.find(x => x.length!==2) : preds)})`)
      return [_def, preds]
    }
    else if (Lisp.Sym('begin') === car(e)) {
      const [_begin, ...exprs] = e
      if (Utils.isEmpty(exprs))
        return []
      return [_begin, exprs.map(x => expand(x, topLevel, env))]
    }
    else if (Lisp.Sym('defun') === car(e) || Lisp.Sym('define-macro') === car(e)) {
      Utils.expect(e, e.length>=3)
      const [_def, name, args, body] = e;
      Utils.expect(args, Utils.isSym(name) && (Utils.isArray(args) || Utils.isSym(args)))
      const expr: List = expand([Lisp.Sym('lambda'), args, body], false, env) as any
      Utils.expect(expr, expr.length>=1, `body list size should be at least 1, got: ${expr.length}`)
      if (_def === Sym('define-macro')) {
        Utils.expect(e, topLevel, 'define-macro only allowed at top level')
        const callee: Proc = evaluate(expr, env) as any
        callee.name = Utils.toString(name)
        Utils.expect(e, Utils.isProc(callee), 'macro must be a procedure')
        macroTable[callee.name] = callee as any
        return []
      }
      return [_def, name, expr]
    }
    else if (Lisp.Sym('lambda') === car(e)) {
      Utils.expect(e, e.length===3)
      const [_lambda, params, expr] = e;
      const allAtoms = Utils.isArray(params) && params.every(Utils.isSym);
      Utils.expect(params, (allAtoms || Utils.isSym(params)), 'Invalid args')
      const body: any = Utils.isArray(expr) ? expr : [Sym('begin'), expr];
      Utils.expect(body, (<any>body).length>=1, `body list size should be at least 1, got: ${body.length}`)
      return [_lambda, params, expand(body, false, env)]
    }
    else if (Lisp.Sym('quasiquote') === car(e)) {
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

    function expandQuasiquote(x: Lisp.Expr): Lisp.Expr {
      if (!Utils.isPair(x)) return [Sym('quote'), x]
      Utils.expect(x, x !== Sym('unquotesplicing'), "can't slice here")
      if (car(x) === Sym('unquote')) {
        Utils.expect(x, Utils.isArray(x) && x.length === 2)
        return cadr(x)
      }
      if (Utils.isPair(car(x)) && caar(x) === Sym('unquotesplicing')) {
        Utils.expect(car(x), Utils.isArray(car(x)) && (<List>car(x)).length === 2)
        return [Sym('append'), cdar(x), expandQuasiquote(cdr(x))]
      }
      else {
        return [Sym('cons'), expandQuasiquote(car(x)), expandQuasiquote(cdr(x))]
      }
    }
  }

  export const evaluate = (e: Expr, a: Env): Expr => {
    if (Utils.isSym(e)) return a.get(e.description!)
    else if (!Utils.isArray(e)) return e
    else {
      switch (car(e)) {
        case Sym('quote'):   return quote(e)
        case Sym('atom'):    return atom(evaluate(cadr(e), a))
        case Sym('eq'):      return eq(evaluate(cadr(e), a),
                                       evaluate(caddr(e), a))
        case Sym('car'):     return car(evaluate(cadr(e), a))
        case Sym('cdr'):     return cdr(evaluate(cadr(e), a))
        case Sym('cons'):    return cons(evaluate(cadr(e), a),
                                         evaluate(caddr(e), a))
        case Sym('cond'):    return evalCond(cadr(e), a)
        case Sym('lambda'):  {
          return new Proc(cadr(e), caddr(e), a) as any
        }
        case Sym('defun'): {
          const callee: Proc = evaluate(caddr(e), a) as any
          callee.name = Utils.toString(cadr(e))
          a.set(callee.name, callee)
          return []
        }
        case Sym('begin'):  {
          return (<any[]>cdr(e))
            .reduce((_, expr) => evaluate(expr, a), cadr(e))
        }
        default: {
          const [proc, ...args] = e.map(expr => evaluate(expr, a))
          if (proc instanceof Proc) {
            // proc
            const env = new Env(proc.params, args, proc.env)
            // env
            const rv = evaluate(proc.expr, env);
            // rv
            return rv
          }
          else if (proc instanceof NativeFunc) {
            const env = new Env(proc.params, args, proc.env)
            // proc
            return proc.call(args, evaluate, env)
          }
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

  export const exec = (code: string, a: Env): Expr => {
    return evaluate(expand(read(code), true, a), a)
  }
}

export namespace Utils {

  export const expect = <E, P extends boolean | ((e: E) => boolean)>(e: E, p: P, msg = ''): E => {
    if (!((typeof p === 'boolean') ? p : p(e))) {
      throw new Error(msg || `${toString(e as any)}: expect error`);
    } else {
      return e
    }
  }

  export const isPair = (x: Lisp.Expr) => !isSym(x) && !isEmpty(x)
  export const isArray = (x: Lisp.Expr): x is Lisp.Expr[] => !isSym(x) && Array.isArray(x)
  export const isAtom = (x: Lisp.Expr): x is Lisp.Atom => isSym(x)
  export const isSym = (x: Lisp.Expr): x is symbol => typeof x === 'symbol'
  export const isEmpty = (x: Lisp.Expr): boolean => isArray(x) && x.length === 0
  export const isCallable = (x: any): x is Lisp.Proc | Lisp.NativeFunc => isProc(x) && isNativeFn(x)
  export const isProc = (x: any): x is Lisp.Proc => x instanceof Lisp.Proc
  export const isNativeFn = (x: any): x is Lisp.NativeFunc => x instanceof Lisp.NativeFunc
  export const toLispBool = (e: boolean): Lisp.Expr => e ? Lisp.TRUE : Lisp.EMPTY

  export const isT = (e: Lisp.Expr): boolean => e === Lisp.TRUE

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

  export const toString = (expr: Lisp.Expr): string => {
    const e: any = expr
    if (typeof e === 'symbol') return e.description!
    if (Utils.isT(Lisp.atom(e))) return String(e)
    if (!Array.isArray(e))  return e
    if (Lisp.car(e) === 'quote') {
      if (e.length === 2)
        return `'${toString(e[1])}`
      return `'${toString(e.slice(1))}`
    }
    if (e instanceof Lisp.Proc) {
      return '[Proc: anonymous]'
    }
    if (e instanceof Lisp.NativeFunc) {
      let expr: Lisp.NativeFunc = e
      return `[NativeFunc: ${expr.name}]`
    }
    if (Utils.isArray(e)) {
      return `(${e.map(c => toString(c)).join(' ')})`
    }
    throw new Error('Unknown in toString: ' + e)
  }
  export const print = (e: Lisp.Expr): void => {
    console.log(toString(e))
  }

  export function mkNativeFunc(env: Lisp.Env, name: string, params: string[], cb: (args: Lisp.Expr, _eval: Function, env: Lisp.Env) => any): Lisp.Expr | Lisp.NativeFunc {
    const func = new class extends Lisp.NativeFunc {
      public name = name;
      public call = cb;
      public env = env;
      public params = params.map(Lisp.Sym);
    };

    env.set(name, func)
    return func
  }
}

export namespace Runtime {

  const {cons, expand, list, Env} = Lisp
  const {isArray, isAtom, expect, map, zip, toString, mkNativeFunc} = Utils

  export const env = new Env()
  env.set('#t', '#t')
  env.set('#f', '#f')

  mkNativeFunc(env, 'debugnf',  ['name', 'x'], ([name, x]: any) => { console.log('[DEBUG-NF]:', Utils.toString(name)); console.log(x); return []; })
  mkNativeFunc(env, 'debugn',   ['name', 'x'], ([name, x]: any) => { console.log('[DEBUG-N]:', Utils.toString(name)); console.log(x); return x; })
  mkNativeFunc(env, 'debugf',   ['x'], x => { console.log('[DEBUG-F]'); console.log(x); return []; })
  mkNativeFunc(env, 'debug',    ['x'], x => { console.log('[DEBUG]'); console.log(x); return x; })
  mkNativeFunc(env, 'printn',   ['name', 'x'], ([name, x]: any) => { console.log(name, toString(x)); return []; })
  mkNativeFunc(env, 'print',    ['x'], ([x]: any) => { console.log(toString(x)); return []; })
  mkNativeFunc(env, 'break',    ['x'], x => { debugger; return x; })

  mkNativeFunc(env, 'append', ['args'], (args: any) => args.reduce((acc: any, val: any) => acc.concat(val)))
  mkNativeFunc(env, '+', ['args'], (args: any) => args.reduce((acc: any, val: any) => acc + val))
  mkNativeFunc(env, '-', ['args'], (args: any) => args.reduce((acc: any, val: any) => acc - val))
  mkNativeFunc(env, '*', ['args'], (args: any) => args.reduce((acc: any, val: any) => acc * val))
  mkNativeFunc(env, '/', ['args'], (args: any) => args.reduce((acc: any, val: any) => acc / val))
  mkNativeFunc(env, '=', ['args'], (args: any) => args.reduce((acc: any, val: any) => Utils.toLispBool(acc === val)))

  mkNativeFunc(env, 'set-macro-character', ['char', 'cb'], ([char, cb]: any, _eval, env) => {
    Lisp.readMacroTable[toString(char)] = locals => {
      const proc = _eval(cb, env);
      mkNativeFunc(proc.env, 'read',      ['read'],      ([locals]: any) => locals.parse())
      mkNativeFunc(proc.env, 'advance',   ['advance'],   ([locals]: any) => locals.advance())
      mkNativeFunc(proc.env, 'current',   ['current'],   ([locals]: any) => locals.current())
      mkNativeFunc(proc.env, 'isEOF',     ['isEOF'],     ([locals]: any) => locals.isEOF())
      mkNativeFunc(proc.env, 'isSpace',   ['isSpace'],   ([locals]: any) => locals.isSpace())
      mkNativeFunc(proc.env, 'isNewLine', ['isNewLine'], ([locals]: any) => locals.isNewLine())
      return _eval([proc, locals, toString(char)], env)
    }
  })

  mkNativeFunc(env, 'call/cc', ['retval'], ([proc]: any, _eval, env) => {
      class RuntimeWarning extends Error { public retval?: any }
      let ball = new RuntimeWarning("Sorry, can't continue this continuation any longer.")
      const throw_ = mkNativeFunc(env, 'call/cc', ['retval'], retval => {
        ball.retval = retval; throw ball
      })
      try {
        if (proc instanceof Lisp.Proc) {
          const env = new Env(proc.params, [throw_ as Lisp.Expr], proc.env)
          return Lisp.evaluate(proc.expr, env)
        }
      } catch(err) {
        if (err === ball) { return ball.retval }
        else { throw err }
      }
  })

  mkNativeFunc(env, 'macroexpand', ['expr'], (args: any, _eval, env) => {
    // args
    const expr = Lisp.car(args)
    // expr
    // env
    const rv = Lisp.expand(expr, true, env);
    // rv
    return rv
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

  // Lisp.exec(`(begin
  //   (defun num-reader (stream char) 5)
  //   (set-macro-character 3 'num-reader)
  // )`, Runtime.env)

  /*
  *
  *  macros
  *
  */
  Lisp.exec(
    "(define-macro if (c t e) `(cond (,c ,t) ('#t ,e)))"
  , env)

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

  Lisp.exec(`
    (defun eval-expr (expr env)
      (cond
        ((atom expr) (env expr))
        ((eq (car expr) 'lambda)
          (lambda (arg)
            (eval-expr caddr (lambda (y)
                              (if (eq (cadr expr) y)
                                  arg
                                  (env y))
              )))))
        ('#t (
          (eval-expr (car expr) env)
          (eval-expr (cadr expr) env)))
      ))
  `, env)

  Lisp.exec(`(print (eval-expr '((lambda (x y) y) (3 5)) (lambda (y) y)))`, env)
}
