namespace Lisp {

  export type Atom = string;
  export type List = Expr[];
  export type Expr = List | Atom;

  export const TRUE:  Atom = '#t';
  export const EMPTY: Expr = [];

  export class Env {
    constructor(params: Expr, args: Expr, private outer?: Env) {
      if (Utils.isArray(params) && Utils.isArray(args))
        this.inner = Object.fromEntries(params.map((p, i) => [p, args[i]]))
      else if (Utils.isAtom(params)) {
        this.inner = { [params]: args }
      }
      else {
        throw new Error('Invalid Env params')
      }
    }
    get(name: Atom): Expr {
      const result = this.inner[name] ?? this.outer?.get(name)
      if (result === undefined)
        throw new Error('undefined variable: ' + name)
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

  export const mkEnv = (params: Expr = [], args: Expr = [], outer?: Env) => new Env(params, args, outer);

  export class Proc {
    constructor(public params: Expr, public expr: Expr, public env: Env) {}
    public name!: string
  }

  export abstract class NativeFunc {
    abstract params: string[];
    abstract name: string;
    abstract env: Env;
    abstract call(args: Expr, _eval: Function, env: Env): Expr
  }

  // primitives (7)
  export const quote = (expr: Expr): Expr => cadr(expr)
  export const atom = (expr: Expr): Expr => Utils.isAtom(expr) ? TRUE : EMPTY
  export const eq   = (x: Expr, y: Expr): Expr => (Utils.isAtom(x) && Utils.isAtom(y) && x === y || Utils.isEmpty(x) && Utils.isEmpty(y)) ? TRUE : EMPTY
  export const cons = (car: Expr, cdr: Expr): Expr => [car, ...Utils.expect(cdr, Utils.isArray, '2nd Argument to cons must be an array.. got: ' + cdr)]
  export const car  = (expr: Expr): Expr => Utils.expect(expr, Utils.isArray, 'Argument to car must be an array.. got: ' + expr)[0]
  export const cdr  = (expr: Expr): Expr => Utils.expect(expr, Utils.isArray, 'Argument to cdr must be an array.. got: ' + expr).slice(1)
  // END primitives

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

  export const read = (expr: string): Expr => {
    let cursor = 0, end = expr.length - 1, open: number[] = [];

    const advance   = () => expr[cursor++];
    const current   = () => expr[cursor];
    const isOpenS   = () => expr[cursor] === '(';
    const isCloseS  = () => expr[cursor] === ')';
    const isSpace   = () => expr[cursor] === ' ';
    const isNewLine = () => expr[cursor] === '\n';
    const isHash    = () => expr[cursor] === '#';
    const isAlpha   = (c: string) => (((c >= 'a') && (c <= 'z')) || ((c >= 'A') && (c <= 'Z')));
    const isDigit   = (c: string) => ((c >= '0') && (c <= '9'));
    const isSpecial = (c: string) => ((c === '.') || (c === '_'));
    const isMathOp  = (c: string) => ((c === '+') || (c === '-') || (c === '*') || (c === '/'));
    const isAlnum   = (c: string) => isAlpha(c) || isDigit(c);
    const isAtom    = (c: string) => isAlnum(c) || isSpecial(c) || isMathOp(c) || isHash();
    const isEOF     = () => cursor > end;
    const eatSpace  = () => { while ((isSpace() || isNewLine()) && !isEOF()) advance() }

    function parseAtom(): Expr {
      if (isAtom(current())) {
        let res: Expr[] = []
        while (isAtom(current()) && !isEOF()) {
          res.push(advance())
        }
        return res.join('');
      }

      throw new Error('Invalid atom:' + current())
    }

    function parseComment(): Expr {
      if (current() === ";") {
        advance()
        while (!isNewLine() && !isEOF())
          advance()
        eatSpace()
        return parse()
      }
      return parseAtom()
    }

    function parseQuote(): Expr {
      if (current() === "'") {
        advance()
        return ['quote', parse()];
      }
      return parseComment()
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

      return parseQuote()
    }

    function parse(): Expr {
      eatSpace()
      const exprs = parseList()
      eatSpace()
      return exprs
    }

    return parse()
  }
  export const expand = (e: Expr, topLevel = false): Expr => {
    Utils.expect(e, e.length!==0)
    if (!Utils.isArray(e)) { return e }
    else if (car(e) === 'quote') {
      Utils.expect(e, e.length===2)
      return e
    }
    else if (car(e) === 'defun') {
      Utils.expect(e, e.length===4)
      const [__, name, args, expr] = e;
      Utils.expect(args, Utils.isAtom(name))
      Utils.expect(args, Utils.isArray(args) || Utils.isAtom(args))
      const body = expand(['lambda', args, expr])
      Utils.expect(body, body.length>=1, `body list size should be at least 1, got: ${body.length}`)
      return ['defun', name, body]
    }
    else if (car(e) === 'lambda') {
      Utils.expect(e, e.length===3)
      const [_lambda, params, expr] = e;
      const allAtoms = Utils.isArray(params) && params.every(Utils.isAtom);
      Utils.expect(params, (allAtoms || Utils.isAtom(params)), 'Invalid args')
      const body = expand(expr);
      Utils.expect(body, body.length>=1, `body list size should be at least 1, got: ${body.length}`)
      return [_lambda, params, body]
    }
    else if (car(e) === 'cond') {
      const [_def, ...exprs] = e
      const preds = exprs.map(pair => [pair[0], expand(pair[1])]);
      Utils.expect(preds, Utils.isArray(preds) && preds.every(x => x.length===2), `found cond entry where (length != 2): (${(Utils.isArray(preds) ? preds.find(x => x.length!==2) : preds)})`)
      return [_def, preds]
    }
    return e.map(x => expand(x))
  }

  export const evaluate = (e: Expr, a: Env): Expr => {
    if (Utils.isAtom(e))return a.get(e)
    else {
      switch (car(e)) {
        case 'quote':   return quote(e)
        case 'atom':    return atom(evaluate(cadr(e), a))
        case 'eq':      return eq(evaluate(cadr(e), a),
                                  evaluate(caddr(e), a))
        case 'car':     return car(evaluate(cadr(e), a))
        case 'cdr':     return cdr(evaluate(cadr(e), a))
        case 'cons':    return cons(evaluate(cadr(e), a),
                                    evaluate(caddr(e), a))
        case 'cond':    return evalCond(cadr(e), a)
        case 'lambda':  {
          return new Proc(cadr(e), caddr(e), a) as any
        }
        case 'defun': {
          const callee: Proc = evaluate(caddr(e), a) as any
          callee.name = cadr(e) as string
          a.set(callee.name, callee)
          return []
        }
        default: {
          const [proc, ...args] = e.map(expr => evaluate(expr, a))
          if (proc instanceof Proc) {
            const env = mkEnv(proc.params, args, proc.env)
            return evaluate(proc.expr, env)
          }
          else if (proc instanceof NativeFunc) {
            const env = mkEnv(proc.params, args, proc.env)
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
    if (Utils.is(evaluate(caar(c), a))) {
      return evaluate(cadar(c), a)
    }
    return evalCond(cdr(c), a)
  }

  export const exec = (code: string, a: Env): Expr => {
    return evaluate(expand(read(code)), a)
  }
}

namespace Utils {

  export const compose = (...fns: [...fns: Function[], arg: any]) => fns.reduceRight((acc, fn) => fn(acc), fns.pop())
  export const debugLog = console.log

  export const strict = <T>(o: T | undefined): T => { if (o === undefined) throw new Error('Strict! (Returned undefined)'); else return o as T; }
  export const expect = <E, P extends boolean | ((e: E) => boolean)>(e: E, p: P, msg = ''): E => { if (!((typeof p === 'boolean') ? p : p(e))) {throw new Error(msg || `${toString(e as any)}: expect error`); } else { return e } }

  export const isArray = Array.isArray
  export const isAtom = (expr: Lisp.Expr): expr is Lisp.Atom => typeof expr === 'string'
  export const isEmpty = (x: Lisp.Expr): boolean => !isAtom(x) && x?.length === 0
  export const is = (e: Lisp.Expr): boolean => e === Lisp.TRUE

  export const map = (func: (m: Lisp.Expr, i: number) => Lisp.Expr, m: Lisp.Expr, __i = 0): Lisp.Expr => {
    if (isArray(m)) {
      return m.map(child => map(func, child, __i++))
    }
    return func(m, __i)
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

  export const toString = (e: Lisp.Expr): string => {
    if (Utils.is(Lisp.atom(e))) return String(e)
    if (Utils.is(Lisp.nil(e)))  return '()'
    if (e?.[0] === 'quote') {
      if (e.length === 2)
        return `'${toString(e[1])}`
      return `'${toString(e.slice(1))}`
    }
    if ((<any>e) instanceof Lisp.Proc) {
      return '[Proc: anonymous]'
    }
    if ((<any>e) instanceof Lisp.NativeFunc) {
      let expr: Lisp.NativeFunc = e as any
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
    return new class extends Lisp.NativeFunc {
      public name = name;
      public call = cb;
      public env = env;
      public params = params;
    };
  }
}

namespace Runtime {

  export const env = Lisp.mkEnv()

  env.set('debugnf',  Utils.mkNativeFunc(env, 'debugnf',  ['x'], ([name, x]) => { console.log('[DEBUG-NF]:', name, x); return []; }))
  env.set('debugn',   Utils.mkNativeFunc(env, 'debugn',   ['x'], ([name, x]) => { console.log('[DEBUG-N]:', name, x); return x; }))
  env.set('debugf',   Utils.mkNativeFunc(env, 'debugf',   ['x'], x => { console.log('[DEBUG-F]', x); return []; }))
  env.set('debug',    Utils.mkNativeFunc(env, 'debug',    ['x'], x => { console.log('[DEBUG]', x); return x; }))
  env.set('print',    Utils.mkNativeFunc(env, 'print',    ['x'], x => { console.log(Utils.toString(x)); return []; }))
  env.set('break',    Utils.mkNativeFunc(env, 'break',    ['x'], x => { debugger; return x; }))

  env.set('+', Utils.mkNativeFunc(env, '+', ['args'], (args: any) => args.reduce((acc: any, val: any) => acc + val)))
  env.set('-', Utils.mkNativeFunc(env, '-', ['args'], (args: any) => args.reduce((acc: any, val: any) => acc - val)))
  env.set('*', Utils.mkNativeFunc(env, '*', ['args'], (args: any) => args.reduce((acc: any, val: any) => acc * val)))
  env.set('/', Utils.mkNativeFunc(env, '/', ['args'], (args: any) => args.reduce((acc: any, val: any) => acc / val)))

  env.set('let',  Utils.mkNativeFunc(env, 'let', ['args'], (args, _eval, env) => {
      let x = Lisp.cons('let', args)
      const [bindings, body] = args
      Utils.expect(x, args.length>1, 'Must provide arguments to "let" function')
      Utils.expect(x, (<any[]>bindings).every(b => Utils.isArray(b) && b.length===2 && Utils.isAtom(b[0])))
      // const [vars, vals] =
  }))

  env.set('call/cc',  Utils.mkNativeFunc(env, 'call/cc', ['retval'], ([proc], _eval, env) => {
      class RuntimeWarning extends Error { public retval?: any }
      let ball = new RuntimeWarning("Sorry, can't continue this continuation any longer.")
      const throw_ = Utils.mkNativeFunc(env, 'call/cc', ['retval'], retval => {
        ball.retval = retval; throw ball
      })
      try {
        if (proc instanceof Lisp.Proc) {
          const env = Lisp.mkEnv(proc.params, [throw_ as Lisp.Expr], proc.env)
          return Lisp.evaluate(proc.expr, env)
        }
      } catch(err) {
        if (err === ball) { return ball.retval}
        else { throw err }
      }
  }))

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
    (defun null. (x)
      (eq x '()))
  `, env)

  Lisp.exec(`
    (defun and. (x y)
      (cond
        (x (cond (y '#t) ('#t '()) ) )
        ('#t '())))
  `, env)

  Lisp.exec(`
    (defun not. (x)
      (cond
        (x '())
        ('#t '#t)))`
  , env)

  Lisp.exec(`
    (defun append. (x y)
      (cond ((null. x) y)
            ('#t (cons (car x) (append. (cdr x) y)))))`
  , env)

  Lisp.exec(`
    (defun pair. (x y)
      (cond ((and. (null. x) (null. y)) '())
            ((and. (not. (atom x)) (not. (atom y)))
              (cons (list  (car x) (car y))
                    (pair. (cdr x) (cdr y))))))`
  , env)

  Lisp.exec(`
    (defun assoc. (x y)
      (cond ((eq (caar y) x) (cadar y))
            ((null. (cdr y)) '())
            ('#t (assoc. x (cdr y)))))`
  , env)
}

namespace MetaEval {

  /*
  *
  *  metacircular evaluator
  *
  */
  Lisp.exec(`(
    (defun eval (e a)
      (cond
        ((atom e) (assoc. e a))
        ((atom (car e))
          (cond
            ((eq (car e) 'quote) (cadr e))
            ((eq (car e) 'atom) (atom   (eval (cadr  e) a)))
            ((eq (car e) 'eq)   (eq     (eval (cadr  e) a)
                                        (eval (caddr e) a)))
            ((eq (car e) 'car)  (car    (eval (cadr  e) a)))
            ((eq (car e) 'cdr)  (cdr    (eval (cadr  e) a)))
            ((eq (car e) 'cons) (cons   (eval (cadr  e) a)
                                        (eval (caddr e) a)))
            ((eq (car e) 'cond) (evcon (cdr e) a))
            ((eq (car e) 'list) (evlis (cdr e) a))
            ('#t (eval (cons (assoc. (car e) a)
                                     (cdr e))
                        a))))

        ((eq (caar e) 'label)
          (eval (cons (caddar e) (cdr e))
                (cons (list (cadar e) (car e)) a)))

        ((eq (caar e) 'lambda)
          (eval (caddar e)
                (append. (pair. (cadar e) (evlis (cdr e) a))
                          a)))))

    (defun evcon (c a)
      (cond
        ((eval (caar c) a) (eval (cadar c) a))
        ('#t               (evcon (cdr c) a))))

    (defun evlis (m a)
      (cond ((null. m) '())
            ('#t (cons (eval  (car m) a)
                       (evlis (cdr m) a)))))
  )`
  , Runtime.env)
}

namespace Testing {

  // Lisp.exec(`
  //   ;    (defun cadr         (x) (car (cdr x)))
  //   ; -> (label cadr (lambda (x) (car (cdr x))))
  //   (print (eval '(defun. capr (x y) (eq x y)) '((a aVar))))
  // `, Runtime.env)

  Lisp.exec(`(debugn 'yppp (call/cc (lambda (throw) (eq 'm (throw 'hello)))))`, Runtime.env)
  Lisp.exec("(print (+ '1 '1 '1))", Runtime.env)

  // Lisp.exec("(eval '(eq 'a 'a) '((a aVar)))", Runtime.env)

  // Lisp.exec("(print (list 'a 'a 'a))", Runtime.env)
  // Lisp.exec("(print (list 'a 'a (list 'a 'a)))", Runtime.env)
  // Lisp.exec("(print (eval '(list 'a 'a 'a) '((a aVar))))", Runtime.env)
  // Lisp.exec("(print (eval '(list 'a 'a (list 'a 'a)) '((a aVar))))", Runtime.env)

  // Lisp.exec("(print (eval '(eq a a) '((a aVar))))", Runtime.env)
  // Lisp.exec("(print (eval '(eq 'a 'a) '()))", Runtime.env)
  // Lisp.exec("(print (eval '(eq 'a 'b) '()))", Runtime.env)
  // Lisp.exec("(print (eq 'x 'x))", Runtime.env)

  // Lisp.exec("(print (evlis '(x x x) '((x cat))))", Runtime.env)

  // Utils.print(Lisp.exec(`
  //   (eval '((label cadr (lambda (x) (car (cdr x)))) '(fst snd)) '()))
  // `, Runtime.env))

  // Utils.debugLog(Lisp.exec(`
  //   (eval
  //     '(
  //       (label cadr (lambda (x) (car (cdr x))))
  //       ((label swap (lambda (x)
  //         (cons (car (cdr x)) (cons (car x) '()))))

  //       '(m b)))
  //     '())
  // `, Runtime.env))

  // Utils.debugLog(Lisp.exec(`
  //   (eval
  //     '(
  //       (label cadr (lambda (x) (car (cdr x))))
  //       ((label swap (lambda (x)
  //         (list (cadr x) (car x))))

  //       '(m b)))
  //     '())
  // `, Runtime.env))

}
