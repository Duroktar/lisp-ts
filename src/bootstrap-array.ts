import colors from 'colors';
import { format } from 'util';

colors.enable();

namespace Lisp {

  const debugLog = console.log

  export type Atom = string;
  export type List = Expr[];
  export type Expr = List | Atom;

  const TRUE:  Atom = '#t';
  const EMPTY: Expr = [];

  export class Env {
    constructor(params: Expr, args: Expr, private outer?: Env) {
      if (isArray(params) && isArray(args)) {
        this.inner = Object.fromEntries(params.map((p, i) => [p, args[i]]))
      } else {
        throw new Error('Invalid Env params')
      }
    }
    get(name: Atom): Expr {
      const result = this.inner[name] ?? this.outer?.get(name)
      if (result === undefined) {
        throw new Error('undefined variable: ' + name)
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
    abstract params: string[];
    abstract name: string;
    abstract env: Env;
    abstract call(args: Expr, _eval: Function, env: Env): Expr
  }

  export const mkEnv = (params: Expr = [], args: Expr = [], outer?: Env) => new Env(params, args, outer);

  export const isArray = Array.isArray
  export const isAtom = (expr: Expr): expr is Atom => typeof expr === 'string'
  export const is = (e: Expr): boolean => e === TRUE
  export const expect = (e: any, p: boolean, msg = '') => { if (!p) throw new Error(msg || `${toString(e)}: expect error`) }

  export const isEmpty = (x: Expr): boolean => !isAtom(x) && x?.length === 0

  export const map = (func: (m: Expr, i: number) => Expr, m: Expr, __i = 0): Expr => {
    if (isArray(m)) {
      return m.map(child => map(func, child, __i++))
    }
    return func(m, __i)
  }

  // primitives
  export const atom = (expr: Expr): Expr => isAtom(expr) ? TRUE : EMPTY
  export const eq = (x: Expr, y: Expr): Expr => {
    const equalAtoms = isAtom(x) && isAtom(y) && x === y;
    const bothEmpty = isEmpty(x) && isEmpty(y);
    return (equalAtoms || bothEmpty) ? TRUE : EMPTY
  }

  const strict = <T>(o: T | undefined): T => { if (o === undefined) throw new Error('Strict! (Returned undefined)'); else return o as T; }

  export const car = (expr: Expr): Expr => strict((<List>expr)?.[0])
  export const cdr = (expr: Expr): Expr => strict((<List>expr).slice(1))
  export const cadr    = (expr: Expr): Expr => car(cdr(expr))
  export const caar    = (expr: Expr): Expr => car(car(expr))
  export const cadar   = (expr: Expr): Expr => car(cdr(car(expr)))
  export const caddr   = (expr: Expr): Expr => car(cdr(cdr(expr)))
  export const cadddr  = (expr: Expr): Expr => car(cdr(cdr(cdr(expr))))

  export const cons = (car: Expr, cdr: Expr): Expr => {
    if (!isArray(cdr)) {
      throw new Error('Barfing on cons')
    }
    return [car, ...<List>cdr]
  }
  export const list    = (...exprs: Expr[]): List => exprs
  export const nil     = (x: Expr): Expr => !isAtom(x) && x?.length === 0 ? TRUE : EMPTY
  export const not     = (x: Expr): Expr => {
    if (is(x)) return EMPTY
    return TRUE
    // cond([x, EMPTY], [TRUE, TRUE])
  }
  export const pair    = (x: Expr, y: Expr): Expr => {
    if (is(nil(x)) && is(nil(y)))
      return EMPTY

    if (is(not(atom(x))) && is(not(atom(y)))) {
      return cons(list(car(x), car(y)),
                  pair(cdr(x), cdr(y)))
    }

    throw new Error('Dunno how I got here!')
    // return <ConsCell>cond(
    //   [and(nil(x), nil(y)), EMPTY],
    //   [and(not(atom(x)), not(atom(y))),
    //     cons(list(car(x), car(y)),
    //         pair(cdr(x), cdr(y)))],
    // )
  }
  export const assoc   = (x: Expr, y: Expr): Expr => {
    if (is(eq(caar(y), y)))
      return cadar(y)
    if (y.length === 0)
      return []
    return assoc(x, cdr(y))
  }

  // lib
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
    const isSpecial = (c: string) => ((c === '.') || (c === '-') || (c === '_'));
    const isAlnum   = (c: string) => isAlpha(c) || isDigit(c);
    const isAtom    = (c: string) => isAlnum(c) || isSpecial(c) || isHash();
    const isEOF     = () => cursor > end;
    const eatSpace  = () => { while ((isSpace() || isNewLine()) && !isEOF()) advance() }

    function parseAtom(): Expr {
      if (isAlpha(current()) || isHash()) {
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
    expect(e, e.length!==0)
    if (!isArray(e)) { return e }
    else if (car(e) === 'quote') {
      expect(e, e.length===2)
      return e
    }
    else if (car(e) === 'defun') {
      expect(e, e.length===4)
      const [__, name, args, expr] = e;
      expect(args, isAtom(name))
      expect(args, isArray(args))
      const body = expand(['lambda', args, expr])
      expect(body, body.length>=1, `body list size should be at least 1, got: ${body.length}`)
      return ['defun', name, body]
    }
    else if (car(e) === 'lambda') {
      expect(e, e.length===3)
      const [_lambda, params, expr] = e;
      const allAtoms = isArray(params) && params.every(isAtom);
      expect(params, (allAtoms || isAtom(params)), 'Invalid args')
      const body = expand(expr);
      expect(body, body.length>=1, `body list size should be at least 1, got: ${body.length}`)
      return [_lambda, params, body]
    }
    else if (car(e) === 'cond') {
      const [_def, ...exprs] = e
      const preds = exprs.map(pair => [pair[0], expand(pair[1])]);
      expect(preds, isArray(preds) && preds.every(x => x.length===2), `found cond entry where (length != 2): (${(isArray(preds) ? preds.find(x => x.length!==2) : preds)})`)
      return [_def, preds]
    }
    return e.map(x => expand(x))
  }

  /*
   * EValuAtE
   *
  */
  export const eval_expr = (e: Expr, a: Env): Expr => {
    // while (true) {
    if (isAtom(e))    {
      // debugLog('[EVAL_EXPR]: looking up SYMBOL:'.padEnd(36, ' '), e);
      return a.get(e)
    }
    else
      switch (car(e)) {
        case 'quote':   return cadr(e)
        case 'atom': {
          // debugLog('[EVAL_EXPR]: calling ATOM:'.padEnd(36, ' '), 'atom')
          // debugLog(' - args    :', e?.[1])
          const adr = cadr(e);
          const rv = atom(eval_expr(adr, a));
          // debugLog(format(' - Returning:', rv).dim);
          return rv
        }
        case 'eq': {
                        return eq(eval_expr(cadr(e), a),
                                  eval_expr(caddr(e), a))}
        case 'car':     return car(eval_expr(cadr(e), a))
        case 'cdr':     return cdr(eval_expr(cadr(e), a))

        case 'cons':    return cons(eval_expr(cadr(e), a),
                                    eval_expr(caddr(e), a))
        case 'cond':    {
          // debugLog('[EVAL_EXPR]: calling COND')
          const args = cadr(e);
          // debugLog('   - args  :', args)
          return eval_cond(args, a)
        }
        case 'lambda':  {
          // debugLog('[EVAL_EXPR]: calling LAMBDA')
          const params = cadr(e);
          // debugLog('   - params:', params)
          const expr = caddr(e);
          // debugLog('   - expr  :', expr)
          return new Proc(params, expr, a) as any
        }
        case 'defun': {
          const name = <string>cadr(e);
          // debugLog('[EVAL_EXPR]: calling DEFUN:'.padEnd(36, ' '), name);
          const params = caddr(e);
          // debugLog('   - params:', params)
          const callee: Proc = eval_expr(params, a) as any;
          callee.name = name
          // debugLog('   - callee:', callee)
          a.set(callee.name, callee)
          return []
        }
        default: {
          const [proc, ...args] = e
            .map(expr => eval_expr(expr, a));

          if (proc instanceof Proc) {
            // debugLog('[EVAL_EXPR]: calling Proc:'.padEnd(36, ' '), proc.name);
            // debugLog('   - params:', proc.params)
            // debugLog('   - args  :', args)
            // debugLog('   - expr  :', proc.expr)

            const rv = eval_expr(proc.expr, mkEnv(proc.params, args, proc.env));
            // debugLog(format(' - Proc       Returning:', rv).dim);
            return rv
          }
          else if (proc instanceof NativeFunc) {
            // debugLog('[EVAL_EXPR]: calling NativeFunc:'.padEnd(36, ' '), proc.name);
            // debugLog('   - params:', proc.params)
            // debugLog('   - args  :', args)

            const rv = proc.call(args, eval_expr, mkEnv(proc.params, args, proc.env));
            // debugLog(format(' - NativeFunc  Returning:', rv).dim);
            return rv
          }
          else {
            // debugLog(format('[EVAL_EXPR]: Fallthrough Expression'.dim));

            // debugLog(format(' - Fallthrough Returning:', args).dim);
            return args
          }
        }
      }
    // }
  }
  let id = 0
  const eval_cond = (c: Expr, a: Env): Expr => {
    let _id = id++;
    // debugLog(`[EVAL_COND]: Cond (${_id})`.green);
    if (isEmpty(c))
      throw new Error('Fallthrough COND !!!')
    const expr = caar(c);
    // debugLog(` - expr   (${_id}):`, expr);
    const result = eval_expr(expr, a);
    const condition = is(result);
    // debugLog(` - result (${_id}):`, condition);
    if (condition) {
      return eval_expr(cadar(c), a)
    }
    return eval_cond(cdr(c), a)
  }
  const eval_list = (m: Expr, a: Env): Expr => {
    // debugLog(format('[EVAL_LIST]: List'));
    if (is(nil(m))) return EMPTY
    return cons(eval_expr(car(m), a), eval_list(cdr(m), a))
  }

  export const print = (e: Expr): void => {
    console.log(toString(e))
  }
  export const toString = (e: Expr): string => {
    if (e[0] === 'quote') {
      if (e.length === 2) {
        return `'${toString(e[1])}`
      }
      return `'${toString(e.slice(1))}`
    }
    if (is(atom(e))) return String(e)
    if (is(nil(e)))  return '()'
    if ((<any>e) instanceof Proc) {
      return '[Proc: anonymous]'
    }
    if ((<any>e) instanceof NativeFunc) {
      let expr: NativeFunc = e as any
      return `[NativeFunc: ${expr.name}]`
    }
    if (isArray(e)) {
      return `(${e.map(c => toString(c)).join(' ')})`
    }
    throw new Error('Unknown in toString: ' + e)
  }

  export const exec = (code: string, a: Env): Expr => {
    return eval_expr(expand(read(code)), a)
  }
}

namespace Runtime {
  export const env = Lisp.mkEnv()

  function mkNativeFunc(name: string, params: string[], cb: (args: Lisp.Expr, _eval: Function, env: Lisp.Env) => any): Lisp.Expr | Lisp.NativeFunc {
    return new class extends Lisp.NativeFunc {
      public name = name;
      public call = cb;
      public env = env;
      public params = params;
    };
  }

  env.set('print', mkNativeFunc('print', ['x'], args => {
    console.log(...(<any[]>args).map(arg => Lisp.toString(arg)));
    return []
  }))
  env.set('debug', mkNativeFunc('debug', ['x'], (args: Lisp.Expr, _eval: Function) => {
    console.log('[DEBUG]', ...<any[]>args);
    // console.log('[DEBUG]', args, 'evaluated:', _eval(args, env));
    return args
  }))

  env.set('eq', mkNativeFunc('eq', ['x', 'y'], ([x, y]) => Lisp.eq(x, y)))
  env.set('assoc', mkNativeFunc('assoc', ['a', 'b'], (args, _, a) => {
    const [lExp, lEnv] = args
    if (!Lisp.isArray(lEnv))
      throw new Error('Env must be an associative array')

    const pair = lEnv.find(pair => Lisp.car(pair) === lExp)

    if (!Lisp.isArray(pair))
      throw new Error('Undefined variable (assoc): ' + lExp)

    return Lisp.cadr(pair);
  }))
  env.set('caar',  mkNativeFunc('caar', ['x'], args => Lisp.caar(args[0])))
  env.set('cadr',  mkNativeFunc('cadr', ['x'], args => Lisp.cadr(args[0])))
  env.set('cadar', mkNativeFunc('cadar', ['x'], args => Lisp.cadar(args[0])))
  env.set('caddr', mkNativeFunc('caddr', ['x'], args => Lisp.caddr(args[0])))

  // env.set('globals', env.entries())

  /*
  *
  *  functions
  *
  */

  // Lisp.exec(`
  //   (defun nil. (x)
  //     (eq x '()))
  // `, env)

  // Lisp.exec(`
  //   (defun and. (x y)
  //     (cond
  //       (x (cond (y '#t) ('#t '()) ) )
  //       ('#t '())))
  // `, env)

  // Lisp.exec(`
  //   (defun not. (x)
  //     (cond
  //       (x '())
  //       ('#t '#t)))`
  // , env)

  // Lisp.exec(`
  //   (defun append. (x y)
  //     (cond ((nil. x) y)
  //           ('#t (cons (car x) (append. (cdr x) y)))))`
  // , env)

  // Lisp.exec(`
  //   (defun pair. (x y)
  //     (cond ((and. (nil. x) (nil. y)) '())
  //           ((and. (not. (atom x)) (not. (atom y)))
  //             (cons (list  (car x) (car y))
  //                   (pair. (cdr x) (cdr y))))))`
  // , env)

  // Lisp.exec(`
  //   (defun assoc. (x y)
  //     (cond ((eq (caar y) x) (cadar y))
  //           ((nil. (cdr y)) '())
  //           ('#t (assoc. x (cdr y)))))`
  // , env)

  /*
  *
  *  metacircular evaluator
  *
  */
  Lisp.exec(`
    (defun eval (e a)
      (cond
        ((atom      e ) (assoc e a))
        ((atom (car e))
          (cond
            ((eq (car e) 'quote) (cadr e))
            ((eq (car e) 'atom)  (atom (eval (cadr  e) a)))
            ((eq (car e) 'eq)    (eq   (eval (cadr  e) a)
                                       (eval (caddr e) a)))
            ((eq (car e) 'car)   (car  (eval (cadr  e) a)))
            ((eq (car e) 'cdr)   (cdr  (eval (cadr  e) a)))
            ((eq (car e) 'cons)  (cons (eval (cadr  e) a)
                                       (eval (caddr e) a)))
            ((eq (car e) 'cond) (evcon (cdr e)))
            ('#t (eval (cons (assoc (car e) a)
                             (cdr e))
                        a))))

        ((eq (caar e) 'label)
          (eval (cons (caddar e) (cdr e))
                (cons (list (cadar e) (car e)) a)))

        ((eq (caar e) 'lambda)
          (eval (caddar e)
                  (append (pair (cadar e) (evlis (cdr e) a))
                          a)))))
  `, env)

  Lisp.exec(`
    (defun evcon (c a)
      (cond ((eval (caar c) a) (eval (cadar c) a))
            ('#t (evcon (cdr c) a))))`
  , env)

  Lisp.exec(`
    (defun evlis (m a)
      (cond ((nil m) '())
            ('#t (cons (eval  (car m) a)
                       (evlis (cdr m) a)))))`
  , env)
}

namespace Testing {
  // console.log(
  //   Lisp.toString(Lisp.read("'(eq. 'a 'a)"))
  // )
  // console.log(
  //   Lisp.toString(Lisp.read("(eval '(eq. 'a 'a) '((x xVar)))"))
  // )

  // console.log(
  //   Lisp.toString(Lisp.exec("(eq 'a 'a)", Runtime.env))
  // )

  // console.log(
  //   Lisp.toString(Lisp.read("(eval '(eq 'a 'a) '((a xVar)))"))
  // )

  // console.log(
  //   Lisp.toString(Lisp.exec("(eval '(eq 'a 'a) '((a xVar)))", Runtime.env))
  // )

  // console.log(
  //   Lisp.eval_expr([['cadr', [ [ 'atom', [ 'quote', 'a' ] ] ]]], Runtime.env)
  // )

  // console.log(
  //   Lisp.toString(Lisp.exec("(atom 'a)", Runtime.env))
  // )

  // console.log('\n--------------------------------------------------------------------------\n')

  // console.log(
  //   Lisp.toString(Lisp.exec("(eval '(atom 'a) '((a xVar)))", Runtime.env))
  // )

  // console.log(
  //   Lisp.toString(Lisp.exec("(eval '(eq 'x 'x) '((x xVar)))", Runtime.env))
  // )

  // console.log(
  //   Lisp.toString(Lisp.exec("(eval '(eq x y) '((x xVar) (y yVar)))", Runtime.env))
  // )

  Lisp.exec("(print (eval '(eq x x) '((x xVar))))", Runtime.env)
  Lisp.exec("(debug '(eval '(eq x x) '((x xVar))))", Runtime.env)

  // Lisp.exec("(debug (eval '(eq. x x) '((x xVar))))", Runtime.env)
  // Lisp.exec("(debug '(print (eq. x x)) '((x xVar)))", Runtime.env)

  // Lisp.print(Lisp.exec("(eval '(eq. 'a 'a) '((a xVar)))", Runtime.env))
  // Lisp.exec("(eval '(print (eq. x x)) '((x xVar)))", Runtime.env)
}

// const env = new Lisp.Env([], [])
// const env2 = new Lisp.Env([], [], env)
// env2.set('nope', 'cap')
// const r = env2.get('nope')
// r
// env2.get('nop')
