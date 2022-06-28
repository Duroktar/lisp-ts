import highlight from "cli-highlight";
import { format, inspect } from "util";
import { EMPTY, FALSE, NIL, TRUE, UNDEF } from "../core/const";
import { Character } from "../core/data/char";
import { Resume } from "../core/data/cont";
import { InvalidCallableExpression, NotImplementedError, RuntimeWarning, UndefinedVariableError } from "../core/data/error";
import { Macro, isSyntax, Syntax } from "../core/data/macro";
import { cons, list, Pair } from "../core/data/pair";
import { currentInputPort, currentOutputPort, InPort, OutPort } from "../core/data/port";
import { Sym } from "../core/data/sym";
import { Vector } from "../core/data/vec";
import { evaluate } from "../core/eval";
import { expand } from "../core/expand";
import { Form, List } from "../core/form";
import * as Lisp from "../core/lisp";
import { print, toString, toStringSafe } from "../core/print";
import { read } from "../core/read";
import { isCallable, isChar, isEmpty, isEofString, isF, isInputPort, isIOPort, isList, isMacro, isNativeProc, isNullOrUndefined, isNum, isOutputPort, isPair, isProc, isString, isSym, isT, isVec } from "../guard";
import { iEnv } from "../interface/iEnv";
import { iWorld } from "../interface/iWorld";
import * as Util from "../utils";

// TODO: I hate this. Fix it
export type AddGlobalsOptions = {
  tscheme?: boolean;
  r5rs?: boolean;
  srfi?: boolean;
  misc?: boolean;
  repl?: boolean;
  sockets?: boolean;
  minimal?: boolean;
};

// TODO: I hate this. Fix it
const defaultOptions: AddGlobalsOptions = {
  tscheme: true,
  r5rs: true,
  srfi: true,
  misc: true,
  repl: true,
  sockets: true,
  minimal: true,
}

export async function addGlobals(
  world: iWorld,
  opts: AddGlobalsOptions = {}
) {

  const options = {...defaultOptions, ...opts }

  const {env, lexicalEnv} = world

  lexicalEnv.syntax('define-syntax', async (args, scope) => {
    const macro = await expand(Lisp.cadr(args), false, world);
    if (isMacro(macro)) macro.name = toString(Lisp.car(args));
    scope.setFrom(Lisp.car(args), macro);
  })

  lexicalEnv.syntax('syntax-rules', (args, scope) => {
    const rv = new Macro(scope, <List>Lisp.car(args), <Pair>Lisp.cdr(args));
    return rv
  })

  if (options.minimal) {
    env.set('#t', TRUE);
    env.set('#f', FALSE);

    env.define('boolean?', ['obj'], ([obj]: any) => Util.toL(obj === TRUE || obj === FALSE));
    env.define('char?', ['obj'], ([obj]: any) => Util.toL(isChar(obj)));
    env.define('vector?', ['obj'], ([obj]: any) => Util.toL(isVec(obj)));
    env.define('procedure?', ['obj'], ([obj]: any) => Util.toL(isProc(obj) || isNativeProc(obj)));
    env.define('pair?', ['obj'], ([obj]: any) => Util.toL(isPair(obj)));
    env.define('number?', ['n'], ([n]: any) => Util.toL(isNum(n)));
    env.define('string?', ['n'], ([n]: any) => Util.toL(isString(n)));
    env.define('port?', ['obj'], ([obj]: any) => Util.toL(isInputPort(obj) || isOutputPort(obj) || isIOPort(obj)));

    env.define('null?', ['n'], ([n]: any) => Util.toL(isEmpty(n)));
    env.define('list?', ['n'], ([n]: any) => Util.toL(isEmpty(n) || (isPair(n) && n.isList())));

    env.define('eqv?', ['a', 'b'], ([a, b]: any) => Util.toL(Util.isEqv(a, b)));
    env.define('eq?', ['a', 'b'], ([a, b]: any) => Util.toL(Util.isEq(a, b)));

    env.define('list', 'args', (args: any) => list(...args));
  }
  // #region [ rgba(20, 50, 80, 0.8) ] - TScheme
  //  - TScheme

  if (options.tscheme) {
    env.set('#<undef>', UNDEF);
    env.set('nil', NIL);

    // env.set('*default-input-port*', <any>InPort.fromStdIn())
    env.set('*current-input-port*', env.get('*default-input-port*'))

    // env.set('*default-output-port*', <any>OutPort.fromStdOut())
    env.set('*current-output-port*', env.get('*default-output-port*'))

    env.set('*default-repl-prompt*', '> ')
    env.set('*current-repl-prompt*', env.get('*default-repl-prompt*'))

    env.define('locals-js', [], (_, a) => { return console.log(inspect(a.keys().sort(), { maxArrayLength: 10000 })) });
    env.define('locals', [], (_, a) => { return a.keys().sort().map(Sym); });
    env.define('env', [], () => { return env; });
    env.define('env->size', [], () => { return env.size(); });
    env.define('env->keys', [], () => { return env.keys().map(Sym); });
    env.define('env->values', [], () => { return env.values(); });
    env.define('env->entries', [], () => { return env.entries(); });

    env.define('debugnf', ['name', 'x'], ([name, x]: any) => { console.log('[DEBUG-NF]:', toString(name)); console.log(x); });
    env.define('debugn', ['name', 'x'], ([name, x]: any) => { console.log('[DEBUG-N]:', toString(name)); console.log(x); return x; });
    env.define('debugf', ['x'], x => { console.log('[DEBUG-F]'); console.log(x); });
    env.define('debug', ['x'], x => { console.log('[DEBUG]'); console.log(x); return x; });
    env.define('printn', ['name', 'x'], ([name, x]: any) => { console.log(toString(name), toString(x)); });
    env.define('printr', ['x'], ([x]: any) => { print(x); return x});
    env.define('prints', ['...xs'], ([...xs]: any) => { console.log(...xs); });
    env.define('print', ['...xs'], ([...xs]: any) => { console.log(...xs.map((x: any) => toString(x))); });
    env.define('show', ['x'], x => { console.log(x); });
    env.define('describe', ['x'], ([x]: any) => toString(x, true));
    env.define('format', ['x'], ([x]: any) => toString(x, true));

    env.define('colorized', ['x', 'language?'], async ([x, language]: any) => {
      language = language ?? 'scheme'
      Util.assert(isString(x), format('wrong type .. `%s` .. expected `string`', typeof x))
      return highlight(x, {language, ignoreIllegals: true})
    });

    env.define('inspect', ['x'], ([x]: any) => {
      const result = inspectObject(x);
      const lines = result.split('\n').map(line => '  ' + line.trimStart()).join('\n');
      console.log(`Inspecting: ${toStringSafe(x)}\n${lines}`);
      // return result
      function inspectObject (x: any) {
        if (isProc(x)) {
          const template = `
            Name: %s
            Type: %s
            Params: %s

            Body: %s
          `
          return format(template, 'Procedure', x.name, toStringSafe(x.params), toStringSafe(x.expr));
        }
        if (isSyntax(x)) {
          const template = `
            Name: %s
            Type: %s

            Literals: %s
            Params: %s
          `
          return format(template, 'Syntax Rules Definition', x.name, toStringSafe(x.params), isPair(x.params) && x.params.map(v => toStringSafe(v)));
        }
        if (isSym(x)) {
          const template = `
            Type: %s
            Value: %s

            Bound?: %s
          `
          return format(template, 'symbol', x.description, env.hasFrom(x))
        }
        const template = `
          Type: %s
          Value: %s
        `
        if (isString(x)) return format(template, 'string', x)
        if (isNum(x)) return format(template, 'number', x)

        return toString(x, true);
      }
    });

    env.define('break', ['args'], async (args: any, env) => { debugger; return await evaluate(args, env); });
    env.define('debug-macro!', ['val'], ([val]: any) => { Syntax.debug = !isF(val) });

    env.define('gensym', [], () => Symbol());

    env.define('set-macro-character', ['char', 'cb'], ([char, cb]: any, env) => {
      lexicalEnv.setFrom(char, async (locals: any) => {
        const proc = await evaluate(cb, env);
        if (isProc(proc)) {
          proc.env.define('read', ['read'], ([locals]: any) => locals.parse());
          proc.env.define('advance', ['advance'], ([locals]: any) => locals.advance());
          proc.env.define('current', ['current'], ([locals]: any) => locals.current());
          proc.env.define('isEOF', ['isEOF'], ([locals]: any) => locals.isEOF());
          proc.env.define('isSpace', ['isSpace'], ([locals]: any) => locals.isSpace());
          proc.env.define('isNewLine', ['isNewLine'], ([locals]: any) => locals.isNewLine());
          return await evaluate(list(proc, locals, toString(char)), env);
        }
        throw new Error('Nope @ set-macro-character');
      });
    });

    env.define('i/o-port?', ['obj'], ([obj]: any) => Util.toL(isIOPort(obj)));
  }
  //#endregion

  // #region [ rgba(0, 50, 150, 0.5) ] - r5rs
  // - r5rs

  if (options.r5rs) {

    //  - 4.2 Derived Expressions
    await Lisp.execute(`
      (define-syntax cond
        (syntax-rules (else =>)
          ((cond (else result1 result2 ...))
          (begin result1 result2 ...))
          ((cond (test => result))
          (let ((temp test))
            (if temp result temp)))
          ((cond (test => result) clause1 clause2 ...)
          (let ((temp test))
            (if temp
                (result temp)
                (cond clause1 clause2 ...))))
          ((cond (test)) test)
          ((cond (test) clause1 clause2 ...)
          (let ((temp test))
            (if temp
                temp
                (cond clause1 clause2 ...))))
          ((cond (test result1 result2 ...))
          (if test (begin result1 result2 ...)))
          ((cond (test result1 result2 ...)
                clause1 clause2 ...)
          (if test
              (begin result1 result2 ...)
              (cond clause1 clause2 ...)))))
    `, world)

    await Lisp.execute(`
      (define-syntax case
        (syntax-rules (else)
          ((case (key ...)
            clauses ...)
          (let ((atom-key (key ...)))
            (case atom-key clauses ...)))
          ((case key
            (else result1 result2 ...))
          (begin result1 result2 ...))
          ((case key
            ((atoms ...) result1 result2 ...))
          (if (memv key '(atoms ...))
              (begin result1 result2 ...)))
          ((case key
            ((atoms ...) result1 result2 ...)
            clause clauses ...)
          (if (memv key '(atoms ...))
              (begin result1 result2 ...)
              (case key clause clauses ...)))))
    `, world)

    await Lisp.execute(`
      (define-syntax and
        (syntax-rules ()
          ([and] #t)
          ([and test] test)
          ([and test1 test2 ...]
            (if test1 [and test2 ...] #f))))
    `, world)

    await Lisp.execute(`
      (define-syntax or
        (syntax-rules ()
          ([or] #f)
          ([or test] test)
          ([or test1 test2 ...]
            (let ([x test1])
              (if x x (or test2 ...))))))
    `, world)

    await Lisp.execute(`
      (define-syntax let
        (syntax-rules ()
          ((let ((variable init) ...) body ...)
            ((lambda (variable ...)
                body ...)
            init ...))
          ((let name ((variable init) ...) body ...)
            (letrec ((name (lambda (variable ...)
                            body ...)))
              (name init ...)))))
    `, world)

    await Lisp.execute(`
      (define-syntax let*
        (syntax-rules ()
          ((let* ((n1 e1) (n2 e2) (n3 e3) ...) body ...)
            (let ((n1 e1))
              (let* ((n2 e2) (n3 e3) ...) body ...)))
          ((let* ((name expression) ...) body ...)
            (let ((name expression) ...) body ...))))
    `, world)

    await Lisp.execute(`
      (define-syntax letrec
        (syntax-rules ()
          ((letrec ((variable init) ...) body ...)
            ((lambda ()
              (define variable init) ...
              body ...)))))
    `, world)

    await Lisp.execute(`
      (define-syntax do
        (syntax-rules ()
          ((do ((variable init step ...) ...)
              (test expression ...)
              command ...)
            (let loop ((variable init) ...)
              (if test
                  (begin expression ...)
                  (begin
                    command ...
                    (loop (do "step" variable step ...) ...)))))
          ((do "step" variable)
            variable)
          ((do "step" variable step)
            step)))
    `, world)

    world.lexicalEnv.set('let-syntax', world.lexicalEnv.get('let'))
    world.lexicalEnv.set('letrec-syntax', world.lexicalEnv.get('letrec'))

    await Lisp.execute(`
      (define-syntax delay
        (syntax-rules ()
          ((delay expression)
            (let ((forced #f)
                  (memo #f))
              (lambda ()
                (if forced
                    memo
                    (begin
                      (set! memo expression)
                      (set! forced #t)
                      memo)))))))
    `, world)

    // await Lisp.execute(
    //   "(define-syntax quasiquote (syntax-rules (unquote unquote-splicing) " +
    //   " (`,expr                 expr)" +
    //   " (`(,@first . rest)      (append first `rest))" +
    //   " (`(first . rest)        (cons `first `rest))" +
    //   " (`#(,@first rest ...)   (list->vector `(,@first rest ...)))" +
    //   " (`#(expr ...)           (list->vector `(expr ...)))" +
    //   " (`expr                  'expr)))"
    // , world)

    //  - 6. Standard procedures
    // - 6.1 Equivalence Predicates

    env.define('eqv?', ['a', 'b'], ([a, b]: any) => Util.toL(Util.isEqv(a, b)));
    env.define('eq?', ['a', 'b'], ([a, b]: any) => Util.toL(Util.isEq(a, b)));
    env.define('equal?', ['a', 'b'], ([a, b]: any) => Util.toL(Util.isEqual(a, b)));
    // END - 6.1 Equivalence predicates

    // - 6.2 Numbers
    // - 6.2.5 Numerical operations
    env.define('number?', ['n'], ([n]: any) => Util.toL(isNum(n)));
    // procedure: complex? obj
    // procedure: real? obj
    // procedure: rational? obj
    // procedure: integer? obj
    // procedure: exact? z
    // procedure: inexact? z
    env.define('=', ['args'], ([l, r]: any) => Util.toL(l === r));
    env.define('>', ['args'], ([l, r]: any) => Util.toL(l > r));
    env.define('<', ['args'], ([l, r]: any) => Util.toL(l < r));
    env.define('>=', ['args'], ([l, r]: any) => Util.toL(l >= r));
    env.define('<=', ['args'], ([l, r]: any) => Util.toL(l <= r));
    env.define('zero?', ['n'], ([n]: any) => Util.toL(n === 0));
    env.define('positive?', ['n'], ([n]: any) => Util.toL(isNum(n) && n > 0));
    env.define('negative?', ['n'], ([n]: any) => Util.toL(isNum(n) && n < 0));
    env.define('odd?', ['n'], ([n]: any) => Util.toL(isNum(n) && n % 2 !== 0));
    env.define('even?', ['n'], ([n]: any) => Util.toL(isNum(n) && n % 2 === 0));
    env.define('max', 'args', (args: any) => args.reduce((acc: any, val: any) => Math.max(acc, val)));
    env.define('min', 'args', (args: any) => args.reduce((acc: any, val: any) => Math.min(acc, val)));
    env.define('+', 'args', (args: any) => {
      Util.assert(Array.isArray(args) && args.every(isNum), 'Passed a non-number to (+)')
      return args.reduce((acc: number, val: number) => acc + val, 0)
    });
    env.define('*', 'args', (args: any) => {
      Util.assert(Array.isArray(args) && args.every(isNum), 'Passed a non-number to (*)')
      return args.reduce((acc: number, val: number) => acc * val, 1)
    });
    env.define('-', 'args', (args: any) => {
      Util.assert(args.length > 0, "procedure requires at least one argument: (-)")
      Util.assert(Array.isArray(args) && args.every(isNum), 'Passed a non-number to (-)')
      if (args.length === 1) return -args[0]
      else return args.reduce((acc: number, val: number) => acc - val)
    });
    env.define('/', 'args', (args: any) => {
      Util.assert(args.length > 0, "procedure requires at least one argument: (/)")
      Util.assert(Array.isArray(args) && args.every(isNum), 'Passed a non-number to (/)')
      return args.reduce((acc: number, val: number) => acc / val)
    });
    env.define('abs', ['n'], ([n]: any) => Math.abs(n));
    env.define('quotient', ['x', 'y'], ([x, y]: any) => {
      Util.assert(isNum(x), 'Arguments to quotient should be numbers')
      Util.assert(isNum(y), 'Arguments to quotient should be numbers')
      return (x/y)|0
    });
    // procedure: remainder n1 n2
    // procedure: modulo n1 n2
    env.define('gcd', ['a', 'b'], ([a, b]: any) => { return Util.gcd(a, b) });
    env.define('lcm', ['a', 'b'], ([a, b]: any) => { return Util.lcm(a, b) });
    // procedure: numerator q
    // procedure: denominator q
    env.define('floor', ['n'], ([n]: any) => Math.floor(n));
    env.define('ceiling', ['n'], ([n]: any) => Math.ceil(n));
    env.define('truncate', ['n'], ([n]: any) => Math.trunc(n));
    env.define('round', ['n'], ([n]: any) => Math.round(n));
    // library procedure: rationalize x y
    env.define('exp', ['n'], ([n]: any) => Math.exp(n));
    env.define('log', ['n'], ([n]: any) => Math.log(n));
    env.define('sin', ['n'], ([n]: any) => Math.sin(n));
    env.define('cos', ['n'], ([n]: any) => Math.cos(n));
    env.define('tan', ['n'], ([n]: any) => Math.tan(n));
    env.define('asin', ['n'], ([n]: any) => Math.asin(n));
    env.define('acos', ['n'], ([n]: any) => Math.acos(n));
    env.define('atan', ['y', 'x'], ([y, x]: any) => isNullOrUndefined(x) ? Math.atan(y) : Math.atan2(y, x));
    env.define('sqrt', ['n'], ([n]: any) => Math.sqrt(n));
    // procedure: expt z1 z2
    // procedure: make-rectangular x1 x2
    // procedure: make-polar x3 x4
    // procedure: real-part z
    // procedure: imag-part z
    // procedure: magnitude z
    // procedure: angle z
    // procedure: exact->inexact z
    // procedure: inexact->exact z
    // END - 6.2.5 Numerical operations

    // - 6.2.6 Numerical input and output
    env.define('number->string', ['n', 'radix?'], ([n, radix]: any) => {
      Util.assert(isNum(n), `"number->string" procedure takes a 'number' as an argument`);
      return n.toString(radix ?? 10);
    });
    env.define('string->number', ['n', 'radix?'], ([n, radix]: any) => {
      Util.assert(isString(n), `"string->number" procedure takes a 'string' as an argument`);
      return parseInt(n, radix ?? 10);
    });
    // END - 6.2.6 Numerical input and output

    // - 6.3 Other Data Types

    // - 6.3.1 Booleans
    env.set('#t', TRUE);
    env.set('#f', FALSE);
    env.define('not', ['obj'], ([obj]: any) => obj === FALSE ? TRUE : FALSE);
    env.define('boolean?', ['obj'], ([obj]: any) => Util.toL(obj === TRUE || obj === FALSE));
    // END - 6.3.1 Booleans

    // - 6.3.2 Pairs and lists
    env.define('pair?', ['obj'], ([obj]: any) => Util.toL(isPair(obj)));
    env.define('cons', ['a', 'b'], ([a, b]: any) => new Pair(a, b));
    env.define('car', 'args', (args: any) => Lisp.car(args));
    env.define('cdr', 'args', (args: any) => Lisp.cdr(args));
    env.define('set-car!', ['l', 'v'], ([l, v]: any) => {
      if (Pair.is(l))
        l.car = v
      else l[0] = v
    });
    env.define('set-cdr!', ['l', 'v'], ([l, v]: any) => {
      Util.assert(isPair(l), 'invalid `set-cdr!` argument: ' + toString(l))
      l.cdr = v
    });

    // library procedure: caar pair
    // library procedure: cadr pair
    // ...: ...
    // library procedure: cdddar pair
    // library procedure: cddddr pair

    env.define('null?', ['n'], ([n]: any) => Util.toL(isEmpty(n)));
    env.define('list?', ['n'], ([n]: any) => Util.toL(isEmpty(n) || (isPair(n) && n.isList())));
    env.define('list', 'args', (args: any) => list(...args));
    env.define('length', ['list'], ([list]: any) => {
      Util.assert(isList(list), 'argument to length must be a list')
      return isPair(list) ? list.length : /* empty */ 0
    });

    env.define('append', 'args', (args: any) => {
      Util.assert(!Array.isArray(args[0]), 'Bad append! Bad!')
      return Util.append(args[0], ...args.slice(1))
    })

    env.define('reverse', ['list'], ([lst]: any) => {
      Util.assert(isList(lst))
      if (isEmpty(lst))
        return lst
      const copy = [...lst]
      return list(...copy.reverse())
    });

    // library procedure: list-tail list k
    // library procedure: list-ref list k

    // library procedure: memq obj list
    // library procedure: memv obj list
    // library procedure: member obj list

    // library procedure: assq obj alist
    // library procedure: assv obj alist
    // library procedure: assoc obj alist
    // END - 6.3.2 Pairs and lists

    // - 6.3.3 Symbols
    env.define('symbol?', ['n'], ([n]: any) => {
      return Util.toL(isSym(n) && !isEmpty(n))
    });
    env.define('symbol->string', ['n'], ([n]: any) => {
      Util.assert(isSym(n), `"symbol->string" procedure takes a 'symbol' as an argument`);
      return toString(n)
    });
    env.define('string->symbol', ['n'], ([n]: any) => {
      Util.assert(isString(n), `"string->symbol" procedure takes a 'string' as an argument`);
      return Sym(n)
    });
    // END - 6.3.3 Symbols

    // - 6.3.4 Characters
    env.define('char?', ['obj'], ([obj]: any) => {
      return Util.toL(isChar(obj))
    });
    // procedure: char=? char1 char2
    // procedure: char<? char1 char2
    // procedure: char>? char1 char2
    // procedure: char<=? char1 char2
    // procedure: char>=? char1 char2

    // library procedure: char-ci=? char1 char2
    // library procedure: char-ci<? char1 char2
    // library procedure: char-ci>? char1 char2
    // library procedure: char-ci<=? char1 char2
    // library procedure: char-ci>=? char1 char2

    // library procedure: char-alphabetic? char
    // library procedure: char-numeric? char
    // library procedure: char-whitespace? char
    // library procedure: char-upper-case? letter
    // library procedure: char-lower-case? letter

    // procedure: char->integer char
    // procedure: integer->char n

    // library procedure: char-upcase char
    // library procedure: char-downcase char
    // END - 6.3.4 Characters

    // - 6.3.5 Strings
    env.define('string?', ['n'], ([n]: any) => Util.toL(isString(n)));
    env.define('string', 'args', args => {
      Util.assert(Array.isArray(args) && args.every(isChar), 'Arguments to `string` must be `char`s')
      return args.map(arg => toString(arg)).join('')
    });
    env.define('make-string', ['k', 'char'], ([k, char = ' ']: any) => {
      Util.assert(isChar(char), 'make-string arg(2) expects a char')
      Util.assert(isNum(k), 'make-string arg(1) expects a number')
      return char.displayText.repeat(k)
    });
    env.define('string-length', ['n'], ([n]: any) => {
      Util.assert(isString(n))
      return n.length
    });
    env.define('string-ref', ['string', 'k'], ([string, k]: any) => {
      Util.assert(isString(string) && string.length >= k)
      Util.assert(isNum(k), format('Invalid `k` param passed to `string-ref`, expected number, got `%s`', typeof k))
      return new Character(string[k])
    });
    env.define('string-set!', ['string', 'k', 'char'], ([string, k, char]: any) => {
      Util.assert(isString(string) && string.length > k)
      Util.assert(isChar(char))
      string = char.displayText
      return UNDEF
    });
    env.define('string->input-port', ['string'], ([string]: any) => {
      Util.assert(isString(string))
      return InPort.fromString(string)
    });
    env.define('string=?', ['string1', 'string2'], ([string1, string2]: any) => {
      Util.assert(isString(string1) && isString(string2))
      return Util.toL(string1 === string2)
    });
    env.define('string-ci=?', ['string1', 'string2'], ([string1, string2]: any) => {
      Util.assert(isString(string1) && isString(string2))
      if (string1.length !== string2.length)
        return Util.toL(false)
      for (let i = 0; i < string1.length; i++) {
        if ((<string>string1[i]).toLowerCase() === string2[i].toLowerCase())
          continue
        return Util.toL(false)
      }
      return Util.toL(true)
    });
    env.define('string<?', ['string1', 'string2'], ([string1, string2]: any) => {
      Util.assert(isString(string1) && isString(string2))
      return Util.toL(string1 < string2)
    });
    env.define('string>?', ['string1', 'string2'], ([string1, string2]: any) => {
      Util.assert(isString(string1) && isString(string2))
      return Util.toL(string1 > string2)
    });
    env.define('string<=?', ['string1', 'string2'], ([string1, string2]: any) => {
      Util.assert(isString(string1) && isString(string2))
      return Util.toL(string1 <= string2)
    });
    env.define('string>=?', ['string1', 'string2'], ([string1, string2]: any) => {
      Util.assert(isString(string1) && isString(string2))
      return Util.toL(string1 >= string2)
    });
    env.define('string-ci<?', ['string1', 'string2'], ([string1, string2]: any) => {
      Util.assert(isString(string1) && isString(string2))
      return Util.toL(string1.toLowerCase() < string2.toLowerCase())
    });
    env.define('string-ci>?', ['string1', 'string2'], ([string1, string2]: any) => {
      Util.assert(isString(string1) && isString(string2))
      return Util.toL(string1.toLowerCase() > string2.toLowerCase())
    });
    env.define('string-ci<=?', ['string1', 'string2'], ([string1, string2]: any) => {
      Util.assert(isString(string1) && isString(string2))
      return Util.toL(string1.toLowerCase() <= string2.toLowerCase())
    });
    env.define('string-ci>=?', ['string1', 'string2'], ([string1, string2]: any) => {
      Util.assert(isString(string1) && isString(string2))
      return Util.toL(string1.toLowerCase() >= string2.toLowerCase())
    });
    env.define('substring', ['string', 'start', 'end'], ([string, start, end]: any) => {
      Util.assert(isString(string))
      return (<string>string).slice(start, end)
    });
    env.define('string-append', ['string', '...xs'], ([string, ...xs]: any) => {
      Util.assert(isString(string))
      return (<string>string).concat(...xs)
    });
    env.define('string->list', ['string', '...'], ([string]: any) => {
      Util.assert(isString(string))
      return (<string>string).split('')
    });
    env.define('list->string', ['list', '...'], ([list]: any) => {
      Util.assert(isPair(list) && list.every(isString))
      return list.toArray().join('')
    });
    env.define('string-copy', ['string'], ([string]: any) => {
      Util.assert(isString(string))
      return String(string)
    });
    env.define('string-fill', ['string', 'char'], ([string, char]: any) => {
      Util.assert(isString(string))
      Util.assert(isChar(char))
      string.replaceAll(/.*/, char.displayText)
      return string
    });
    env.define('string-pad-end', ['string', 'maxLength', '.', 'fillString'], ([string, maxLength, ...[fillString]]: any) => {
      Util.assert(isString(string));
      Util.assert(isNum(maxLength));
      return (string as string).padEnd(maxLength, fillString);
    });
    env.define('string-pad-start', ['string', 'maxLength', '.', 'fillString'], ([string, maxLength, ...[fillString]]: any) => {
      Util.assert(isString(string));
      Util.assert(isNum(maxLength));
      return (string as string).padStart(maxLength, fillString);
    });
    // END - 6.3.5 Strings

    // - 6.3.6 Vectors
    env.define('vector?', ['obj'], ([obj]: any) => {
      return Util.toL(isVec(obj))
    });
    env.define('make-vector', ['k', 'fill?'], ([k, fill = 0]: any) => {
      Util.assert(isNum(k), 'make-vector not given a size')
      // Util.assert(fill === undefined, 'make-vector fill option not implemented')
      const arr = Array(k).fill(fill);
      return new Vector(arr);
    });
    env.define('vector', 'args', (args: any) => {
      return new Vector(args)
    });
    env.define('vector-length', ['vec'], ([vec]: any) => {
      Util.assert(isVec(vec), `vector-length expected a 'Vector' but got '${typeof vec}'`)
      return vec.data.length
    });
    env.define('vector-ref', ['vec', 'k'], ([vec, k]: any) => {
      Util.assert(isVec(vec), `vector-ref arg(1) expected a 'Vector' but got '${typeof vec}'`)
      Util.assert(isNum(k), `vector-ref arg(2) expected a Number. Got: ${typeof k}`)
      return vec.data[k]
    });
    env.define('vector-set!', ['vec', 'k', 'obj'], ([vec, k, obj]: any, a) => {
      if (!isVec(vec))
        debugger
      Util.assert(isVec(vec), `vector-set! arg(1) expected a 'Vector' but got '${typeof vec}'`)
      Util.assert(isNum(k), `vector-set! arg(2) expected a Number. Got: ${typeof k}`)
      Util.assert(obj !== undefined, `vector-set! arg(3) is undefined`)
      return vec.data[k] = obj
    });
    env.define('vector->list', ['vec'], ([vec]: any) => {
      Util.assert(isVec(vec), `vector-list expected a 'Vector' but got '${typeof vec}'`)
      return list(...vec.data)
    });
    env.define('list->vector', ['list'], ([list]: any) => {
      Util.assert(isPair(list), `list->vector expected a list. Got: ${typeof list}`)
      return new Vector(list.toArray())
    });
    env.define('vector-fill!', ['vec', 'fill'], ([vec, fill]: any) => {
      Util.assert(isVec(vec), `vector-fill! expected a Vector. Got: ${typeof vec}`)
      Util.assert(fill, `vector-fill! arg(2) expected an argument`)
      for (let i = 0; i < vec.data.length; i++) {
        vec.data[i] = fill
      }
    });
    // END - 6.3.6 Vectors

    // - 6.4 Control Features
    env.define('procedure?', ['obj'], ([obj]: any) => {
      return Util.toL(isProc(obj) || isNativeProc(obj))
    });
    env.define('apply', 'args', async (args_: any, env) => {
      const [proc, args] = args_
      Util.assert(isCallable(proc), 'called apply with a non procedure')
      Util.assert(isList(args), 'called apply with a non procedure')
      return await proc.call(args, env)
    });
    env.define('map', ['proc', '.', 'args'], async ([proc, ...lists]: any) => {
      Util.assert(Array.isArray(lists) && lists.length >= 1 && lists.every(isPair), 'error 3823489')
      Util.assert(isCallable(proc), 'error 823746')
      const first = lists[0]
      Util.assert(isPair(first), 'error 1364')
      const res: any[] = [];
      for (let i = 0; i < first.length; i++) {
        res[i] = []
        lists.forEach(l => {
          res[i].push(l.at(i))
        })
        res[i] = await proc.call(list(...res[i]), env)
      }

      const a = list(...res)
      return a
    });
    // library procedure: for-each proc list1 list2 ...
    // library procedure: force promise
    env.define('call-with-current-continuation', ['throw'], async ([proc]: any, env: iEnv) => {
      const ball = new RuntimeWarning("Sorry, can't continue this continuation any longer.");
      env.define('throw', ['retval'], ([retval]: any) => {
        ball.retval = retval; throw ball;
      });
      if (isProc(proc) || isNativeProc(proc)) {
        try {
          return await proc.call(list(env.get('throw')), env);
        } catch (err) {
          if (err instanceof RuntimeWarning) {
            return ball.retval;
          }
          else {
            throw err;
          }
        }
      }
      throw new InvalidCallableExpression(proc);
    });

    // procedure: values obj ...
    // procedure: call-with-values producer consumer
    // procedure: dynamic-wind before thunk after
    // END - 6.4 Control Features

    // - 6.5 Eval
    // procedure: eval expression environment-specifier
    // procedure: scheme-report-environment version
    // procedure: null-environment version
    // optional procedure: interaction-environment
    // END - 6.5 Eval

    // 6.6 Input and output

    // - 6.6.1 Ports
    env.define('call-with-input-file', ['string', 'proc'], ([string, proc]: any) => {
      throw new NotImplementedError('call-with-input-file')
    });

    env.define('input-port?', ['obj'], ([obj]: any) => Util.toL(isInputPort(obj)));
    env.define('output-port?', ['obj'], ([obj]: any) => Util.toL(isOutputPort(obj)));

    env.define('current-input-port', [], () => env.get('*current-input-port*'));
    env.define('current-output-port', [], () => env.get('*current-output-port*'));

    env.define('set-current-input-port!', ['port'], ([port]: any) => env.set('*current-input-port*', port));
    env.define('set-current-output-port!', ['port'], ([port]: any) => env.set('*current-output-port*', port));
    // END - 6.6.1 Ports

    // - 6.6.2 Input
    env.define('read', ['port'], async ([port]: any) => {
      const p: InPort = port ?? currentInputPort(world)
      // console.log('reading port', p.name)
      const data = await read(p, world);
      // console.log('reading port (data):', data)
      return data
    });

    env.define('read-char', ['port'], async ([port]: any) => {
      const p: InPort = port ?? currentInputPort(world)
      return await p.readChar()
    });

    env.define('peek-char', ['port'], async ([port]: any) => {
      const p: InPort = port ?? currentInputPort(world)
      return await p.peekChar()
    });

    env.define('eof-object?', ['obj'], ([obj]: any) => Util.toL(isEofString(obj)));

    env.define('char-ready?', ['port'], ([port]: any) => {
      const p: InPort = port ?? currentInputPort(world)
      return Util.toL(isEofString(p) || p.charReady())
    });
    // END - 6.6.2 Input

    // - 6.6.3 Output
    env.define('putchar', ['char', 'port?'], ([obj, port]: any) => {
      const p: OutPort = port ?? currentOutputPort(world)
      p.write(obj)
      return
    });
    env.define('write', ['obj', 'port?'], ([obj, port]: any) => {
      const p: OutPort = port ?? currentOutputPort(world)
      p.write(toString(obj))
      return
    });
    env.define('writeln', ['obj', 'port?'], ([obj, port]: any) => {
      const p: OutPort = port ?? currentOutputPort(world)
      p.write(toString(obj))
      p.write('\n')
      return
    });
    env.define('display', ['obj', 'port?'], ([obj, port]: any) => {
      const p: OutPort = port ?? currentOutputPort(world)
      p.write(obj)
      return
    });
    env.define('newline', ['port?'], ([port]: any) => {
      const p: OutPort = port ?? currentOutputPort(world)
      p.write('\n')
    });
    env.define('write-char', ['char', 'port?'], ([char, port]: any) => {
      Util.assert(isChar(char), `not a character: ${char}`)
      const p: OutPort = port ?? currentOutputPort(world)
      p.write(char.displayText)
      return
    });
    // END - 6.6.3 Output

    // - 6.6.4 System interface
    // optional procedure: load filename
    // optional procedure: transcript-on filename
    // optional procedure: transcript-off
    // END - 6.6.4 System interface

  }
  //#endregion

  // #region [ rgba(0, 100, 0, 0.3)] - SRFI
  //  - SRFIs

  if (options.srfi) {

    // SRFI 6: Basic String Ports
    env.define('open-input-string', ['string'], ([string]: any) => {
      Util.assert(typeof string === 'string')
      return InPort.fromString(string)
    });
    /// END SRFI 6

  }
  // #endregion

  // #region [ rgba(0, 100, 0, 0.3)] - Misc

  if (options.misc) {

    env.define('macroexpand', ['expr'], async ([expr]: any) => {
      // console.log(toStringSafe(env.get<Procedure>('equal?').expr))
      const rv = await expand(expr, true, world);
      return rv
    });

    env.define('tokenize', ['expr'], async ([expr]: any) => {
      return await Lisp.tokenize(expr, world)
    });

    env.define('putchar2', ['char1', 'char2', 'port?'], ([obj1, obj2, port]: any) => {
      const p: OutPort = port ?? currentOutputPort(world)
      p.write(obj1)
      p.write(obj2)
      return
    });

    env.define('try', ['callable'], async ([callable]: any) => {
      try {
        if (isProc(callable) || isNativeProc(callable)) {
          const rv = await callable.call(EMPTY);
          return cons(TRUE, rv);
        }
        return cons(FALSE, 'InvalidCallableExpression')
      } catch (err) {
        if (err instanceof Error)
          return cons(FALSE, err.message);
        if (typeof err === 'string')
          return cons(FALSE, err);
        return cons(FALSE, 'UnknownError');
      }
    });

    env.define('resume-from-error', [], () => {
      throw new Resume()
    })
  }
  // #endregion

  // #region [ rgba(0, 100, 0, 0.3)] - REPL

  if (options.repl) {

    env.define('exit-repl', [], () => {
      if (isT(world.env.get('*in-repl-mode*')))
        world.env.set('*in-repl-mode*', Util.toL(false))
      else
        throw new Error('not in repl mode');
    })

    env.define('set-current-repl-prompt!', ['prompt'], ([prompt]: any) => {
      world.env.set('*current-repl-prompt*', prompt)
    })

    env.define('get-current-repl-prompt', [], () => {
      return world.env.get('*current-repl-prompt*')
    })

    env.define('revert-to-default-repl-prompt!', [], () => {
      world.env.set('*current-repl-prompt*', world.env.get('*default-repl-prompt*'))
    })

    env.define('repl', [], async () => {
      world.env.set('*in-repl-mode*', Util.toL(true))

      let lastInput: any, lastExpand: any, lastOutput: any, greet = true;

      while (isT(world.env.get('*in-repl-mode*'))) {
        const p = currentInputPort(world)
        const o = currentOutputPort(world)
        try {
          if (greet) o.write(world.env.get('*current-repl-prompt*'))
          lastInput = await read(p, world);
          lastExpand = await expand(lastInput, true, world)
          lastOutput = await evaluate(lastExpand, world.env)
          const outputText = toStringSafe(lastOutput);
          o.write(outputText)
          if (!outputText.endsWith('\n'))
            o.write('\n')
        } catch (err) {
          if (err instanceof Resume) {
            world.env.set('*in-repl-mode*', Util.toL(false))
            throw err
          }
          if (err instanceof Error) {
            // if (err instanceof MissingParenthesisError) {
            //   multiline = true
            //   continue
            // }
            if (err instanceof UndefinedVariableError) {
              if (err.message.endsWith('#<eof-object>')) {
                continue
              }
            }
            o.write(err.message)
            lastOutput = err.message
            continue
          }

          console.log('error: ' + err)
          process.exit(1)
        }
      }
    });
  }
  // #endregion

  env.define('run-tests', [], async () => {
    await Lisp.execute('(load "tests/runner.scm")', world)
  });

}
