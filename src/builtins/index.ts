import highlight from "cli-highlight";
import { format, inspect } from "util";
import { isSyntax, Macro, Syntax } from "../core/callable/macro";
import { FALSE, NIL, TRUE, UNDEF } from "../core/const";
import { Character } from "../core/data/char";
import { Resume } from "../core/data/cont";
import { InvalidCallableExpression, NotImplementedError, RuntimeWarning, UndefinedVariableError } from "../core/data/error";
import { cons, list, Pair } from "../core/data/pair";
import { MutableString, Str } from "../core/data/string";
import { Sym } from "../core/data/sym";
import { Vector } from "../core/data/vec";
import { evaluate } from "../core/eval";
import { expand } from "../core/expand";
import { Form, List } from "../core/form";
import * as Lisp from "../core/lisp";
import { currentInputPort, currentOutputPort, InPort, OutPort } from "../core/port";
import { print, toDisplayString, toString, toStringSafe } from "../core/print";
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

export function addGlobals(
  world: iWorld,
  opts: AddGlobalsOptions = {}
) {

  const options = {...defaultOptions, ...opts }

  const {env, lexicalEnv} = world

  lexicalEnv.syntax('define-syntax', (args, scope) => {
    const macro = expand(Lisp.cadr(args), world);
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

    env.set('*default-repl-prompt*', Str('> '))
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
    env.define('print', ['...xs'], ([...xs]: any) => { console.log(...xs.map((x: Form) => toDisplayString(x))); });
    env.define('show', ['x'], x => { console.log(x); });
    env.define('describe', ['x'], ([x]: any) => Str(toString(x, false)));
    env.define('format', ['x'], ([x]: any) => Str(toString(x, true)));

    env.define('colorized', ['x', 'language?'], ([x, language]: any) => {
      language = language ?? 'scheme'
      Util.assert(isString(x), format('wrong type .. `%s` .. expected `string`', typeof x))
      return highlight(x.toString(), {language, ignoreIllegals: true})
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

    env.define('break', ['args'], (args: any, env) => { debugger; return evaluate(args, env); });
    env.define('debug-macro!', ['val'], ([val]: any) => { Syntax.debug = !isF(val) });

    env.define('gensym', [], () => Symbol());

    // env.define('set-macro-character', ['char', 'cb'], ([char, cb]: any, env) => {
    //   lexicalEnv.setFrom(char, (locals: any) => {
    //     const proc = evaluate(cb, env);
    //     if (isProc(proc)) {
    //       proc.env.define('read', ['read'], ([locals]: any) => locals.parse());
    //       proc.env.define('advance', ['advance'], ([locals]: any) => locals.advance());
    //       proc.env.define('current', ['current'], ([locals]: any) => locals.current());
    //       proc.env.define('isEOF', ['isEOF'], ([locals]: any) => locals.isEOF());
    //       proc.env.define('isSpace', ['isSpace'], ([locals]: any) => locals.isSpace());
    //       proc.env.define('isNewLine', ['isNewLine'], ([locals]: any) => locals.isNewLine());
    //       return evaluate(list(proc, locals, toString(char)), env);
    //     }
    //     throw new Error('Nope @ set-macro-character');
    //   });
    // });

    env.define('i/o-port?', ['obj'], ([obj]: any) => Util.toL(isIOPort(obj)));
    env.define('read-from-string', ['obj'], ([obj]: any) => Lisp.tokenize(obj, world));

    env.define('string-pad-end', ['string', 'maxLength', '.', 'fillString'], ([string, maxLength, ...[fillString]]: any) => {
      Util.assert(isString(string));
      Util.assert(isNum(maxLength));
      return Str(string.toString().padEnd(maxLength, fillString));
    });
    env.define('string-pad-start', ['string', 'maxLength', '.', 'fillString'], ([string, maxLength, ...[fillString]]: any) => {
      Util.assert(isString(string));
      Util.assert(isNum(maxLength));
      return Str(string.toString().padStart(maxLength, fillString));
    });
  }
  //#endregion

  /*
    "here! (1)" "(let ((a 1) (b 2)) (cons a b))"
    calling apply on: pp-loop
    "here! (2)"
    "here! (2.2)" (let ((a 1) (b 2)) (cons a b))
    "pair? (2.2)" #t
    "null? (2.2)" #f
    "here! (3)"
    called apply with a non procedure
  */
  // #region [ rgba(0, 50, 150, 0.5) ] - r5rs
  // - r5rs

  if (options.r5rs) {

    //  - 4.2 Derived Expressions
    Lisp.execute(`
      (define-syntax cond (syntax-rules (else =>)
        ([cond] #f)
        ([cond (else expr1 expr2 ...)]
          (begin expr1 expr2 ...))
        ([cond (test => function) clause ...]
          (let ((temp test))
            (if temp
                (function temp)
                (cond clause ...))))
        ([cond (test expression ...) clause ...]
          (if test
              (begin expression ...)
              (cond clause ...)))))
    `, world)

    Lisp.execute(`
      (define-syntax case (syntax-rules (else)
        ([case key] #f)
        ([case key (else expr1 expr2 ...)]
          (begin expr1 expr2 ...))
        ([case key
              ((cell ...) expr1 expr2 ...)
              clause ...]
          (let ((temp key))
            (if (member temp '(cell ...))
                (begin expr1 expr2 ...)
                (case temp
                      clause ...))))))
    `, world)

    Lisp.execute(`
      (define-syntax and (syntax-rules ()
        ([and] #t)
        ([and test] test)
        ([and test1 test2 ...]
          (let ((temp test1))
            (if (not temp)
                temp
                (and test2 ...))))))
    `, world)

    Lisp.execute(`
      (define-syntax or (syntax-rules ()
        ([or test] test)
        ([or test1 test2 ...]
          (let ((temp test1))
            (if temp
                temp
                (or test2 ...))))))
    `, world)

    Lisp.execute(`
      (define-syntax let (syntax-rules ()
        ((let ((variable init) ...) body ...)
          ((lambda (variable ...)
              body ...)
          init ...))
        ((let name ((variable init) ...) body ...)
          (letrec ((name (lambda (variable ...)
                          body ...)))
            (name init ...)))))
    `, world)

    Lisp.execute(`
      (define-syntax let* (syntax-rules ()
        ((let* ((n1 e1) (n2 e2) (n3 e3) ...) body ...)
          (let ((n1 e1))
            (let* ((n2 e2) (n3 e3) ...) body ...)))
        ((let* ((name expression) ...) body ...)
          (let ((name expression) ...) body ...))))
    `, world)

    Lisp.execute(`
      (define-syntax letrec (syntax-rules ()
        ((letrec ((variable init) ...) body ...)
          ((lambda ()
            (define variable init) ...
            body ...)))))
    `, world)

    world.lexicalEnv.set('let-syntax', world.lexicalEnv.get('let'))
    world.lexicalEnv.set('letrec-syntax', world.lexicalEnv.get('letrec'))

    Lisp.execute(`
      (define-syntax do (syntax-rules ()
        ((do ((variable init step ...) ...)   ; Allow 0 or 1 step
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

    Lisp.execute(`
      (define-syntax delay (syntax-rules ()
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

    // Lisp.execute(
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
      return Str(n.toString(radix ?? 10));
    });
    env.define('string->number', ['n', 'radix?'], ([n, radix = 10]: any) => {
      Util.assert(isString(n), `"string->number" procedure takes a 'string' as an argument`);
      if (n.toString().includes('e'))
        return parseInt(new Number(n.toString()).toFixed(0).toString(), radix);
      return parseInt(n.toString(), radix)
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

    env.define('reverse!', ['list'], ([lst]: any) => {
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
      return Str(n.description!)
    });
    env.define('string->symbol', ['n'], ([n]: any) => {
      Util.assert(isString(n), `"string->symbol" procedure takes a 'string' as an argument`);
      return Sym(n.toString())
    });
    // END - 6.3.3 Symbols

    // - 6.3.4 Characters
    env.define('char?', ['obj'], ([obj]: any) => {
      return Util.toL(isChar(obj))
    });
    env.define('char=?', ['char1', 'char2'], ([char1, char2]: any) => {
      return Util.toL(isChar(char1) && isChar(char2) && char1.equal(char2))
    });
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
    env.define('char-alphabetic?', ['char'], ([char]: any) => {
      // Note: The alphabetic characters are the 52 upper and lower case letters.
      Util.assert(isChar(char))
      return Util.toL(char.displayText.match(/[A-z]/) !== null)
    });
    // library procedure: char-numeric? char
    env.define('char-numeric?', ['char'], ([char]: any) => {
      // Note: The numeric characters are the ten decimal digits.
      Util.assert(isChar(char))
      return Util.toL(char.displayText.match(/[0-9]/) !== null)
    });
    // library procedure: char-whitespace? char
    env.define('char-whitespace?', ['char'], ([char]: any) => {
      // Note: The whitespace characters are space, tab, line feed, form feed, and carriage return.
      Util.assert(isChar(char))
      return Util.toL(char.displayText.match(/\s/) !== null)
    });
    // library procedure: char-upper-case? letter
    env.define('char-upper-case?', ['letter'], ([letter]: any) => {
      Util.assert(isChar(letter))
      return Util.toL(letter.displayText.match(/[A-Z]/) !== null)
    });
    // library procedure: char-lower-case? letter
    env.define('char-lower-case?', ['letter'], ([letter]: any) => {
      Util.assert(isChar(letter))
      return Util.toL(letter.displayText.match(/[a-z]/) !== null)
    });

    // procedure: char->integer char
    env.define('char->integer', ['char'], ([char]: any) => {
      Util.assert(isChar(char))
      Util.assert(char.displayText.match(/[0-9]/) !== null)
      return Number(char.displayText)
    });
    // procedure: integer->char n
    env.define('integer->char', ['n'], ([n]: any) => {
      Util.assert(isNum(n))
      return new Character(n.toString(10))
    });

    // library procedure: char-upcase char
    // library procedure: char-downcase char
    // END - 6.3.4 Characters

    // - 6.3.5 Strings
    env.define('string?', ['n'], ([n]: any) => Util.toL(isString(n)));
    env.define('string', 'args', args => {
      Util.assert(Array.isArray(args) && args.every(isChar), 'Arguments to `string` must be `char`s')
      return Str(args.map(arg => toString(arg)).join(''))
    });
    env.define('make-string', ['k', 'char'], ([k, char = ' ']: any) => {
      Util.assert(isChar(char), 'make-string arg(2) expects a char')
      Util.assert(isNum(k), 'make-string arg(1) expects a number')
      return Str(char.displayText.repeat(k))
    });
    env.define('string-length', ['n'], ([n]: any) => {
      Util.assert(isString(n))
      return n.length
    });
    env.define('string-ref', ['string', 'k'], ([string, k]: any) => {
      Util.assert(isString(string) && string.length >= k)
      Util.assert(isNum(k), format('Invalid `k` param passed to `string-ref`, expected number, got `%s`', typeof k))
      return new Character(string.toString()[k])
    });
    env.define('string-set!', ['string', 'k', 'char'], ([string, k, char]: any) => {
      Util.assert(isString(string) && string.length > k)
      Util.assert(isChar(char))
      string = char.displayText
      return UNDEF
    });
    env.define('string->input-port', ['string'], ([string]: any) => {
      Util.assert(isString(string))
      return InPort.fromString(string.toString())
    });
    env.define('string=?', ['string1', 'string2'], ([string1, string2]: any) => {
      Util.assert(isString(string1) && isString(string2))
      return Util.toL(string1.equal(string2))
    });
    env.define('string-ci=?', ['string1', 'string2'], ([string1, string2]: any) => {
      Util.assert(isString(string1) && isString(string2))
      if (string1.length !== string2.length)
        return Util.toL(false)
      return Util.toL(string1.toLowerCase() === string2.toLowerCase())
    });
    env.define('string<?', ['string1', 'string2'], ([string1, string2]: any) => {
      Util.assert(isString(string1) && isString(string2))
      return Util.toL(string1.toString() < string2.toString())
    });
    env.define('string>?', ['string1', 'string2'], ([string1, string2]: any) => {
      Util.assert(isString(string1) && isString(string2))
      return Util.toL(string1.toString() > string2.toString())
    });
    env.define('string<=?', ['string1', 'string2'], ([string1, string2]: any) => {
      Util.assert(isString(string1) && isString(string2))
      return Util.toL(string1.toString() <= string2.toString())
    });
    env.define('string>=?', ['string1', 'string2'], ([string1, string2]: any) => {
      Util.assert(isString(string1) && isString(string2))
      return Util.toL(string1.toString() >= string2.toString())
    });
    env.define('string-ci<?', ['string1', 'string2'], ([string1, string2]: any) => {
      Util.assert(isString(string1) && isString(string2))
      return Util.toL(string1.toString().toLowerCase() < string2.toString().toLowerCase())
    });
    env.define('string-ci>?', ['string1', 'string2'], ([string1, string2]: any) => {
      Util.assert(isString(string1) && isString(string2))
      return Util.toL(string1.toString().toLowerCase() > string2.toString().toLowerCase())
    });
    env.define('string-ci<=?', ['string1', 'string2'], ([string1, string2]: any) => {
      Util.assert(isString(string1) && isString(string2))
      return Util.toL(string1.toString().toLowerCase() <= string2.toString().toLowerCase())
    });
    env.define('string-ci>=?', ['string1', 'string2'], ([string1, string2]: any) => {
      Util.assert(isString(string1) && isString(string2))
      return Util.toL(string1.toString().toLowerCase() >= string2.toString().toLowerCase())
    });
    env.define('substring', ['string', 'start', 'end'], ([string, start, end]: any) => {
      Util.assert(isString(string))
      return Str(string.toString().slice(start, end))
    });
    env.define('string-append', ['string', '.', 'xs'], ([string, ...xs]: any) => {
      Util.assert(isString(string))
      return Str(string.toString().concat(...xs.map((s: MutableString) => s.toString())))
    });
    env.define('string->list', ['string'], ([string]: any) => {
      Util.assert(isString(string))
      return list(...string.toString().split('').map(c => new Character(c)))
    });
    env.define('list->string', ['list', '...'], ([list]: any) => {
      Util.assert(isPair(list) && list.every(isString))
      return Str(list.toArray().join(''))
    });
    env.define('string-copy', ['string'], ([string]: any) => {
      Util.assert(isString(string))
      return Str(string.toString())
    });
    env.define('string-fill!', ['string', 'char'], ([string, char]: any) => {
      Util.assert(isString(string))
      Util.assert(isChar(char))
      return string.replaceAll(/.*/, char.displayText)
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
    env.define('apply', ['proc', '.', 'args'], ([proc, ...args]: any, env) => {
      Util.assert(isCallable(proc), 'called apply with a non procedure')
      // Note: `Apply` calls `proc` with the elements of the list
      // `(append (list arg1 ...) args)` as the actual arguments.
      return proc.call(
        Util.append(list(...args.slice(0, -1)), args[args.length - 1])
      , env)
    });
    env.define('map', ['proc', '.', 'args'], ([proc, ...lists]: any) => {
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
        res[i] = proc.call(list(...res[i]), env)
      }

      const a = list(...res)
      return a
    });
    // library procedure: for-each proc list1 list2 ...
    // library procedure: force promise
    env.define('call-with-current-continuation', ['throw'], ([proc]: any, env: iEnv) => {
      const ball = new RuntimeWarning("Sorry, can't continue this continuation any longer.");

      const fn = env.define('throw', ['retval'], ([retval]: any) => {
        ball.retval = retval; throw ball;
      });

      if (isProc(proc) || isNativeProc(proc)) {
        try {
          return proc.call(list(fn), env);
        } catch (err) {
          if (err instanceof RuntimeWarning) {
            // console.log('RuntimeWarning (call/cc)')
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
    env.define('dynamic-wind', ['before', 'thunk', 'after'], ([before, thunk, after]: any, env: iEnv) => {
      Util.assert(isCallable(before))
      Util.assert(isCallable(thunk))
      Util.assert(isCallable(after))
      before.call(NIL, env);
      try {
        return thunk.call(NIL, env);
      } catch (err) {
        // console.log('RuntimeWarning (dynamic-wind)')
      } finally {
        after.call(NIL, env);
      }
    });
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
    env.define('read', ['port'], ([port]: any) => {
      const p: InPort = port ?? currentInputPort(world)
      // console.log('reading port', p.name)
      const data = read(p, world);
      // console.log('reading port (data):', data)
      return data
    });

    env.define('read-char', ['port'], ([port]: any) => {
      const p: InPort = port ?? currentInputPort(world)
      return p.readChar()
    });

    env.define('peek-char', ['port'], ([port]: any) => {
      const p: InPort = port ?? currentInputPort(world)
      return p.peekChar()
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
      return NIL
    });

    // NOTE: `Write' is intended for producing machine-readable output and `display' is for producing human-readable output.
    env.define('write', ['obj', 'port?'], ([obj, port]: any) => {
      const p: OutPort = port ?? currentOutputPort(world)
      p.write(toString(obj))
      return NIL
    });
    env.define('display', ['obj', 'port?'], ([obj, port]: any) => {
      const p: OutPort = port ?? currentOutputPort(world)
      const s = toString(obj, undefined, undefined, false);
      console.log('displaying to port:', p.name, s)
      p.write(s)
      return NIL
    });
    env.define('newline', ['port?'], ([port]: any) => {
      const p: OutPort = port ?? currentOutputPort(world)
      p.write('\n')
      return NIL
    });
    env.define('write-char', ['char', 'port?'], ([char, port]: any) => {
      Util.assert(isChar(char), `not a character: ${char}`)
      const p: OutPort = port ?? currentOutputPort(world)
      p.write(char.displayText)
      console.log('write-char', char.sym.description!)
      return NIL
    });
    env.define('displayln', ['obj', 'port?'], ([obj, port]: any) => {
      const p: OutPort = port ?? currentOutputPort(world)
      p.write(toString(obj, undefined, undefined, false))
      p.write('\n')
      return NIL
    });
    env.define('writeln', ['obj', 'port?'], ([obj, port]: any) => {
      const p: OutPort = port ?? currentOutputPort(world)
      p.write(toString(obj))
      p.write('\n')
      return NIL
    });
    env.define('display-to-string', ['obj'], ([obj]: any) => {
      console.log('displaying to string i guess, lol', {obj})
      return Str(toString(obj, undefined, undefined, false))
    });
    env.define('write-to-string', ['obj'], ([obj]: any) => {
      console.log('writing to string i guess, lol', {obj})
      return Str(toString(obj))
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

    env.define('macroexpand', ['expr'], ([expr]: any) => {
      // console.log(toStringSafe(env.get<Procedure>('equal?').expr))
      const rv = expand(expr, world, true);
      return rv
    });

    env.define('tokenize', ['expr'], ([expr]: any) => {
      return Lisp.tokenize(expr, world)
    });

    env.define('putchar2', ['char1', 'char2', 'port?'], ([obj1, obj2, port]: any) => {
      const p: OutPort = port ?? currentOutputPort(world)
      p.write(obj1)
      p.write(obj2)
      return
    });

    env.define('try', ['callable'], ([callable]: any) => {
      try {
        if (isProc(callable) || isNativeProc(callable)) {
          const rv = callable.call(NIL);
          return cons(TRUE, rv);
        }
        return cons(FALSE, Str('InvalidCallableExpression'))
      } catch (err) {
        if (err instanceof RuntimeWarning) {
          console.log('RuntimeWarning (try)')
          return err.retval // continuation
          // throw err
        }
        if (err instanceof Error) {
          return cons(FALSE, Str(err.message));
        }
        if (typeof err === 'string')
          return cons(FALSE, Str(err));
        return cons(FALSE, Str('UnknownError'));
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

    env.define('repl', [], () => {
      world.env.set('*in-repl-mode*', Util.toL(true))

      let lastInput: any, lastExpand: any, lastOutput: any, greet = true;

      while (isT(world.env.get('*in-repl-mode*'))) {
        const p = currentInputPort(world)
        const o = currentOutputPort(world)
        try {
          if (greet) o.write(world.env.get<MutableString>('*current-repl-prompt*'))
          lastInput = read(p, world);
          lastExpand = expand(lastInput, world, true)
          lastOutput = evaluate(lastExpand, world.env)
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

  env.define('run-tests', [], () => {
    Lisp.execute('(load "tests/runner.scm")', world)
  });

}
