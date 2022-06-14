import highlight from "cli-highlight";
import { format, inspect } from "util";
import { EMPTY, FALSE, NIL, TRUE, UNDEF } from "../core/const";
import { Resume } from "../core/data/cont";
import { Character } from "../core/data/char";
import { cons, list, Pair } from "../core/data/pair";
import { isSyntaxRulesDef, SyntaxRulesDef } from "../core/data/syntax";
import { Vector } from "../core/data/vec";
import { InvalidCallableExpression, NotImplementedError, RuntimeWarning, UndefinedVariableError } from "../core/data/error";
import { evaluate } from "../core/eval";
import { expand } from "../core/expand";
import { Form } from "../core/form";
import * as Lisp from "../core/lisp";
import { currentInputPort, currentOutputPort, InPort, OutPort } from "../core/data/port";
import { read } from "../core/read";
import { Sym } from "../core/data/sym";
import { print, toString, toStringSafe } from "../core/print";
import { isCallable, isChar, isEmpty, isEofString, isF, isInputPort, isIOPort, isList, isNativeProc, isNone, isNum, isOutputPort, isPair, isProc, isString, isSym, isT, isVec } from "../guard";
import { iEnv } from "../interface/iEnv";
import { iWorld } from "../interface/iWorld";
import * as Util from "../utils";
import { AddGlobalsOptions } from "./Options";
import { mkNativeProc } from "./utils";

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

  const {env, readerEnv, lexicalEnv} = world

  if (options.minimal) {
    env.set('#t', TRUE);
    env.set('#f', FALSE);

    mkNativeProc(env, 'boolean?', ['obj'], ([obj]: any) => Util.toL(obj === TRUE || obj === FALSE));
    mkNativeProc(env, 'char?', ['obj'], ([obj]: any) => Util.toL(isChar(obj)));
    mkNativeProc(env, 'vector?', ['obj'], ([obj]: any) => Util.toL(isVec(obj)));
    mkNativeProc(env, 'procedure?', ['obj'], ([obj]: any) => Util.toL(isProc(obj) || isNativeProc(obj)));
    mkNativeProc(env, 'pair?', ['obj'], ([obj]: any) => Util.toL(isPair(obj)));
    mkNativeProc(env, 'number?', ['n'], ([n]: any) => Util.toL(isNum(n)));
    mkNativeProc(env, 'string?', ['n'], ([n]: any) => Util.toL(isString(n)));
    mkNativeProc(env, 'port?', ['obj'], ([obj]: any) => Util.toL(isInputPort(obj) || isOutputPort(obj) || isIOPort(obj)));

    mkNativeProc(env, 'null?', ['n'], ([n]: any) => Util.toL(isEmpty(n)));
    mkNativeProc(env, 'list?', ['n'], ([n]: any) => Util.toL(isEmpty(n) || (isPair(n) && n.isList())));

    mkNativeProc(env, 'eqv?', ['a', 'b'], ([a, b]: any) => Util.toL(Util.isEqv(a, b)));
    mkNativeProc(env, 'eq?', ['a', 'b'], ([a, b]: any) => Util.toL(Util.isEq(a, b)));
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

    mkNativeProc(env, 'locals-js', [], (_, a) => { return console.log(inspect(a.keys().sort(), { maxArrayLength: 10000 })) });
    mkNativeProc(env, 'locals', [], (_, a) => { return a.keys().sort().map(Sym); });
    mkNativeProc(env, 'env', [], () => { return env; });
    mkNativeProc(env, 'env->size', [], () => { return env.size(); });
    mkNativeProc(env, 'env->keys', [], () => { return env.keys().map(Sym); });
    mkNativeProc(env, 'env->values', [], () => { return env.values(); });
    mkNativeProc(env, 'env->entries', [], () => { return env.entries(); });

    mkNativeProc(env, 'debugnf', ['name', 'x'], ([name, x]: any) => { console.log('[DEBUG-NF]:', toString(name)); console.log(x); });
    mkNativeProc(env, 'debugn', ['name', 'x'], ([name, x]: any) => { console.log('[DEBUG-N]:', toString(name)); console.log(x); return x; });
    mkNativeProc(env, 'debugf', ['x'], x => { console.log('[DEBUG-F]'); console.log(x); });
    mkNativeProc(env, 'debug', ['x'], x => { console.log('[DEBUG]'); console.log(x); return x; });
    mkNativeProc(env, 'printn', ['name', 'x'], ([name, x]: any) => { console.log(toString(name), toString(x)); });
    mkNativeProc(env, 'printr', ['x'], ([x]: any) => { print(x); return x});
    mkNativeProc(env, 'prints', ['...xs'], ([...xs]: any) => { console.log(...xs); });
    mkNativeProc(env, 'print', ['...xs'], ([...xs]: any) => { console.log(...xs.map((x: any) => toString(x))); });
    mkNativeProc(env, 'show', ['x'], x => { console.log(x); });
    mkNativeProc(env, 'describe', ['x'], ([x]: any) => toString(x, true));
    mkNativeProc(env, 'format', ['x'], ([x]: any) => toString(x, true));

    mkNativeProc(env, 'colorized', ['x', 'language?'], async ([x, language]: any) => {
      language = language ?? 'scheme'
      Util.assert(isString(x), format('wrong type .. `%s` .. expected `string`', typeof x))
      return highlight(x, {language, ignoreIllegals: true})
    });

    mkNativeProc(env, 'inspect', ['x'], ([x]: any) => {
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
        if (isSyntaxRulesDef(x)) {
          const template = `
            Name: %s
            Type: %s

            Literals: %s
            Params: %s
          `
          return format(template, 'Syntax Rules Definition', x.name, toStringSafe(x.literals), x.params.map(v => toStringSafe(v)));
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

    mkNativeProc(env, 'break', ['args'], async (args: any, env) => { debugger; return await evaluate(args, env); });
    mkNativeProc(env, 'debug-macro!', ['val'], ([val]: any) => { SyntaxRulesDef.debug = !isF(val) });

    mkNativeProc(env, 'gensym', [], () => Symbol());

    mkNativeProc(env, 'set-macro-character', ['char', 'cb'], ([char, cb]: any, env) => {
      lexicalEnv.setFrom(char, async (locals: any) => {
        const proc = await evaluate(cb, env);
        if (isProc(proc)) {
          mkNativeProc(proc.env, 'read', ['read'], ([locals]: any) => locals.parse());
          mkNativeProc(proc.env, 'advance', ['advance'], ([locals]: any) => locals.advance());
          mkNativeProc(proc.env, 'current', ['current'], ([locals]: any) => locals.current());
          mkNativeProc(proc.env, 'isEOF', ['isEOF'], ([locals]: any) => locals.isEOF());
          mkNativeProc(proc.env, 'isSpace', ['isSpace'], ([locals]: any) => locals.isSpace());
          mkNativeProc(proc.env, 'isNewLine', ['isNewLine'], ([locals]: any) => locals.isNewLine());
          return await evaluate(list(proc, locals, toString(char)), env);
        }
        throw new Error('Nope @ set-macro-character');
      });
    });

    mkNativeProc(env, 'i/o-port?', ['obj'], ([obj]: any) => Util.toL(isIOPort(obj)));
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
          ((let ((name val) ...) body1 body2 ...)
            ((lambda (name ...) body1 body2 ...)
            val ...))))
    `, world)

    await Lisp.execute(`
      (define-syntax let*
        (syntax-rules ()
          ((let* () body1 body2 ...)
            (let () body1 body2 ...))
          ((let* ((name1 val1) (name2 val2) ...)
              body1 body2 ...)
            (let ((name1 val1))
              (let* ((name2 val2) ...)
                body1 body2 ...)))))
    `, world)

    // from: https://stackoverflow.com/questions/2835582/what-if-any-is-wrong-with-this-definition-of-letrec-in-scheme
    await Lisp.execute(`
    (define-syntax letrec
      (syntax-rules ()
        ((letrec ((name val) ...) body bodies ...)
        ((lambda ()
          (define name val) ... body bodies ...)))))
    `, world)

    await Lisp.execute(`
    (define-syntax do
      (syntax-rules ()
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

    //  - 6. Standard procedures
    // - 6.1 Equivalence Predicates

    mkNativeProc(env, 'eqv?', ['a', 'b'], ([a, b]: any) => Util.toL(Util.isEqv(a, b)));
    mkNativeProc(env, 'eq?', ['a', 'b'], ([a, b]: any) => Util.toL(Util.isEq(a, b)));
    mkNativeProc(env, 'equal?', ['a', 'b'], ([a, b]: any) => Util.toL(Util.isEqual(a, b)));
    // END - 6.1 Equivalence predicates

    // - 6.2 Numbers
    // - 6.2.5 Numerical operations
    mkNativeProc(env, 'number?', ['n'], ([n]: any) => Util.toL(isNum(n)));
    // procedure: complex? obj
    // procedure: real? obj
    // procedure: rational? obj
    // procedure: integer? obj
    // procedure: exact? z
    // procedure: inexact? z
    mkNativeProc(env, '=', ['args'], ([l, r]: any) => Util.toL(l === r));
    mkNativeProc(env, '>', ['args'], ([l, r]: any) => Util.toL(l > r));
    mkNativeProc(env, '<', ['args'], ([l, r]: any) => Util.toL(l < r));
    mkNativeProc(env, '>=', ['args'], ([l, r]: any) => Util.toL(l >= r));
    mkNativeProc(env, '<=', ['args'], ([l, r]: any) => Util.toL(l <= r));
    mkNativeProc(env, 'zero?', ['n'], ([n]: any) => Util.toL(n === 0));
    mkNativeProc(env, 'positive?', ['n'], ([n]: any) => Util.toL(isNum(n) && n > 0));
    mkNativeProc(env, 'negative?', ['n'], ([n]: any) => Util.toL(isNum(n) && n < 0));
    mkNativeProc(env, 'odd?', ['n'], ([n]: any) => Util.toL(isNum(n) && n % 2 !== 0));
    mkNativeProc(env, 'even?', ['n'], ([n]: any) => Util.toL(isNum(n) && n % 2 === 0));
    mkNativeProc(env, 'max', 'args', (args: any) => args.reduce((acc: any, val: any) => Math.max(acc, val)));
    mkNativeProc(env, 'min', 'args', (args: any) => args.reduce((acc: any, val: any) => Math.min(acc, val)));
    mkNativeProc(env, '+', 'args', (args: any) => {
      if (!(Array.isArray(args) && args.every(isNum)))
        debugger
      Util.assert(Array.isArray(args) && args.every(isNum), 'Passed a non-number to (+)')
      return args.reduce((acc: number, val: number) => acc + val, 0)
    });
    mkNativeProc(env, '*', 'args', (args: any) => {
      Util.assert(Array.isArray(args) && args.every(isNum), 'Passed a non-number to (*)')
      return args.reduce((acc: number, val: number) => acc * val, 1)
    });
    mkNativeProc(env, '-', 'args', (args: any) => {
      Util.assert(args.length > 0, "procedure requires at least one argument: (-)")
      Util.assert(Array.isArray(args) && args.every(isNum), 'Passed a non-number to (-)')
      if (args.length === 1) return -args[0]
      else return args.reduce((acc: number, val: number) => acc - val)
    });
    mkNativeProc(env, '/', 'args', (args: any) => {
      Util.assert(args.length > 0, "procedure requires at least one argument: (/)")
      Util.assert(Array.isArray(args) && args.every(isNum), 'Passed a non-number to (/)')
      return args.reduce((acc: number, val: number) => acc / val)
    });
    mkNativeProc(env, 'abs', ['n'], ([n]: any) => Math.abs(n));
    mkNativeProc(env, 'quotient', ['x', 'y'], ([x, y]: any) => {
      Util.assert(isNum(x), 'Arguments to quotient should be numbers')
      Util.assert(isNum(y), 'Arguments to quotient should be numbers')
      return (x/y)|0
    });
    // procedure: remainder n1 n2
    // procedure: modulo n1 n2
    mkNativeProc(env, 'gcd', ['a', 'b'], ([a, b]: any) => { return Util.gcd(a, b) });
    mkNativeProc(env, 'lcm', ['a', 'b'], ([a, b]: any) => { return Util.lcm(a, b) });
    // procedure: numerator q
    // procedure: denominator q
    mkNativeProc(env, 'floor', ['n'], ([n]: any) => Math.floor(n));
    mkNativeProc(env, 'ceiling', ['n'], ([n]: any) => Math.ceil(n));
    mkNativeProc(env, 'truncate', ['n'], ([n]: any) => Math.trunc(n));
    mkNativeProc(env, 'round', ['n'], ([n]: any) => Math.round(n));
    // library procedure: rationalize x y
    mkNativeProc(env, 'exp', ['n'], ([n]: any) => Math.exp(n));
    mkNativeProc(env, 'log', ['n'], ([n]: any) => Math.log(n));
    mkNativeProc(env, 'sin', ['n'], ([n]: any) => Math.sin(n));
    mkNativeProc(env, 'cos', ['n'], ([n]: any) => Math.cos(n));
    mkNativeProc(env, 'tan', ['n'], ([n]: any) => Math.tan(n));
    mkNativeProc(env, 'asin', ['n'], ([n]: any) => Math.asin(n));
    mkNativeProc(env, 'acos', ['n'], ([n]: any) => Math.acos(n));
    mkNativeProc(env, 'atan', ['y', 'x'], ([y, x]: any) => isNone(x) ? Math.atan(y) : Math.atan2(y, x));
    mkNativeProc(env, 'sqrt', ['n'], ([n]: any) => Math.sqrt(n));
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
    mkNativeProc(env, 'number->string', ['n', 'radix?'], ([n, radix]: any) => {
      Util.assert(isNum(n), `"number->string" procedure takes a 'number' as an argument`);
      return n.toString(radix ?? 10);
    });
    mkNativeProc(env, 'string->number', ['n', 'radix?'], ([n, radix]: any) => {
      Util.assert(isString(n), `"string->number" procedure takes a 'string' as an argument`);
      return parseInt(n, radix ?? 10);
    });
    // END - 6.2.6 Numerical input and output

    // - 6.3 Other Data Types

    // - 6.3.1 Booleans
    env.set('#t', TRUE);
    env.set('#f', FALSE);
    mkNativeProc(env, 'not', ['obj'], ([obj]: any) => obj === FALSE ? TRUE : FALSE);
    mkNativeProc(env, 'boolean?', ['obj'], ([obj]: any) => Util.toL(obj === TRUE || obj === FALSE));
    // END - 6.3.1 Booleans

    // - 6.3.2 Pairs and lists
    mkNativeProc(env, 'pair?', ['obj'], ([obj]: any) => Util.toL(isPair(obj)));
    mkNativeProc(env, 'cons', ['a', 'b'], ([a, b]: any) => new Pair(a, b));
    mkNativeProc(env, 'car', 'args', (args: any) => Lisp.car(args));
    mkNativeProc(env, 'cdr', 'args', (args: any) => Lisp.cdr(args));
    mkNativeProc(env, 'set-car!', ['l', 'v'], ([l, v]: any) => {
      if (Pair.is(l))
        l.car = v
      else l[0] = v
    });
    mkNativeProc(env, 'set-cdr!', ['l', 'v'], ([l, v]: any) => {
      Util.assert(isPair(l), 'invalid `set-cdr!` argument: ' + toString(l))
      l.cdr = v
    });

    // library procedure: caar pair
    // library procedure: cadr pair
    // ...: ...
    // library procedure: cdddar pair
    // library procedure: cddddr pair

    mkNativeProc(env, 'null?', ['n'], ([n]: any) => Util.toL(isEmpty(n)));
    mkNativeProc(env, 'list?', ['n'], ([n]: any) => Util.toL(isEmpty(n) || (isPair(n) && n.isList())));
    mkNativeProc(env, 'list', 'args', (args: any) => list(...args));
    mkNativeProc(env, 'length', ['list'], ([list]: any) => {
      Util.assert(isList(list), 'argument to length must be a list')
      return isPair(list) ? list.length : /* empty */ 0
    });

    mkNativeProc(env, 'append', 'args', (args: any) => {
      Util.assert(!Array.isArray(args[0]), 'Bad append! Bad!')
      return Util.append(args[0], ...args.slice(1))
    })

    mkNativeProc(env, 'reverse', ['list'], ([lst]: any) => {
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
    mkNativeProc(env, 'symbol?', ['n'], ([n]: any) => {
      return Util.toL(isSym(n) && !isEmpty(n))
    });
    mkNativeProc(env, 'symbol->string', ['n'], ([n]: any) => {
      Util.assert(isSym(n), `"symbol->string" procedure takes a 'symbol' as an argument`);
      return toString(n)
    });
    mkNativeProc(env, 'string->symbol', ['n'], ([n]: any) => {
      Util.assert(isString(n), `"string->symbol" procedure takes a 'string' as an argument`);
      return Sym(n)
    });
    // END - 6.3.3 Symbols

    // - 6.3.4 Characters
    mkNativeProc(env, 'char?', ['obj'], ([obj]: any) => {
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
    mkNativeProc(env, 'string?', ['n'], ([n]: any) => Util.toL(isString(n)));
    mkNativeProc(env, 'string', 'args', args => {
      Util.assert(Array.isArray(args) && args.every(isChar), 'Arguments to `string` must be `char`s')
      return args.map(arg => toString(arg)).join('')
    });
    mkNativeProc(env, 'make-string', ['k', 'char'], ([k, char = ' ']: any) => {
      Util.assert(isChar(char), 'make-string [arg(2)] expects a char')
      Util.assert(isNum(k), 'make-string [arg(1)] expects a number')
      return char.displayText.repeat(k)
    });
    mkNativeProc(env, 'string-length', ['n'], ([n]: any) => {
      Util.assert(isString(n))
      return n.length
    });
    mkNativeProc(env, 'string-ref', ['string', 'k'], ([string, k]: any) => {
      Util.assert(isString(string) && string.length >= k)
      Util.assert(isNum(k), format('Invalid `k` param passed to `string-ref`, expected number, got `%s`', typeof k))
      return new Character(string[k])
    });
    mkNativeProc(env, 'string-set!', ['string', 'k', 'char'], ([string, k, char]: any) => {
      Util.assert(isString(string) && string.length > k)
      Util.assert(isChar(char))
      string = char.displayText
      return UNDEF
    });
    mkNativeProc(env, 'string->input-port', ['string'], ([string]: any) => {
      Util.assert(isString(string))
      return InPort.fromString(string)
    });
    mkNativeProc(env, 'string=?', ['string1', 'string2'], ([string1, string2]: any) => {
      Util.assert(isString(string1) && isString(string2))
      return Util.toL(string1 === string2)
    });
    mkNativeProc(env, 'string-ci=?', ['string1', 'string2'], ([string1, string2]: any) => {
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
    mkNativeProc(env, 'string<?', ['string1', 'string2'], ([string1, string2]: any) => {
      Util.assert(isString(string1) && isString(string2))
      return Util.toL(string1 < string2)
    });
    mkNativeProc(env, 'string>?', ['string1', 'string2'], ([string1, string2]: any) => {
      Util.assert(isString(string1) && isString(string2))
      return Util.toL(string1 > string2)
    });
    mkNativeProc(env, 'string<=?', ['string1', 'string2'], ([string1, string2]: any) => {
      Util.assert(isString(string1) && isString(string2))
      return Util.toL(string1 <= string2)
    });
    mkNativeProc(env, 'string>=?', ['string1', 'string2'], ([string1, string2]: any) => {
      Util.assert(isString(string1) && isString(string2))
      return Util.toL(string1 >= string2)
    });
    mkNativeProc(env, 'string-ci<?', ['string1', 'string2'], ([string1, string2]: any) => {
      Util.assert(isString(string1) && isString(string2))
      return Util.toL(string1.toLowerCase() < string2.toLowerCase())
    });
    mkNativeProc(env, 'string-ci>?', ['string1', 'string2'], ([string1, string2]: any) => {
      Util.assert(isString(string1) && isString(string2))
      return Util.toL(string1.toLowerCase() > string2.toLowerCase())
    });
    mkNativeProc(env, 'string-ci<=?', ['string1', 'string2'], ([string1, string2]: any) => {
      Util.assert(isString(string1) && isString(string2))
      return Util.toL(string1.toLowerCase() <= string2.toLowerCase())
    });
    mkNativeProc(env, 'string-ci>=?', ['string1', 'string2'], ([string1, string2]: any) => {
      Util.assert(isString(string1) && isString(string2))
      return Util.toL(string1.toLowerCase() >= string2.toLowerCase())
    });
    mkNativeProc(env, 'substring', ['string', 'start', 'end'], ([string, start, end]: any) => {
      Util.assert(isString(string))
      return (<string>string).slice(start, end)
    });
    mkNativeProc(env, 'string-append', ['string', '...xs'], ([string, ...xs]: any) => {
      Util.assert(isString(string))
      return (<string>string).concat(...xs)
    });
    mkNativeProc(env, 'string->list', ['string', '...'], ([string]: any) => {
      Util.assert(isString(string))
      return (<string>string).split('')
    });
    mkNativeProc(env, 'list->string', ['list', '...'], ([list]: any) => {
      Util.assert(isPair(list) && list.every(isString))
      return list.toArray().join('')
    });
    mkNativeProc(env, 'string-copy', ['string'], ([string]: any) => {
      Util.assert(isString(string))
      return String(string)
    });
    mkNativeProc(env, 'string-fill', ['string', 'char'], ([string, char]: any) => {
      Util.assert(isString(string))
      Util.assert(isChar(char))
      string.replaceAll(/.*/, char.displayText)
      return string
    });
    mkNativeProc(env, 'string-pad-end', ['string', 'maxLength', '.', 'fillString'], ([string, maxLength, ...[fillString]]: any) => {
      Util.assert(isString(string));
      Util.assert(isNum(maxLength));
      return (string as string).padEnd(maxLength, fillString);
    });
    mkNativeProc(env, 'string-pad-start', ['string', 'maxLength', '.', 'fillString'], ([string, maxLength, ...[fillString]]: any) => {
      Util.assert(isString(string));
      Util.assert(isNum(maxLength));
      return (string as string).padStart(maxLength, fillString);
    });
    // END - 6.3.5 Strings

    // - 6.3.6 Vectors
    mkNativeProc(env, 'vector?', ['obj'], ([obj]: any) => {
      return Util.toL(isVec(obj))
    });
    mkNativeProc(env, 'make-vector', ['k', 'fill?'], ([k, fill]: any) => {
      Util.assert(k !== undefined, 'make-vector not given a size')
      Util.assert(fill === undefined, 'make-vector fill option not implemented')
      const arr = Array.from<Form>(k).fill(fill);
      return new Vector(arr);
    });
    mkNativeProc(env, 'vector', 'args', (args: any) => {
      return new Vector(args)
    });
    mkNativeProc(env, 'vector-length', ['vec'], ([vec]: any) => {
      Util.assert(isVec(vec), `vector-length expected a Vector. Got: ${typeof vec}`)
      return vec.data.length
    });
    mkNativeProc(env, 'vector-ref', ['vec', 'k'], ([vec, k]: any) => {
      Util.assert(isVec(vec), `vector-ref [arg(1)] expected a Vector. Got: ${typeof vec}`)
      Util.assert(isNum(k), `vector-ref [arg(2)] expected a Number. Got: ${typeof vec}`)
      return vec.data[k]
    });
    mkNativeProc(env, 'vector-set!', ['vec', 'k', 'obj'], ([vec, k, obj]: any) => {
      Util.assert(isVec(vec), `vector-ref [arg(1)] expected a Vector. Got: ${typeof vec}`)
      Util.assert(isNum(k), `vector-ref [arg(2)] expected a Number. Got: ${typeof vec}`)
      Util.assert(obj !== undefined, `vector-ref [arg(3)] is undefined`)
      return vec.data[k] = obj
    });
    mkNativeProc(env, 'vector->list', ['vec'], ([vec]: any) => {
      Util.assert(isVec(vec), `vector-list expected a Vector. Got: ${typeof vec}`)
      return list(...vec.data)
    });
    mkNativeProc(env, 'list->vector', ['list'], ([list]: any) => {
      Util.assert(isPair(list), `list->vector expected a list. Got: ${typeof list}`)
      return new Vector(list.toArray())
    });
    mkNativeProc(env, 'vector-fill!', ['vec', 'fill'], ([vec, fill]: any) => {
      Util.assert(isVec(vec), `vector-list expected a Vector. Got: ${typeof vec}`)
      Util.assert(fill, `vector-list [arg(2)] expected an argument`)
      for (let i = 0; i < vec.data.length; i++) {
        vec.data[i] = fill
      }
    });
    // END - 6.3.6 Vectors

    // - 6.4 Control Features
    mkNativeProc(env, 'procedure?', ['obj'], ([obj]: any) => {
      return Util.toL(isProc(obj) || isNativeProc(obj))
    });
    mkNativeProc(env, 'apply', 'args', async (args_: any, env) => {
      const [proc, args] = args_
      Util.assert(isCallable(proc), 'called apply with a non procedure')
      Util.assert(isList(args), 'called apply with a non procedure')
      return await proc.call(args, env)
    });
    mkNativeProc(env, 'map', ['proc', '.', 'args'], async ([proc, ...lists]: any) => {
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
    mkNativeProc(env, 'call-with-current-continuation', ['throw'], async ([proc]: any, env: iEnv) => {
      const ball = new RuntimeWarning("Sorry, can't continue this continuation any longer.");
      const throw_ = mkNativeProc(env, 'throw', ['retval'], ([retval]: any) => {
        ball.retval = retval; throw ball;
      });
      if (isProc(proc) || isNativeProc(proc)) {
        try {
          return await proc.call(list(throw_), env);
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
    mkNativeProc(env, 'call-with-input-file', ['string', 'proc'], ([string, proc]: any) => {
      throw new NotImplementedError('call-with-input-file')
    });

    mkNativeProc(env, 'input-port?', ['obj'], ([obj]: any) => Util.toL(isInputPort(obj)));
    mkNativeProc(env, 'output-port?', ['obj'], ([obj]: any) => Util.toL(isOutputPort(obj)));

    mkNativeProc(env, 'current-input-port', [], () => env.get('*current-input-port*'));
    mkNativeProc(env, 'current-output-port', [], () => env.get('*current-output-port*'));

    mkNativeProc(env, 'set-current-input-port!', ['port'], ([port]: any) => env.set('*current-input-port*', port));
    mkNativeProc(env, 'set-current-output-port!', ['port'], ([port]: any) => env.set('*current-output-port*', port));
    // END - 6.6.1 Ports

    // - 6.6.2 Input
    mkNativeProc(env, 'read', ['port'], async ([port]: any) => {
      const p: InPort = port ?? currentInputPort(world)
      // console.log('reading port', p.name)
      const data = await read(p, readerEnv);
      // console.log('reading port (data):', data)
      return data
    });

    mkNativeProc(env, 'read-char', ['port'], async ([port]: any) => {
      const p: InPort = port ?? currentInputPort(world)
      return await p.readChar()
    });

    mkNativeProc(env, 'peek-char', ['port'], async ([port]: any) => {
      const p: InPort = port ?? currentInputPort(world)
      return await p.peekChar()
    });

    mkNativeProc(env, 'eof-object?', ['obj'], ([obj]: any) => Util.toL(isEofString(obj)));

    mkNativeProc(env, 'char-ready?', ['port'], ([port]: any) => {
      const p: InPort = port ?? currentInputPort(world)
      return Util.toL(isEofString(p) || p.charReady())
    });
    // END - 6.6.2 Input

    // - 6.6.3 Output
    mkNativeProc(env, 'putchar', ['char', 'port?'], ([obj, port]: any) => {
      const p: OutPort = port ?? currentOutputPort(world)
      p.write(obj)
      return
    });
    mkNativeProc(env, 'write', ['obj', 'port?'], ([obj, port]: any) => {
      const p: OutPort = port ?? currentOutputPort(world)
      p.write(toString(obj))
      return
    });
    mkNativeProc(env, 'writeln', ['obj', 'port?'], ([obj, port]: any) => {
      const p: OutPort = port ?? currentOutputPort(world)
      p.write(toString(obj))
      p.write('\n')
      return
    });
    mkNativeProc(env, 'display', ['obj', 'port?'], ([obj, port]: any) => {
      const p: OutPort = port ?? currentOutputPort(world)
      p.write(obj)
      return
    });
    mkNativeProc(env, 'newline', ['port?'], ([port]: any) => {
      const p: OutPort = port ?? currentOutputPort(world)
      p.write('\n')
    });
    mkNativeProc(env, 'write-char', ['char', 'port?'], ([char, port]: any) => {
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
    mkNativeProc(env, 'open-input-string', ['string'], ([string]: any) => {
      Util.assert(typeof string === 'string')
      return InPort.fromString(string)
    });
    /// END SRFI 6

  }
  // #endregion

  // #region [ rgba(0, 100, 0, 0.3)] - Misc

  if (options.misc) {

    mkNativeProc(env, 'macroexpand', ['expr'], async ([expr]: any) => {
      // console.log(toStringSafe(env.get<Procedure>('equal?').expr))
      const rv = await expand(expr, true, env);
      return rv
    });

    mkNativeProc(env, 'tokenize', ['expr'], async ([expr]: any) => {
      return await Lisp.tokenize(expr, world)
    });

    mkNativeProc(env, 'putchar2', ['char1', 'char2', 'port?'], ([obj1, obj2, port]: any) => {
      const p: OutPort = port ?? currentOutputPort(world)
      p.write(obj1)
      p.write(obj2)
      return
    });

    mkNativeProc(env, 'try', ['callable'], async ([callable]: any) => {
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

    mkNativeProc(env, 'resume-from-error', [], () => {
      throw new Resume()
    })
  }
  // #endregion

  // #region [ rgba(0, 100, 0, 0.3)] - REPL

  if (options.repl) {

    mkNativeProc(env, 'exit-repl', [], () => {
      if (isT(world.env.get('*in-repl-mode*')))
        world.env.set('*in-repl-mode*', Util.toL(false))
      else
        throw new Error('not in repl mode');
    })

    mkNativeProc(env, 'set-current-repl-prompt!', ['prompt'], ([prompt]: any) => {
      world.env.set('*current-repl-prompt*', prompt)
    })

    mkNativeProc(env, 'get-current-repl-prompt', [], () => {
      return world.env.get('*current-repl-prompt*')
    })

    mkNativeProc(env, 'revert-to-default-repl-prompt!', [], () => {
      world.env.set('*current-repl-prompt*', world.env.get('*default-repl-prompt*'))
    })

    mkNativeProc(env, 'repl', [], async () => {
      world.env.set('*in-repl-mode*', Util.toL(true))

      let lastInput: any, lastExpand: any, lastOutput: any, greet = true;

      while (isT(world.env.get('*in-repl-mode*'))) {
        const p = currentInputPort(world)
        const o = currentOutputPort(world)
        try {
          if (greet) o.write(world.env.get('*current-repl-prompt*'))
          lastInput = await read(p, world.readerEnv);
          lastExpand = await expand(lastInput, true, world.lexicalEnv)
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

  mkNativeProc(env, 'run-tests', [], async () => {
    await Lisp.execute('(load "tests/runner.scm")', world)
  });

}
