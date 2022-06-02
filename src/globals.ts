import assert from "assert";
import { FALSE, NIL, TRUE, UNDEF } from "./core/const";
import { Resume } from "./core/cont";
import { Env } from "./core/env";
import { InvalidCallableExpression, NotImplementedError, UndefinedVariableError } from "./core/error";
import { evaluate } from "./core/eval";
import { expand } from "./core/expand";
import * as Lisp from "./core/lisp";
import { currentInputPort, currentOutputPort, InPort, IOPort, isEofString, isInputPort, isOutputPort, OutPort } from "./core/port";
import { isNativeProc, isProc, NativeProc } from "./core/proc";
import { read } from "./core/read";
import { Sym } from "./core/sym";
import { SyntaxRulesDef } from "./core/syntax";
import type { List, Form } from "./core/forms";
import { print, toString, toStringSafe } from "./core/toString";
import { Vector } from "./core/vec";
import { Environment } from "./env";
import { loadFile, parseLoadSymbol } from "./load";
import * as Util from "./utils";

type AddGlobalsOptions = {
  tscheme?: boolean
  r5rs?: boolean
  srfi?: boolean
  misc?: boolean
  repl?: boolean
  sockets?: boolean
};

const defaultOptions: AddGlobalsOptions = {
  tscheme: true,
  r5rs: true,
  srfi: true,
  misc: true,
  repl: true,
  sockets: true,
}

export async function addGlobals(
  global: Environment,
  opts: AddGlobalsOptions = {}
) {

  const options = {...defaultOptions, ...opts }

  const {env, readerEnv, lexicalEnv} = global

  // #region [ rgba(20, 50, 80, 0.8) ] - TScheme
  //  - TScheme

  if (options.tscheme) {
    env.set('#<undef>', UNDEF);
    env.set('else', TRUE);
    env.set('otherwise', TRUE);
    env.set('nil', NIL);

    env.set('#cwd', process.cwd());

    env.set('*default-input-port*', <any>InPort.fromStdIn())
    env.set('*current-input-port*', env.get('*default-input-port*'))

    env.set('*default-output-port*', <any>OutPort.fromStdOut())
    env.set('*current-output-port*', env.get('*default-output-port*'))

    env.set('*default-repl-prompt*', '> ')
    env.set('*current-repl-prompt*', env.get('*default-repl-prompt*'))

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
    mkNativeProc(env, 'inspect', ['x'], ([x]: any) => { return toString(x, true); });
    mkNativeProc(env, 'break', ['args'], async (args: any, env) => { debugger; return await evaluate(args, env); });
    mkNativeProc(env, 'error', ['x', 'code?'], ([x, code = 1]: any) => { console.error(x); process.exit(code); });
    mkNativeProc(env, 'debug-macro!', ['val'], ([val]: any) => { SyntaxRulesDef.debug = !Util.isF(val) });

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
        return await evaluate([proc, locals, toString(char)], env);
      }
      throw new Error('Nope @ set-macro-character');
    });
  });
  }
  //#endregion

  // #region [ rgba(0, 50, 150, 0.5) ] - r5rs
  // - r5rs

  if (options.r5rs) {
    //  - 4.2 Derived Expressions
    await Lisp.execute(`

      ; - 4.2 Derived expression types
      (define-syntax cond
        (syntax-rules (else =>)
          ((cond (else result1 result2 ...))
          (begin result1 result2 ...))
          ((cond (test => result))
          (let ((temp test))
            (if temp (result temp))))
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

      (define-syntax and
        (syntax-rules ()
          ([and] #t)
          ([and test] test)
          ([and test1 test2 ...]
            (if test1 [and test2 ...] #f))))

      (define-syntax or
        (syntax-rules ()
          ([or] #f)
          ([or test] test)
          ([or test1 test2 ...]
            (let ([x test1])
              (if x x (or test2 ...))))))

      (define-syntax let
        (syntax-rules ()
          ((let ((name val) ...) body1 body2 ...)
            ((lambda (name ...) body1 body2 ...)
            val ...))))

      (define-syntax let*
        (syntax-rules ()
          ((let* () body1 body2 ...)
            (let () body1 body2 ...))
          ((let* ((name1 val1) (name2 val2) ...)
              body1 body2 ...)
            (let ((name1 val1))
              (let* ((name2 val2) ...)
                body1 body2 ...)))))

      ;; from: https://stackoverflow.com/questions/2835582/what-if-any-is-wrong-with-this-definition-of-letrec-in-scheme
      (define-syntax letrec
        (syntax-rules ()
          ((letrec ((name val) ...) body bodies ...)
          ((lambda ()
            (define name val) ... body bodies ...)))))

    `, global)

    //  - 6. Standard procedures
    // - 6.1 Equivalence Predicates
    mkNativeProc(env, 'eqv?', ['a', 'b'], ([a, b]: any) => Util.toL(Util.isEq(a, b)));
    mkNativeProc(env, 'eq?', ['a', 'b'], ([a, b]: any) => Util.toL(Util.isEq(a, b)));
    mkNativeProc(env, 'equal?', ['a', 'b'], ([a, b]: any) => Util.toL(toString(a) === toString(b)));
    // END - 6.1 Equivalence predicates

    // - 6.2 Numbers
    // - 6.2.5 Numerical operations
    mkNativeProc(env, 'number?', ['n'], ([n]: any) => Util.toL(Util.isNum(n)));
    // procedure: complex? obj
    // procedure: real? obj
    // procedure: rational? obj
    // procedure: integer? obj
    // procedure: exact? z
    // procedure: inexact? z
    mkNativeProc(env, '=', ['args'], (args: any) => args.reduce((acc: any, val: any) => Util.toL(acc === val)));
    mkNativeProc(env, '>', ['args'], (args: any) => args.reduce((acc: any, val: any) => Util.toL(acc > val)));
    mkNativeProc(env, '<', ['args'], (args: any) => args.reduce((acc: any, val: any) => Util.toL(acc < val)));
    mkNativeProc(env, '>=', ['args'], ([l, r]: any) => Util.toL(l >= r));
    mkNativeProc(env, '<=', ['args'], ([l, r]: any) => Util.toL(l <= r));
    mkNativeProc(env, 'zero?', ['n'], ([n]: any) => Util.toL(n === 0));
    mkNativeProc(env, 'positive?', ['n'], ([n]: any) => Util.toL(Util.isNum(n) && n > 0));
    mkNativeProc(env, 'negative?', ['n'], ([n]: any) => Util.toL(Util.isNum(n) && n < 0));
    mkNativeProc(env, 'odd?', ['n'], ([n]: any) => Util.toL(Util.isNum(n) && n % 2 !== 0));
    mkNativeProc(env, 'even?', ['n'], ([n]: any) => Util.toL(Util.isNum(n) && n % 2 === 0));
    mkNativeProc(env, 'max', ['args'], (args: any) => args.reduce((acc: any, val: any) => Math.max(acc, val)));
    mkNativeProc(env, 'min', ['args'], (args: any) => args.reduce((acc: any, val: any) => Math.min(acc, val)));
    mkNativeProc(env, '+', ['args'], (args: any) => args.reduce((acc: any, val: any) => acc + val, 0));
    mkNativeProc(env, '*', ['args'], (args: any) => args.reduce((acc: any, val: any) => acc * val, 1));
    mkNativeProc(env, '-', ['args'], (args: any) => {
      Util.assert(args.length > 0, "procedure requires at least one argument: (-)")
      if (args.length === 1) return -args[0]
      else return args.reduce((acc: any, val: any) => acc - val)
    });
    mkNativeProc(env, '/', ['args'], (args: any) => {
      Util.assert(args.length > 0, "procedure requires at least one argument: (/)")
      args.reduce((acc: any, val: any) => acc / val)
    });
    mkNativeProc(env, 'abs', ['n'], ([n]: any) => Math.abs(n));
    mkNativeProc(env, 'quotient', ['x', 'y'], ([x, y]: any) => x/y|0);
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
    mkNativeProc(env, 'atan', ['y', 'x'], ([y, x]: any) => Util.isNone(x) ? Math.atan(y) : Math.atan2(y, x));
    mkNativeProc(env, 'sqrt', ['n'], ([n]: any) => Math.sqrt(n));
    mkNativeProc(env, 'expt', ['n'], ([n]: any) => Math.exp(n));

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
      Util.assert(Util.isNum(n), `"number->string" procedure takes a 'number' as an argument`);
      return (<number>n).toString(radix ?? 10);
    });
    mkNativeProc(env, 'string->number', ['n', 'radix?'], ([n, radix]: any) => {
      Util.assert(Util.isString(n), `"string->number" procedure takes a 'string' as an argument`);
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
    mkNativeProc(env, 'pair?', ['obj'], ([obj]: any) => Util.toL(Util.isPair(obj)));
    mkNativeProc(env, 'cons', ['a', 'b'], ([a, b]: any) => [a].concat(b));
    mkNativeProc(env, 'car', ['args'], (args: any) => Lisp.car(args));
    mkNativeProc(env, 'cdr', ['args'], (args: any) => Lisp.cdr(args));
    mkNativeProc(env, 'set-car!', ['l', 'v'], ([l, v]: any) => l[0] = v);
    mkNativeProc(env, 'set-cdr!', ['l', 'v'], ([l, v]: any) => l[1] = v);

    // library procedure: caar pair
    // library procedure: cadr pair
    // ...: ...
    // library procedure: cdddar pair
    // library procedure: cddddr pair

    mkNativeProc(env, 'null?', ['n'], ([n]: any) => Util.toL(Util.isEmpty(n)));
    mkNativeProc(env, 'list?', ['n'], ([n]: any) => Util.toL(Util.isList(n)));
    mkNativeProc(env, 'list', ['args'], (args: any) => args);
    mkNativeProc(env, 'length', ['list'], ([list]: any) => Util.isList(list) && list.length);
    // library procedure: append list ...
    mkNativeProc(env, 'reverse', ['list'], ([list]: any) => Util.isList(list) && [...list].reverse());

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
    mkNativeProc(env, 'symbol?', ['n'], ([n]: any) => Util.toL(Util.isSym(n)));
    mkNativeProc(env, 'symbol->string', ['n'], ([n]: any) => {
      Util.assert(Util.isSym(n), `"symbol->string" procedure takes a 'symbol' as an argument`);
      return toString(n)
    });
    mkNativeProc(env, 'string->symbol', ['n'], ([n]: any) => {
      Util.assert(Util.isString(n), `"string->symbol" procedure takes a 'string' as an argument`);
      return Sym(n)
    });
    // END - 6.3.3 Symbols

    // - 6.3.4 Characters
    mkNativeProc(env, 'char?', ['obj'], ([obj]: any) => {
      return Util.toL(Util.isChar(obj))
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
    mkNativeProc(env, 'string?', ['n'], ([n]: any) => Util.toL(Util.isString(n)));
    mkNativeProc(env, 'string', ['args'], ([args]: any) => args.join(''));
    mkNativeProc(env, 'make-string', ['k', 'char'], ([k, char = ' ']: any) => {
      assert(Util.isNum(k), 'make-string [arg(1)] expects a number')
      assert(Util.isChar(char), 'make-string [arg(2)] expects a char')
      return char.displayText.repeat(k)
    });
    mkNativeProc(env, 'string-length', ['n'], ([n]: any) => {
      Util.assert(Util.isString(n))
      return n.length
    });
    mkNativeProc(env, 'string-ref', ['string', 'k'], ([string, k]: any) => {
      Util.assert(Util.isString(string) && string.length >= k)
      return string[k]
    });
    mkNativeProc(env, 'string-set!', ['string', 'k', 'char'], ([string, k, char]: any) => {
      assert(Util.isString(string) && string.length > k)
      assert(Util.isChar(char))
      string = char.displayText
      return UNDEF
    });
    mkNativeProc(env, 'string->input-port', ['string'], ([string]: any) => {
      assert(typeof string === 'string')
      return InPort.fromString(string)
    });
    mkNativeProc(env, 'string=?', ['string1', 'string2'], ([string1, string2]: any) => {
      Util.assert(Util.isString(string1) && Util.isString(string2))
      return Util.toL(string1 === string2)
    });
    mkNativeProc(env, 'string-ci=?', ['string1', 'string2'], ([string1, string2]: any) => {
      Util.assert(Util.isString(string1) && Util.isString(string2))
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
      Util.assert(Util.isString(string1) && Util.isString(string2))
      return Util.toL(string1 < string2)
    });
    mkNativeProc(env, 'string>?', ['string1', 'string2'], ([string1, string2]: any) => {
      Util.assert(Util.isString(string1) && Util.isString(string2))
      return Util.toL(string1 > string2)
    });
    mkNativeProc(env, 'string<=?', ['string1', 'string2'], ([string1, string2]: any) => {
      Util.assert(Util.isString(string1) && Util.isString(string2))
      return Util.toL(string1 <= string2)
    });
    mkNativeProc(env, 'string>=?', ['string1', 'string2'], ([string1, string2]: any) => {
      Util.assert(Util.isString(string1) && Util.isString(string2))
      return Util.toL(string1 >= string2)
    });
    mkNativeProc(env, 'string-ci<?', ['string1', 'string2'], ([string1, string2]: any) => {
      Util.assert(Util.isString(string1) && Util.isString(string2))
      return Util.toL(string1.toLowerCase() < string2.toLowerCase())
    });
    mkNativeProc(env, 'string-ci>?', ['string1', 'string2'], ([string1, string2]: any) => {
      Util.assert(Util.isString(string1) && Util.isString(string2))
      return Util.toL(string1.toLowerCase() > string2.toLowerCase())
    });
    mkNativeProc(env, 'string-ci<=?', ['string1', 'string2'], ([string1, string2]: any) => {
      Util.assert(Util.isString(string1) && Util.isString(string2))
      return Util.toL(string1.toLowerCase() <= string2.toLowerCase())
    });
    mkNativeProc(env, 'string-ci>=?', ['string1', 'string2'], ([string1, string2]: any) => {
      Util.assert(Util.isString(string1) && Util.isString(string2))
      return Util.toL(string1.toLowerCase() >= string2.toLowerCase())
    });
    mkNativeProc(env, 'substring', ['string', 'start', 'end'], ([string, start, end]: any) => {
      Util.assert(Util.isString(string) && string.length >= start && string.length >= end && start < end)
      return (<string>string).slice(start, end)
    });
    mkNativeProc(env, 'string-append', ['string', '...xs'], ([string, ...xs]: any) => {
      Util.assert(Util.isString(string))
      return (<string>string).concat(...xs)
    });
    mkNativeProc(env, 'string->list', ['string', '...'], ([string]: any) => {
      Util.assert(Util.isString(string))
      return (<string>string).split('')
    });
    mkNativeProc(env, 'list->string', ['list', '...'], ([list]: any) => {
      Util.assert(Util.isList(list) && list.map(o => Util.isString(o)))
      return (<List>list).join('')
    });
    mkNativeProc(env, 'string-copy', ['string'], ([string]: any) => {
      Util.assert(Util.isString(string))
      return String(string)
    });
    mkNativeProc(env, 'string-fill', ['string', 'char'], ([string, char]: any) => {
      Util.assert(Util.isString(string))
      Util.assert(Util.isChar(char))
      string.replaceAll(/.*/, char)
      return string
    });
    mkNativeProc(env, 'string-pad-end', ['string', 'maxLength', '.', 'fillString'], ([string, maxLength, ...[fillString]]: any) => {
      Util.assert(Util.isString(string));
      Util.assert(Util.isNum(maxLength));
      return (string as string).padEnd(maxLength, fillString);
    });
    mkNativeProc(env, 'string-pad-start', ['string', 'maxLength', '.', 'fillString'], ([string, maxLength, ...[fillString]]: any) => {
      Util.assert(Util.isString(string));
      Util.assert(Util.isNum(maxLength));
      return (string as string).padStart(maxLength, fillString);
    });
    // END - 6.3.5 Strings

    // - 6.3.6 Vectors
    mkNativeProc(env, 'vector?', ['obj'], ([obj]: any) => {
      return Util.toL(Util.isVec(obj))
    });
    mkNativeProc(env, 'make-vector', ['k', 'fill?'], ([k, fill]: any) => {
      assert(fill === undefined, 'make-vector fill option not implemented')
      return new Vector(k)
    });
    mkNativeProc(env, 'vector', ['args'], ([args]: any) => {
      return new Vector(args)
    });
    mkNativeProc(env, 'vector->length', ['vec'], ([vec]: any) => {
      assert(Util.isVec(vec), `vector-length expected a Vector. Got: ${typeof vec}`)
      return (<Vector>vec).data.length
    });
    mkNativeProc(env, 'vector->ref', ['vec', 'k'], ([vec, k]: any) => {
      assert(Util.isVec(vec), `vector-ref [arg(1)] expected a Vector. Got: ${typeof vec}`)
      assert(Util.isNumber(k), `vector-ref [arg(2)] expected a Number. Got: ${typeof vec}`)
      return (<Vector>vec).data[k]
    });
    mkNativeProc(env, 'vector->set!', ['vec', 'k', 'obj'], ([vec, k, obj]: any) => {
      assert(Util.isVec(vec), `vector-ref [arg(1)] expected a Vector. Got: ${typeof vec}`)
      assert(Util.isNumber(k), `vector-ref [arg(2)] expected a Number. Got: ${typeof vec}`)
      assert(obj, `vector-ref [arg(3)] is undefined`)
      return (<Vector>vec).data[k] = obj
    });
    mkNativeProc(env, 'vector->list', ['vec'], ([vec]: any) => {
      assert(Util.isVec(vec), `vector-list expected a Vector. Got: ${typeof vec}`)
      return (<Vector>vec).data
    });
    mkNativeProc(env, 'list->vector', ['list'], ([list]: any) => {
      assert(Util.isList(list), `list->vector expected a list. Got: ${typeof list}`)
      return new Vector(list)
    });
    mkNativeProc(env, 'vector->fill!', ['vec', 'fill'], ([vec, fill]: any) => {
      assert(Util.isVec(vec), `vector-list expected a Vector. Got: ${typeof vec}`)
      assert(fill, `vector-list [arg(2)] expected an argument`)
      for (let i = 0; i < vec.data.length; i++) {
        vec.data[i] = fill
      }
    });
    // END - 6.3.6 Vectors

    // - 6.4 Control Features
    mkNativeProc(env, 'procedure?', ['obj'], ([obj]: any) => {
      return Util.toL(isProc(obj) || isNativeProc(obj))
    });
    // procedure: apply proc arg1 ... args
    // library procedure: map proc list1 list2 ...
    // library procedure: for-each proc list1 list2 ...
    // library procedure: force promise
    mkNativeProc(env, 'call-with-current-continuation', ['throw'], ([proc]: any, env: Env) => {
      class RuntimeWarning extends Error { public retval?: any; }
      const ball = new RuntimeWarning("Sorry, can't continue this continuation any longer.");
      const throw_ = mkNativeProc(env, 'throw', ['retval'], ([retval]: any) => {
        ball.retval = retval; throw ball;
      });
      try {
        if (isNativeProc(proc)) {
          return proc.call([throw_ as Form], env);
        }
        throw new InvalidCallableExpression(proc);
      } catch (err) {
        if (err instanceof RuntimeWarning) {
          return ball.retval;
        }
        else {
          throw err;
        }
      }
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

    mkNativeProc(env, 'open-input-file', ['filename'], ([filename]: any) => {
      assert(typeof filename === 'string')
      return InPort.fromFile(filename)
    });
    mkNativeProc(env, 'open-output-file', ['filename'], ([filename]: any) => {
      assert(typeof filename === 'string')
      return OutPort.fromFile(filename)
    });
    // END - 6.6.1 Ports

    // - 6.6.2 Input
    mkNativeProc(env, 'read', ['port'], async ([port]: any) => {
      const p: InPort = port ?? currentInputPort(global)
      // console.log('reading port', p.name)
      const data = await read(p, readerEnv);
      // console.log('reading port (data):', data)
      return data
    });

    mkNativeProc(env, 'read-char', ['port'], async ([port]: any) => {
      const p: InPort = port ?? currentInputPort(global)
      return await p.readChar()
    });

    mkNativeProc(env, 'peek-char', ['port'], async ([port]: any) => {
      const p: InPort = port ?? currentInputPort(global)
      return await p.peekChar()
    });

    mkNativeProc(env, 'eof-object?', ['obj'], ([obj]: any) => Util.toL(isEofString(obj)));

    mkNativeProc(env, 'char-ready?', ['port'], ([port]: any) => {
      const p: InPort = port ?? currentInputPort(global)
      return Util.toL(isEofString(p) || p.charReady())
    });
    // END - 6.6.2 Input

    // - 6.6.3 Output
    mkNativeProc(env, 'putchar', ['char', 'port?'], ([obj, port]: any) => {
      const p: OutPort = port ?? currentOutputPort(global)
      p.write(obj)
      return
    });
    mkNativeProc(env, 'write', ['obj', 'port?'], ([obj, port]: any) => {
      const p: OutPort = port ?? currentOutputPort(global)
      p.write(obj)
      return
    });
    mkNativeProc(env, 'display', ['obj', 'port?'], ([obj, port]: any) => {
      const p: OutPort = port ?? currentOutputPort(global)
      p.write(obj)
      return
    });
    mkNativeProc(env, 'newline', ['port?'], ([port]: any) => {
      const p: OutPort = port ?? currentOutputPort(global)
      p.write('\n')
      return
    });
    mkNativeProc(env, 'write-char', ['char', 'port?'], ([char, port]: any) => {
      assert(Util.isChar(char), `not a character: ${char}`)
      const p: OutPort = port ?? currentOutputPort(global)
      p.write(char.displayText)
      return
    });
    // END - 6.6.3 Output

    // - 6.6.4 System interface
    mkNativeProc(env, 'load', ['file'], async ([file]: any) => {
      if (Util.isSym(file)) {
        return await loadFile(parseLoadSymbol(file), global)
      }
      return await loadFile(file, global)
    });

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
      assert(typeof string === 'string')
      return InPort.fromString(string)
    });
    /// END SRFI 6

  }
  // #endregion

  // #region [ rgba(0, 100, 0, 0.3)] - Sockets

  if (options.sockets) {

    mkNativeProc(env, 'socket-client->input-port', ['address'], ([address]: any) => {
      assert(typeof address === 'string')
      return InPort.fromSocketClient(address)
    });
    mkNativeProc(env, 'socket-client->output-port', ['address'], ([address]: any) => {
      assert(typeof address === 'string')
      return OutPort.fromSocketClient(address)
    });
    mkNativeProc(env, 'socket-server->input-port', ['address'], ([port]: any) => {
      assert(typeof port === 'string' || typeof port === 'number')
      return InPort.fromSocketServer(port)
    });
    mkNativeProc(env, 'socket-server->output-port', ['address'], ([port]: any) => {
      assert(typeof port === 'string' || typeof port === 'number')
      return OutPort.fromSocketServer(port)
    });
    mkNativeProc(env, 'socket-server->i/o-port', ['address'], ([port]: any) => {
      assert(typeof port === 'string' || typeof port === 'number')
      return IOPort.fromSocketServer(port)
    });
  }
  // #endregion

  // #region [ rgba(0, 100, 0, 0.3)] - Misc

  if (options.misc) {

    mkNativeProc(env, 'macroexpand', ['expr'], async ([args]: any) => await expand(args, true, env));

    mkNativeProc(env, 'putchar2', ['char1', 'char2', 'port?'], ([obj1, obj2, port]: any) => {
      const p: OutPort = port ?? currentOutputPort(global)
      p.write(obj1)
      p.write(obj2)
      return
    });

    mkNativeProc(env, 'try', ['callable'], ([callable]: any) => {
      try {
        if (isNativeProc(callable)) {
          return [TRUE, callable.call([], env)];
        }
        return [FALSE, ['InvalidCallableExpression']]
      } catch (err) {
        if (err instanceof Error)
          return [FALSE, [err.message]];
        if (typeof err === 'string')
          return [FALSE, [err]];
        return [FALSE, ['UnknownError']];
      }
    });

    mkNativeProc(env, 'reload', ['file'], async ([file]: any) => {
      if (Util.isSym(file)) {
        return await loadFile(parseLoadSymbol(file), global, true)
      }
      return await loadFile(file, global, true)
    });

    mkNativeProc(env, 'resume-from-error', [], () => {
      throw new Resume()
    })
  }
  // #endregion

  // #region [ rgba(0, 100, 0, 0.3)] - REPL

  if (options.repl) {

    mkNativeProc(env, 'exit-repl', [], () => {
      if (Util.isT(global.env.get('*in-repl-mode*')))
        global.env.set('*in-repl-mode*', Util.toL(false))
      else
        throw new Error('not in repl mode');
    })

    mkNativeProc(env, 'set-current-repl-prompt!', ['prompt'], ([prompt]: any) => {
      global.env.set('*current-repl-prompt*', prompt)
    })

    mkNativeProc(env, 'get-current-repl-prompt', [], () => {
      return global.env.get('*current-repl-prompt*')
    })

    mkNativeProc(env, 'revert-to-default-repl-prompt!', [], () => {
      global.env.set('*current-repl-prompt*', global.env.get('*default-repl-prompt*'))
    })

    mkNativeProc(env, 'repl', [], async () => {
      global.env.set('*in-repl-mode*', Util.toL(true))

      let lastInput: any, lastExpand: any, lastOutput: any;

      while (Util.isT(global.env.get('*in-repl-mode*'))) {
        const p = currentInputPort(global)
        const o = currentOutputPort(global)
        try {
          o.write(global.env.get('*current-repl-prompt*'))
          lastInput = await read(p, global.readerEnv);
          lastExpand = await expand(lastInput, true, global.lexicalEnv)
          lastOutput = await evaluate(lastExpand, global.env)
          const r = toStringSafe(lastOutput);
          if (r !== undefined) o.write(r)
          o.write('\n')
        } catch (err) {
          if (err instanceof Resume) {
            global.env.set('*in-repl-mode*', Util.toL(false))
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
            o.write(err.message + '\n')
          }
          console.log('error: ' + err)
        }
      }
    });
  }
  // #endregion


  function mkNativeProc(
    env: Env, name: string, params: string[],
    cb: (args: Form, env: Env) => any
  ): Form | NativeProc {

    const func = new class extends NativeProc {
      public name = name;
      public env = env;
      public params = params.map(Sym);
      public call = cb;
    };

    env.set(name, func);
    return func;
  }
}
