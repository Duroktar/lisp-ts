import { join } from "path";
import { FALSE, TRUE } from "./lib/const";
import { callWithCC } from "./lib/cont";
import { Env } from "./lib/env";
import { evaluate } from "./lib/eval";
import { expand } from "./lib/expand";
import * as Lisp from "./lib/lisp";
import { readMacroTable } from "./lib/macro";
import { Sym } from "./lib/sym";
import { List } from "./lib/terms";
import { executeFile } from "./load";
import { gcd, lcm } from "./math";
import * as Util from "./utils";

export const env = new Env();

env.set('#t', TRUE);
env.set('#f', FALSE);

env.set('else', TRUE);
env.set('otherwise', TRUE);
env.set('cwd', process.cwd());

const { mkNativeFunc } = Util

mkNativeFunc(env, 'cons', ['a', 'b'], ([a, b]: any) => [a].concat(b));
mkNativeFunc(env, 'car', ['args'], (args: any) => Lisp.car(args));
mkNativeFunc(env, 'cdr', ['args'], (args: any) => Lisp.cdr(args));
mkNativeFunc(env, 'set-cdr!', ['l', 'v'], ([l, v]: any) => l[1] = v);

mkNativeFunc(env, 'locals', [], (_, a) => { return a; });
mkNativeFunc(env, 'env', [], () => { return env; });
mkNativeFunc(env, 'env->size', [], () => { return env.size(); });
mkNativeFunc(env, 'env->keys', [], () => { return env.keys(); });
mkNativeFunc(env, 'env->values', [], () => { return env.values(); });
mkNativeFunc(env, 'env->entries', [], () => { return env.entries(); });

mkNativeFunc(env, 'debugnf', ['name', 'x'], ([name, x]: any) => { console.log('[DEBUG-NF]:', Util.toString(name)); console.log(x); });
mkNativeFunc(env, 'debugn', ['name', 'x'], ([name, x]: any) => { console.log('[DEBUG-N]:', Util.toString(name)); console.log(x); return x; });
mkNativeFunc(env, 'debugf', ['x'], x => { console.log('[DEBUG-F]'); console.log(x); });
mkNativeFunc(env, 'debug', ['x'], x => { console.log('[DEBUG]'); console.log(x); return x; });
mkNativeFunc(env, 'printn', ['name', 'x'], ([name, x]: any) => { console.log(Util.toString(name), Util.toString(x)); });
mkNativeFunc(env, 'printr', ['x'], ([x]: any) => { Util.print(x); return x});
mkNativeFunc(env, 'prints', ['...xs'], ([...xs]: any) => { console.log(...xs); });
mkNativeFunc(env, 'print', ['...xs'], ([...xs]: any) => { console.log(...xs.map((x: any) => Util.toString(x))); });
mkNativeFunc(env, 'show', ['x'], x => { console.log(x); });
mkNativeFunc(env, 'display', ['x'], ([x]: any) => { Util.print(x, false, 'lambda'); });
mkNativeFunc(env, 'newline', [], () => { console.log(); });
mkNativeFunc(env, 'inspect', ['x'], ([x]: any) => { return Util.toString(x, true); });
mkNativeFunc(env, 'break', ['x'], x => { debugger; return x; });
mkNativeFunc(env, 'error', ['x', 'code?'], ([x, code = 1]: any) => { console.error(x); process.exit(code); });

mkNativeFunc(env, 'gensym', [], () => Symbol());

mkNativeFunc(env, 'load', ['file', 'topLevel?'], ([file, topLevel = true]: any, a) => {
  executeFile(join(<string>env.get('cwd'), file), topLevel ? env : a)
});

mkNativeFunc(env, 'pair?', ['obj'], ([obj]: any) => Util.toL(Util.isPair(obj)));
mkNativeFunc(env, 'eq?', ['a', 'b'], ([a, b]: any) => Util.toL(Util.isEq(a, b)));
mkNativeFunc(env, 'eqv?', ['a', 'b'], ([a, b]: any) => Util.toL(Util.isEq(a, b)));
mkNativeFunc(env, 'equal?', ['a', 'b'], ([a, b]: any) => Util.toL(Util.toString(a) === Util.toString(b)));

mkNativeFunc(env, 'length', ['list'], ([list]: any) => Util.isList(list) && list.length);
mkNativeFunc(env, 'reverse', ['list'], ([list]: any) => Util.isList(list) && [...list].reverse());

mkNativeFunc(env, 'min', ['args'], (args: any) => args.reduce((acc: any, val: any) => Math.min(acc, val)));
mkNativeFunc(env, 'max', ['args'], (args: any) => args.reduce((acc: any, val: any) => Math.max(acc, val)));
mkNativeFunc(env, 'abs', ['n'], ([n]: any) => Math.abs(n));
mkNativeFunc(env, 'quotient', ['x', 'y'], ([x, y]: any) => x/y|0);
mkNativeFunc(env, 'gcd', ['a', 'b'], ([a, b]: any) => { return gcd(a, b) });
mkNativeFunc(env, 'lcm', ['a', 'b'], ([a, b]: any) => { return lcm(a, b) });
mkNativeFunc(env, 'floor', ['n'], ([n]: any) => Math.floor(n));
mkNativeFunc(env, 'ceiling', ['n'], ([n]: any) => Math.ceil(n));
mkNativeFunc(env, 'truncate', ['n'], ([n]: any) => Math.trunc(n));
mkNativeFunc(env, 'round', ['n'], ([n]: any) => Math.round(n));
mkNativeFunc(env, 'exp', ['n'], ([n]: any) => Math.exp(n));
mkNativeFunc(env, 'log', ['n'], ([n]: any) => Math.log(n));
mkNativeFunc(env, 'sin', ['n'], ([n]: any) => Math.sin(n));
mkNativeFunc(env, 'cos', ['n'], ([n]: any) => Math.cos(n));
mkNativeFunc(env, 'tan', ['n'], ([n]: any) => Math.tan(n));
mkNativeFunc(env, 'asin', ['n'], ([n]: any) => Math.asin(n));
mkNativeFunc(env, 'acos', ['n'], ([n]: any) => Math.acos(n));
mkNativeFunc(env, 'atan', ['y', 'x'], ([y, x]: any) => Util.isNone(x) ? Math.atan(y) : Math.atan2(y, x));
mkNativeFunc(env, 'sqrt', ['n'], ([n]: any) => Math.sqrt(n));
mkNativeFunc(env, 'expt', ['n'], ([n]: any) => Math.exp(n));

mkNativeFunc(env, '+', ['args'], (args: any) => args.reduce((acc: any, val: any) => acc + val, 0));
mkNativeFunc(env, '*', ['args'], (args: any) => args.reduce((acc: any, val: any) => acc * val, 1));
mkNativeFunc(env, '-', ['args'], (args: any) => {
  Util.assert(args.length > 0, "procedure requires at least one argument: (-)")
  if (args.length === 1) return -args[0]
  else return args.reduce((acc: any, val: any) => acc - val)
});
mkNativeFunc(env, '/', ['args'], (args: any) => {
  Util.assert(args.length > 0, "procedure requires at least one argument: (/)")
  args.reduce((acc: any, val: any) => acc / val)
});
mkNativeFunc(env, '=', ['args'], (args: any) => args.reduce((acc: any, val: any) => Util.toL(acc === val)));
mkNativeFunc(env, '>', ['args'], (args: any) => args.reduce((acc: any, val: any) => Util.toL(acc > val)));
mkNativeFunc(env, '<', ['args'], (args: any) => args.reduce((acc: any, val: any) => Util.toL(acc < val)));
mkNativeFunc(env, '>=', ['args'], ([l, r]: any) => Util.toL(l >= r));
mkNativeFunc(env, '<=', ['args'], ([l, r]: any) => Util.toL(l <= r));

mkNativeFunc(env, 'zero?', ['n'], ([n]: any) => Util.toL(n === 0));
mkNativeFunc(env, 'number?', ['n'], ([n]: any) => Util.toL(Util.isNum(n)));
mkNativeFunc(env, 'positive?', ['n'], ([n]: any) => Util.toL(Util.isNum(n) && n > 0));
mkNativeFunc(env, 'negative?', ['n'], ([n]: any) => Util.toL(Util.isNum(n) && n < 0));
mkNativeFunc(env, 'odd?', ['n'], ([n]: any) => Util.toL(Util.isNum(n) && n % 2 !== 0));
mkNativeFunc(env, 'even?', ['n'], ([n]: any) => Util.toL(Util.isNum(n) && n % 2 === 0));
mkNativeFunc(env, 'boolean?', ['n'], ([n]: any) => Util.toL(n === FALSE || n === TRUE));
mkNativeFunc(env, 'null?', ['n'], ([n]: any) => Util.toL(Util.isEmpty(n)));
mkNativeFunc(env, 'list?', ['n'], ([n]: any) => Util.toL(Util.isList(n)));
mkNativeFunc(env, 'symbol?', ['n'], ([n]: any) => Util.toL(Util.isSym(n)));
mkNativeFunc(env, 'symbol->string', ['n'], ([n]: any) => {
  Util.assert(Util.isSym(n), `"symbol->string" procedure takes a 'symbol' as an argument`);
  return Util.toString(n)
});
mkNativeFunc(env, 'number->string', ['n', 'radix?'], ([n, radix]: any) => {
  Util.assert(Util.isNum(n), `"number->string" procedure takes a 'number' as an argument`);
  return (<number>n).toString(radix ?? 10);
});
mkNativeFunc(env, 'string->number', ['n', 'radix?'], ([n, radix]: any) => {
  Util.assert(Util.isString(n), `"string->number" procedure takes a 'string' as an argument`);
  return parseInt(n, radix ?? 10);
});
mkNativeFunc(env, 'string->symbol', ['n'], ([n]: any) => {
  Util.assert(Util.isString(n), `"string->symbol" procedure takes a 'string' as an argument`);
  return Sym(n)
});
mkNativeFunc(env, 'string?', ['n'], ([n]: any) => Util.toL(Util.isString(n)));
mkNativeFunc(env, 'string-length', ['n'], ([n]: any) => {
  Util.assert(Util.isString(n))
  return n.length
});
mkNativeFunc(env, 'string-ref', ['string', 'k'], ([string, k]: any) => {
  Util.assert(Util.isString(string) && string.length >= k)
  return string[k]
});
mkNativeFunc(env, 'string-set', ['string', 'k', 'char'], ([string, k, char]: any) => {
  Util.assert(Util.isString(string) && string.length >= k)
  Util.assert(Util.isChar(char))
  string[k] = char
  return []
});
mkNativeFunc(env, 'string=?', ['string1', 'string2'], ([string1, string2]: any) => {
  Util.assert(Util.isString(string1) && Util.isString(string2))
  return Util.toL(string1 === string2)
});
mkNativeFunc(env, 'string-ci=?', ['string1', 'string2'], ([string1, string2]: any) => {
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
mkNativeFunc(env, 'string<?', ['string1', 'string2'], ([string1, string2]: any) => {
  Util.assert(Util.isString(string1) && Util.isString(string2))
  return Util.toL(string1 < string2)
});
mkNativeFunc(env, 'string>?', ['string1', 'string2'], ([string1, string2]: any) => {
  Util.assert(Util.isString(string1) && Util.isString(string2))
  return Util.toL(string1 > string2)
});
mkNativeFunc(env, 'string<=?', ['string1', 'string2'], ([string1, string2]: any) => {
  Util.assert(Util.isString(string1) && Util.isString(string2))
  return Util.toL(string1 <= string2)
});
mkNativeFunc(env, 'string>=?', ['string1', 'string2'], ([string1, string2]: any) => {
  Util.assert(Util.isString(string1) && Util.isString(string2))
  return Util.toL(string1 >= string2)
});
mkNativeFunc(env, 'string-ci<?', ['string1', 'string2'], ([string1, string2]: any) => {
  Util.assert(Util.isString(string1) && Util.isString(string2))
  return Util.toL(string1.toLowerCase() < string2.toLowerCase())
});
mkNativeFunc(env, 'string-ci>?', ['string1', 'string2'], ([string1, string2]: any) => {
  Util.assert(Util.isString(string1) && Util.isString(string2))
  return Util.toL(string1.toLowerCase() > string2.toLowerCase())
});
mkNativeFunc(env, 'string-ci<=?', ['string1', 'string2'], ([string1, string2]: any) => {
  Util.assert(Util.isString(string1) && Util.isString(string2))
  return Util.toL(string1.toLowerCase() <= string2.toLowerCase())
});
mkNativeFunc(env, 'string-ci>=?', ['string1', 'string2'], ([string1, string2]: any) => {
  Util.assert(Util.isString(string1) && Util.isString(string2))
  return Util.toL(string1.toLowerCase() >= string2.toLowerCase())
});
mkNativeFunc(env, 'substring', ['string', 'start', 'end'], ([string, start, end]: any) => {
  // Util.assert(Util.isString(string) && string.length >= start && string.length >= end && start < end)
  return (<string>string).slice(start, end)
});
mkNativeFunc(env, 'string-append', ['string', '...xs'], ([string, ...xs]: any) => {
  // Util.assert(Util.isString(string))
  return (<string>string).concat(...xs)
});
mkNativeFunc(env, 'string->list', ['string', '...'], ([string]: any) => {
  Util.assert(Util.isString(string))
  return (<string>string).split('')
});
mkNativeFunc(env, 'list->string', ['list', '...'], ([list]: any) => {
  Util.assert(Util.isList(list) && list.map(o => Util.isString(o)))
  return (<List>list).join('')
});
mkNativeFunc(env, 'string-copy', ['string'], ([string]: any) => {
  Util.assert(Util.isString(string))
  return String(string)
});
mkNativeFunc(env, 'string-fill', ['string', 'char'], ([string, char]: any) => {
  Util.assert(Util.isString(string))
  Util.assert(Util.isChar(char))
  string.replaceAll(/.*/, char)
  return string
});
mkNativeFunc(env, 'string-pad-end', ['string', 'maxLength', '.', 'fillString'], ([string, maxLength, ...[fillString]]: any) => {
  Util.assert(Util.isString(string));
  Util.assert(Util.isNum(maxLength));
  return (string as string).padEnd(maxLength, fillString);
});
mkNativeFunc(env, 'string-pad-start', ['string', 'maxLength', '.', 'fillString'], ([string, maxLength, ...[fillString]]: any) => {
  Util.assert(Util.isString(string));
  Util.assert(Util.isNum(maxLength));
  return (string as string).padStart(maxLength, fillString);
});
mkNativeFunc(env, 'procedure?', ['obj'], ([obj]: any) => {
  return Util.toL(Util.isCallable(obj))
});

mkNativeFunc(env, 'not', ['n'], ([n]: any) => Util.toL(n === FALSE));

mkNativeFunc(env, 'set-macro-character', ['char', 'cb'], ([char, cb]: any, env) => {
  readMacroTable[Util.toString(char)] = locals => {
    const proc = evaluate(cb, env);
    if (Util.isCallable(proc)) {
      mkNativeFunc(proc.env, 'read', ['read'], ([locals]: any) => locals.parse());
      mkNativeFunc(proc.env, 'advance', ['advance'], ([locals]: any) => locals.advance());
      mkNativeFunc(proc.env, 'current', ['current'], ([locals]: any) => locals.current());
      mkNativeFunc(proc.env, 'isEOF', ['isEOF'], ([locals]: any) => locals.isEOF());
      mkNativeFunc(proc.env, 'isSpace', ['isSpace'], ([locals]: any) => locals.isSpace());
      mkNativeFunc(proc.env, 'isNewLine', ['isNewLine'], ([locals]: any) => locals.isNewLine());
      return evaluate([proc, locals, Util.toString(char)], env);
    }
    throw new Error('Nope @ set-macro-character');
  };
});

mkNativeFunc(env, 'call/cc', ['throw'], callWithCC);
mkNativeFunc(env, 'call-with-current-continuation', ['throw'], callWithCC);

mkNativeFunc(env, 'try', ['callable'], ([callable]: any) => {
  try {
    if (Util.isCallable(callable)) {
      return [TRUE, callable.call([])];
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

mkNativeFunc(env, 'macroexpand', ['expr'], (args: any, env) => {
  return expand(args[0], true, env);
});

/*
*
*  reader macros
*
*/

/*
*
*  macros
*
*/
Lisp.execute(`
(begin

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

  (define-syntax case
    (syntax-rules (else)
      ([case (key ...)
          clauses ...]
        (let ([atom-key (key ...)])
          (case atom-key clauses ...)))
      ((case key
          (else result1 result2 ...))
        (begin result1 result2 ...))
      ([case key
          ((atoms ...) result1 result2 ...)]
        (if [memv key '(atoms ...)]
          (begin result1 result2 ...)))
      ([case key
          ((atoms ...) result1 result2 ...)
          clause clauses ...]
        (if [memv key '(atoms ...)]
            (begin result1 result2 ...)
            (case key clause clauses ...)))))

  (define-syntax let*
    (syntax-rules ()
      ((let* () body1 body2 ...)
        (let () body1 body2 ...))
      ((let* ((name1 val1) (name2 val2) ...)
          body1 body2 ...)
        (let ((name1 val1))
          (let* ((name2 val2) ...)
            body1 body2 ...)))))
)`, env)

// exec(
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
Lisp.execute(`
(begin
  (defun caar   (x) (car (car x)))
  (defun cadr   (x) (car (cdr x)))
  (defun cdar   (x) (cdr (car x)))
  (defun cddr   (x) (cdr (cdr x)))

  (defun caaar  (x) (car (car (car x))))
  (defun caadr  (x) (car (car (cdr x))))
  (defun cadar  (x) (car (cdr (car x))))
  (defun caddr  (x) (car (cdr (cdr x))))
  (defun cdaar  (x) (cdr (car (car x))))
  (defun cdadr  (x) (cdr (car (cdr x))))
  (defun cddar  (x) (cdr (cdr (car x))))
  (defun cdddr  (x) (cdr (cdr (cdr x))))

  (defun caaaar (x) (car (car (car (car x)))))
  (defun caaadr (x) (car (car (car (cdr x)))))
  (defun caadar (x) (car (car (cdr (car x)))))
  (defun caaddr (x) (car (car (cdr (cdr x)))))
  (defun cadaar (x) (car (cdr (car (car x)))))
  (defun cadadr (x) (car (cdr (car (cdr x)))))
  (defun caddar (x) (car (cdr (cdr (car x)))))
  (defun cadddr (x) (car (cdr (cdr (cdr x)))))

  (defun cdaaar (x) (cdr (car (car (car x)))))
  (defun cdaadr (x) (cdr (car (car (cdr x)))))
  (defun cdadar (x) (cdr (car (cdr (car x)))))
  (defun cdaddr (x) (cdr (car (cdr (cdr x)))))
  (defun cddaar (x) (cdr (cdr (car (car x)))))
  (defun cddadr (x) (cdr (cdr (car (cdr x)))))
  (defun cdddar (x) (cdr (cdr (cdr (car x)))))
  (defun cddddr (x) (cdr (cdr (cdr (cdr x)))))

  (defun list x x)

  (defun append (lst1 lst2)
    (if (pair? lst1)
        (cons (car lst1) (append (cdr lst1) lst2))
        lst2))

  (defun list-ref (lst i)
    (car (list-tail lst i)))

  (defun list-set! (lst i x)
    (set-car! (list-tail lst i) x))

  (defun list-tail (lst i)
    (if (< 0 i)
        (list-tail (cdr lst) (- i 1))
        lst))

  (defun memv (x lst)
    (if (pair? lst)
        (if (eqv? x (car lst))
            lst
            (memv x (cdr lst)))
        #f))

  (define memq memv)

  (defun member (x lst)
    (if (pair? lst)
        (if (equal? x (car lst))
            lst
            (member x (cdr lst)))
        #f))

  (defun assv (x lst)
    (if (pair? lst)
        (let ((couple (car lst)))
          (if (eqv? x (car couple))
              couple
              (assv x (cdr lst))))
        #f))

  (define assq assv)

  (defun assoc (x lst)
    (if (pair? lst)
        (let ((couple (car lst)))
          (if (equal? x (car couple))
              couple
              (assoc x (cdr lst))))
        #f))

  (defun remainder (x y)
    (- x (* y (quotient x y))))

  (defun modulo (x y)
    (let ((q (quotient x y)))
      (let ((r (- x (* y q))))
        (if (eqv? r 0)
            0
            (if (eqv? (< x 0) (< y 0))
                r
                (+ r y))))))

  (defun sub1 (x) (- x 1))
)`, env)
