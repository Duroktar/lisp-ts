import { gcd, lcm } from "./math";
import { Env } from "./lib/env";
import * as Errors from "./lib/errors";
import { Lisp } from "./lib/lisp";
import { Expr, List } from "./lib/terms";
import { assert, isList, isCallable, isEmpty, isNone, isNum, mkNativeFunc, print, toL, toString, isSym, isString, isChar } from "./utils";

export const env = new Env();

env.set('#t', Lisp.TRUE);
env.set('#f', Lisp.FALSE);

env.set('else', Lisp.TRUE);
env.set('otherwise', Lisp.TRUE);
mkNativeFunc(env, 'car', ['args'], (args: any) => Lisp.car(args));
mkNativeFunc(env, 'cdr', ['args'], (args: any) => Lisp.cdr(args));

mkNativeFunc(env, 'debugnf', ['name', 'x'], ([name, x]: any) => { console.log('[DEBUG-NF]:', toString(name)); console.log(x); return []; });
mkNativeFunc(env, 'debugn', ['name', 'x'], ([name, x]: any) => { console.log('[DEBUG-N]:', toString(name)); console.log(x); return x; });
mkNativeFunc(env, 'debugf', ['x'], x => { console.log('[DEBUG-F]'); console.log(x); return []; });
mkNativeFunc(env, 'debug', ['x'], x => { console.log('[DEBUG]'); console.log(x); return x; });
mkNativeFunc(env, 'printn', ['name', 'x'], ([name, x]: any) => { console.log(name, toString(x)); return []; });
mkNativeFunc(env, 'printl', ['x'], ([name, x]: any) => { console.log(name, toString(x)); return []; });
mkNativeFunc(env, 'printr', ['x'], ([x]: any) => { print(x); return x});
mkNativeFunc(env, 'print', ['x'], ([x]: any) => { print(x); });
mkNativeFunc(env, 'inspect', ['x'], ([x]: any) => { return toString(x, true); });
mkNativeFunc(env, 'display', ['x'], ([x]: any) => { print(x, false, 'lambda'); });
mkNativeFunc(env, 'break', ['x'], x => { debugger; return x; });

mkNativeFunc(env, 'gensym', [], () => Symbol());

mkNativeFunc(env, 'append', ['list', '...'], ([args]: any) => args.reduce((acc: any, val: any) => acc.concat(val)));
mkNativeFunc(env, 'length', ['list'], ([list]: any) => isList(list) && list.length);
mkNativeFunc(env, 'reverse', ['list'], ([list]: any) => isList(list) && [...list].reverse());
mkNativeFunc(env, 'list-tail', ['list', 'k'], ([list, k]: any) => {
  assert(isList(list), 'argument to list-tail must be a list')
  assert(list.length >= k, 'list has fewer than k elements')
  return (<any[]>list).slice(k)
});
mkNativeFunc(env, 'list-ref', ['list', 'k'], ([list, k]: any) => {
  assert(isList(list), 'argument to list-tail must be a list')
  assert(list.length >= k, 'list has fewer than k elements')
  return (<any[]>list).at(k)
});
mkNativeFunc(env, 'memq', ['obj', 'list'], ([obj, list]: any) => {
  assert(isList(list), 'argument to list-tail must be a list') // TODO
  const l = <any[]>list;
  const i = l.findIndex(o => /* eq? */o === obj);
  if (i === -1) {
    return Lisp.FALSE
  }
  return l.slice(i)
});
mkNativeFunc(env, 'memv', ['obj', 'list'], ([obj, list]: any) => {
  assert(isList(list), 'argument to list-tail must be a list') // TODO
  const l = <any[]>list;
  const i = l.findIndex(o => /* eqv? */o === obj);
  if (i === -1) {
    return Lisp.FALSE
  }
  return l.slice(i)
});
mkNativeFunc(env, 'member', ['obj', 'list'], ([obj, list]: any) => {
  assert(isList(list), 'argument to list-tail must be a list') // TODO
  const l = <any[]>list;
  const i = l.findIndex(o => /* equal? */o === obj);
  if (i === -1) {
    return Lisp.FALSE
  }
  return l.slice(i)
});

mkNativeFunc(env, 'min', ['args'], (args: any) => args.reduce((acc: any, val: any) => Math.min(acc, val)));
mkNativeFunc(env, 'max', ['args'], (args: any) => args.reduce((acc: any, val: any) => Math.max(acc, val)));
mkNativeFunc(env, 'abs', ['n'], ([n]: any) => Math.abs(n));
// mkNativeFunc(env, 'modulo', ['a', 'b'], ([a, b]: any) => Math.round(a / b)); // TODO: https://schemers.org/Documents/Standards/R5RS/HTML/
mkNativeFunc(env, 'remainder', ['a', 'b'], ([a, b]: any) => a % b);
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
mkNativeFunc(env, 'atan', ['y', 'x'], ([y, x]: any) => isNone(x) ? Math.atan(y) : Math.atan2(y, x));
mkNativeFunc(env, 'sqrt', ['n'], ([n]: any) => Math.sqrt(n));
mkNativeFunc(env, 'expt', ['n'], ([n]: any) => Math.exp(n));

mkNativeFunc(env, '+', ['args'], (args: any) => args.reduce((acc: any, val: any) => acc + val, 0));
mkNativeFunc(env, '*', ['args'], (args: any) => args.reduce((acc: any, val: any) => acc * val, 1));
mkNativeFunc(env, '-', ['args'], (args: any) => {
  assert(args.length > 0, "procedure requires at least one argument: (-)")
  return args.reduce((acc: any, val: any) => acc - val)
});
mkNativeFunc(env, '/', ['args'], (args: any) => {
  assert(args.length > 0, "procedure requires at least one argument: (/)")
  args.reduce((acc: any, val: any) => acc / val)
});
mkNativeFunc(env, '=', ['args'], (args: any) => args.reduce((acc: any, val: any) => toL(acc === val)));
mkNativeFunc(env, '>', ['args'], (args: any) => args.reduce((acc: any, val: any) => toL(acc > val)));
mkNativeFunc(env, '<', ['args'], (args: any) => args.reduce((acc: any, val: any) => toL(acc < val)));
mkNativeFunc(env, '>=', ['args'], ([l, r]: any) => toL(l >= r));
mkNativeFunc(env, '<=', ['args'], ([l, r]: any) => toL(l <= r));

mkNativeFunc(env, 'zero?', ['n'], ([n]: any) => n === 0);
mkNativeFunc(env, 'number?', ['n'], ([n]: any) => toL(isNum(n)));
mkNativeFunc(env, 'positive?', ['n'], ([n]: any) => toL(isNum(n) && n > 0));
mkNativeFunc(env, 'negative?', ['n'], ([n]: any) => toL(isNum(n) && n < 0));
mkNativeFunc(env, 'odd?', ['n'], ([n]: any) => toL(isNum(n) && n % 2 !== 0));
mkNativeFunc(env, 'even?', ['n'], ([n]: any) => toL(isNum(n) && n % 2 === 0));
mkNativeFunc(env, 'boolean?', ['n'], ([n]: any) => toL(n === Lisp.FALSE || n === Lisp.TRUE));
mkNativeFunc(env, 'null?', ['n'], ([n]: any) => toL(isEmpty(n)));
mkNativeFunc(env, 'list?', ['n'], ([n]: any) => toL(isList(n)));
mkNativeFunc(env, 'symbol?', ['n'], ([n]: any) => toL(isSym(n)));
mkNativeFunc(env, 'symbol->string', ['n'], ([n]: any) => {
  assert(isSym(n), `"symbol->string" procedure takes a 'symbol' as an argument`);
  return toString(n)
});
mkNativeFunc(env, 'string->symbol', ['n'], ([n]: any) => {
  assert(isString(n), `"string->symbol" procedure takes a 'string' as an argument`);
  return Lisp.Sym(n)
});
mkNativeFunc(env, 'string?', ['n'], ([n]: any) => toL(isString(n)));
mkNativeFunc(env, 'string-length', ['n'], ([n]: any) => {
  assert(isString(n))
  return n.length
});
mkNativeFunc(env, 'string-ref', ['string', 'k'], ([string, k]: any) => {
  assert(isString(string) && string.length >= k)
  return string[k]
});
mkNativeFunc(env, 'string-set', ['string', 'k', 'char'], ([string, k, char]: any) => {
  assert(isString(string) && string.length >= k)
  assert(isChar(char))
  string[k] = char
  return []
});
mkNativeFunc(env, 'string=?', ['string1', 'string2'], ([string1, string2]: any) => {
  assert(isString(string1) && isString(string2))
  return string1 === string2
});
mkNativeFunc(env, 'string-ci=?', ['string1', 'string2'], ([string1, string2]: any) => {
  assert(isString(string1) && isString(string2))
  if (string1.length !== string2.length)
    return false
  for (let i = 0; i < string1.length; i++) {
    if ((<string>string1[i]).toLowerCase() === string2[i].toLowerCase())
      continue
    return false
  }
});
mkNativeFunc(env, 'string<?', ['string1', 'string2'], ([string1, string2]: any) => {
  assert(isString(string1) && isString(string2))
  return string1 < string2
});
mkNativeFunc(env, 'string>?', ['string1', 'string2'], ([string1, string2]: any) => {
  assert(isString(string1) && isString(string2))
  return string1 > string2
});
mkNativeFunc(env, 'string<=?', ['string1', 'string2'], ([string1, string2]: any) => {
  assert(isString(string1) && isString(string2))
  return string1 <= string2
});
mkNativeFunc(env, 'string>=?', ['string1', 'string2'], ([string1, string2]: any) => {
  assert(isString(string1) && isString(string2))
  return string1 >= string2
});
mkNativeFunc(env, 'string-ci<?', ['string1', 'string2'], ([string1, string2]: any) => {
  assert(isString(string1) && isString(string2))
  return string1.toLowerCase() < string2.toLowerCase()
});
mkNativeFunc(env, 'string-ci>?', ['string1', 'string2'], ([string1, string2]: any) => {
  assert(isString(string1) && isString(string2))
  return string1.toLowerCase() > string2.toLowerCase()
});
mkNativeFunc(env, 'string-ci<=?', ['string1', 'string2'], ([string1, string2]: any) => {
  assert(isString(string1) && isString(string2))
  return string1.toLowerCase() <= string2.toLowerCase()
});
mkNativeFunc(env, 'string-ci>=?', ['string1', 'string2'], ([string1, string2]: any) => {
  assert(isString(string1) && isString(string2))
  return string1.toLowerCase() >= string2.toLowerCase()
});
mkNativeFunc(env, 'substring', ['string', 'start', 'end'], ([string, start, end]: any) => {
  assert(isString(string) && string.length >= start && string.length >= end && start < end)
  return (<string>string).substring(start, end)
});
mkNativeFunc(env, 'string-append', ['string', '...'], ([string, ...xs]: any) => {
  assert(isString(string) && xs.map((x: any) => isString(x)))
  return (<string>string).concat(...xs)
});
mkNativeFunc(env, 'string->list', ['string', '...'], ([string]: any) => {
  assert(isString(string))
  return (<string>string).split('')
});
mkNativeFunc(env, 'list->string', ['list', '...'], ([list]: any) => {
  assert(isList(list) && list.map(o => isString(o)))
  return (<List>list).join('')
});
mkNativeFunc(env, 'string-copy', ['string'], ([string]: any) => {
  assert(isString(string))
  return String(string)
});
mkNativeFunc(env, 'string-fill', ['string'], ([string, char]: any) => {
  assert(isString(string))
  assert(isChar(char))
  string.replaceAll(/.*/, char)
  return []
});
mkNativeFunc(env, 'procedure?', ['obj'], ([obj]: any) => {
  return toL(isCallable(obj))
});

mkNativeFunc(env, 'not', ['n'], ([n]: any) => toL(n === Lisp.FALSE));

mkNativeFunc(env, 'set-macro-character', ['char', 'cb'], ([char, cb]: any, env) => {
  Lisp.readMacroTable[toString(char)] = locals => {
    const proc = Lisp.evaluate(cb, env);
    if (isCallable(proc)) {
      mkNativeFunc(proc.env, 'read', ['read'], ([locals]: any) => locals.parse());
      mkNativeFunc(proc.env, 'advance', ['advance'], ([locals]: any) => locals.advance());
      mkNativeFunc(proc.env, 'current', ['current'], ([locals]: any) => locals.current());
      mkNativeFunc(proc.env, 'isEOF', ['isEOF'], ([locals]: any) => locals.isEOF());
      mkNativeFunc(proc.env, 'isSpace', ['isSpace'], ([locals]: any) => locals.isSpace());
      mkNativeFunc(proc.env, 'isNewLine', ['isNewLine'], ([locals]: any) => locals.isNewLine());
      return Lisp.evaluate([proc, locals, toString(char)], env);
    }
    throw new Error('Nope @ set-macro-character');
  };
});

mkNativeFunc(env, 'call/cc', ['throw'], ([proc]: any, env) => {
  class RuntimeWarning extends Error { public retval?: any; }
  let ball = new RuntimeWarning("Sorry, can't continue this continuation any longer.");
  const throw_ = mkNativeFunc(env, 'throw', ['retval'], retval => {
    ball.retval = retval; throw ball;
  });
  try {
    if (isCallable(proc)) {
      return proc.call([throw_ as Expr]);
    }
    throw new Errors.InvalidCallableExpression(proc);
  } catch (err) {
    if (err instanceof RuntimeWarning) {
      // console.log(`exiting call/cc [${id}] (THROWN)`)
      return ball.retval;
    }
    else {
      throw err;
    }
  }
});

mkNativeFunc(env, 'macroexpand', ['expr'], (args: any, env) => {
  return Lisp.expand(Lisp.car(args), true, env);
});

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
Lisp.exec(`
  (define-macro while (condition body)
    \`(let loop ()
          (cond (,condition
              (begin ,body)
              (loop)))))
`, env);

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
Lisp.exec(`(defun caar   (x) (car (car x)))`, env);
Lisp.exec(`(defun cadr   (x) (car (cdr x)))`, env);
Lisp.exec(`(defun cdar   (x) (cdr (car x)))`, env);
Lisp.exec(`(defun cddr   (x) (cdr (cdr x)))`, env);

Lisp.exec(`(defun caaar  (x) (car (car (car x))))`, env);
Lisp.exec(`(defun caadr  (x) (car (car (cdr x))))`, env);
Lisp.exec(`(defun cadar  (x) (car (cdr (car x))))`, env);
Lisp.exec(`(defun caddr  (x) (car (cdr (cdr x))))`, env);
Lisp.exec(`(defun cdaar  (x) (cdr (car (car x))))`, env);
Lisp.exec(`(defun cdadr  (x) (cdr (car (cdr x))))`, env);
Lisp.exec(`(defun cddar  (x) (cdr (cdr (car x))))`, env);
Lisp.exec(`(defun cdddr  (x) (cdr (cdr (cdr x))))`, env);

Lisp.exec(`(defun caaaar (x) (car (car (car (car x)))))`, env);
Lisp.exec(`(defun caaadr (x) (car (car (car (cdr x)))))`, env);
Lisp.exec(`(defun caadar (x) (car (car (cdr (car x)))))`, env);
Lisp.exec(`(defun caaddr (x) (car (car (cdr (cdr x)))))`, env);
Lisp.exec(`(defun cadaar (x) (car (cdr (car (car x)))))`, env);
Lisp.exec(`(defun cadadr (x) (car (cdr (car (cdr x)))))`, env);
Lisp.exec(`(defun caddar (x) (car (cdr (cdr (car x)))))`, env);
Lisp.exec(`(defun cadddr (x) (car (cdr (cdr (cdr x)))))`, env);

Lisp.exec(`(defun cdaaar (x) (cdr (car (car (car x)))))`, env);
Lisp.exec(`(defun cdaadr (x) (cdr (car (car (cdr x)))))`, env);
Lisp.exec(`(defun cdadar (x) (cdr (car (cdr (car x)))))`, env);
Lisp.exec(`(defun cdaddr (x) (cdr (car (cdr (cdr x)))))`, env);
Lisp.exec(`(defun cddaar (x) (cdr (cdr (car (car x)))))`, env);
Lisp.exec(`(defun cddadr (x) (cdr (cdr (car (cdr x)))))`, env);
Lisp.exec(`(defun cdddar (x) (cdr (cdr (cdr (car x)))))`, env);
Lisp.exec(`(defun cddddr (x) (cdr (cdr (cdr (cdr x)))))`, env);

Lisp.exec(`(defun list x x)`, env);

Lisp.exec(`
  (defun assq (x y)
    (cond ((eq? (caar y) x) (cadar y))
          ((null? (cdr y)) '())
          (else (assq x (cdr y)))))`
, env)
Lisp.exec(`
  (defun assv (x y)
    (cond ((eqv? (caar y) x) (cadar y))
          ((null? (cdr y)) '())
          (else (assv x (cdr y)))))`
, env)
Lisp.exec(`
  (defun assoc (x y)
    (cond ((equal? (caar y) x) (cadar y))
          ((null? (cdr y)) '())
          (else (assoc x (cdr y)))))`
, env)

Lisp.exec(`(defun sub1 (x) (- x 1))`, env)
