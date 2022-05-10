import { Env } from "./lib/env";
import * as Errors from "./lib/errors";
import { Lisp } from "./lib/lisp";
import { Expr } from "./lib/terms";
import { isCallable, mkNativeFunc, print, toL, toString } from "./utils";

export const env = new Env();
env.set('#t', '#t');
env.set('#f', '#f');

mkNativeFunc(env, 'debugnf', ['name', 'x'], ([name, x]: any) => { console.log('[DEBUG-NF]:', toString(name)); console.log(x); return []; });
mkNativeFunc(env, 'debugn', ['name', 'x'], ([name, x]: any) => { console.log('[DEBUG-N]:', toString(name)); console.log(x); return x; });
mkNativeFunc(env, 'debugf', ['x'], x => { console.log('[DEBUG-F]'); console.log(x); return []; });
mkNativeFunc(env, 'debug', ['x'], x => { console.log('[DEBUG]'); console.log(x); return x; });
mkNativeFunc(env, 'printn', ['name', 'x'], ([name, x]: any) => { console.log(name, toString(x)); return []; });
mkNativeFunc(env, 'printl', ['x'], ([name, x]: any) => { console.log(name, toString(x)); return []; });
mkNativeFunc(env, 'inspect', ['x'], ([x]: any) => { return toString(x, true); });
mkNativeFunc(env, 'display', ['x'], ([x]: any) => { print(x); });
mkNativeFunc(env, 'print', ['x'], ([x]: any) => { print(x); });
mkNativeFunc(env, 'break', ['x'], x => { debugger; return x; });

mkNativeFunc(env, 'gensym', [], () => Symbol());

mkNativeFunc(env, 'append', ['args'], (args: any) => args.reduce((acc: any, val: any) => acc.concat(val)));
mkNativeFunc(env, '+', ['args'], (args: any) => args.reduce((acc: any, val: any) => acc + val));
mkNativeFunc(env, '-', ['args'], (args: any) => args.reduce((acc: any, val: any) => acc - val));
mkNativeFunc(env, '*', ['args'], (args: any) => args.reduce((acc: any, val: any) => acc * val));
mkNativeFunc(env, '/', ['args'], (args: any) => args.reduce((acc: any, val: any) => acc / val));
mkNativeFunc(env, '=', ['args'], (args: any) => args.reduce((acc: any, val: any) => toL(acc === val)));
mkNativeFunc(env, '>', ['args'], (args: any) => args.reduce((acc: any, val: any) => toL(acc > val)));
mkNativeFunc(env, '<', ['args'], (args: any) => args.reduce((acc: any, val: any) => toL(acc < val)));
mkNativeFunc(env, '>=', ['args'], ([l, r]: any) => toL(l >= r));
mkNativeFunc(env, '<=', ['args'], ([l, r]: any) => toL(l <= r));

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
Lisp.exec(
  "(define-macro if (c t e) `(cond (,c ,t) ('#t ,e)))",
  env);

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
Lisp.exec(`(defun cadar  (x) (car (cdr (car x))))`, env);
Lisp.exec(`(defun caddr  (x) (car (cdr (cdr x))))`, env);
Lisp.exec(`(defun caddar (x) (car (cdr (cdr (car x)))))`, env);
Lisp.exec(`(defun list x x)`, env);
