import 'colors';
import { createEnvironment } from './env';
import * as Lisp from './core/lisp'

const env = createEnvironment();

// const r = tokenize(`(load "stdlib/r5rs.scm")`, readerEnv);

// Lisp.execute('(debug-macro! #t)', env)

(async () => {
  // await Lisp.execute('(load "stdlib/r5rs.scm")', env)

  await Lisp.execute('(repl)', env)

  // Lisp.execute('(begin (write-char #\\() (newline))', env)
  // Lisp.execute('(display (read (open-input-string "hello world")))', env)

  // await Lisp.debugExecute('(load "tests/runner.scm")', env);

  // await Lisp.execute('(load "tests/runner.scm")', env)
  // Lisp.execute('(load "tests/utils.scm")', env)
  // Lisp.execute('(load "tests/spec.scm")', env)
  // Lisp.execute('(load "tests/r5rs.scm")', env)
  // Lisp.execute('(load "samples/fac.scm")', env)
  // Lisp.execute('(load "samples/do.scm")', env)
  // Lisp.execute('(load "samples/hygiene.scm")', env)
  // Lisp.execute('(load "samples/assoc.scm")', env)
  // Lisp.execute('(load "samples/cond.scm")', env)
  // Lisp.execute('(load "samples/repl.scm")', env)
  // Lisp.execute('(load "samples/let.scm")', env)
  // Lisp.execute('(load "samples/letrec.scm")', env)
  // Lisp.execute('(load "samples/macro.scm")', env)
  // Lisp.execute('(load "samples/syntax.scm")', env)

  // Lisp.execute(`(test 'yes (if (> 3 2) 'yes 'no))`, env)
  // Lisp.execute(`(cons "a" '(b c))`, env)
  // Lisp.execute(`(cond ((> 3 2) 'greater) ((< 3 2) 'less))`, env)
  // Lisp.execute(`(test '#4.1.1     28  (begin (define x 28) x))`, env)

  // console.log(r)
  // console.log(toString(r))
})()

