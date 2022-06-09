import 'colors';
import { execute } from './core/lisp';
import { createEnvironment } from './env';
// import * as Lisp from './core/lisp'


(async () => {
  const env = await createEnvironment();

  // const t = await tokenize(`(if (> 3 2) 'yes 'no)`, env)
  // console.log('tokenized:', toString(t))
  // const p = await parse(`(if (> 3 2) 'yes 'no)`, env)
  // console.log('expanded:', toString(p))
  // const e = await execute(`(if (> 3 2) 'yes 'no)`, env)
  // console.log('evaluated:', toString(e))

  // const r = tokenize(`(load "stdlib/r5rs.scm")`, readerEnv);

  // await execute('(debug-macro! #t)', env)
  // await execute('(load "stdlib/r5rs.scm")', env)

  // await execute('(repl)', env)

  await execute('(load "tests/runner.scm")', env);

  // await execute('(load "samples/scratch.scm")', env)
  // await execute('(load "samples/do.scm")', env)
  // await execute('(load "samples/let.scm")', env)

  // await execute('(load "samples/quotes.scm")', env);
  // await execute('(load "samples/begin.scm")', env);
  // Lisp.execute('(begin (write-char #\\() (newline))', env)
  // Lisp.execute('(display (read (open-input-string "hello world")))', env)

  // Lisp.execute('(load "tests/utils.scm")', env)
  // Lisp.execute('(load "tests/spec.scm")', env)
  // Lisp.execute('(load "tests/r5rs.scm")', env)
  // Lisp.execute('(load "samples/fac.scm")', env)
  // Lisp.execute('(load "samples/hygiene.scm")', env)
  // Lisp.execute('(load "samples/assoc.scm")', env)
  // await execute('(load "samples/cond.scm")', env)
  // Lisp.execute('(load "samples/repl.scm")', env)
  // await execute('(load "samples/pair.scm")', env)
  // await execute('(load "samples/letrec.scm")', env)
  // await execute('(load "samples/macro.scm")', env)
  // Lisp.execute('(load "samples/syntax.scm")', env)
  // await execute('(load "samples/map.scm")', env)
  // await execute('(load "samples/call-cc.scm")', env)

  // Lisp.execute(`(test 'yes (if (> 3 2) 'yes 'no))`, env)
  // Lisp.execute(`(cons "a" '(b c))`, env)
  // Lisp.execute(`(cond ((> 3 2) 'greater) ((< 3 2) 'less))`, env)
  // Lisp.execute(`(test '#4.1.1     28  (begin (define x 28) x))`, env)

  // console.log(r)
  // console.log(toString(r))
})().catch(console.error)

