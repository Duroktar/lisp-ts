import 'colors';
import { execute } from './core/lisp';
import { createServerWorld } from './world/server';

(async () => {
  const env = createServerWorld();

  // const t = tokenize(`(if (> 3 2) 'yes 'no)`, env)
  // console.log('tokenized:', toString(t))
  // const p = parse(`(if (> 3 2) 'yes 'no)`, env)
  // console.log('expanded:', toString(p))
  // const e = execute(`(if (> 3 2) 'yes 'no)`, env)
  // console.log('evaluated:', toString(e))

  // const r = tokenize(`(load "stdlib/r5rs.scm")`, readerEnv);

  // execute('(debug-macro! #t)', env)
  // execute('(load "stdlib/r5rs.scm")', env)

  // execute('(repl)', env)

  // execute('(load "tests/runner.scm")', env);

  // execute('(load "samples/scratch.scm")', env)
  // execute('(load "samples/do.scm")', env)
  // execute('(load "samples/let.scm")', env)

  // execute('(load "samples/quotes.scm")', env);
  // execute('(load "samples/begin.scm")', env);
  // Lisp.execute('(begin (write-char #\\() (newline))', env)
  // Lisp.execute('(display (read (open-input-string "hello world")))', env)

  // Lisp.execute('(load "tests/utils.scm")', env)
  // Lisp.execute('(load "tests/spec.scm")', env)
  // Lisp.execute('(load "tests/r5rs.scm")', env)
  // Lisp.execute('(load "samples/fac.scm")', env)
  execute('(load "samples/hygiene.scm")', env)
  // Lisp.execute('(load "samples/assoc.scm")', env)
  // execute('(load "samples/cond.scm")', env)
  // Lisp.execute('(load "samples/repl.scm")', env)
  // execute('(load "samples/pair.scm")', env)
  // execute('(load "samples/letrec.scm")', env)
  // execute('(load "samples/macro.scm")', env)
  // Lisp.execute('(load "samples/syntax.scm")', env)
  // execute('(load "samples/map.scm")', env)
  // execute('(load "samples/call-cc.scm")', env)

  // Lisp.execute(`(test 'yes (if (> 3 2) 'yes 'no))`, env)
  // Lisp.execute(`(cons "a" '(b c))`, env)
  // Lisp.execute(`(cond ((> 3 2) 'greater) ((< 3 2) 'less))`, env)
  // Lisp.execute(`(test '#4.1.1     28  (begin (define x 28) x))`, env)

  // console.log(r)
  // console.log(toString(r))
})().catch(console.error)

