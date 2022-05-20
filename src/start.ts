import 'colors'
import { env } from "./globals";
import * as Lisp from "./core/lisp";

// Lisp.execute('(load "stdlib/r5rs.scm")', env)

// Lisp.execute('(load "tests/runner.scm")', env)
// Lisp.execute('(load "tests/utils.scm")', env)
// Lisp.execute('(load "tests/spec.scm")', env)
// Lisp.execute('(load "tests/r5rs.scm")', env)
// Lisp.execute('(load "samples/fac.scm")', env)
// Lisp.execute('(load "samples/do.scm")', env)
// Lisp.execute('(load "samples/hygiene.scm")', env)
Lisp.execute('(load "samples/let.scm")', env)
// Lisp.execute('(load "samples/assoc.scm")', env)

// Lisp.execute(`(test 'yes (if (> 3 2) 'yes 'no))`, env)
// Lisp.execute(`(cons "a" '(b c))`, env)
// Lisp.execute(`(cond ((> 3 2) 'greater) ((< 3 2) 'less))`, env)
// Lisp.execute(`(test '#4.1.1     28  (begin (define x 28) x))`, env)
