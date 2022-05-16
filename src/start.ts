
import { env } from "./globals";
import * as Lisp from "./lib/lisp";

// Lisp.execute('(load "samples/fac.scm")', env)
// Lisp.execute('(load "samples/let.scm")', env)
// Lisp.execute('(load "samples/do.scm")', env)
Lisp.execute('(load "tests/utils.scm")', env)
// Lisp.execute(`(test 'yes (if (> 3 2) 'yes 'no))`, env)
Lisp.execute(`(test #t (eq? '() '()))`, env)
