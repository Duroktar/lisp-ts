
import { env } from "./globals";
import * as Lisp from "./lib/lisp";

// Lisp.execute('(load "samples/fac.scm")', env)
// Lisp.execute('(load "samples/let.scm")', env)
Lisp.execute('(load "samples/do.scm")', env)
