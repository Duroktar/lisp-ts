import { env } from "./globals";
import { Lisp } from "./lib/lisp";

Lisp.execute(`

(load "tests/utils.scm")
(set-verbose-test #t)

(load "tests/spec.scm")
(load "tests/r5rs.scm")

`, env);
