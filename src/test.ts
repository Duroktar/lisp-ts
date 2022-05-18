import { env } from "./globals";
import { execute } from "./core/lisp";

execute(`

(load "stdlib/r5rs.scm")

(load "tests/utils.scm")
(set-verbose-test #t)

(load "tests/spec.scm")
(load "tests/r5rs.scm")

`, env);
