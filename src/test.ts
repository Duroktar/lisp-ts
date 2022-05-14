import { env } from "./globals";
import { execute } from "./lib/lisp";

execute(`

(load "tests/utils.scm")
(set-verbose-test #t)

(load "tests/spec.scm")
(load "tests/r5rs.scm")

`, env);
