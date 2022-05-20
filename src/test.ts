import { env } from "./globals";
import { execute } from "./core/lisp";

execute('(load "tests/runner.scm")', env);
