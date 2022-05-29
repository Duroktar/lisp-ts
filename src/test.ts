import "colors"
import { createEnvironment } from "./env";
import { execute } from "./core/lisp";

const env = createEnvironment()

execute('(load "tests/runner.scm")', env);
