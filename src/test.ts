import "colors"
import { createEnvironment } from "./env";
import { debugExecute } from "./core/lisp";

const env = createEnvironment();

debugExecute('(load "tests/runner.scm")', env)
  .catch(err => console.error(err))
