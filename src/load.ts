import * as Lisp from "./lib/lisp";
import { readFileSync } from "fs";
import { Env } from "./lib/env";
import { read } from "./lib/read";

export const readFile = (path: string) => {
  const file = String(readFileSync(path));
  return read(file);
};

export const parseFile = (path: string, a: Env) => {
  const file = String(readFileSync(path));
  return Lisp.parse(file, a);
};

export const executeFile = (path: string, a: Env) => {
  const file = String(readFileSync(path));
  return Lisp.execute(file, a);
};
