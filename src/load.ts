import { Lisp } from "./lib/lisp";
import { readFileSync } from "fs";
import { Env } from "./lib/env";

export const readFile = (path: string) => {
  const file = String(readFileSync(path));
  return Lisp.read(file);
};

export const parseFile = (path: string, a: Env) => {
  const file = String(readFileSync(path));
  return Lisp.parse(file, a);
};

export const executeFile = (path: string, a: Env) => {
  const file = String(readFileSync(path));
  return Lisp.execute(file, a);
};
