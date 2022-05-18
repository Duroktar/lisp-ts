import * as Lisp from "./core/lisp";
import { readFileSync } from "fs";
import { Env } from "./core/env";
import { read } from "./core/read";

export const readFile = (path: string) => {
  const file = String(readFileSync(path));
  return read(file);
};

export const parseFile = (path: string, env: Env) => {
  const file = String(readFileSync(path));
  return Lisp.parse(file, env);
};

export const executeFile = (path: string, env: Env) => {
  const file = String(readFileSync(path));
  // console.log('loading:', path);
  Lisp.execute(file, env);
  // console.log('loading:', path);
  return
};
