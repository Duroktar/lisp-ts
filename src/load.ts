import * as Lisp from "./core/lisp";
import { readFileSync } from "fs";
import { Env } from "./core/env";
import { read } from "./core/read";
import { join, isAbsolute, relative } from "path";
import assert from "assert";
import { env } from "./globals";
import { TSchemeModule } from "./core/module";

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
  return Lisp.execute(file, env);
};

export const loadFile = (file: string, bustCache = false) => {
  if (!TSchemeModule.loaderCache.has(file) || bustCache) {
    const cacheData = new TSchemeModule(file)

    if (isAbsolute(file)) {
      executeFile(join(env.get('#cwd'), file), env);
    } else {
      const fromPath = env.get('#cwd');

      assert(typeof fromPath === 'string',
        `can't resolve path to imported file`);

      const relpath = relative(fromPath, file);
      executeFile(relpath, env);
    }
    TSchemeModule.loaderCache.set(file, cacheData);
  }

  return TSchemeModule.loaderCache.get(file)
};
