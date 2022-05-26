import * as Lisp from "./core/lisp";
import { readFileSync } from "fs";
import { Env } from "./core/env";
import { read } from "./core/read";
import { join, isAbsolute, relative } from "path";
import assert from "assert";
import { TSchemeModule } from "./core/module";
import { InPort, SourceFile } from "./core/port";

export const readFile = (path: string, r: Env) => {
  return read(new InPort(new SourceFile(path)), r);
};

export const parseFile = (path: string, l: Env, r: Env) => {
  const file = String(readFileSync(path));
  return Lisp.parse(file, l, r);
};

export const executeFile = (path: string, env: Env, l: Env, r: Env) => {
  const file = String(readFileSync(path));
  return Lisp.execute(file, env, l, r);
};

export const loadFile = (file: string, env: Env, l: Env, r: Env, bustCache = false) => {
  if (!TSchemeModule.loaderCache.has(file) || bustCache) {
    const cacheData = new TSchemeModule(file)

    if (isAbsolute(file)) {
      executeFile(join(env.get('#cwd'), file), env, l, r);
    } else {
      const fromPath = env.get('#cwd');

      assert(typeof fromPath === 'string',
        `can't resolve path to imported file`);

      const relpath = relative(fromPath, file);
      executeFile(relpath, env, l, r);
    }
    TSchemeModule.loaderCache.set(file, cacheData);
  }

  return TSchemeModule.loaderCache.get(file)
};
