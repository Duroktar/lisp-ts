import assert from "assert";
import { isAbsolute, join, relative } from "path";
import { EOF } from "./core/const";
import { evaluate } from "./core/eval";
import { expand } from "./core/expand";
import { TSchemeModule } from "./core/module";
import { InPort, SourceFile } from "./core/port";
import { read } from "./core/read";
import { Term } from "./core/terms";
import { Environment } from "./env";

export const readFile = (path: string, global: Environment): Term[] => {
  const port = new InPort(new SourceFile(path), 'file');
  const getNext = () => read(port, global.readerEnv);
  let terms = []
  for (let next = getNext(); next !== EOF; next = getNext()) {
    terms.push(next);
  }
  return terms;
};

export const parseFile = (path: string, global: Environment): Term[] => {
  const file = readFile(path, global);
  return file.map(f => expand(f, true, global.lexicalEnv));
};

export const executeFile = (path: string, global: Environment) => {
  const file = parseFile(path, global);
  return file.map(f => evaluate(f, global.env));
};

export const loadFile = (file: string, global: Environment, bustCache = true) => {
  if (!TSchemeModule.loaderCache.has(file) || bustCache) {
    const cacheData = new TSchemeModule(file)

    if (isAbsolute(file)) {
      executeFile(join(global.env.get('#cwd'), file), global);
    } else {
      const fromPath = global.env.get('#cwd');

      assert(typeof fromPath === 'string',
        `can't resolve path to imported file`);

      const relpath = relative(fromPath, file);
      executeFile(relpath, global);
    }
    TSchemeModule.loaderCache.set(file, cacheData);
  }

  return TSchemeModule.loaderCache.get(file)
};
