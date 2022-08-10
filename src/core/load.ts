import assert from "assert";
import { existsSync } from "fs";
import { isAbsolute, join, relative } from "path";
import { EOF } from "./const";
import { evaluate } from "./eval";
import { expand } from "./expand";
import { Form } from "./form";
import { TSchemeModuleFS } from "./module";
import { InPort, Port } from "./port";
import { ServerSourceFile } from "./port/File/server";
import { read } from "./read";
import { isString } from "../guard";
import { decorateErrorWithSourceInfo } from "./error";
import { iEnv } from "../interface/iEnv";

export function parseLoadSymbol(sym: symbol, ext = '.scm') {
  const repr = sym.description
  assert(repr, 'A symbol has no name..')
  assert(
    repr.includes('/') &&
    repr.split('/')[0].match(/^(stdlib|tests|samples)$/),

    `Must import from a known namespace (eg: stdlib, etc.)`
  )
  return repr + ext
}

export function readFile(path: string, env: iEnv): [Form[], Port] {
  const port = new InPort(new ServerSourceFile(path), 'file');
  const getNext = () => read(port, env);
  let terms = [];
  let next = getNext();
  while (
    !EOF.equal(next)
  ) {
    terms.push(next);
    next = getNext();
  }
  return [terms, port];
};

export function parseFile(path: string, env: iEnv): [Form[], Port] {
  const [file, port] = readFile(path, env);
  return decorateErrorWithSourceInfo(() => {
    const rv: Form[] = []
    for (let f of file) {
      rv.push(expand(f, env, true))
    }
    return [rv, port];
  }, port)
};

export function executeFile(path: string, env: iEnv): Form[] {
  const [file, port] = parseFile(path, env);
  return decorateErrorWithSourceInfo(() => {
    const rv: Form[] = []
    for (let form of file) {
      rv.push(evaluate(form, env))
    }
    return rv;
  }, port)
};

export function loadFile(file: string, env: iEnv, bustCache = true) {
  return loadFileFromCache(file, env, bustCache)
};

export function loadFromLibrary(file: string, env: iEnv, bustCache = true) {
  const libPath = join('stdlib', file);
  return loadFile(libPath, env, bustCache)
}

export function loadFileFromCache(file: string, env: iEnv, bustCache = true) {
  if (!TSchemeModuleFS.loaderCache.has(file) || bustCache) {
    const cacheData = new TSchemeModuleFS(file)

    if (isAbsolute(file)) {
      const absPath = join(env.get('#cwd').toString(), file);
      executeFile(absPath, env);
    } else {
      const fromPath = env.get('#cwd');

      assert(isString(fromPath),
        `can't resolve path to imported file`);

      const relPath = relative(fromPath.toString(), file);

      assert(existsSync(relPath),
        `import not found: ${relPath}`)

      executeFile(relPath, env);
    }
    TSchemeModuleFS.loaderCache.set(file, cacheData);
  }

  return TSchemeModuleFS.loaderCache.get(file)!
};
