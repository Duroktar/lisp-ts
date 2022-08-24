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
import { iEnv } from "../interface/iEnv";
import { LogConfig } from "../logging";

const DEBUG = LogConfig.load;

function debugLog(...args: any[]): void {
  if (DEBUG) { console.log('[Load]:'.blue, ...args); }
}

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
  debugLog('read'.dim.yellow, path)
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
  const rv: Form[] = []
  debugLog('parse'.dim.blue, path)
  for (let f of file) {
    rv.push(expand(f, env, true))
  }
  return [rv, port];
};

export function executeFile(path: string, env: iEnv): Form[] {
  const [file, port] = parseFile(path, env);
  const rv: Form[] = []
  debugLog('execute'.red, path)
  for (let form of file) {
    rv.push(evaluate(form, env))
  }
  return rv;
};

export function loadFile(file: string, env: iEnv, bustCache = true) {
  debugLog('load'.dim.green, file, 'bustcache:'.dim, String(bustCache).dim.blue)
  return loadFileFromCache(file, env, bustCache)
};

export function loadFromLibrary(file: string, env: iEnv, bustCache = true) {
  debugLog('loadFromLibrary'.dim.green, file, 'bustcache:'.dim, String(bustCache).dim.blue)
  const libPath = join('stdlib', file);
  return loadFile(libPath, env, bustCache)
}

export function loadFileFromCache(file: string, env: iEnv, bustCache = true) {
  debugLog('loadFileFromCache'.dim.green, file, 'bustcache:'.dim, String(bustCache).dim.blue)
  if (!TSchemeModuleFS.loaderCache.has(file) || bustCache) {
    const cacheData = new TSchemeModuleFS(file)

    if (isAbsolute(file)) {
      const absPath = join(env.get('#cwd').toString(), file);

      debugLog('loading file'.green, '(absPath)'.dim, absPath)
      executeFile(absPath, env);
    } else {
      const fromPath = env.get('#cwd');

      assert(isString(fromPath),
        `can't resolve path to imported file`);

      const relPath = relative(fromPath.toString(), file);

      assert(existsSync(relPath),
        `import not found: ${relPath}`)

      debugLog('loading file'.green, '(relpath)'.dim, relPath)
      executeFile(relPath, env);
    }
    TSchemeModuleFS.loaderCache.set(file, cacheData);
  }

  debugLog('returning cached file'.yellow, file)
  return TSchemeModuleFS.loaderCache.get(file)!
};
