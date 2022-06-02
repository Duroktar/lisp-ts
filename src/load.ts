import assert from "assert";
import { existsSync } from "fs";
import { isAbsolute, join, relative } from "path";
import { EOF } from "./core/const";
import { evaluate } from "./core/eval";
import { expand } from "./core/expand";
import { TSchemeModule } from "./core/module";
import { InPort, SourceFile } from "./core/port";
import { read } from "./core/read";
import { Term } from "./core/terms";
import { Environment } from "./env";

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

export const readFile = async (path: string, global: Environment): Promise<Term[]> => {
  const port = new InPort(new SourceFile(path), 'file');
  const getNext = async () => await read(port, global.readerEnv);
  let terms = []
  for (let next = await getNext(); next !== EOF; next = await getNext()) {
    terms.push(next);
  }
  return terms;
};

export const parseFile = async (path: string, global: Environment): Promise<Term[]> => {
  const file = await readFile(path, global);
  const rv: Term[] = []
  for (let f of file) {
    rv.push(await expand(f, true, global.lexicalEnv))
  }
  return rv;
};

export const executeFile = async (path: string, global: Environment): Promise<Term[]> => {
  const file = await parseFile(path, global);
  const rv: Term[] = []
  for (let f of file) {
    rv.push(await evaluate(f, global.env))
  }
  return rv;
};

export const loadFile = async (file: string, global: Environment, bustCache = true) => {
  if (!TSchemeModule.loaderCache.has(file) || bustCache) {
    const cacheData = new TSchemeModule(file)

    if (isAbsolute(file)) {
      const absPath = join(global.env.get('#cwd'), file);
      await executeFile(absPath, global);
    } else {
      const fromPath = global.env.get('#cwd');

      assert(typeof fromPath === 'string',
        `can't resolve path to imported file`);

      const relPath = relative(fromPath, file);

      assert(existsSync(relPath),
        `import not found: ${relPath}`)

      await executeFile(relPath, global);
    }
    TSchemeModule.loaderCache.set(file, cacheData);
  }

  return TSchemeModule.loaderCache.get(file)
};
