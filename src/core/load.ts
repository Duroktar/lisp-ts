import assert from "assert";
import { existsSync } from "fs";
import { isAbsolute, join, relative } from "path";
import { EOF } from "./const";
import { evaluate } from "./eval";
import { expand } from "./expand";
import { Form } from "./form";
import { TSchemeModuleFS } from "./module";
import { InPort } from "./port";
import { ServerSourceFile } from "./port/File/server";
import { read } from "./read";
import { iWorld } from "../interface/iWorld";

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

export function readFile(path: string, world: iWorld): Form[] {
  const port = new InPort(new ServerSourceFile(path), 'file');
  const getNext = () => read(port, world);
  let terms = []
  for (let next = getNext(); next !== EOF; next = getNext()) {
    terms.push(next);
  }
  return terms;
};

export function parseFile(path: string, world: iWorld): Form[] {
  const file = readFile(path, world);
  const rv: Form[] = []
  for (let f of file) {
    rv.push(expand(f, true, world))
  }
  return rv;
};

export function executeFile(path: string, world: iWorld): Form[] {
  const file = parseFile(path, world);
  const rv: Form[] = []
  for (let f of file) {
    rv.push(evaluate(f, world.env))
  }
  return rv;
};

export function loadFromLibrary(file: string, world: iWorld, bustCache = true) {
  const libPath = join('stdlib', file);
  return loadFile(libPath, world, bustCache)
}

export function loadFile(file: string, world: iWorld, bustCache = true) {
  if (!TSchemeModuleFS.loaderCache.has(file) || bustCache) {
    const cacheData = new TSchemeModuleFS(file)

    if (isAbsolute(file)) {
      const absPath = join(world.env.get('#cwd'), file);
      executeFile(absPath, world);
    } else {
      const fromPath = world.env.get('#cwd');

      assert(typeof fromPath === 'string',
        `can't resolve path to imported file`);

      const relPath = relative(fromPath, file);

      assert(existsSync(relPath),
        `import not found: ${relPath}`)

      executeFile(relPath, world);
    }
    TSchemeModuleFS.loaderCache.set(file, cacheData);
  }

  return TSchemeModuleFS.loaderCache.get(file)
};
