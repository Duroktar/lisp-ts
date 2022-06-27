import { Env } from "../core/data/env";
import { addGlobals } from "../builtins";
import { addServerFeatures } from "../builtins/server";
import type { iWorld } from "../interface/iWorld";

export async function createServerWorld(globals = true): Promise<iWorld> {
  const readerEnv = new Env();
  const lexicalEnv = new Env();
  const env = new Env();

  if (globals) await addServerFeatures({env, readerEnv, lexicalEnv})
  if (globals) await addGlobals({env, readerEnv, lexicalEnv})

  return {env, readerEnv, lexicalEnv}
}
