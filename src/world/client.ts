import { Env } from "../core/data/env";
import { addGlobals } from "../builtins";
import { addClientFeatures } from "../builtins/client";
import type { iWorld } from "../interface/iWorld";

export async function createClientWorld(globals = true): Promise<iWorld> {
  const readerEnv = new Env();
  const lexicalEnv = new Env();
  const env = new Env();

  if (globals) addClientFeatures({env, readerEnv, lexicalEnv})
  if (globals) addGlobals({env, readerEnv, lexicalEnv})

  return {env, readerEnv, lexicalEnv}
}
