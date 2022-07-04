import { Env } from "../core/data/env";
import { addGlobals } from "../builtins";
import { addServerFeatures } from "../builtins/server";
import type { iWorld } from "../interface/iWorld";

export function createServerWorld(globals = true): iWorld {
  const readerEnv = new Env();
  const lexicalEnv = new Env();
  const env = new Env();

  if (globals) addServerFeatures({env, readerEnv, lexicalEnv})
  if (globals) addGlobals({env, readerEnv, lexicalEnv})

  return {env, readerEnv, lexicalEnv}
}
