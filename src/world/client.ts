import { Env } from "../core/data/env";
import { addGlobals } from "../lib";
import { addClientFeatures } from "../lib/client";
import type { iWorld } from "../interface/iWorld";

export async function createClientWorld(globals = true): Promise<iWorld> {
  const readerEnv = new Env();
  const lexicalEnv = new Env();
  const env = new Env();

  if (globals) await addClientFeatures({env, readerEnv, lexicalEnv})
  if (globals) await addGlobals({env, readerEnv, lexicalEnv})

  return {env, readerEnv, lexicalEnv}
}
