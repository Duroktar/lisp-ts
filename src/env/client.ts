import { Env } from "../core/env";
import { addGlobals } from "../builtins";
import { addClientFeatures } from "../builtins/client";
import { iEnv } from "../interface/iEnv";

export function createClientEnvironment(globals = true): iEnv {
  const env = new Env();

  if (globals) addClientFeatures(env)
  if (globals) addGlobals(env)

  return env
}
