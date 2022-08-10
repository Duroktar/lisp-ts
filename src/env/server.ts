import { Env } from "../core/env";
import { addGlobals } from "../builtins";
import { addServerFeatures } from "../builtins/server";
import { iEnv } from "../interface/iEnv";

export function createServerEnvironment(globals = true): iEnv {
  const env = new Env();

  if (globals) addServerFeatures(env)
  if (globals) addGlobals(env)

  return env
}
