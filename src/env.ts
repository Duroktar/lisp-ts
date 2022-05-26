import { Env } from "./core/env";
import { addGlobals } from "./globals";

export function createEnvironment(globals = true) {
  const env = new Env();
  const readerEnv = new Env();
  const lexicalEnv = new Env();

  if (globals) addGlobals(env, lexicalEnv, readerEnv)

  return {env, readerEnv, lexicalEnv}
}
