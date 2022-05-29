import { Env } from "./core/env";
import { addGlobals } from "./globals";

export type Environment = {
  readerEnv: Env
  lexicalEnv: Env
  env: Env
}

export function createEnvironment(globals = true): Environment {
  const readerEnv = new Env();
  const lexicalEnv = new Env();
  const env = new Env();

  if (globals) addGlobals({env, readerEnv, lexicalEnv})

  return {env, readerEnv, lexicalEnv}
}
