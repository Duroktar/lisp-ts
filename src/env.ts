import { Env } from "./core/env";
import { addGlobals } from "./globals";

export type Environment = {
  readerEnv: Env
  lexicalEnv: Env
  env: Env
}

export async function createEnvironment(globals = true): Promise<Environment> {
  const readerEnv = new Env();
  const lexicalEnv = new Env();
  const env = new Env();

  if (globals) await addGlobals({env, readerEnv, lexicalEnv})

  return {env, readerEnv, lexicalEnv}
}
