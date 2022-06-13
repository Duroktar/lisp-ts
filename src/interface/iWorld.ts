import { iEnv } from "./iEnv";

export type iWorld = {
  readerEnv: iEnv;
  lexicalEnv: iEnv;
  env: iEnv;
};
