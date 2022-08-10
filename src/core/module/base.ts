import { Token } from "../read";

export class TSchemeModule {
  constructor(public path: string, public displayName: string) {}

  public token?: Token
}
