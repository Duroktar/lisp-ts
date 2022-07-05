import { basename, dirname } from "path";
import { TSchemeModule } from "./base";

export class TSchemeModuleFS extends TSchemeModule {
  public basename: string;
  public dirname: string;
  constructor(path: string) {
    const name = basename(path);
    super(path, name);
    this.basename = name;
    this.dirname = dirname(path);
  }

  get name(): string {
    return this.basename.split('.').shift()!;
  }

  static loaderCache = new Map<string, TSchemeModuleFS>();
}
