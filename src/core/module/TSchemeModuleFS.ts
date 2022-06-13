import { basename, dirname } from "path";
import { Atom } from "../forms";
import { TSchemeModule } from "./index";

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

  static loaderCache = new Map<Atom, TSchemeModuleFS>();
}