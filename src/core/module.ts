import { basename, dirname } from "path";
import { Atom } from "./forms";


export class TSchemeModule {
  public basename: string;
  public dirname: string;
  constructor(public path: string) {
    this.basename = basename(path);
    this.dirname = dirname(path);
  }

  get name(): string {
    return this.basename.split('.').shift()!;
  }

  static loaderCache = new Map<Atom, TSchemeModule>();
}
