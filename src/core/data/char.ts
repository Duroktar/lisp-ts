import { isSym } from "../../guard";
import { assert } from "../../utils";
import type { Form } from "../form";
import { Sym } from "./sym";

export class Character {
  public sym: symbol
  constructor(
    sym: symbol | string,
  ) {
    this.sym = isSym(sym) ? sym : Sym(sym)
    assert(isSym(this.sym), 'Characters require a symbol');
    assert(this.sym.description, 'Invalid Character Symbol description')

    this.displayText = this.sym.description.toLowerCase() === 'newline' ? '\n' :
                       this.sym.description.toLowerCase() === 'space'   ? ' ' :
                       /* otherwise */ this.sym.description
  }

  public equal(other: Form): boolean {
    return (other instanceof Character)
      && this.displayText === other.displayText
  }

  public displayText: string
}
