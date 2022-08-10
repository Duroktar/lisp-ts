import { assert } from "../../utils";
import type { Form } from "../form";
import { CharToken } from "../read";

export class Character {
  constructor(
    public sym: symbol,
    public token?: CharToken,
  ) {
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

export const Char = (c: string, token?: CharToken) => {
  return new Character(Symbol.for(c), token)
}
