import assert from "assert";
import { isSym } from "../utils";

export class Character {
  constructor(
    public sym: symbol,
  ) {
    assert(isSym(sym), 'Characters require a symbol');
    assert(sym.description, 'Invalid Character Symbol description')
    this.displayText = sym.description.toLowerCase() === 'newline' ? '\n' :
                       sym.description.toLowerCase() === 'space'   ? ' ' :
                       /* otherwise */ sym.description
  }

  public displayText: string
}
