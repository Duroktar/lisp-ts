import { assert } from "../../utils";
import type { Form } from "../form";

export class Vector {
  constructor(public data: Form[]) {
    const msg = 'Vector data must be an array. Got ';
    assert(Array.isArray(data), msg + typeof data)
  }
  public dup() {
    return new Vector([...this.data]);
  }
}
