import assert from "assert";
import { format } from "util";
import type { Form } from "./forms";

export class Vector {
  constructor(public data: Form[]) {
    assert(Array.isArray(data), format('Vector data must be an array. Got `%s`.', typeof data))
  }
}
