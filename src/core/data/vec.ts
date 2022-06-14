import { format } from "util";
import { assert } from "../../utils";
import type { Form } from "../form";

export class Vector {
  constructor(public data: Form[]) {
    assert(Array.isArray(data), format('Vector data must be an array. Got `%s`.', typeof data))
  }
}
