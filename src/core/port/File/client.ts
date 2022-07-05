// import { readFileSync } from "fs";
import { File } from "../index";

export class ClientSourceFile extends File {
  constructor(filepath: string) {
    throw new Error('UnimplementedError: ClientSourceFile')
    super()
    // this.data = String(readFileSync(filepath));
  }
}
