// import { readFileSync } from "fs";
import { File } from "../index";


export class ClientSourceFile extends File {
  constructor(filepath: string) {
    throw new Error('UnimplementedError: ClientSourceFile')
    super()
    // this.data = String(readFileSync(filepath));
  }
  readline(): string {
    const [line, ...lines] = this.data.split('\n');
    this.data = lines.join('\n');
    return line;
  }
  read(): string {
    const x = this.data[0] ?? File.EOF_STRING;
    this.data = this.data.slice(1);
    return x;
  }
  write(text: string): void {
    this.data = this.data.concat(text);
  }
  close() { }
  private data: string = '';
}
