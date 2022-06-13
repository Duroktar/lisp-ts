import { readFileSync } from "fs";
import { File } from "../index";


export class ServerSourceFile implements File {
  constructor(filepath: string) {
    this.data = String(readFileSync(filepath));
  }
  async readline(): Promise<string> {
    const [line, ...lines] = this.data.split('\n');
    this.data = lines.join('\n');
    return line;
  }
  async read(): Promise<string> {
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
