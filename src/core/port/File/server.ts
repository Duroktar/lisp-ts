import { readFileSync } from "fs";
import rlSYnc from "readline-sync";
import { debounce, Position } from "../../../utils";
import { File } from "../index";

rlSYnc.setDefaultOptions({prompt: ''});

export class StdIn extends File {
  async readline(): Promise<string> {
    this.on('readline')
    return rlSYnc.prompt({hideEchoBack: false, history: true, prompt: ''})
  }
  async read(): Promise<string> {
    const rv = rlSYnc.prompt({ hideEchoBack: false, history: false, prompt: '' });
    if (this._buffer.length === 0) {
      this._buffer.push(...rv, File.EOF_STRING);
    } else {
      this.on('read')
    }
    return this._buffer.shift()!;
  }
  write(text: string): void {
    throw new Error("Cannot write to stdin");
  }
  close(): void { }
  private _buffer: string[] = [];
}

export class StdOut extends File {
  write(output: string | number): void {
    if (typeof output !== "number")
      this._write(output);

    else
      this._write(String(output));
  }
  flush = debounce(() => process.stdout);
  readline(): Promise<string> {
    throw new Error("Cannot read from stdout (readline)");
  }
  read(): Promise<string> {
    throw new Error("Cannot read from stdout (read)");
  }
  close(): void { }
  private _write(value: string) {
    process.stdout.write(value, () => []);
  }
}

export class ServerSourceFile extends File {
  constructor(filepath: string) {
    super()
    this.data = String(readFileSync(filepath));
  }
  async readline(): Promise<string> {
    const [line, ...lines] = this.data.split('\n');
    this.data = lines.join('\n');
    this.on('readline')
    return line;
  }
  async read(): Promise<string> {
    const x = this.data[0] ?? File.EOF_STRING;
    this.data = this.data.slice(1);
    this.on('read')
    return x;
  }
  write(text: string): void {
    this.data = this.data.concat(text);
  }
  close() { }
  private data: string = '';
}
