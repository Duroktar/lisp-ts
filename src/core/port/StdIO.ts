import rlSYnc from "readline-sync"
import { debounce } from "../../utils";
import { File } from "./index";
rlSYnc.setDefaultOptions({prompt: ''});

export class StdIn implements File {
  async readline(): Promise<string> {
    return rlSYnc.prompt({hideEchoBack: false, history: true, prompt: ''})
  }
  async read(): Promise<string> {
    const rv = rlSYnc.prompt({ hideEchoBack: false, history: false, prompt: '' });
    if (this._buffer.length === 0) {
      this._buffer.push(...rv, File.EOF_STRING);
    }
    return this._buffer.shift()!;
  }
  write(text: string): void {
    throw new Error("Cannot write to stdin");
  }
  close(): void { }
  private _buffer: string[] = [];
}

export class StdOut implements File {
  write(output: string | number): void {
    if (typeof output !== "number")
      this._write(output);

    else
      this._write(String(output));
  }
  flush = debounce(() => process.stdout);
  read(): Promise<string> {
    throw new Error("Cannot read from stdout");
  }
  close(): void { }
  private _write(value: string) {
    process.stdout.write(value, () => []);
  }
}
