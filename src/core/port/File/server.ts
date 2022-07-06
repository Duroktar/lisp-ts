import { readFileSync } from "fs";
import rlSYnc from "readline-sync";
import { File } from "../index";

rlSYnc.setDefaultOptions({prompt: ''});

export class StdIn extends File {
  read(): string {
    if (this.data[this.cursor] === undefined) {
      const rv = rlSYnc.prompt({ hideEchoBack: false, history: false, prompt: '' });
      this.data = this.data.concat(rv, '\n');
    }
    return super.read();
  }
  write(text: string): void {
    throw new Error("Cannot write to stdin");
  }
}

export class StdOut extends File {
  write(output: string | number): void {
    if (typeof output !== "number")
      this._write(output);

    else
      this._write(output.toString());
  }
  read(): string {
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
    this.data = String(readFileSync(filepath))
  }
}
