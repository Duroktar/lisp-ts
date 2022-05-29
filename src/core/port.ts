import assert from "assert"
import { existsSync, readFileSync, readSync, writeSync } from "fs"
import type { delimiter, whitespace } from "../syntax"
import { delimiters } from "./const"
import { quotes } from "./macro"

export abstract class File {
  abstract read(): string
  abstract write(text: string): void
  static EOF_STRING = '#<eof-object>'
}


export class SourceFile implements File {
  constructor(filepath: string) {
    this.data = String(readFileSync(filepath))
  }
  read(): string {
    const x = this.data[0] ?? File.EOF_STRING
    this.data = this.data.slice(1)
    return x
  }
  write(text: string): void {
    this.data = this.data.concat(text)
  }
  private data: string = ''
}

export class RawText implements File {
  constructor(private data: string) {}
  read(): string {
    const x = this.data[0] ?? File.EOF_STRING
    this.data = this.data.slice(1)
    return x
  }
  write(text: string): void {
    this.data = this.data.concat(text)
  }
  close(): void {
    this.closed = true
  }
  open(): void {
    this.closed = false
  }
  get isClosed() { return this.closed }
  private closed = false
}

export class StdIn implements File {
  write(text: string): void {
    throw new Error("Method not implemented.")
  }
  read(): string {
    return String.fromCodePoint(getcharSync())
  }
}

export class StdOut implements File {
  write(output: string | number): void {
    if (typeof output !== "number") {
      for (let char of String(output)) {
        putcharSync(char.charCodeAt(0))
      }
      return
    }
    putcharSync(output)
  }
  read(): string {
    throw new Error("Method not implemented.")
  }
}

export abstract class Port {
  constructor(
    public file: File,
    public name: string,
  ) {}

  public close() {
    this.closed = true
  }

  public closed = false
}

export class InPort extends Port {
  constructor(file: File, name: string) {
    super(file, `input-port:${name}`)
  }
  static fromFile(file: string) {
    return new InPort(new SourceFile(file), 'file')
  }
  static fromString(text: string) {
    return new InPort(new RawText(text), 'string')
  }
  static fromStdIn() {
    return new InPort(new StdIn(), 'stdin')
  }
  public readChar(): string {
    if (!this.closed) {
      if (this.char === '') this.char = this.file.read()
      if (this.char === '') return File.EOF_STRING
      const char = this.char; this.char = '';
      return char
    }
    throw new Error('attempted to read from closed port')
  }
  public peekChar(): string {
    if (this.char === '') {
      const char = this.readChar()
      this.char = char
    }
    return this.char
  }
  public charReady() {
    return this.char !== ''
  }
  private char = ''
}

export class OutPort extends Port {
  constructor(file: File, name: string) {
    super(file, `output-port:${name}`)
  }
  static fromFile(file: string) {
    return new OutPort(new SourceFile(file), 'file')
  }
  static fromString(text: string) {
    return new OutPort(new RawText(text), 'string')
  }
  static fromStdOut() {
    return new OutPort(new StdOut(), 'stdout')
  }
  public write(text: string): void {
    if (this.closed)
      throw new Error('attempted to write to closed port')
    this.file.write(text)
  }
}

// library procedure
export const callWithInputFile = (file: string, proc: Function) => {
  assert(existsSync(file))
}
// library procedure
export const callWithOutputFile = (file: string, proc: Function) => {
  /* assert(existsSync(file)) // unspecified */
}

// procedure
export const isInputPort = (obj: any) => {
  return obj instanceof InPort
}
// procedure
export const isOutputPort = (obj: any) => {
  return obj instanceof OutPort
}

let inputPort = InPort.fromStdIn();
let outputPort = OutPort.fromStdOut();

// procedure
export const currentInputPort = () => inputPort
// procedure
export const currentOutputPort = () => outputPort

class UnimplementedOptionalProcedureError extends Error {}

// optional procedure
export const withInputFromFile = (str: string, proc: Function) => {
  throw new UnimplementedOptionalProcedureError('withInputFromFile')
}
// optional procedure
export const withOutputFromFile = (str: string, proc: Function) => {
  throw new UnimplementedOptionalProcedureError('withOutputFromFile')
}

// procedure
export const openInputFile = (filename: string) => {
  return InPort.fromFile(filename)
}
// procedure
export const openOutputFile = (filename: string) => {
  return OutPort.fromFile(filename)
}

// procedure
export const closeInputPort = (port: InPort) => {}
// procedure
export const closeOutputPort = (port: OutPort) => {}


export const readChar = (port = currentInputPort()) => {
  return port.readChar()
}

export const peekChar = (port = currentInputPort()) => {
  return port.peekChar()
}

export const isEofObject = (obj: any) => obj === File.EOF_STRING
export const isDelimiter = (c: any): c is delimiter => isWhiteSpace(c) || delimiters.has(c);
export const isQuoteChar = (c: any): boolean => quotes[c] !== undefined;
export const isWhiteSpace = (c: any): c is whitespace => c === ' ' || c === '\t' || c === '\n'

export const putcharSync = (c: number) => {
  assert(typeof c === 'number')
  let buffer = Buffer.alloc(1);
  buffer[0] = c;
  writeSync(1, buffer, 0, 1);
  return c;
};

export const getcharSync = () => {
  let buffer = Buffer.alloc(1);
  if (readSync(0, buffer, 0, 1, <any>undefined))
    return buffer[0];
  return -1;
};
