import assert from "assert"
import { existsSync, readFileSync, readSync, writeSync } from "fs"
import type { delimiter, whitespace } from "../syntax"
import { isNewline } from "../utils"
import { delimiters, EOF } from "./const"
import { quotes } from "./macro"

interface File {
  read(): string
  write(text: string): void
}

export class SourceFile implements File {
  constructor(filepath: string) {
    this.data = String(readFileSync(filepath))
  }
  read(): string {
    const x = this.data[0] ?? EOF
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
    const x = this.data[0] ?? EOF
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
  write(text: string): void {
    assert(text.length === 1)
    putcharSync(text.charCodeAt(0))
    // for (let char of text) {
    //   putcharSync(char.charCodeAt(0))
    // }
  }
  read(): string {
    throw new Error("Method not implemented.")
  }
}

export abstract class Port {
  constructor(public file: File) {
  }

  public close() {
    this.closed = true
  }

  public closed = false
}

export class InPort extends Port {
  static fromFile(file: string) {
    return new InPort(new SourceFile(file))
  }
  static fromText(text: string) {
    return new InPort(new RawText(text))
  }
  public readChar(): string {
    if (!this.closed) {
      if (this.char === '') this.char = this.file.read()
      if (this.char === '') return EOF
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
  public peekCharNonWhitespace(): string {
    const c = this.peekChar()
    if (isEofObject(c))
      return EOF

    if (!isWhiteSpace(c))
      if (c === ';')
        return this.skipComment()
      else
        return c

    this.readChar()

    return this.peekCharNonWhitespace()
  }
  public skipComment(): string {
    const c = this.readChar()

    if (isEofObject(c))
      return EOF

    if (isNewline(c))
      return this.peekCharNonWhitespace()
    else return this.skipComment()
  }
  // public readToken(): string {
  //   let acc = this.readChar()
  //   while (isWhiteSpace(acc))
  //     acc = this.readChar()
  //   if (acc === '(') {
  //     // read-list
  //     return acc
  //   }
  //   if (acc === '#') {
  //     acc += this.readChar()
  //     if (isBoolean(acc))
  //       return acc
  //     else if (acc[1] === 'x' || acc[1] === 'X') {
  //       // "#x" or "#X"
  //       // read-hex
  //       throw new Error('hex numbers not supported')
  //     } else {
  //       // assume: "#("
  //       return acc // list->vector
  //     }
  //   }
  //   if (acc === "'")
  //     return acc
  //   return acc
  // }

  private char = ''
}

export class OutPort extends Port {
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
  // return toL(obj instanceof InPort)
}
// procedure
export const isOutputPort = (obj: any) => {
  // return toL(obj instanceof OutPort)
}

let inputPort = new InPort(new StdIn());
let outputPort = new OutPort(new StdOut());

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
  return new InPort(new SourceFile(filename))
}
// procedure
export const openOutputFile = (filename: string) => {
  return new OutPort(new SourceFile(filename))
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

export const peekCharNonWhitespace = (port = currentInputPort()) => {
  let c = peekChar(port)
  if (isEofObject(c))
    return -1
}

export const isEofObject = (obj: any) => obj === EOF
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
