import { iEnv } from "../../interface/iEnv"
import { MutableString } from "../data/string"
import { Token } from "../read"

export abstract class File {
  public name = 'file'
  public cursor = 0
  public data = ''
  public closed = false

  read(): string {
    const x = this.data[this.cursor]
    if (x !== undefined) this.cursor++
    return x ?? File.EOF_STRING
  }
  write(text: string): void {
    this.data = this.data.concat(text)
  }
  close(): void {
    this.closed = true
  }

  static EOF_STRING = '#<eof-object>'
}

export class RawText extends File {
  constructor(public data: string) {
    super()
  }
}

export abstract class Port {
  constructor(
    public file: File,
    public name: string,
  ) {}

  public close() {
    this.file.close()
    this.closed = true
  }

  public token?: Token
  public closed = false
}

export class IOPort extends Port {
  constructor(file: File, name: string) {
    super(file, `input-port:${name}`)
  }
  static fromString(text: string) {
    return new IOPort(new RawText(text), 'string')
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
  public write(text: string): void {
    if (this.closed)
      throw new Error('attempted to write to closed port')
    this.file.write(text)
  }
  private char = ''
}

export class InPort extends Port {
  constructor(file: File, name: string) {
    super(file, `input-port:${name}`)
  }
  static fromString(text: string) {
    return new InPort(new RawText(text), 'string')
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
  static fromString(text: string) {
    return new OutPort(new RawText(text), 'string')
  }
  public write(text: string | MutableString): void {
    if (this.closed)
      throw new Error('attempted to write to closed port')
    this.file.write(text.toString())
  }
}

// library procedure
export const callWithInputFile = (file: string, proc: Function) => {
  // assert(existsSync(file))
}
// library procedure
export const callWithOutputFile = (file: string, proc: Function) => {
  /* assert(existsSync(file)) // unspecified */
}

// procedure
export const currentInputPort = (env: iEnv): InPort => env.get('*current-input-port*') as any
// procedure
export const currentOutputPort = (env: iEnv): OutPort => env.get('*current-output-port*') as any

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
export const closeInputPort = (port: InPort) => {}
// procedure
export const closeOutputPort = (port: OutPort) => {}


export const readChar = (port: InPort) => {
  return port.readChar()
}

export const peekChar = (port: InPort) => {
  return port.peekChar()
}
