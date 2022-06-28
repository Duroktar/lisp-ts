import type { iWorld } from "../../interface/iWorld"

export abstract class File {
  abstract read(): Promise<string>
  abstract write(text: string): void
  abstract close(): void
  static EOF_STRING = '#<eof-object>'
}

export class RawText implements File {
  constructor(private data: string) {}
  async readline(): Promise<string> {
    const [line, ...lines] = this.data.split('\n')
    this.data = lines.join('\n')
    return line
  }
  async read(): Promise<string> {
    const x = this.data[0] ?? File.EOF_STRING
    this.data = this.data.slice(1)
    return x
  }
  write(text: string): void {
    this.data = this.data.concat(text)
  }
  close(): void { }
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

  public closed = false
}

export class IOPort extends Port {
  constructor(file: File, name: string) {
    super(file, `input-port:${name}`)
  }
  static fromString(text: string) {
    return new IOPort(new RawText(text), 'string')
  }
  public async readChar(): Promise<string> {
    if (!this.closed) {
      if (this.char === '') this.char = await this.file.read()
      if (this.char === '') return File.EOF_STRING
      const char = this.char; this.char = '';
      return char
    }
    throw new Error('attempted to read from closed port')
  }
  public async peekChar(): Promise<string> {
    if (this.char === '') {
      const char = await this.readChar()
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
  public async readChar(): Promise<string> {
    if (!this.closed) {
      if (this.char === '') this.char = await this.file.read()
      if (this.char === '') return File.EOF_STRING
      const char = this.char; this.char = '';
      return char
    }
    throw new Error('attempted to read from closed port')
  }
  public async peekChar(): Promise<string> {
    if (this.char === '') {
      const char = await this.readChar()
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
  public write(text: string): void {
    if (this.closed)
      throw new Error('attempted to write to closed port')
    this.file.write(text)
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
export const currentInputPort = (ctx: iWorld): InPort => ctx.env.get<any>('*current-input-port*')
// procedure
export const currentOutputPort = (ctx: iWorld): OutPort => ctx.env.get<any>('*current-output-port*')

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
