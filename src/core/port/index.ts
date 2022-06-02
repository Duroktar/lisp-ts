import rlSYnc from "readline-sync"
import assert from "assert"
import { fromEvent, Observable, withLatestFrom, first, firstValueFrom, map, flatMap, mergeMap, takeUntil, ReplaySubject, lastValueFrom, Subject } from 'rxjs';
import { existsSync, readFileSync, writeSync } from "fs"
import { Server, Socket } from "socket.io"
import { io, Socket as Client } from "socket.io-client";
import type { delimiter, whitespace } from "../../syntax"
import { delimiters } from "../const"
import { quotes } from "../macro"
import { isEmpty, isNewline } from "../../utils";
import { Queue } from "../queue";
import { Environment } from "../../env";
rlSYnc.setDefaultOptions({prompt: ''});

export abstract class File {
  abstract read(): Promise<string>
  abstract write(text: string): void
  abstract close(): void
  static EOF_STRING = '#<eof-object>'
}

export class SourceFile implements File {
  constructor(filepath: string) {
    this.data = String(readFileSync(filepath))
  }
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
  close() { }
  private data: string = ''
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

export class StdIn implements File {
  async readline(): Promise<string> {
    return rlSYnc.prompt({hideEchoBack: false, history: true, prompt: ''})
  }
  async read(): Promise<string> {
    if (isEmpty(this._buffer)) {
      const rv = rlSYnc.prompt({ hideEchoBack: false, history: false, prompt: '' });
      this._buffer.push(...rv, File.EOF_STRING)
    }
    return this._buffer.shift()!
  }
  write(text: string): void {
    throw new Error("Cannot write to stdin")
  }
  close(): void { }
  private _buffer: string[] = []
}

export class SocketClient implements File {
  private data: string[] = []
  private socket: Client
  private fifo = new Queue()
  private connection?: Socket;
  constructor(address: string) {
    this.socket = io(address, {})
    this.socket.on('connection', connection => {
      this.connection = connection
      connection.on('data', (data: string) => {
        this.fifo.putNowait(data)
        this.fifo.putNowait('\n')
      })
    })
  }
  async readline(): Promise<string> {
    let data
    do { data = await this.read() }
    while (!isNewline(data) && !isEofString(data))
    return data
  }
  async read(): Promise<string> {
    if (this.data.length === 0) {
      const x = await this.fifo.get()
      assert(typeof x === 'string', 'data must be a string (SocketServer)')
      this.data.push(...x)
    }
    return this.data.shift()!
  }
  write(text: string): void {
    this.socket.send(text)
  }
  close() {
    delete this.connection
    this.socket.close()
  }
}

export class SocketServer implements File {
  private data: string[] = []
  private socket: Server
  private fifo = new Queue()
  private connection?: Socket;
  constructor(port: string | number) {
    this.socket = new Server(Number(port), {})
    this.socket.on('connection', (connection) => {
      this.connection = connection
      connection.on('data', data => {
        this.fifo.putNowait(data)
        this.fifo.putNowait('\n')
      })
    })
  }
  async read(): Promise<string> {
    if (this.data.length === 0) {
      const x = await this.fifo.get()
      assert(typeof x === 'string', 'data must be a string (SocketServer)')
      this.data.push(...x)
    }
    return this.data.shift()!
  }
  write(output: string | number): void {
    if (this.connection)
      this.socket.emit('data', String(output))
    else
      console.log('no connection!')
  }
  close() {
    delete this.connection
    this.socket.close()
  }
}

export class StdOut implements File {
  write(output: string | number): void {
    if (typeof output !== "number")
      process.stdout.write(output)
    else
      process.stdout.write(String(output))
  }
  read(): Promise<string> {
    throw new Error("Cannot read from stdout")
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
  static fromFile(file: string) {
    return new IOPort(new SourceFile(file), 'file')
  }
  static fromString(text: string) {
    return new IOPort(new RawText(text), 'string')
  }
  static fromStdIn() {
    return new IOPort(new StdIn(), 'stdin')
  }
  static fromSocketClient(address: string): any {
    return new IOPort(new SocketClient(address), `socket-client:${address}`)
  }
  static fromSocketServer(port: string | number): any {
    return new IOPort(new SocketServer(port), `socket-server:${port}`)
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
  static fromFile(file: string) {
    return new InPort(new SourceFile(file), 'file')
  }
  static fromString(text: string) {
    return new InPort(new RawText(text), 'string')
  }
  static fromStdIn() {
    return new InPort(new StdIn(), 'stdin')
  }
  static fromSocketClient(address: string): any {
    return new InPort(new SocketClient(address), `socket-client:${address}`)
  }
  static fromSocketServer(port: string | number): any {
    return new InPort(new SocketServer(port), `socket-server:${port}`)
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
  static fromFile(file: string) {
    return new OutPort(new SourceFile(file), 'file')
  }
  static fromString(text: string) {
    return new OutPort(new RawText(text), 'string')
  }
  static fromStdOut() {
    return new OutPort(new StdOut(), 'stdout')
  }
  static fromSocketClient(address: string): any {
    return new OutPort(new SocketClient(address), 'socket-client')
  }
  static fromSocketServer(port: string | number): any {
    return new OutPort(new SocketServer(port), 'socket-server')
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

// procedure
export const currentInputPort = (ctx: Environment): InPort => ctx.env.get<any>('*current-input-port*')
// procedure
export const currentOutputPort = (ctx: Environment): OutPort => ctx.env.get<any>('*current-output-port*')

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


export const readChar = (port: InPort) => {
  return port.readChar()
}

export const peekChar = (port: InPort) => {
  return port.peekChar()
}

export const isEofString = (obj: any) => obj === File.EOF_STRING
export const isDelimiter = (c: any): c is delimiter => isWhiteSpace(c) || delimiters.has(c);
export const isQuoteChar = (c: any): boolean => quotes[c] !== undefined;
export const isWhiteSpace = (c: any): c is whitespace => c === ' ' || c === '\t' || c === '\n'
