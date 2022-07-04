import assert from "assert";
import { Server, Socket } from "socket.io";
import { File } from "../index";

export class SocketServer extends File {
  private data: string[] = [];
  private socket: Server;
  private buffer = new Array();
  private connection?: Socket;
  constructor(port: string | number) {
    super()
    this.socket = new Server(Number(port), {});
    this.socket.on('connection', (connection) => {
      this.connection = connection;
      connection.on('data', data => {
        this.buffer.push(data);
        this.buffer.push('\n');
      });
    });
  }
  readline(): string {
    throw new Error('attempted to readline from socket');
  }
  read(): string {
    if (this.data.length === 0) {
      const x = this.buffer.shift();
      assert(typeof x === 'string', 'data must be a string (SocketServer)');
      this.data.push(...x);
    }
    this.on('read')
    return this.data.shift()!;
  }
  write(output: string | number): void {
    if (this.connection)
      this.socket.emit('data', String(output));

    else
      console.log('no connection!');
  }
  close() {
    delete this.connection;
    this.socket.close();
    this.on('close')
  }
}
