import { Socket } from "socket.io";
import { io, Socket as Client } from "socket.io-client";
import { isNewline, isEofString } from "../../../guard";
import { assert } from "../../../utils";
import { File } from "../index";


export class SocketClient extends File {
  private data: string[] = [];
  private socket: Client;
  private buffer = new Array();
  private connection?: Socket;
  constructor(address: string) {
    super()
    this.socket = io(address, {});
    this.socket.on('connection', connection => {
      this.connection = connection;
      connection.on('data', (data: string) => {
        this.buffer.push(data);
        this.buffer.push('\n');
      });
    });
  }
  readline(): string {
    this.on('readline')
    let data;
    do { data = this.read(); }
    while (!isNewline(data) && !isEofString(data));
    return data;
  }
  read(): string {
    this.on('read')
    if (this.data.length === 0) {
      const x = this.buffer.shift();
      assert(typeof x === 'string', 'data must be a string (SocketServer)');
      this.data.push(...x);
    }
    return this.data.shift()!;
  }
  write(text: string): void {
    this.socket.send(text);
  }
  close() {
    delete this.connection;
    this.socket.close();
    this.on('close')
  }
}
