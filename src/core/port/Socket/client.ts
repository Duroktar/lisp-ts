import { Socket } from "socket.io";
import { io, Socket as Client } from "socket.io-client";
import { isNewline, isEofString } from "../../../guard";
import { assert } from "../../../utils";
import { Queue } from "../../data/queue";
import { File } from "../index";


export class SocketClient implements File {
  private data: string[] = [];
  private socket: Client;
  private fifo = new Queue();
  private connection?: Socket;
  constructor(address: string) {
    this.socket = io(address, {});
    this.socket.on('connection', connection => {
      this.connection = connection;
      connection.on('data', (data: string) => {
        this.fifo.putNowait(data);
        this.fifo.putNowait('\n');
      });
    });
  }
  async readline(): Promise<string> {
    let data;
    do { data = await this.read(); }
    while (!isNewline(data) && !isEofString(data));
    return data;
  }
  async read(): Promise<string> {
    if (this.data.length === 0) {
      const x = await this.fifo.get();
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
  }
}
