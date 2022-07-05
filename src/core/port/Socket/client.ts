import { Socket } from "socket.io";
import { io, Socket as Client } from "socket.io-client";
import { assert } from "../../../utils";
import { File } from "../index";


export class SocketClient extends File {
  private socket: Client;
  private buffer = new Array();
  private connection?: Socket;
  constructor(address: string) {
    super()
    this.socket = io(address, {});
    this.socket.on('connection', connection => {
      this.connection = connection;
      connection.on('data', (data: string) => {
        this.data = this.data.concat(data, '\n')
      });
    });
  }
  write(text: string): void {
    this.socket.send(text);
  }
  close() {
    delete this.connection;
    this.socket.close();
  }
}
