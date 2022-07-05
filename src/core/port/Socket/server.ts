import assert from "assert";
import { Server, Socket } from "socket.io";
import { File } from "../index";

export class SocketServer extends File {
  private socket: Server;
  private connection?: Socket;
  constructor(port: string | number) {
    super()
    this.socket = new Server(Number(port), {});
    this.socket.on('connection', (connection) => {
      this.connection = connection;
      connection.on('data', data => {
        this.data = this.data.concat(data, '\n')
      });
    });
  }
  write(output: string | number): void {
    if (this.connection) {
      this.socket.emit('data', String(output));
    } else
      console.log('no connection!');
  }
  close() {
    delete this.connection;
    this.socket.close();
  }
}
