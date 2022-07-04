import { SocketServerUnavailableError } from "../core/data/error";
import { InPort, OutPort } from "../core/port";
import { SocketClient } from "../core/port/Socket/client";
import { iWorld } from "../interface/iWorld";
import { assert } from "../utils";

export function addClientFeatures(world: iWorld) {
  const { env } = world

  env.set('*default-input-port*', <any>InPort.fromString(''))
  env.set('*default-output-port*', <any>InPort.fromString(''))

  env.set('#cwd', window.location.href);

  env.define('error', ['x', 'code?'], ([x, code = 1]: any) => { console.error(x); alert(code); });

  env.define('socket-client->input-port', ['address'], ([address]: any) => {
    assert(typeof address === 'string')
    return new InPort(new SocketClient(address), `socket-client:${address}`)
  });
  env.define('socket-client->output-port', ['address'], ([address]: any) => {
    assert(typeof address === 'string')
    return new OutPort(new SocketClient(address), `socket-client:${address}`)
  });
  env.define('socket-server->input-port', ['address'], ([port]: any) => {
    throw new SocketServerUnavailableError()
  });
  env.define('socket-server->output-port', ['address'], ([port]: any) => {
    throw new SocketServerUnavailableError()
  });
  env.define('socket-server->i/o-port', ['address'], ([port]: any) => {
    throw new SocketServerUnavailableError()
  });
}

// procedure
export const openInputFile = (filename: string) => {
  throw new Error('client: `openInputFile` not implemented')
}
// procedure
export const openOutputFile = (filename: string) => {
  throw new Error('client: `openOutputFile` not implemented')
}
