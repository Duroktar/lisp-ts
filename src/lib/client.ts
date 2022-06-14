import { SocketServerUnavailableError } from "../core/data/error";
import { InPort, OutPort } from "../core/data/port";
import { SocketClient } from "../core/data/port/Socket/client";
import { iWorld } from "../interface/iWorld";
import { assert } from "../utils";
import { mkNativeProc } from "./utils";

export async function addClientFeatures(world: iWorld) {
  const { env } = world

  env.set('*default-input-port*', <any>InPort.fromString(''))
  env.set('*default-output-port*', <any>InPort.fromString(''))

  env.set('#cwd', window.location.href);

  mkNativeProc(env, 'error', ['x', 'code?'], ([x, code = 1]: any) => { console.error(x); alert(code); });

  mkNativeProc(env, 'socket-client->input-port', ['address'], ([address]: any) => {
    assert(typeof address === 'string')
    return new InPort(new SocketClient(address), `socket-client:${address}`)
  });
  mkNativeProc(env, 'socket-client->output-port', ['address'], ([address]: any) => {
    assert(typeof address === 'string')
    return new OutPort(new SocketClient(address), `socket-client:${address}`)
  });
  mkNativeProc(env, 'socket-server->input-port', ['address'], ([port]: any) => {
    throw new SocketServerUnavailableError()
  });
  mkNativeProc(env, 'socket-server->output-port', ['address'], ([port]: any) => {
    throw new SocketServerUnavailableError()
  });
  mkNativeProc(env, 'socket-server->i/o-port', ['address'], ([port]: any) => {
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
