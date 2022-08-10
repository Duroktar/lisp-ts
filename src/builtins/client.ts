import { SocketServerUnavailableError } from "../core/error";
import { Str } from "../core/data/string";
import { InPort, OutPort } from "../core/port";
import { SocketClient } from "../core/port/Socket/client";
import { isString } from "../guard";
import { assert } from "../utils";
import { NIL } from "../core/const";
import { iEnv } from "../interface/iEnv";

export function addClientFeatures(env: iEnv) {

  env.set('*default-input-port*', <any>InPort.fromString(''))
  env.set('*default-output-port*', <any>InPort.fromString(''))

  env.set('#cwd', Str(window.location.href));

  env.define('error', ['x', 'code?'], ([x, code = 1]: any) => {
    console.error(x);
    alert(code);
    return NIL;
  });

  env.define('socket-client->input-port', ['address'], ([address]: any) => {
    assert(isString('string'))
    return new InPort(new SocketClient(address), `socket-client:${address}`)
  });
  env.define('socket-client->output-port', ['address'], ([address]: any) => {
    assert(isString('string'))
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
