import { InPort, IOPort, OutPort } from "../core/port";
import { SocketClient } from "../core/port/Socket/client";
import { SocketServer } from "../core/port/Socket/server";
import { ServerSourceFile, StdIn, StdOut } from "../core/port/File/server";
import { isSym } from "../guard";
import { iWorld } from "../interface/iWorld";
import { loadFile, loadFromLibrary, parseLoadSymbol } from "../core/load";
import { assert } from "../utils";

export async function addServerFeatures(world: iWorld) {
  const { env } = world

  env.set('#cwd', process.cwd());

  env.define('error', ['x', 'code?'], ([x, code = 1]: any) => {
    console.error(x);
    process.exit(code);
  });

  env.set('*default-input-port*', <any>new InPort(new StdIn(), 'stdin'))
  env.set('*default-output-port*', <any>new OutPort(new StdOut(), 'stdout'))

  env.define('socket-client->input-port', ['address'], ([address]: any) => {
    assert(typeof address === 'string')
    return new InPort(new SocketClient(address), `socket-client:${address}`)
  });
  env.define('socket-client->output-port', ['address'], ([address]: any) => {
    assert(typeof address === 'string')
    return new OutPort(new SocketClient(address), `socket-client:${address}`)
  });
  env.define('socket-server->input-port', ['address'], ([port]: any) => {
    assert(typeof port === 'string' || typeof port === 'number')
    return new InPort(new SocketServer(port), `socket-server:${port}`)
  });
  env.define('socket-server->output-port', ['address'], ([port]: any) => {
    assert(typeof port === 'string' || typeof port === 'number')
    return new OutPort(new SocketServer(port), `socket-server:${port}`)
  });
  env.define('socket-server->i/o-port', ['address'], ([port]: any) => {
    assert(typeof port === 'string' || typeof port === 'number')
    return new IOPort(new SocketServer(port), `socket-server:${port}`)
  });

  env.define('open-input-file', ['filename'], ([filename]: any) => {
    assert(typeof filename === 'string')
    return openInputFile(filename)
  });
  env.define('open-output-file', ['filename'], ([filename]: any) => {
    assert(typeof filename === 'string')
    return openOutputFile(filename)
  });

  env.define('load', ['file'], async ([file]: any) => {
    if (isSym(file)) {
      return await loadFile(parseLoadSymbol(file), world)
    }
    return await loadFile(file, world)
  });

  env.define('load-from-library', ['file'], async ([file]: any) => {
    return await loadFromLibrary(file, world)
  });

  env.define('reload', ['file'], async ([file]: any) => {
    if (isSym(file)) {
      return await loadFile(parseLoadSymbol(file), world, true)
    }
    return await loadFile(file, world, true)
  });

}

// procedure
export const openInputFile = (filename: string) => {
  return new InPort(new ServerSourceFile(filename), 'file')
}
// procedure
export const openOutputFile = (filename: string) => {
  return new OutPort(new ServerSourceFile(filename), 'file')
}
