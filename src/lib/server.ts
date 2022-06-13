import { InPort, IOPort, OutPort } from "../core/port";
import { SocketClient } from "../core/port/Socket/client";
import { SocketServer } from "../core/port/Socket/server";
import { ServerSourceFile } from "../core/port/File/server";
import { StdIn, StdOut } from "../core/port/StdIO";
import { isSym } from "../guard";
import { iWorld } from "../interface/iWorld";
import { loadFile, parseLoadSymbol } from "../load";
import { assert } from "../utils";
import { mkNativeProc } from "./utils";

export async function addServerFeatures(world: iWorld) {
  const { env } = world

  env.set('#cwd', process.cwd());

  mkNativeProc(env, 'error', ['x', 'code?'], ([x, code = 1]: any) => {
    console.error(x);
    process.exit(code);
  });

  env.set('*default-input-port*', <any>new InPort(new StdIn(), 'stdin'))
  env.set('*default-output-port*', <any>new OutPort(new StdOut(), 'stdout'))

  mkNativeProc(env, 'socket-client->input-port', ['address'], ([address]: any) => {
    assert(typeof address === 'string')
    return new InPort(new SocketClient(address), `socket-client:${address}`)
  });
  mkNativeProc(env, 'socket-client->output-port', ['address'], ([address]: any) => {
    assert(typeof address === 'string')
    return new OutPort(new SocketClient(address), `socket-client:${address}`)
  });
  mkNativeProc(env, 'socket-server->input-port', ['address'], ([port]: any) => {
    assert(typeof port === 'string' || typeof port === 'number')
    return new InPort(new SocketServer(port), `socket-server:${port}`)
  });
  mkNativeProc(env, 'socket-server->output-port', ['address'], ([port]: any) => {
    assert(typeof port === 'string' || typeof port === 'number')
    return new OutPort(new SocketServer(port), `socket-server:${port}`)
  });
  mkNativeProc(env, 'socket-server->i/o-port', ['address'], ([port]: any) => {
    assert(typeof port === 'string' || typeof port === 'number')
    return new IOPort(new SocketServer(port), `socket-server:${port}`)
  });

  mkNativeProc(env, 'open-input-file', ['filename'], ([filename]: any) => {
    assert(typeof filename === 'string')
    return openInputFile(filename)
  });
  mkNativeProc(env, 'open-output-file', ['filename'], ([filename]: any) => {
    assert(typeof filename === 'string')
    return openOutputFile(filename)
  });

  mkNativeProc(env, 'load', ['file'], async ([file]: any) => {
    if (isSym(file)) {
      return await loadFile(parseLoadSymbol(file), world)
    }
    return await loadFile(file, world)
  });

  mkNativeProc(env, 'load', ['file'], async ([file]: any) => {
    if (isSym(file)) {
      return await loadFile(parseLoadSymbol(file), world)
    }
    return await loadFile(file, world)
  });

  mkNativeProc(env, 'reload', ['file'], async ([file]: any) => {
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
