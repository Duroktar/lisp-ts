import { InPort, IOPort, OutPort } from "../core/port";
import { SocketClient } from "../core/port/Socket/client";
import { SocketServer } from "../core/port/Socket/server";
import { ServerSourceFile, StdIn, StdOut } from "../core/port/File/server";
import { isIdent, isString, isSym } from "../guard";
import { iWorld } from "../interface/iWorld";
import { loadFile, loadFromLibrary, parseLoadSymbol } from "../core/load";
import { assert } from "../utils";
import { toString } from "../core/print";
import { Str } from "../core/data/string";

export function addServerFeatures(world: iWorld) {
  const { env } = world

  env.set('#cwd', Str(process.cwd()));

  env.define('error', 'args', ([...args]: any) => {
    console.error(args.map(toString).join(' '));
    process.exit(1);
  });

  env.set('*default-input-port*', <any>new InPort(new StdIn(), 'stdin'))
  env.set('*default-output-port*', <any>new OutPort(new StdOut(), 'stdout'))

  env.define('socket-client->input-port', ['address'], ([address]: any) => {
    assert(isString(address))
    return new InPort(new SocketClient(address.toString()), `socket-client:${address}`)
  });
  env.define('socket-client->output-port', ['address'], ([address]: any) => {
    assert(isString(address))
    return new OutPort(new SocketClient(address.toString()), `socket-client:${address}`)
  });
  env.define('socket-server->input-port', ['address'], ([port]: any) => {
    assert(isString(port) || typeof port === 'number')
    return new InPort(new SocketServer(port.toString()), `socket-server:${port}`)
  });
  env.define('socket-server->output-port', ['address'], ([port]: any) => {
    assert(isString(port) || typeof port === 'number')
    return new OutPort(new SocketServer(port.toString()), `socket-server:${port}`)
  });
  env.define('socket-server->i/o-port', ['address'], ([port]: any) => {
    assert(isString(port) || typeof port === 'number')
    return new IOPort(new SocketServer(port.toString()), `socket-server:${port}`)
  });

  env.define('open-input-file', ['filename'], ([filename]: any) => {
    assert(isString(filename))
    return openInputFile(filename.toString())
  });
  env.define('open-output-file', ['filename'], ([filename]: any) => {
    assert(isString(filename))
    return openOutputFile(filename.toString())
  });

  env.define('load', ['file'], ([file]: any) => {
    assert(isString(file) || isIdent(file))
    if (isSym(file)) {
      return loadFile(parseLoadSymbol(file), world)
    }
    return loadFile(file.toString(), world)
  });

  env.define('load-from-library', ['file'], ([file]: any) => {
    assert(isString(file) || isIdent(file))
    return loadFromLibrary(file.toString(), world)
  });

  env.define('reload', ['file'], ([file]: any) => {
    assert(isString(file) || isIdent(file))
    if (isSym(file)) {
      return loadFile(parseLoadSymbol(file), world, true)
    }
    return loadFile(file.toString(), world, true)
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
