import { InPort, IOPort, OutPort } from "../core/port";
import { SocketClient } from "../core/port/Socket/client";
import { SocketServer } from "../core/port/Socket/server";
import { ServerSourceFile, StdIn, StdOut } from "../core/port/File/server";
import { isCallable, isIdent, isString, isSym } from "../guard";
import { loadFile, loadFromLibrary, parseLoadSymbol } from "../core/load";
import { assert, sequence } from "../utils";
import { toString } from "../core/print";
import { Str } from "../core/data/string";
import { iEnv } from "../interface/iEnv";
import { car } from "../core/lisp";
import { evaluate } from "../core/eval";
import { list } from "../core/data/pair";

export function addServerFeatures(env: iEnv) {

  env.set('#cwd', Str(process.cwd()));

  env.define('error', 'args', ([...args]: any) => {
    console.log('heEEEEY')
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

  env.define('with-input-from-file', ['string', 'thunk'], ([string, thunk]: any) => {
    assert(isString(string), 'with-input-from-file "string" arg should be a string', string)
    assert(isCallable(thunk), 'with-input-from-file "thunk" arg should be a function', thunk)
    const port = openInputFile(string.toString())
    const previousPort = env.get('*current-input-port*')
    env.set('*current-input-port*', port)
    const rv = evaluate(list(thunk), env)
    port.close()
    env.set('*current-input-port*', previousPort)
    return rv
  });

  env.define('load', ['file'], ([file]: any) => {
    assert(isString(file) || isIdent(file))
    if (isSym(file)) {
      return loadFile(parseLoadSymbol(file.value), env)
    }
    return loadFile(file.toString(), env)
  });

  env.syntax('load-from-library', (args, env) => {
    const [file] = sequence(car, args)
    assert(isString(file) || isIdent(file), 'load-from-library expects a string or identifier', file)
    return loadFromLibrary(file.toString(), env)
  });

  env.define('reload', ['file'], ([file]: any) => {
    assert(isString(file) || isIdent(file))
    if (isSym(file)) {
      return loadFile(parseLoadSymbol(file.value), env, true)
    }
    return loadFile(file.toString(), env, true)
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
