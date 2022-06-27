import { highlight } from 'cli-highlight';
import colors from 'colors';
import { InvalidArgumentError, program } from 'commander';
import { existsSync, mkdirSync, writeFileSync } from 'fs';
import path, { join } from 'path';
import getPath from 'platform-folders';
import repl from 'pretty-repl';
import { Recoverable, ReplOptions } from 'repl';
import io from 'socket.io-client';
import * as Errors from "./core/data/error";
import { evaluate } from './core/eval';
import * as Lisp from "./core/lisp";
import { toString, toStringSafe } from "./core/print";
import { createServerWorld } from './world/server';
import { iWorld } from "./interface/iWorld";
import * as Utils from "./utils";

const APPDATA = Utils.exists(getPath('appdata'), 'Error looking up appdata directory!');
const HISTORY_FILE_PTH = join(APPDATA, 'lisp-ts', 'repl', 'history', '0.log');

const LANGUAGE_ID = 'scheme'; // for highlighter
const LANGUAGE_VERSION = '1.0';

type TSchemeReplOptions = {
  attach: string | number | null | undefined
  r5rs: boolean
  colors: boolean
}

export const initializeREPL = async (env: iWorld, options: TSchemeReplOptions) => {

  if (existsSync(HISTORY_FILE_PTH) === false) {
      mkdirSync(path.dirname(HISTORY_FILE_PTH), { recursive: true })
      writeFileSync(HISTORY_FILE_PTH, '')
  }

  // await Lisp.execute(`(load "stdlib/r5rs.scm")`, env)

  if (options.colors)
    console.error(`Welcome to ${'lisp-ts'.blue} ${('v' + LANGUAGE_VERSION).yellow}`)
  else
    console.error(`Welcome to lisp-ts ${('v' + LANGUAGE_VERSION)}`)
}

export async function start(options: TSchemeReplOptions) {
  const world = await createServerWorld()

  const prettyOpts = options.colors ? { colorize: colorizer } : {}

  const prompt = options.colors
    ? `${'lisp-ts'.blue}${'.>'.yellow} `
    : 'lisp-ts.> '

  const replOptions: ReplOptions = {
    ...prettyOpts,
    ignoreUndefined: true,
    useColors: options.colors,
    preview: false,
    prompt,
    writer,
  };

  if (options.attach) {
    const getAttachTargetAddress = (val: string | number) => {
      if (typeof val === 'string') return val;
      else return `http://localhost:${val}`;
    }

    const targetAddress = getAttachTargetAddress(options.attach);

    const socket = io(targetAddress)

    let connected = false

    socket.on('connect', () => {
      console.log('connected')
      connected = true
    })

    socket.on('disconnect', () => {
      console.log('disconnected')
      connected = false
    })

    async function _emit(cmd: string, context: any, filename: string, callback: any) {
      try {
        if (!connected) {
          callback(null, '(ERROR "Not connected...")')
        }

        if (cmd === '\n' || cmd === undefined) {
          return callback(null)
        }

        await Lisp.tokenize(cmd, world)
        socket.emit('data', cmd)

        socket.once('data', (result, ...rest) => {
          callback(null, result)
        })
      } catch (err) {
        errorHandler(err, callback)
      }
    }

    replOptions.eval = _emit
  }
  else {
    async function _eval(cmd: string, context: any, filename: string, callback: any) {
      try {
        const x = await Lisp.parse(cmd, world)
        // console.log('`eval_` Parsed:', toString(x))
        const val = await evaluate(x, world.env)
        // console.log('`eval_` Evaluated:', toStringSafe(val))
        callback(null, toString(val))
      } catch (err) {
        // if (err instanceof Errors.RuntimeWarning) {
        //   throw err
        // }
        errorHandler(err, callback)
      }
    }

    replOptions.eval = _eval
  }

  function writer(output: string) {
    if (typeof output !== 'string') {
      return String(output)
    }
    switch (output) {
      case undefined:
        return options.colors ? colors.dim('undefined') : 'undefined'
      default:
        return output.startsWith('Error: ')
          ? output
          : options.colors ? colorizer(output) : output
    }
  }

  await initializeREPL(world, options)

  repl
    .start({ ...replOptions })
    .setupHistory(HISTORY_FILE_PTH, err => { if (err) throw err; })

}

function colorizer(output: string) {
  return highlight(output, {language: LANGUAGE_ID, ignoreIllegals: true})
}

function errorHandler(err: unknown, callback: any): any {
  if (err instanceof Error) {
    if (err instanceof Errors.UndefinedVariableError) {
      if (err.message.endsWith('undefined variable: undefined'))
        return callback(null)
      if (err.message.endsWith('undefined variable: #<eof-object>'))
        return callback(null)
      return callback(null, err.message)
    }
    if (err instanceof Errors.MissingParenthesisError) {
      return callback(new Recoverable(err))
    }
    if (err instanceof Errors.UnexpectedParenthesisError) {
      return callback(null, err.message)
    }
    if (err instanceof Errors.MalformedStringError) {
      return callback(null, err.message)
    }
    return callback(null, err.message)
  }
  return callback(null, err)
}

function parseAttachOption(value: string) {
  // parseInt takes a string and a radix
  const parsedValue = parseInt(value, 10);
  if (!isNaN(parsedValue)) {
    return parsedValue;
  }

  if (value.match(/(ws|www|http:|https:)+[^\s]+[\w]/))
    return value
  throw new InvalidArgumentError('Attach must be a port number or url')
}

const options: TSchemeReplOptions = program
  .name('string-util')
  .description('CLI to some JavaScript string utilities')
  .version('0.8.0')
  .option('-a, --attach <value>', 'Attach to a running TScheme program through websocket.', parseAttachOption)
  .option('--r5rs <boolean>', 'Include the r5rs standard library.', true)
  .option('--colors <boolean>', 'Use colors in the terminal output.', v => v === 'false' ? false : true)
  .parse()
  .opts()


start(options).catch(console.error)
