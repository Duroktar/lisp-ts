import { highlight } from 'cli-highlight';
import colors from 'colors';
import { existsSync, mkdirSync, writeFileSync } from 'fs';
import path, { join } from 'path';
import getPath from 'platform-folders';
import repl from 'pretty-repl';
import { Recoverable } from 'repl';
import * as Utils from "./utils";
import * as Errors from "./core/error";
import * as Lisp from "./core/lisp";
import { toString } from "./core/toString";
import { evaluate } from './core/eval';
import { createEnvironment, Environment } from './env';

const APPDATA = Utils.exists(getPath('appdata'), 'Error looking up appdata directory!');
const HISTORY_FILE_PTH = join(APPDATA, 'lisp-ts', 'repl', 'history', '0.log');

const LANGUAGE_ID = 'scheme';
const LANGUAGE_VERSION = '1.0';

export const initializeREPL = async (env: Environment) => {

  if (existsSync(HISTORY_FILE_PTH) === false) {
      mkdirSync(path.dirname(HISTORY_FILE_PTH), { recursive: true })
      writeFileSync(HISTORY_FILE_PTH, '')
  }

  await Lisp.execute(`(load "stdlib/r5rs.scm")`, env)

  console.error(`Welcome to ${'lisp-ts'.blue} ${('v' + LANGUAGE_VERSION).yellow}`)
}

export const start = async (prompt: string) => {
  const env = createEnvironment()

  async function _eval(cmd: string, context: any, filename: string, callback: any) {
    try {
      const x = await Lisp.parse(cmd, env)
      const val = await evaluate(x, env.env)
      callback(null, toString(val))
    } catch (err) {
      errorHandler(err, callback)
    }
  }

  function colorizer(output: string) {
    return highlight(output, {language: LANGUAGE_ID, ignoreIllegals: true})
  }

  function writer(output: string) {
    if (typeof output !== 'string') {
      return String(output)
    }
    switch (output) {
      case undefined:
        return colors.dim('undefined')
      default:
        return output.startsWith('Error: ')
          ? output
          : colorizer(output)
    }
  }

  function errorHandler(err: unknown, callback: any): any {
    if (err instanceof Error) {
      if (err instanceof Errors.UndefinedVariableError) {
        if (err.message.endsWith('undefined variable: undefined'))
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

  const prettyOpts = { colorize: colorizer }

  await initializeREPL(env)

  repl
    .start({ eval: _eval, writer, prompt, ignoreUndefined: true, ...prettyOpts })
    .setupHistory(HISTORY_FILE_PTH, err => { if (err) throw err; })
}

start(`${'lisp-ts'.blue}${'.>'.yellow} `)
  .catch(console.error)
