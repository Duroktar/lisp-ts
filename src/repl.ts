import colors from 'colors'
import getPath from 'platform-folders';
import repl from 'pretty-repl'
import { Recoverable } from 'repl'
import { highlight } from 'cli-highlight';
import { Errors, Lisp, Runtime, Utils } from './bootstrap';
import path, { join } from 'path';
import { existsSync, mkdirSync, writeFileSync } from 'fs';

const APPDATA = Utils.assert(getPath('appdata'), 'Error looking up appdata directory!');

export namespace Repl {

  const LANGUAGE_ID = 'scheme';
  const LANGUAGE_VERSION = '1.0';
  const HISTORY_FILE_PTH = join(APPDATA, 'lisp-ts', 'repl', 'history', '0.log');

  export const initializeREPL = () => {
    if (existsSync(HISTORY_FILE_PTH) === false) {
        mkdirSync(path.dirname(HISTORY_FILE_PTH), { recursive: true })
        writeFileSync(HISTORY_FILE_PTH, '')
    }

    console.error(`Welcome to ${'lisp-ts'.blue} ${('v' + LANGUAGE_VERSION).yellow}`)
  }

  export const start = (prompt: string) => {

    function _eval(cmd: string, context: any, filename: string, callback: any) {
      try {
        const x = Lisp.parse(cmd, Runtime.env)
        const val = Lisp.evaluate(x, Runtime.env)
        callback(null, Utils.toString(val))
      } catch (err) {
        errorHandler(err, callback)
      }
    }

    function colorizer(output: string) {
      return highlight(output, {language: LANGUAGE_ID, ignoreIllegals: true})
    }

    function writer(output: string) {
      switch (output) {
        case undefined:
          return colors.dim('undefined')
        default:
          return output.startsWith('Error: ')
            ? '\n' + output
            : colorizer(output)
      }
    }

    function errorHandler(err: unknown, callback: any): any {
      if (err instanceof Error) {
        if (err instanceof Errors.UndefinedVariableError) {
          return callback(null, err.message)
        }
        if (err instanceof Errors.MissingParenthesisError) {
          return callback(new Recoverable(err))
        }
        if (err instanceof Errors.UnexpectedParenthesisError) {
          return callback(null, err.formattedError)
        }
        if (err instanceof Errors.MalformedStringError) {
          return callback(null, err.formattedError)
        }
        return callback(null, err.message)
      }
      return callback(null, err)
    }

    const prettyOpts = { colorize: colorizer }

    initializeREPL()

    repl
      .start({ eval: _eval, writer, prompt, ignoreUndefined: true, ...prettyOpts })
      .setupHistory(HISTORY_FILE_PTH, err => { if (err) throw err; })
  }
}

Repl.start(`${'lisp-ts'.blue}${'.>'.yellow} `)
