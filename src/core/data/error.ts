import lc from "line-column"
import type { Form } from "../form";
import type { Syntax } from "../callable/macro/syntax";
import { toString, toStringSafe } from "../print";
import { Position } from "../../utils";

export class RuntimeWarning extends Error { public retval?: any; }

export class InvalidEnvArgumentsError extends Error {
  constructor(
    public params: Form,
    public args: Form
  ) {
    super('Error: Invalid Env params');
  }
}
export class UndefinedVariableError extends Error {
  constructor(name: string) {
    super(`Error: undefined variable: ${name}`);
  }
}
export class InvalidCallableExpression extends Error {
  constructor(expr: Form) {
    super(`Error: expression is not callable: ${toString(expr, true)}`);
  }
}
export class UnexpectedParenthesisError extends Error {
  constructor() {
    super(`Error: Unexpected ")"`);
  }
}
export class MissingParenthesisError extends Error {
  constructor() {
    super(`Error: Missing ')'`);
  }
}
export class MalformedStringError extends Error {
  constructor(public cursor: number, public source: string) {
    const pos = { ...lc(source).fromIndex(cursor - 1)!, cursor }
    super(formatError(source, {start: pos, end: pos, message: `Error: Missing '"'`}));
  }
}
export class InvalidCharacterError extends Error {
  constructor(public cursor: number, public source: string, char: string) {
    const pos = { ...lc(source).fromIndex(cursor - 1)!, cursor }
    super(formatError(source, {start: pos, end: pos, message: `Error: Invalid Character ${char}`}));
  }
}
export class MatchError extends Error {
  constructor(
    public def: Syntax,
    public form: Form,
  ) {
    super(`no matches found for pattern: ${toStringSafe(form)}`)
  }
}

export class SocketServerUnavailableError extends Error {
  constructor() {
    super('Server not available on the client')
  }
}

export class AssertionError extends Error {
  constructor(
    msg?: string,
  ) {
    super(msg)
  }

}

export class NotImplementedError extends Error {}

export class InputError extends Error {
  constructor(
    public def: Syntax,
    public form: Form,
  ) {
    super()
  }
}

export type FormatErrorOptions = {
  message?: string;
  start: Position;
  end: Position;
};

export function formatError(source: string, options: FormatErrorOptions) {
  const lines = source.split('\n');
  const res: string[] = [];
  for (let lineNo = lines.length - 1; lineNo >= 0; lineNo--) {
    if (options.end.line === options.start.line) {
      const fst = '^'.padStart(options.start.col, ' ');
      const snd = '^'.padStart(options.end.col - (options.start.col + 1), ' ');
      if (lineNo === options.end.line - 1) {
        res.push(`${fst}${snd}`);
      }
    }
    else if (lineNo === options.start.line - 1) {
      res.push('^'.padStart(options.start.col, ' '));
    }
    else if (lineNo === options.end.line - 1) {
      res.push('^'.padStart(options.end.col, ' '));
    }
    res.push(lines[lineNo]);
  }
  return `${options.message}\n\n${res.reverse().join('\n')}`;
}
