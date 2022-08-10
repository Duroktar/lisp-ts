import lc from "line-column"
import {Label, Range, Report, ReportKind, Source, mkStringWriter} from "ariadne-ts"
import type { Form } from "./form";
import type { Syntax } from "./callable/syntax";
import { toString, toStringSafe } from "./print";
import { Position } from "./../utils";
import { Port } from "./port";

export type SourceInfo = {
  source: string
  sourceName: string
};

export enum ErrorCode {
  InvalidArgumentError = 1,
  MissingParenthesisError,
  MalformedStringError,
  AssertionError,
  UndefinedVariableError,
}

export class RuntimeWarning extends Error { public retval?: any; }
export class SwitchFallthroughError extends Error { public retval?: any; }

class BaseError extends Error {
  constructor(
    public openIdx: number,
    public closeIdx: number,
  ) {
    super();
  }
  public source!: string
  public sourceName!: string
  withSource(source: string) {
    this.source = source
    return this
  }
  withSourceName(name: string) {
    this.sourceName = name
    return this
  }
}

export class InvalidEnvArgumentsError extends Error {
  constructor(
    public params: Form,
    public args: Form
  ) {
    super('Error: Invalid Env params');
  }
}
export class UndefinedVariableError extends Error {
  static errno: number = ErrorCode.UndefinedVariableError;
  constructor(public name: string, public expr?: Form) {
    super(`Error: undefined variable: ${name}`);
    if (expr && 'token' in expr && expr.token) {
      const msg = mkStringWriter()

      const source = expr.token.port.file.data
      const sourceName = expr.token.port.file.name

      const variableLabel = Label.from([sourceName, expr.token.range]);

      Report.build(ReportKind.Error, sourceName, 34)
        .with_code(UndefinedVariableError.errno)
        .with_message(`undefined variable: '${name.cyan}'`)
        .with_label(variableLabel.with_message('Variable used here'))
        .finish()
        .print([sourceName, Source.from(source)])

      this.message = msg.unwrap()
    }
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
export class MissingParenthesisError extends BaseError {
  static errno: number = ErrorCode.MissingParenthesisError;
  get message() {
    const msg = mkStringWriter()

    const {openIdx, closeIdx, source, sourceName} = this

    const startLabel = Label.from([sourceName, Range.new(openIdx, openIdx + 1)]);
    const expectLabel = Label.from([sourceName, Range.new(closeIdx, closeIdx + 1)]);

    Report.build(ReportKind.Error, sourceName, 34)
      .with_code(MissingParenthesisError.errno)
      .with_message(`Missing '${')'.cyan}'`)
      .with_label(startLabel.with_message(`This where the expression ${'opened'.cyan}.`))
      .with_label(expectLabel.with_message(`This is where it likely should have ${'closed'.blue}.`))
      .finish()
      .print([sourceName, Source.from(source)])

    return msg.unwrap()
  }
}
export class InvalidArgumentError extends BaseError {
  static errno: number = ErrorCode.InvalidArgumentError;
  get message() {
    const msg = mkStringWriter()

    const {openIdx, closeIdx, source, sourceName} = this

    const startLabel = Label.from([sourceName, Range.new(openIdx, openIdx + 1)]);
    const expectLabel = Label.from([sourceName, Range.new(closeIdx, closeIdx + 1)]);

    Report.build(ReportKind.Error, sourceName, 34)
      .with_code(MissingParenthesisError.errno)
      .with_message(`Missing '${')'.cyan}'`)
      .with_label(startLabel.with_message(`This where the expression ${'opened'.cyan}.`))
      .with_label(expectLabel.with_message(`This is where it likely should have ${'closed'.blue}.`))
      .finish()
      .print([sourceName, Source.from(source)])

    return msg.unwrap()
  }
}
export class MalformedStringError extends BaseError {
  static errno: number = ErrorCode.MalformedStringError;
  get message() {
    const msg = mkStringWriter()

    const {openIdx, closeIdx, source, sourceName} = this

    const startLabel = Label.from([sourceName, Range.new(openIdx, openIdx + 1)]);
    const expectLabel = Label.from([sourceName, Range.new(closeIdx, closeIdx + 1)]);

    Report.build(ReportKind.Error, sourceName, 34)
      .with_code(MalformedStringError.errno)
      .with_message(`Missing closing '${'"'.cyan}'`)
      .with_label(startLabel.with_message(`This where the string ${'opened'.cyan}.`))
      .with_label(expectLabel.with_message(`This is where it should have ${'closed'.blue}.`))
      .finish()
      .print([sourceName, Source.from(source)])

    return msg.unwrap()
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
    public message: string,
    public form?: Form,
  ) {
    super(`${message}\t${form ? toStringSafe(form) : ''}`)
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

export const decorateErrorWithSourceInfo = <T>(fn: () => T, port: Port) => {
  try {
    return fn()
  } catch (err) {
    throw err
  }
}
